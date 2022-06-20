(ns odie.sokoban.aws-utils
  "Utilities that deal specifically with AWS data"
  (:require [clojure.string :as str]
            [clojure.walk :refer [postwalk]]
            [odie.sokoban.utils :as u]
            [odie.sokoban.globals :as g]
            [cognitect.aws.client.api :as aws]
            [cognitect.aws.service :as aws-service]
            ))


(defn arn? [s]
  (str/starts-with? s "arn:aws:"))

(defn collect-arn [coll]
  (let [res (transient [])]
    (postwalk
     (fn [x]
       ;; Collect any items that look like an arn
       (when (arn? x)
         (conj! res x))

       ;; always return the thing itself
       x)
     coll)
    (persistent! res)))

(defn arn->map
  "Parse an ARN string `s` into its component parts, represented as a map"
  [s]
  (let [base-format (str/replace s #"[a-zA-Z0-9\-]" "")]

    (case base-format
      ":::::"
      (zipmap
       [:partition :service :region :account-id :resource-id]
       (drop 1 (str/split s #":")))

      "::::::"
      (zipmap
       [:partition :service :region :account-id :resource-type :resource-id]
       (drop 1 (str/split s #":")))

      ":::::/"
      (zipmap
       [:partition :service :region :account-id :resource-type :resource-id]
       (let [vals (drop 1 (str/split s #":"))
             last-vals (u/str-split-first (last vals) "/")]
         (concat (butlast vals) last-vals))))))

(defn aws-error?
  "Check if a reply from AWS represents some kind of error."
  [o]
  (= (:cognitect.anomalies/category o) :cognitect.anomalies/incorrect))

(defn aws-client [api-kw]
  (aws/client {:api api-kw
               :credentials-provider @g/credentials-provider}))


(defmacro aws-when-let
  "Like when-let, but only continues with body if the aws/invoke call in the
  bindings succeeds. Otherwise, return the error."
  {:added "1.0"}
  [bindings & body]
  (u/assert-args
   (vector? bindings) "a vector for its binding"
   (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [form (bindings 0)
        tst (bindings 1)]
    `(let [temp# ~tst]
       (if (aws-error? temp#)
         temp#
         (let [~form temp#]
           ~@body)))))

(defmacro aws-when-let*
  "bindings => binding-form test
  When test is true, evaluates body with binding-form bound to the value of test"
  [bindings & body]
  (u/assert-args
   (vector? bindings) "a vector for its binding"
   (< 1 (count bindings)) "at least 2 forms in binding vector"
   (= 0 (mod (count bindings) 2)) "even number of forms in binding vector")
  (let [form (bindings 0) tst (bindings 1) more (vec (drop 2 bindings))]
    `(let [temp# ~tst]
       (if (or (aws-error? temp#) (nil? temp#))
         temp#
         (let [~form temp#]
           ~(if (seq more)
              `(aws-when-let* ~more ~@body)
              `(do ~@body)))))))

(defn aws-ops
  "Given a client, list all the available operation as a list of keywords. This is useful for interactive exploration."
  [client]
  (sort (keys (aws/ops client))))

(defn param
  "Given a kv, shape it to be accepted as parameters when instantiating a CloudFormation template."
  [key-name value]
  {:ParameterKey (name key-name)
   :ParameterValue
   (cond
     (keyword? value)
     (name value)
     :else
     value)})

(defn ->params
  "Given a map, shape it to be accepted as parameters when instantiating a CloudFormation template. "
  [m]
  (map (fn [[k v]] (param k v)) m))

(defn tag
  "Given a kv, shape it to be accepted as tags in AWS API calls"
  [key-name value]
  {:Key (name key-name)
   :Value (if (keyword? value)
            (name value)
            value)})

(defn ->tags
  "Given a map, shape it to be accepted as tags in AWS API calls"
  [m]
  (map (fn [[k v]] (tag k v)) m))

(defn tags-get
  "Given an list of tags sent back by aws, try to retrieve the :value of the given keyname."
  [tags keyname]

  (let [pair (u/find-first #(= (or (:key %) (:Key %)) keyname) tags)]
    (:value pair)))

(defn tags->map
  "Given a list of tags, return the equivalent map"
  [tags]
  (->> tags
       (map #(vector (keyword (or (:key %) (:Key %))) (or (:value %) (:Value %))))
       (into {})))

(defn env-var [var-name var-val]
  {:Name var-name
   :Value var-val})

(defn ->env-vars
  "Given a map of var-name=>var-val, shape it to be accepted as tags in AWS API calls"
  [vars-map]
  (map (fn [[k v]] (env-var k v) ) vars-map))


;; As per AWS documentation
;; A resource status can be any of the following states
;;
;; Create in progress – The resource is in the process of being created in AWS.
;;
;; Create complete – The resource has been successfully created in AWS.
;;
;; Create failed – The resource could not be created in AWS.
;;
;; Update in progress – The resource is in the process of being updated in AWS.
;;
;; Update complete – The resource was successfully updated in AWS.
;;
;; Update failed – The resource could not be updated in AWS.
;;
;; Delete in progress – The resource is in the process of being deleted in AWS.
;;
;; Delete complete – The resource has been deleted in AWS.
;;
;; Rollback in progress – An operation has failed and AWS CloudFormation is attempting to restore the resource to its previous state.
;;
;; Rollback failed – A rollback has failed. The AWS resources in a CloudFormation stack that have this status are in an inconsistent state. You may have to delete and recreate the stack.
(defn parse-status
  "Split a resource status string into `:action` and `:status` parts.
  Hopefully, this makes it slightly easier to reason about."
  [s]
  (let [parts (str/split s #"_")]
    {:action (keyword (str/lower-case (first parts)))
     :state (keyword (str/lower-case (str/join "-" (rest parts))))}))

(defn parse-status-replace
  [key-name m]
  (assoc m key-name
         (parse-status (key-name m))))

(defn status-finished?
  "Does the status represent something that has reached a stable condition?
  If AWS reports something is `:complete` or `:failed`, we take this to mean
  no more actions will be performed.

  Note that it's possible for a resource move into the `:rollback` action
  if either `:create` or `:update` fails. We are not able to detect such a
  distinction here, however."
  [status]
  (let [state (:state status)]
    (if (or (= state :complete)
            (= state :failed))
      true
      false)))


(defn status-transitioned-into-failure-state?
  [s1 s2]
  (if (and (= (:state s1) :in-progress)
           (= (:state s2) :failed))
    true
    false))
