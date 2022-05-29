(ns odie.sokoban.aws-utils
  "Utilities that deal specifically with AWS data"
  (:require [clojure.string :as str]
            [clojure.walk :refer [postwalk]]
            [clojure.spec.alpha :as s]
            [odie.sokoban.utils :as u]
            [cognitect.aws.client.api :as aws]
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
   :Value (name value)})

(defn ->tags
  "Given a map, shape it to be accepted as tags in AWS API calls"
  [m]
  (map (fn [[k v]] (tag k v)) m))
