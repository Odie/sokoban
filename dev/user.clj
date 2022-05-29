(ns user
  (:require [clojure.string :as str]
            [cognitect.aws.client.api :as aws]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [cognitect.aws.credentials :as credentials]
            [clojure.spec.alpha :as s]
            [clojure.walk :refer [postwalk]]
            [phrase.alpha :refer [phrase-first]]
            [odie.globals :as g]
            ))

(defn key-starts-with? [k s]
  (str/starts-with? (name k) s))

(defn expand-home [s]
  (if (.startsWith s "~")
    (clojure.string/replace-first s "~" (System/getProperty "user.home"))
    s))

(defn load-credentials []
  (->> (expand-home "~/.sokoban/credentials.edn")
       slurp
       edn/read-string))

(defn credential-by-name
  ([creds]
   (credential-by-name [creds "default"]))

  ([creds cred-name]
   (if (= cred-name "default")
     (let [default-name (get creds "default")]
       (get creds default-name))
     (get creds cred-name))))

(defn load-envs []
  (->> (expand-home "~/.sokoban/env.edn")
       slurp
       edn/read-string))

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

(defn str-split-at [s idx]
  (list (subs s 0 idx) (subs s idx)))

(defn str-split-first
  "Split the string using the first occurrence of the `substr`"
  [s substr]
  (let [idx (str/index-of s substr)]
    (list (subs s 0 idx) (subs s (+ idx (count substr))))))

(defn arn->map [s]
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
             last-vals (str-split-first (last vals) "/")]
         (concat (butlast vals) last-vals))))))

(defn aws-ops [client]
  (keys (aws/ops client)))

(defn param [key-name value]
  {:ParameterKey (name key-name)
   :ParameterValue
   (cond
     (keyword? value)
     (name value)
     :else
     value)})

(defn ->params [m]
  (map (fn [[k v]] (param k v)) m))

(defn tag [key-name value]
  {:Key (name key-name)
   :Value (name value)})

(defn ->tags [m]
  (map (fn [[k v]] (tag k v)) m))

(defn on-spec? [spec data]
  ;; Is the data valid according to the spec?
  (if (s/valid? spec data)
    ;; If so... just say it's okay...
    true

    ;; Otherwise, say why it's not on spec
    (s/explain-data {} spec data)))

;; ;; MOVEME! This should come from some data file
;; ;; (def default-credentials-name "kengo")
;; ;; (def creds (load-credentials default-credentials-name))
;; ;; (def cred-provider
;; ;;   (credentials/basic-credentials-provider
;; ;;    (load-credentials default-credentials-name)))

(defn setup-roles--req-data [context]
  (let [{:keys [app-name account-id]} context]
    {:StackName (str app-name "-infrastructure-roles")
     :Capabilities ["CAPABILITY_NAMED_IAM"]
     :TemplateBody (slurp (io/resource "cf-templates/infrastructure-roles.yml"))
     :Parameters (->params {:AdminRoleName (str app-name "-adminrole")
                            :ExecutionRoleName (str app-name "-executionrole")
                            :DNSDelegationRoleName (str app-name "-DNSDelegationRole")
                            :AppDNSDelegatedAccounts account-id
                            ;; :AppDomainName "kengoson.com"
                            ;; AppDomainHostedZoneID:
                            :AppName app-name
                            })
     :Tags (->tags {:sokoban-application app-name})}))


(defn setup-credential-provider! [context credentials cred-name]
  (let [cred (credential-by-name credentials cred-name)]
    (reset! g/credentials-provider (credentials/basic-credentials-provider cred))))

(defn fetch-account-id []
  (let [sts (aws/client {:api :sts :credentials-provider @g/credentials-provider})
        aws-identity (aws/invoke sts {:op :GetCallerIdentity})]
    (:Account aws-identity)))

(defn start-app []
  ;; Load up app-wide credentials store
  ;; It's a bit strange to load up ALL the credentials though
  (reset! g/credentials (load-credentials))

  ;; Load up the default credentials
  (setup-credential-provider! g/app-context @g/credentials "default")

  (swap! g/app-context assoc :account-id (fetch-account-id)))


(comment
  (reset! g/app-context (empty @g/app-context))

  (start-app)

  ;; Try to load a credential from the data from the settings file
  (let [cred (credential-by-name @credentials "kengo")]
    (swap! app-context assoc :credential cred))

  ;; Setup the name of the app we're working with
  ;; This will determine the name of the cluster.
  ;; It will also be prefixed on various resources so it's easy
  ;; to understand they ought to be grouped together.
  (g/set-app-name! "kengo")

  (setup-roles--req-data @g/app-context)

  ;; First let's establish our identity
  ;; - Who are we running as?
  ;; - Which account are we attached to?

  ;;---------------------------------------------
  ;;
  ;; STS
  ;;
  (def sts (aws/client {:api :sts
                        :credentials-provider cred-provider}))

  (def aws-identity
    (aws/invoke sts {:op :GetCallerIdentity}))

  (def account-id (:Account aws-identity))

  ;; MOVEME! This should come from some data file
  (def app-name "kengo")

  ;;---------------------------------------------
  ;;
  ;; S3
  ;;

  (def s3 (aws/client {:api :s3
                       :credentials-provider cred-provider}))

  (->> (aws/ops s3)
       keys)

  ;;---------------------------------------------
  ;;
  ;; EC2
  ;;
  (def ec2 (aws/client {:api :ec2
                        :credentials-provider cred-provider}))

  (->> ec2
       aws/ops
       keys
       (filter #(key-starts-with? % "Describe"))
       )

  ;;---------------------------------------------
  ;;
  ;; ECS
  ;;
  (def ecs (aws/client {:api :ecs
                        :credentials-provider cred-provider}))

  (->> ecs
       aws/ops
       keys
       ;; (filter #(key-starts-with? % "Describe"))
       )

  (def clusters
    (aws/invoke ecs {:op :ListClusters}))

  (aws/doc ecs :DescribeClusters)

  (collect-arn clusters)

  (slurp (io/resource "cf-templates/infrastructure-roles.yml"))


  ;;---------------------------------------------
  ;;
  ;; Cloud formation
  ;;
  (def cf (aws/client {:api :cloudformation
                       :credentials-provider @g/credentials-provider}))

  (aws/validate-requests cf)

  (->> (aws-ops cf)
       sort)

  ;; Show the documentation for the CreateStack endpoint
  (aws/doc cf :CreateStack)
  (aws/doc cf :UpdateStack)
  (aws/doc cf :DeleteStack)
  (aws/doc cf :ListStacks)

  (aws/invoke cf {:op :ListStacks})

  ;; Put together an outgoing request
  (def req-data
    {:StackName "sokoban-test-stack"
     :Capabilities ["CAPABILITY_NAMED_IAM"]
     :TemplateBody (slurp (io/resource "cf-templates/infrastructure-roles.yml"))
     :Parameters (->params {:AdminRoleName (str app-name "-adminrole")
                            :ExecutionRoleName (str app-name "-executionrole")
                            :DNSDelegationRoleName (str app-name "-DNSDelegationRole")
                            :AppDNSDelegatedAccounts account-id
                            ;; :AppDomainName "kengoson.com"
                            ;; AppDomainHostedZoneID:
                            :AppName app-name
                            })
     :Tags (->tags {:sokoban-application "test"})})

  (on-spec? (aws/request-spec-key cf :CreateStack)
             req-data)


  (aws/invoke cf {:op :CreateStack
                  :request
                  req-data})

  (aws/invoke cf {:op :UpdateStack
                  :request
                  req-data})

  (on-spec? (aws/request-spec-key cf :CreateStack)
            (setup-roles--req-data @g/app-context)

            )

  (aws/invoke cf {:op :CreateStack
                  :request (setup-roles--req-data @g/app-context)})

  {
   :StackName "sokoban-test-stack"
   ;; :TemplateBody (slurp (io/resource "cf-templates/infrastructure-roles.yml"))
   :Parameters (->params {:sokoban-application "test app"})
   :Tags (->tags {:sokoban-application "test"})
   }

 )
