(ns odie.sokoban.dev
  "Temporary namespace used for experimentation and putting the program together"
  (:require [cognitect.aws.client.api :as aws]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [cognitect.aws.credentials :as credentials]
            [odie.sokoban.globals :as g]
            [odie.sokoban.utils :as u]
            [odie.sokoban.aws-utils :as au]
            ))

(defn credentials-filepath []
  (u/expand-home "~/.sokoban/credentials.edn"))

(defn load-credentials []
  (->> (credentials-filepath)
       slurp
       edn/read-string))

(defn save-credentials [data]
  (u/write-edn (credentials-filepath) data))

(defn credential-by-name
  ([creds]
   (credential-by-name [creds "default"]))

  ([creds cred-name]
   (if (= cred-name "default")
     (let [default-name (get creds "default")]
       (get creds default-name))
     (get creds cred-name))))

(defn load-envs []
  (->> (u/expand-home "~/.sokoban/env.edn")
       slurp
       edn/read-string))

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
     :Parameters (au/->params {:AdminRoleName (str app-name "-AdminRole")
                               :ExecutionRoleName (str app-name "-ExecutionRole")
                               :DNSDelegationRoleName (str app-name "-DNSDelegationRole")
                               :AppDNSDelegatedAccounts account-id
                               ;; :AppDomainName "kengoson.com"
                               ;; :AppDomainHostedZoneID:
                               :AppName app-name
                               })
     :Tags (au/->tags {:sokoban-application app-name})}))


(defn setup-credential-provider! [credentials cred-name]
  (let [cred (credential-by-name credentials cred-name)]
    (reset! g/credentials-provider (credentials/basic-credentials-provider cred))))

(defn fetch-account-id []
  (let [sts (aws/client {:api :sts :credentials-provider @g/credentials-provider})
        aws-identity (aws/invoke sts {:op :GetCallerIdentity})]
    (:Account aws-identity)))

(defn fetch-account-id! [context]
  (swap! context assoc :account-id (fetch-account-id)))

(defn set-active-credential!
  [cred-name]
  (let [new-cred-name (if (= cred-name "default")
                        (get @g/credentials "default")
                        cred-name)]
    (reset! g/active-credential-name new-cred-name)
    (setup-credential-provider! @g/credentials new-cred-name)))

(defn start-app! []
  ;; Load up app-wide credentials store
  ;; It's a bit strange to load up ALL the credentials though
  (reset! g/credentials (load-credentials))

  ;; Load up the default credentials
  (set-active-credential! "default")

  ;; (setup-credential-provider! g/credentials-provider @g/credentials "default")

  ;; Make sure we have the account-id loaded
  (when-not (get-in @g/credentials [@g/active-credential-name :account-id])
    ;; If we don't have it, fetch it now
    (if-let [account-id (fetch-account-id)]
      (do
        ;; Save it back into the settings file
        (swap! g/credentials assoc-in [@g/active-credential-name :account-id] account-id)
        (save-credentials @g/credentials)

        ;; Make account-id available as part of the app-context
        (swap! g/app-context assoc :account-id (fetch-account-id))

        )
      (throw (ex-info "Cannot fetch account-id" {}))))

  (swap! g/app-context assoc :account-id (get-in @g/credentials [@g/active-credential-name :account-id])))

(defn dev-start-app! []
  (start-app!)

  ;; Setup the name of the app we're working with
  ;; This will determine the name of the cluster.
  ;; It will also be prefixed on various resources so it's easy
  ;; to understand they ought to be grouped together.
  (g/set-app-name! "kengo")
  )

(comment
  (reset! g/app-context (empty @g/app-context))

  (dev-start-app!)

  (setup-roles--req-data @g/app-context)

  ;; First let's establish our identity
  ;; - Who are we running as?
  ;; - Which account are we attached to?

  ;;---------------------------------------------
  ;;
  ;; STS
  ;;
  (def sts (au/aws-client {:api :sts}))

  (def aws-identity
    (aws/invoke sts {:op :GetCallerIdentity}))

  (def account-id (:Account aws-identity))

  ;; MOVEME! This should come from some data file
  (def app-name "kengo")

  ;;---------------------------------------------
  ;;
  ;; S3
  ;;

  (def s3 (au/aws-client {:api :s3}))

  (->> (aws/ops s3)
       keys)

  ;;---------------------------------------------
  ;;
  ;; EC2
  ;;
  (def ec2 (au/aws-client {:api :ec2}))

  (->> ec2
       aws/ops
       keys
       (filter #(u/key-starts-with? % "Describe"))
       )

  ;;---------------------------------------------
  ;;
  ;; ECS
  ;;
  (def ecs (au/aws-client {:api :ecs}))

  (->> ecs
       aws/ops
       keys
       ;; (filter #(key-starts-with? % "Describe"))
       )

  (def clusters
    (aws/invoke ecs {:op :ListClusters}))

  (aws/doc ecs :DescribeClusters)

  (au/collect-arn clusters)

  (slurp (io/resource "cf-templates/infrastructure-roles.yml"))


  ;;---------------------------------------------
  ;;
  ;; Cloud formation
  ;;
  (def cf (aws/client {:api :cloudformation
                       :credentials-provider @g/credentials-provider}))

  (aws/validate-requests cf)

  (->> (au/aws-ops cf)
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
     :Parameters (au/->params {:AdminRoleName (str app-name "-adminrole")
                            :ExecutionRoleName (str app-name "-executionrole")
                            :DNSDelegationRoleName (str app-name "-DNSDelegationRole")
                            :AppDNSDelegatedAccounts account-id
                            ;; :AppDomainName "kengoson.com"
                            ;; AppDomainHostedZoneID:
                            :AppName app-name
                            })
     :Tags (au/->tags {:sokoban-application "test"})})

  (u/on-spec? (aws/request-spec-key cf :CreateStack)
             req-data)


  (aws/invoke cf {:op :CreateStack
                  :request
                  req-data})

  (aws/invoke cf {:op :UpdateStack
                  :request
                  req-data})

  (u/on-spec? (aws/request-spec-key cf :CreateStack)
            (setup-roles--req-data @g/app-context)

            )

  (aws/invoke cf {:op :CreateStack
                  :request (setup-roles--req-data @g/app-context)})

 )
