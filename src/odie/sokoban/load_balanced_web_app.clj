(ns odie.sokoban.load-balanced-web-app
  (:require [clojure.java.io :as io]
            [odie.sokoban.aws-utils :as au]
            [odie.sokoban.utils :as u]
            [cognitect.aws.client.api :as aws]
            [odie.sokoban.globals :as g]))

;; Steps
;; 1. Setup roles
;; 2. Setup common resources
;; 3. Setup load balanced web service


(defn setup-roles--req-data [{:keys [app-name account-id]}]
  {:StackName (str app-name "-infrastructure-roles")
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
   :Tags (au/->tags {:sokoban-application app-name})})


(defn setup-common-infrastructure--req-data [params]
  (let [{:keys [AppName EnvironmentName]} params]
    {:StackName (format "%s-%s" AppName EnvironmentName)
     :Capabilities ["CAPABILITY_NAMED_IAM"]
     :TemplateBody (slurp (io/resource "cf-templates/shared.yml"))
     :Parameters (au/->params params)
     :Tags (au/->tags {:sokoban-application AppName
                       :sokoban-environment EnvironmentName})}))

(defn cf-stack-ensure
  "Create or update the stack to the current state.

  If nothing needs to be created/updated, AWS will return an error that looks like:

  Note the returned value could either be from a :CreateStack call or a :UpdateStack call
  "
  [client req]

  ;; Try to create the cf stack
  (let [reply (aws/invoke cf {:op :CreateStack
                              :request req})]

    ;; If the stack already exists, try to update it
    (if (= (get-in reply [:ErrorResponse :Error :Code]) "AlreadyExistsException")

      ;; Call :UpdateStack and return the reply
      (aws/invoke cf {:op :UpdateStack
                      :request req})

      ;; Otherwise, we're going to assume nothing went wrong.
      ;; Answer with the reply of :CreateStack
      reply)))

(defn temp []
  (let [environment "test"
        app-name (:app-name @g/app-context)
        account-id (:account-id @g/app-context)
        params {:AppName app-name
                :EnvironmentName environment
                :ALBWorkloads "api"
                ;; EFSWorkloads:
                ;; Type: String
                ;; NATWorkloads:
                ;; Type: String
                :ToolsAccountPrincipalARN (str "arn:aws:iam::" account-id ":root")
                ;; AppDNSName:
                ;; Type: String
                ;; AppDNSDelegationRole:
                ;; Type: String
                ;; Aliases:
                ;; Type: String
                :CreateHTTPSListener "false"
                :ServiceDiscoveryEndpoint (format "%s.%s.%s" environment app-name "local")
                }
        req (setup-common-infrastructure--req-data params)
        spec? (u/on-spec? (aws/request-spec-key cf :CreateStack) req)]

    (if (true? spec?)
      (cf-create-stack-ensure cf req)
      spec?)
    )
  )

(comment
  (def cf (aws/client {:api :cloudformation
                       :credentials-provider @g/credentials-provider}))

  (aws/validate-requests cf)

  ;; Step 1: Setup roles

  (let [req (setup-roles--req-data (select-keys @g/app-context [:app-name :account-id]))
        spec? (u/on-spec? (aws/request-spec-key cf :CreateStack) req)]
    (if (true? spec?)
      (cf-create-stack-ensure cf req)
      spec?))

  ;; Step 2: Setup common infrastructure

  (let [environment "test"
        app-name (:app-name @g/app-context)
        account-id (:account-id @g/app-context)
        params {:AppName app-name
                :EnvironmentName environment
                :ALBWorkloads "api"
                ;; EFSWorkloads:
                ;; Type: String
                ;; NATWorkloads:
                ;; Type: String
                :ToolsAccountPrincipalARN (str "arn:aws:iam::" account-id ":root")
                ;; AppDNSName:
                ;; Type: String
                ;; AppDNSDelegationRole:
                ;; Type: String
                ;; Aliases:
                ;; Type: String
                :CreateHTTPSListener "false"
                :ServiceDiscoveryEndpoint (format "%s.%s.%s" environment app-name "local")
                }
        req (setup-common-infrastructure--req-data params)
        spec? (u/on-spec? (aws/request-spec-key cf :CreateStack) req)]

    (if (true? spec?)
      (cf-create-stack-ensure cf req)
      spec?)
    ;; req
    )

  )
