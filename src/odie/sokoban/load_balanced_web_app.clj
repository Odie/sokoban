(ns odie.sokoban.load-balanced-web-app
  (:require [clojure.java.io :as io]
            [odie.sokoban.aws-utils :as au]
            [odie.sokoban.utils :as u]
            [cognitect.aws.client.api :as aws]
            [odie.sokoban.globals :as g]
            [odie.sokoban.dev :as dev]
            ))

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


(defn setup-ECS-Fargate-infrastructure--req-data [params]
  (let [{:keys [AppName EnvironmentName]} params]
    {:StackName (format "%s-%s" AppName EnvironmentName)
     :Capabilities ["CAPABILITY_NAMED_IAM"]
     :TemplateBody (slurp (io/resource "cf-templates/ECS-Fargate.yml"))
     :Parameters (au/->params params)
     :Tags (au/->tags {:sokoban-application AppName
                       :sokoban-environment EnvironmentName})}))

(defn setup-ECS-EC2-infrastructure--req-data [params]
  (let [{:keys [AppName EnvironmentName]} params]
    {:StackName (format "%s-%s" AppName EnvironmentName)
     :Capabilities ["CAPABILITY_NAMED_IAM"]
     :TemplateBody (slurp (io/resource "cf-templates/ECS-EC2.yml"))
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
  (let [reply (aws/invoke client {:op :CreateStack
                                  :request req})]

    ;; If the stack already exists, try to update it
    (if (= (get-in reply [:ErrorResponse :Error :Code]) "AlreadyExistsException")

      ;; Call :UpdateStack and return the reply
      (aws/invoke client {:op :UpdateStack
                          :request req})

      ;; Otherwise, we're going to assume nothing went wrong.
      ;; Answer with the reply of :CreateStack
      reply)))

(defn cf-fetch-stack-status [cf stackname]
  ;; TODO! If a "NextToken" is present in the reply,
  ;; we should make more requests to assemble the full
  ;; status of the stack
  (aws/invoke cf {:op :ListStackResources
                  :request {:StackName stackname}}))

(defn resource-created? [resource-summary]
  (= (:ResourceStatus resource-summary) "CREATE_COMPLETE"))

(defn resource-deleted? [resource-summary]
  (let [status (:ResourceStatus resource-summary)]
    (or (= status "DELETE_COMPLETE")
        (= status "DELETE_SKIPPED"))))

(defn cf-watch-for-stack-completion [cf stackname]
  (let [start-time (System/nanoTime)
        result (loop []
                 (let [reply (cf-fetch-stack-status cf stackname)
                       summaries (:StackResourceSummaries reply)]

                   ;; If we can't fetch the summaries for some reason...
                   (if-not summaries

                     ;; Just return whatever AWS sent us
                     reply

                     ;; We can fetch the summaries...
                     (do
                       ;; Print it
                       (->> summaries
                            (map #(select-keys % [:LogicalResourceId :ResourceStatus]))
                            clojure.pprint/print-table
                            )

                       ;; Keep looping every few seconds until everything has either been
                       ;; created or deleted (in case there is an error).
                       (if-not (or (every? #(resource-created? %) summaries)
                                   (every? #(resource-deleted? %) summaries))
                         (do
                           (Thread/sleep 5000)
                           (recur)))))))]

    (println (format "Waited %.2f seconds" (float (/ (- (System/nanoTime) start-time) 1000000000))))
    result))

(defn setup-load-balanced-web-service--req-data [params]
  ;; FIXME!!! The way we gather and destructure the params is a bit
  ;; clumsy at the moment. There are slight variations on how variables
  ;; are named. Ex: EnvironmentName vs EnvName.
  ;; Maybe the generic settings like app name and environment name
  ;; should be passed in separately?
  (let [{:keys [AppName EnvName WorkloadName]} params]
    {:StackName (format "%s-%s-%s" AppName EnvName WorkloadName)
     :Capabilities ["CAPABILITY_NAMED_IAM"]
     :TemplateBody (slurp (io/resource "cf-templates/load-balanced-web-service.yml"))
     :Parameters (au/->params params)
     :Tags (au/->tags {:sokoban-application AppName
                       :sokoban-environment EnvName
                       :sokoban-service WorkloadName
                       })}))

(comment
  (dev/dev-start-app!)

  ;;-----------------------------------------------
  ;;
  ;; Explore KMS keys
  (def kms (aws/client {:api :kms
                       :credentials-provider @g/credentials-provider}))

  (aws/doc kms :ListKeys)

  (def aws-keys
    (->> (aws/invoke kms {:op :ListKeys})
         :Keys
         (map #(:KeyId %))))

  (aws/doc kms :DescribeKey)

  (def aws-keys-details
    (->> aws-keys
         (map #(aws/invoke kms {:op :DescribeKey
                                :request {:KeyId %}}))))

  ;;-----------------------------------------------
  ;;
  ;; Explore EC2 KeyPairs
  (def ec2 (aws/client {:api :ec2
                        :credentials-provider @g/credentials-provider}))

  (aws/validate-requests ec2)

  (aws/doc ec2 :DescribeKeyPairs)

  (aws/invoke ec2 {:op :DescribeKeyPairs})

  (aws/doc ec2 :CreateKeyPair)

  (def create-key-reply
    (aws/invoke ec2 {:op :CreateKeyPair
                     :request {}}))

  ;;-----------------------------------------------
  ;;
  ;; Interactively operate CloudFormation
  (def cf (aws/client {:api :cloudformation
                       :credentials-provider @g/credentials-provider}))

  (aws/validate-requests cf)

  (g/set-app-name! "orange")



  ;; Step 1: Setup roles
  (def create-result
    (let [req (setup-roles--req-data (select-keys @g/app-context [:app-name :account-id]))
          spec? (u/on-spec? (aws/request-spec-key cf :CreateStack) req)]
      (if (true? spec?)
        (cf-stack-ensure cf req)
        spec?)))

  (if (:StackId create-result)
    (cf-watch-for-stack-completion cf (:StackId create-result)))

  ;; Step 2a: Setup ECS Fargate cluster
  (def create-result
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
          req (setup-ECS-Fargate-infrastructure--req-data params)
          spec? (u/on-spec? (aws/request-spec-key cf :CreateStack) req)]

      (if (true? spec?)
        (cf-stack-ensure cf req)
        spec?)
      ))

  ;; Watch and wait for the creation to complete
  (if (:StackId create-result)
    (cf-watch-for-stack-completion cf (:StackId create-result))
    create-result)

  ;; Step 2b: Setup ECS EC2 cluster
  (def create-result
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
                  :EC2InstanceType "t3.small"
                  :AsgMinSize "0"
                  :AsgMaxSize "5"
                  :AsgDesiredCapacity "1"
                  }
          req (setup-ECS-EC2-infrastructure--req-data params)
          spec? (u/on-spec? (aws/request-spec-key cf :CreateStack) req)]

      (if (true? spec?)
        (cf-stack-ensure cf req)
        spec?)
      ))

  ;; Watch and wait for the creation to complete
  (if (:StackId create-result)
    (cf-watch-for-stack-completion cf (:StackId create-result))
    create-result)

  (def create-result nil)

  ;; Step 3: Setup load-balanced web service
  (def create-result
    (let [environment "test"
          app-name (:app-name @g/app-context)
          account-id (:account-id @g/app-context)
          service-name "api"
          params {:AppName app-name
                  :EnvName environment
                  :WorkloadName service-name
                  :ContainerImage "044973601964.dkr.ecr.us-west-1.amazonaws.com/demo/api:cee7709"
                  :ContainerPort "80"
                  :TaskCPU "256"
                  :TaskMemory "512"
                  :TaskCount "1"
                  :LogRetention "30"
                  :TargetContainer service-name
                  :TargetPort "80"
                  }
          req (setup-load-balanced-web-service--req-data params)
          spec? (u/on-spec? (aws/request-spec-key cf :CreateStack) req)]

      ;; spec?
      (if (true? spec?)
        (cf-stack-ensure cf req)
        spec?)
      )
    )

  ;; Watch and wait for the creation to complete
  (if (:StackId create-result)
    (cf-watch-for-stack-completion cf (:StackId create-result))
    create-result
    )

  )
