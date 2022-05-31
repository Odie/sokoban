(ns odie.sokoban.load-balanced-web-app
  (:require [clojure.java.io :as io]
            [odie.sokoban.aws-utils :as au]
            [odie.sokoban.utils :as u]
            [cognitect.aws.client.api :as aws]
            [odie.sokoban.globals :as g]
            [odie.sokoban.dev :as dev]
            [clojure.string :as str]))

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
  (merge
   (aws/invoke cf {:op :DescribeStacks
                   :request {:StackName stackname}})
   (aws/invoke cf {:op :ListStackResources
                   :request {:StackName stackname}})))

(defn resource-created? [resource-summary]
  (= (:ResourceStatus resource-summary) "CREATE_COMPLETE"))

(defn resource-deleted? [resource-summary]
  (let [status (:ResourceStatus resource-summary)]
    (or (= status "DELETE_COMPLETE")
        (= status "DELETE_SKIPPED"))))

(defn resource-op-complete? [resource-summary]
  (or (resource-created? resource-summary)
      (resource-deleted? resource-summary)
      (str/includes? (:ResourceStatus resource-summary) "COMPLETE")))

(defn cf-watch-for-stack-completion [cf stackname]
  (let [start-time (System/nanoTime)
        result (loop []
                 (let [reply (cf-fetch-stack-status cf stackname)
                       ]

                   ;; If we can't fetch the summaries for some reason...
                   (if (aws-error? reply)

                     ;; Just return whatever AWS sent us
                     reply

                     ;; We can fetch the summaries...
                     (let [summaries (:StackResourceSummaries reply)
                           stack (get-in reply [:Stacks 0])

                           data (concat [{:LogicalResourceId (format "%s stack" (:StackName stack))
                                          :ResourceStatus (:StackStatus stack)}]
                                        (map #(select-keys % [:LogicalResourceId :ResourceStatus]) summaries))
                           ]
                       ;; Print it
                       (clojure.pprint/print-table data)

                       ;; Keep looping every few seconds until everything has either been
                       ;; created or deleted (in case there is an error).
                       (if (not (every? #(resource-op-complete? %) data))
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

(defn aws-error? [o]
  (= (:cognitect.anomalies/category o) :cognitect.anomalies/incorrect))

(defn aws-client [api-kw]
  (aws/client {:api api-kw
               :credentials-provider @g/credentials-provider}))

(defn keypair-by-name
  "Given the name of a keypair, try to locate its secret key in SSM"
  [n]

  (let [ec2 (aws-client :ec2)

        ;; Ask EC2 about the keypair
        keypair-reply (aws/invoke ec2 {:op :DescribeKeyPairs
                                       :request {:KeyNames [n]}})]

    (if (aws-error? keypair-reply)
      keypair-reply

      (let [;; Get the ID of the key
            key-id (get-in keypair-reply [:KeyPairs 0 :KeyPairId])

            ssm (aws-client :ssm)

            ;; Ask SSM about the key
            key-reply (aws/invoke ssm {:op :GetParameter
                                       :request {:Name (str "/ec2/keypair/" key-id)
                                                 :WithDecryption true}})]

        (if (aws-error? key-reply)
          key-reply

          (get-in key-reply [:Parameter :Value]))))))

(defn cluster-instance-key
  "Given the name of a ECS EC2 stack, return the private key instance key"
  [stack-name]

  (let [cf (aws-client :cloudformation)

        keypair-res-reply (aws/invoke cf {:op :DescribeStackResource
                                          :request {:StackName stack-name
                                                    :LogicalResourceId "EcsInstanceKeyPair"}})]
    (if (aws-error? keypair-res-reply)
      keypair-res-reply

      (keypair-by-name (get-in keypair-res-reply [:StackResourceDetail :PhysicalResourceId])))))

(comment

  (keypair-by-name "orange-test-instance-key")
  (cluster-instance-key "orange-test")

  (aws/invoke cf {:op :DescribeStacks
                  :request {:StackName "orange-test"}
                  })

  (dev/dev-start-app!)

  (aws/doc cf :ListStackResources)
  (aws/doc cf :DescribeStackResource)

  ;;-----------------------------------------------
  ;;
  ;; Explore KMS keys
  (def kms (aws-client :kms))

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
  (def ec2 (aws-client :ec2))

  (aws/validate-requests ec2)

  (aws/doc ec2 :DescribeKeyPairs)

  (aws/invoke ec2 {:op :DescribeKeyPairs})

  (aws/doc ec2 :CreateKeyPair)

  (def create-key-reply
    (aws/invoke ec2 {:op :CreateKeyPair
                     :request {}}))

  ;;-----------------------------------------------
  ;;
  ;; Interactively operate Cloud Formation
  (def cf (aws-client :cloudformation))

  (aws/validate-requests cf)

  (g/set-app-name! "orange")

  (aws/invoke cf {:op :ListStacks})


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
                  :UserData (format"#!/bin/bash
echo ECS_CLUSTER=\"%s\" >> /etc/ecs/ecs.config;
echo ECS_BACKEND_HOST= >> /etc/ecs/ecs.config;
" (format "%s-%s"app-name environment))
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

  (aws/doc ec2 :DescribeKeyPairs)

  ;; Ask AWS to describe the key that was just created
  (def reply
    (aws/invoke ec2 {:op :DescribeKeyPairs
                     :request {:KeyNames ["orange-test-instance-key"]}
                     }))

  ;; Get the ID of the key
  (def key-id
    (-> reply
        (get-in [:KeyPairs 0 :KeyPairId])))

  (def key-reply
    (aws/invoke ssm {:op :GetParameter
                     :request {:Name (str "/ec2/keypair/" key-id)
                               :WithDecryption true}}))

  (spit (u/expand-home (str "~/.ssh/" "orange-test-instance-key")) (get-in key-reply [:Parameter :Value]))



  ;; Fetch the newly created key
  (def ssm (aws-client :ssm))

  (aws/doc ssm :GetParameter)

  (aws/doc cf :ListStackResources)
  (aws/invoke cf {:op :ListStackResources
                  :request {:StackName "orange-test"}
                  })



  ;; Watch and wait for the creation to complete
  (if (:StackId create-result)
    (cf-watch-for-stack-completion cf (:StackId create-result))
    create-result
    )

  )
