(ns odie.sokoban.load-balanced-web-app
  (:require [clojure.java.io :as io]
            [odie.sokoban.aws-utils :as au]
            [odie.sokoban.utils :as u]
            [cognitect.aws.client.api :as aws]
            [odie.sokoban.globals :as g]
            [odie.sokoban.dev :as dev]
            [clojure.string :as str]
            [hato.client :as hc]
            [me.raynes.fs :as fs]
            [clojure.pprint]
            ))

;; Emulating the way aws copilot works...
;; We setup all the different services as stacks of their own, with related resources
;; that are controlled together.
;; We always start with a 'base stack' which sets up the ECS cluster.

(defn make-base-stack-name [app-name env-name]
  (format "%s-%s" app-name env-name))

(defn base-stack-name
  ([]
   (base-stack-name @g/app-context))
  ([context]
   (make-base-stack-name (:app-name context) (:env-name context))))

(defn make-cluster-name [context]
  (format "%s-%s" (:app-name context) (:env-name context)))

(defn make-service-name [context svr-name]
  (format "%s-%s-%s" (:app-name context) (:env-name context) svr-name))

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
  (let [{:keys [AppName EnvName]} params]
    {:StackName (make-base-stack-name AppName EnvName)
     :Capabilities ["CAPABILITY_NAMED_IAM"]
     :TemplateBody (slurp (io/resource "cf-templates/ECS-Fargate.yml"))
     :Parameters (au/->params params)
     :Tags (au/->tags {:sokoban-application AppName
                       :sokoban-environment EnvName})}))

(defn setup-ECS-EC2-infrastructure--req-data [params]
  (let [{:keys [AppName EnvName]} params]
    {:StackName (make-base-stack-name AppName EnvName)
     :Capabilities ["CAPABILITY_NAMED_IAM"]
     :TemplateBody (slurp (io/resource "cf-templates/ECS-EC2.yml"))
     :Parameters (au/->params params)
     :Tags (au/->tags {:sokoban-application AppName
                       :sokoban-environment EnvName})}))

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
      (str/includes? (:ResourceStatus resource-summary) "COMPLETE")
      (str/includes? (:ResourceStatus resource-summary) "FAILED")
      ))

(defn cf-watch-for-stack-completion [cf stackname]
  (let [start-time (System/nanoTime)
        result (loop []
                 (au/aws-when-let*
                  [reply (cf-fetch-stack-status cf stackname)
                   summaries (:StackResourceSummaries reply)
                   stack (get-in reply [:Stacks 0])
                   data (concat [{:LogicalResourceId (format "%s stack" (:StackName stack))
                                  :ResourceStatus (:StackStatus stack)}]
                                (map #(select-keys % [:LogicalResourceId :ResourceStatus]) summaries))]

                  ;; Print it
                  (clojure.pprint/print-table data)

                  ;; Keep looping every few seconds until everything has either been
                  ;; created or deleted (in case there is an error).
                  (when (not (every? #(resource-op-complete? %) data))
                    (Thread/sleep 5000)
                    (recur))))]

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

(defn setup-service--req-data [params]
  (let [{:keys [AppName EnvName WorkloadName]} params]
    {:StackName (format "%s-%s-%s" AppName EnvName WorkloadName)
     :Capabilities ["CAPABILITY_NAMED_IAM"]
     :TemplateBody (slurp (io/resource "cf-templates/service.yml"))
     :Parameters (au/->params params)
     :Tags (au/->tags {:sokoban-application AppName
                       :sokoban-environment EnvName
                       :sokoban-service WorkloadName
                       })}))

(defn keypair-by-name
  "Given the name of a keypair, try to locate its secret key in SSM"
  [n]

  (au/aws-when-let*
   [ec2 (au/aws-client :ec2)

    ;; Ask EC2 about the keypair
    keypair-reply (aws/invoke ec2 {:op :DescribeKeyPairs
                                   :request {:KeyNames [n]}})

    ;; Get the ID of the key
    key-id (get-in keypair-reply [:KeyPairs 0 :KeyPairId])

    ssm (au/aws-client :ssm)

    ;; Ask SSM about the key
    key-reply (aws/invoke ssm {:op :GetParameter
                               :request {:Name (str "/ec2/keypair/" key-id)
                                         :WithDecryption true}})]
   (get-in key-reply [:Parameter :Value])))


(defn cluster-instance-key
  "Given the name of a ECS EC2 stack, return the private key instance key"
  [stack-name]

  (au/aws-when-let*
   [cf (au/aws-client :cloudformation)

    keypair-res-reply (aws/invoke cf {:op :DescribeStackResource
                                      :request {:StackName stack-name
                                                :LogicalResourceId "EcsInstanceKeyPair"}})]
   (keypair-by-name (get-in keypair-res-reply [:StackResourceDetail :PhysicalResourceId]))))

(cluster-instance-key "orange-test")

(defn services-set-desired-count [cluster-name desired-count]
  (au/aws-when-let*
   [ecs (au/aws-client :ecs)
    reply (aws/invoke ecs {:op :ListServices
                           :request {:cluster cluster-name}})]

   (->> (:serviceArns reply)
        (map (fn [srv]
               (aws/invoke ecs {:op :UpdateService
                                :request {:service srv
                                          :cluster cluster-name
                                          :desiredCount desired-count}}))))))

(defn fetch-my-ip []
  (let [response (hc/get "http://checkip.amazonaws.com/")]
    (when (= (:status response) 200)
      (str/trim (:body response)))))

(def my-ip (memoize fetch-my-ip))

(defn fetch-stack [stack-name]
  (au/aws-when-let*
   [cf (au/aws-client :cloudformation)
    reply (aws/invoke cf {:op :DescribeStacks
                          :request {:StackName stack-name}})]
   (get-in reply [:Stacks 0])))

(defn stack-outputs-get-entry [stack keyname]
  (some #(when (= (:OutputKey %) keyname)
           %)
        (:Outputs stack)))

(defn stack-outputs-get [stack keyname]
  (->> (:Outputs stack)
       (some #(when (= (:OutputKey %) keyname)
                %))
       :OutputValue))

(defn stack-outputs->map [stack]
  (->> (:Outputs stack)
       (map #(vector (:OutputKey %) (:OutputValue %)))
       (into {})))

(defn make-ssh-ip-permission [ip description]
  {:IpProtocol "tcp"
   :FromPort 22
   :ToPort 22
   :IpRanges [(cond-> {:CidrIp (str ip "/32")}
               description (assoc :Description description))]})

(defn stack-open-ssh-port [stack ip description]
  (let [ssh-permission (make-ssh-ip-permission ip description)
        sec-group-id (stack-outputs-get stack "EnvironmentSecurityGroup")
        ec2 (au/aws-client :ec2)]

    ;; Add a rule to the security group
    (au/aws-when-let
     [auth-reply (aws/invoke ec2 {:op :AuthorizeSecurityGroupIngress
                                  :request {:GroupId sec-group-id
                                            :IpPermissions [ssh-permission]}})]

     ;; Add a description
     (aws/invoke ec2 {:op :UpdateSecurityGroupRuleDescriptionsIngress
                      :request {:GroupId sec-group-id
                                :IpPermissions [ssh-permission]}}))))

(defn stack-close-ssh-port [stack ip]
  (let [ssh-permission (make-ssh-ip-permission ip nil)
        sec-group-id (stack-outputs-get stack "EnvironmentSecurityGroup")
        ec2 (au/aws-client :ec2)]

    ;; Remove the rule from the security group
    (aws/invoke ec2 {:op :RevokeSecurityGroupIngress
                     :request {:GroupId sec-group-id
                               :IpPermissions [ssh-permission]}})))

(defn stack-set-capacity-provider-desired-count [stack n]
  (au/aws-when-let*
   [ecs (au/aws-client :ecs)
    ;; Get the cluster
    cluster-resp (aws/invoke ecs {:op :DescribeClusters
                                  :request {:clusters [(stack-outputs-get stack "ClusterId")]}})
    cluster (get-in cluster-resp [:clusters 0])

    ;; Get the capacity provider
    provider-resp (aws/invoke ecs {:op :DescribeCapacityProviders
                                   :request {:capacityProviders (:capacityProviders cluster)}})
    as-name (get-in provider-resp [:capacityProviders 0 :autoScalingGroupProvider :autoScalingGroupArn])

    as (au/aws-client :autoscaling)

    ;; Get the autoscaling group
    groups (aws/invoke as {:op :DescribeAutoScalingGroups
                           :request {}})

    asg (u/find-first #(= (:AutoScalingGroupARN %) as-name)
                      (:AutoScalingGroups groups))]

   ;; Set the desired count on the provider
   (aws/invoke as {:op :UpdateAutoScalingGroup
                   :request {:AutoScalingGroupName (:AutoScalingGroupName asg)
                             :DesiredCapacity n}})))

(defn service-by-name [context service-name]
  (au/aws-when-let*
   [cluster-name (make-cluster-name context)
    ecs (au/aws-client :ecs)

    ;; Grab a list of services in the cluster
    reply (aws/invoke ecs {:op :ListServices
                           :request {:cluster cluster-name}})
    service-arns (:serviceArns reply)

    ;; Grab the service details, including tags
    services-reply (aws/invoke ecs {:op :DescribeServices
                                    :request {:cluster cluster-name
                                              :services service-arns
                                              :include ["TAGS"]}})
    services (:services services-reply)

    ;; We're looking for a service that matches these entries
    target-map {:sokoban-service service-name
                :sokoban-application (:app-name @g/app-context)
                :sokoban-environment (:env-name @g/app-context)}]

   ;; Retrieve the tags of the service as a map
   ;; Grab all entries in the target map
   ;; If the resulting map is the same as the target map, we
   ;; found the service we're looking for
   (->> services
        (u/find-first #(= target-map
                          (select-keys (au/tags->map (:tags %))
                                       (keys target-map)))))))

(defn service-redeploy-with-latest-image [context service-name]
  (au/aws-when-let*
   [cluster-name (make-cluster-name context)
    ecs (au/aws-client :ecs)
    reply (aws/invoke ecs {:op :ListServices
                           :request {:cluster cluster-name}})
    service-arns (:serviceArns reply)

    ;; FIXME? Trying to identify the right service through ARN string matching?
    ;; It'd be better to match against something we have explicit control over.
    ;; One way is to the use `service-by-name` so we can be very sure of the
    ;; service we're deploying. Though that might itself become problematic
    ;; if there is a large number of services in the cluster.
    target-subs  (str (make-service-name context service-name) "-Service")
    target (u/find-first #(str/includes? % target-subs) service-arns)]
   (println "Redeploying: " target)
   (aws/invoke ecs {:op :UpdateService
                    :request {:service target
                              :cluster cluster-name
                              :forceNewDeployment true}})
  ))

(defn role-create-cf-role
  "Create a role used by Sokoban for use when it is deleting CF stacks."
  []
  (au/aws-when-let*
   [iam (au/aws-client :iam)
    create-reply (aws/invoke iam {:op :CreateRole
                                  :request {:RoleName "sk-cf-role"
                                            :Description "Temporary role used while deleting a CF stack"
                                            :Path "/sokoban/util/"
                                            :AssumeRolePolicyDocument (slurp (io/resource "cf-assume-role.json"))
                                            }})
    put-reply (aws/invoke iam {:op :PutRolePolicy
                               :request {:PolicyDocument (slurp (io/resource "cf-assume-role-inlines.json"))
                                         :PolicyName "sk-cf-role-inlines"
                                         :RoleName "sk-cf-role"
                                         }
                               })]
   [create-reply put-reply]))

(comment

 (au/aws-when-let*
  [iam (au/aws-client :iam)]
  (aws/invoke iam {:op :PutRolePolicy :request {:PolicyDocument (slurp (io/resource "cf-assume-role-inlines.json"))
                                                :PolicyName "sk-cf-role-inlines-11"
                                                :RoleName "sk-cf-role"
                                                }
                   }))

 (slurp (io/resource "cf-assume-role-inlines.json"))

 )

(defn stack-delete-all
  "Delete all related stacks in one go.

  DANGER!
  This *will* wipe out all related resources. You will loose all associated data!"
  [context]

  (au/aws-when-let*
   [cf (au/aws-client :cloudformation)
    stacks-reply (aws/invoke cf {:op :DescribeStacks})
    stacks (:Stacks stacks-reply)

    app-env {:sokoban-application (:app-name context)
             :sokoban-environment (:env-name context)}

    target-stacks (->> stacks

                       ;; Look for stacks that have the same app, env pair as the current context
                       (filter (fn[stack]
                                 (= (select-keys (au/tags->map (:Tags stack)) [:sokoban-application :sokoban-environment])
                                    app-env)))

                       ;; Sort items with most app, env, service tuples to the top
                       ;; We're going to delete the stacks in this order
                       (sort-by (fn [stack]
                                  (count (select-keys (au/tags->map (:Tags stack)) [:sokoban-application :sokoban-environment :sokoban-service]))
                                  ) >))
    ]
   (->> target-stacks
        (reduce (fn [results stack]
                  (let [stack-name (:StackName stack)
                        _ (println "Deleting: " stack-name)
                        delete-reply (aws/invoke cf {:op :DeleteStack
                                                     :request {:StackName stack-name
                                                               :RoleARN "arn:aws:iam::044973601964:role/sokoban/util/sk-cf-role"

                                                               }})]
                    (if (au/aws-error? delete-reply)
                      (reduced results)
                      (do
                        (cf-watch-for-stack-completion cf stack-name)

                        (conj results delete-reply)))))
                []))))


(defn hosted-zones []
  (au/aws-when-let*
   [r53 (au/aws-client :route53)
    zones-reply (aws/invoke r53 {:op :ListHostedZones
                                 :request {}})
    zones (:HostedZones zones-reply)]
   (->> zones
        (filter #(= false (get-in % [:Config :PrivateZone]))))))

(defn hosted-zone-by-name
  [zone-name]
  (let [zone-name (if (= \. (last zone-name))
                    zone-name
                    (str zone-name "."))]

    (->> (hosted-zones)
         (filter #(= zone-name (:Name %))))))


(comment
  (role-create-cf-role)

  (stack-delete-all @g/app-context)

  (g/set-app-name! "orange")

  (service-redeploy-with-latest-image @g/app-context "api")

  (keypair-by-name "orange-test-instance-key")
  (cluster-instance-key "orange-test")

  ;; Setup the dev environment
  (do
    (dev/dev-start-app!)
    (g/set-app-name! "banana")
    (g/set-env-name! "test")

    (def ec2 (au/aws-client :ec2))
    (aws/validate-requests ec2)

    (def cf (au/aws-client :cloudformation))
    (aws/validate-requests cf)

    (def stack (fetch-stack "orange-test"))

    (def as (au/aws-client :autoscaling))

    )

  (aws/doc cf :ListStackResources)
  (aws/doc cf :DescribeStackResource)

  ;;-----------------------------------------------
  ;;
  ;; Explore KMS keys
  (def kms (au/aws-client :kms))

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
  (def ec2 (au/aws-client :ec2))

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
  (def cf (au/aws-client :cloudformation))

  (aws/validate-requests cf)


  (aws/invoke cf {:op :ListStacks})


  ;; Step 1: Setup roles
  (def create-result
    (let [req (setup-roles--req-data (select-keys @g/app-context [:app-name :account-id]))
          spec? (u/on-spec? (aws/request-spec-key cf :CreateStack) req)]
      (if (true? spec?)
        (cf-stack-ensure cf req)
        spec?)))

  (if-not (au/aws-error? create-result)
    (cf-watch-for-stack-completion cf (:StackId create-result))
    create-result)

  ;; Step 2a: Setup ECS Fargate cluster
  (def create-result
    (let [environment (:env-name @g/app-context)
          app-name (:app-name @g/app-context)
          account-id (:account-id @g/app-context)
          params {:AppName app-name
                  :EnvName environment
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
  (if-not (au/aws-error? create-result)
    (cf-watch-for-stack-completion cf (:StackId create-result))
    create-result)

  ;; Step 2b: Setup ECS EC2 cluster
  (def create-result
    (let [environment (:env-name @g/app-context)
          app-name (:app-name @g/app-context)
          account-id (:account-id @g/app-context)
          params {:AppName app-name
                  :EnvName environment
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
  (if-not (au/aws-error? create-result)
    (cf-watch-for-stack-completion cf (:StackId create-result))
    create-result)

  (def create-result nil)

  ;; Step 3: Setup load-balanced web service
  (def create-result
    (let [environment (:env-name @g/app-context)
          app-name (:app-name @g/app-context)
          account-id (:account-id @g/app-context)
          service-name "api"
          params {:AppName app-name
                  :EnvName environment
                  :WorkloadName service-name
                  :ContainerImage "044973601964.dkr.ecr.us-west-1.amazonaws.com/kengoson-backend"
                  ;; "044973601964.dkr.ecr.us-west-1.amazonaws.com/demo/api:cee7709"
                  :ContainerPort "8000"
                  :TaskCPU "256"
                  :TaskMemory "512"
                  :TaskCount "1"
                  :LogRetention "30"
                  :TargetContainer service-name
                  :TargetPort "8000"
                  :LaunchType "EC2"
                  }
          req (setup-load-balanced-web-service--req-data params)
          spec? (u/on-spec? (aws/request-spec-key cf :CreateStack) req)]

      ;; spec?
      (if (true? spec?)
        (cf-stack-ensure cf req)
        spec?)
      )
    )

  (if-not (au/aws-error? create-result)
    (cf-watch-for-stack-completion cf (:StackId create-result))
    create-result)

  ;; Step 4: Setup internal services
  (def create-result
    (let [environment (:env-name @g/app-context)
          app-name (:app-name @g/app-context)
          account-id (:account-id @g/app-context)
          service-name "cache"
          params {:AppName app-name
                  :EnvName environment
                  :WorkloadName service-name
                  :ContainerImage "redis:7-alpine"
                  :ContainerPort "6379"
                  :TaskCPU "256"
                  :TaskMemory "512"
                  :TaskCount "1"
                  :LogRetention "30"
                  ;; :TargetContainer service-name
                  ;; :TargetPort "5000"
                  :LaunchType "EC2"
                  }
          req (setup-service--req-data params)
          spec? (u/on-spec? (aws/request-spec-key cf :CreateStack) req)]

      ;; spec?
      (if (true? spec?)
        (cf-stack-ensure cf req)
        spec?)
      )
    )

  (if-not (au/aws-error? create-result)
    (cf-watch-for-stack-completion cf (:StackId create-result))
    create-result)

  ;;---------------------------------------------------------------------
  ;; Misc actions

  ;; Grab SSH key
  ;; Grab the instance key and place it into the user's .ssh directory
  ;; WARNING: We're just overwriting without any warning here
  (let [stack-name (base-stack-name @g/app-context)
        key-fpath (u/expand-home (format "~/.ssh/%s-instance-key.pem" stack-name))]

    (spit key-fpath (cluster-instance-key (base-stack-name @g/app-context)))
    (fs/chmod "600" key-fpath))

  (def ecs (au/aws-client :ecs))

  (aws/validate-requests ecs)

  (def stack (fetch-stack (base-stack-name)))

  (stack-outputs-get stack "EnvironmentSecurityGroup")


  ;; Turn on all services
  (services-set-desired-count (base-stack-name) 1)

  ;; Turn off all services
  (services-set-desired-count (base-stack-name) 0)

  (stack-open-ssh-port stack (my-ip) "SSH for Jonathan")

  (stack-close-ssh-port stack (my-ip))

  ;; Manually change how many ec2 instances we're running
  (stack-set-capacity-provider-desired-count (fetch-stack (base-stack-name @g/app-context)) 0)


  (stack-set-capacity-provider-desired-count stack 1)

  (fetch-stack "docker")

  (services-set-desired-count (base-stack-name @g/app-context) 0)

  (service-redeploy-with-latest-image @g/app-context "api")

  (au/aws-when-let*
   [cluster-name (make-cluster-name @g/app-context)
    ecs (au/aws-client :ecs)
    reply (aws/invoke ecs {:op :ListServices
                           :request {:cluster cluster-name}})
    service-arns (:serviceArns reply)

    ;; FIXME!!! Trying to identify the right service through string matching
    ;; This might be problematic if AWS ever changes the way they name the
    ;; service based on the TaskDefinition
    target-subs  (str (make-service-name @g/app-context "api") "-Service")
    target (u/find-first #(str/includes? % target-subs) service-arns)]
   (aws/invoke ecs {:op :UpdateService
                    :request {:service target
                              :cluster cluster-name
                              :forceNewDeployment true}}))



   ;; (def targets target-stacks)
   ;; (def stacks stacks)
   ;; (aws/invoke ecs {:op :UpdateService
   ;;                  :request {:service target
   ;;                            :cluster cluster-name
   ;;                            :forceNewDeployment true}})


  (->> stacks
       (filter (fn[stack]
                 (let [tags (au/tags->map (:Tags stack))]
                   (= (select-keys tags [:sokoban-application :sokoban-environment])
                      {:sokoban-application (:app-name @g/app-context)
                       :sokoban-environment (:env-name @g/app-context)}))))
       (sort-by (fn [stack]
                  (count (select-keys (au/tags->map (:Tags stack)) [:sokoban-application :sokoban-environment :sokoban-service]))
                  ) >)
       )

  (au/aws-when-let*
   [iam (au/aws-client :iam)
    reply (aws/invoke iam {:op :CreateRole
                           :request {:RoleName "sk-cf-role"
                                     :Description "Temporary role used while deleting a CF stack"
                                     :Path "/sokoban/util/"
                                     :AssumeRolePolicyDocument (slurp (io/resource "cf-assume-role.json"))
                                     }})
    ]
   reply)

)
