(ns odie.sokoban.aws-api
  (:require [cognitect.aws.service :as aws-service]
            [cognitect.aws.client.api :as aws]
            [odie.sokoban.aws-utils :as au]))


(defonce used-apis (atom #{}))
(defonce op-to-service-table (atom {}))

(defn aws-ops
  "Given the API keyword, such as :ecs or :ec2, return a list of known APIs we can
  call."
  [api-keyword]
  (let [supported-operations (->> (aws-service/service-description (name api-keyword))
                                  :operations)]
    (->> (keys supported-operations))))

(defn use-api!
  "Declare our intention to use a specific API.
  We're trying to incrementally build up the `aws-api-lookup` table so we have
  Op=>Service-name mappings.
  "
  [api-keyword]

  (when-not (contains? @used-apis api-keyword)
    (let [new-mappings (->> (aws-ops api-keyword)
                            (map #(vector % api-keyword))
                            (into {}))
          old-lookup @op-to-service-table
          new-lookup (merge old-lookup new-mappings)]
      (when-not (= (count new-lookup) (+ (count old-lookup) (count new-mappings)))
        (throw (ex-info "`aws-use!` encountered duplicate operation names" {:api-to-add api-keyword :used-apis used-apis})))
      (reset! op-to-service-table new-lookup)
      (swap! used-apis conj api-keyword))))


(defn invoke* [client op req]
  (au/aws-when-let*
   [client (if (keyword? client)
             (au/aws-client client)
             client)]
   (aws/invoke client {:op op
                       :req req})))

(defn op-service
  "Lookup the service for the operation, e.g. :ListClusters => :ecs"
  [op]
  (let [service (@op-to-service-table op)]

    ;; If we don't know about the operation, this means the lookup table hasn't been
    ;; populated with ops from the service the caller intends to use.
    ;; TODO: We can probably just walk the resource files that describes the API endpoints
    ;; and populate this map automatically
    (when-not service
      (throw (ex-info "Unknown operation, please call `use-api!` first"
                      {:reason :service-lookup-failed
                       :op op
                       :used-apis used-apis})))
    service))

(defn invoke
  "A simplified version of the cognitect AWS api.
  This function provides a way to just invoke an API without having to know which
  service it belongs to."
  [op req]
  (au/aws-when-let*
   ;; Lookup the service using the op we want to call
   [service (op-service op)
    client (au/aws-client service)]
   (aws/invoke client {:op op
                       :req req})))


(defn doc [op-keyword]
  (au/aws-when-let*
   ;; Lookup the service using the op we want to call
   [service (op-service op-keyword)
    ;; TODO!!! It's not clear why the cognitect api requires for us to make a client
    ;; object before we can fetch the documentation. Though this really shouldn't be
    ;; a problem, since we're only going to call this function a dev time.
    client (au/aws-client service)]
   (aws/doc client op-keyword)))


(comment
  (doc :ListClusters)

  (do
    (use-api! :ecs)
    (use-api! :ec2)
    (use-api! :cloudformation)
    @used-apis)

  )
