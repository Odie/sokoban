(ns odie.sokoban.service
  (:require [clj-yaml.core :as yml]
            ;; [cfn-yaml.core :as cfn-yml]
            ;; [yaml.core :as yaml]
            [odie.sokoban.globals :as g]
            [clojure.java.io :as io]
            [charred.api :as charred]))

(comment

  (let [template (read-json
                  (slurp (io/resource "cf-templates/service.json")))]
    (get-in template [:Resources :TaskDefinition :Properties]))

  (println
   (yml/generate-string template))

  (println
   (yml/generate-string
    {:LogGroup
     {:Metadata
      {"sokoban:description" "A CloudWatch log group to hold your service logs"}
      :Type "AWS::Logs::LogGroup"
      :Properties
      {:LogGroupName :null
       :RetentionInDays :null}}})))
