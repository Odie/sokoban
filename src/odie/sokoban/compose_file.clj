(ns odie.sokoban.compose-file
  (:require [clojure.string :as str]
            [com.rpl.specter :as s]
            [clj-yaml.core :as yml]))


(defn parse-port-mapping
  "Given a string like \"8000:8000\", return a map that looks like
  {:from \"8000\" :to \"8000\"}"
  [s]
  (zipmap [:from :to]
          (str/split s #":")))

(defn parse-port-mappings [mappings-str-list]
  (->> mappings-str-list
       (map parse-port-mapping)))

(defn parse-env-vars
  "Given a list of environment var strings in the format of
  \"varname=val\", return a map in the format of
  {varname val}"
  [vars-str-list]
  (->> vars-str-list
       (map #(str/split % #"="))
       (into {})))

(defn process-compose-file-data [compose-data]
  (->> compose-data
       (s/transform [:services s/MAP-VALS (s/must :ports)] parse-port-mappings)
       (s/transform [:services s/MAP-VALS (s/must :environment)] parse-env-vars)))

(defn read-compose-file [file]
  ;; Convert it from yaml to a data collection
  (->> (yml/parse-string (slurp file))
       ;; Perform some processing to make working with the file programmatically
       ;; more straightforward
       (process-compose-file-data)))
