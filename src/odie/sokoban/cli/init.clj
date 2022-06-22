(ns odie.sokoban.cli.init
  (:require [odie.sokoban.utils :as u]
            [clojure.string :as str]
            [me.raynes.fs :as fs]
            [clj-yaml.core :as yml]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [com.rpl.specter :as s]
            [odie.sokoban.cli :as cli]
            [colorize.core :refer [blue]]
            ))


(defn read-compose-file []
  ;; Try to read the docker compose file
  (let [compose-filepath (fs/file "compose.yml")]
    (if (not (.exists compose-filepath))
      (println "No compose file to process!")

      ;; Convert it from yaml to a data collection
      (let [compose-file (yml/parse-string (slurp compose-filepath))]
        compose-file))))

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


(defn init [params]

  ;; ;; Try to read the docker compose yaml file
  ;; (let [compose-file (->> (read-compose-file)
  ;;                         (process-compose-file-data))]

  ;;   )

  (let [selected (cli/choice (blue "Please make a choice")
                             [{:text "hello"}
                              {:text "world"}
                              {:text "abc"}
                              ])]
    (println "Selected")
    (clojure.pprint/pprint selected)
    )
  )

(def dispatch-table
  [{:cmds ["init"] :cmds-opts [] :fn init}])



(comment

  (choice "Please make a choice"
          [{:text "hello"}
           {:text "world"}])

  (render-choice ["hello" "world"] 0)

  (fs/with-mutable-cwd
    (fs/chdir (fs/expand-home "~/dev/kengoson/backend"))
    (init {})
    )

  )
