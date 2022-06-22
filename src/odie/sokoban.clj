(ns odie.sokoban
  (:require [babashka.cli :as cli]
            [odie.sokoban.cli.init :as init])
  (:gen-class))

(def dispatch-table
  (concat init/dispatch-table))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (cli/dispatch dispatch-table args)
  (System/exit 0)
  )
