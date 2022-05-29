(ns odie.sokoban.utils
  "General utility functions"
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            ))


(defn key-starts-with?
  "Does the given key start with the substring?"
  [k substr]
  (str/starts-with? (name k) substr))

(defn expand-home [s]
  (if (.startsWith s "~")
    (clojure.string/replace-first s "~" (System/getProperty "user.home"))
    s))

(defn str-split-at
  "Split a string at a given index, returning a list of two separated strings."
  [s idx]
  (list (subs s 0 idx) (subs s idx)))

(defn str-split-first
  "Split the string using the first occurrence of the `substr`, removing the `substr` itself."
  [s substr]
  (let [idx (str/index-of s substr)]
    (list (subs s 0 idx) (subs s (+ idx (count substr))))))

(defn on-spec?
  "Check if the given `data` is valid given the spec. Either return true if it is valid, or a map that describes errors. This is useful while putting together request data in the right format."
  [spec data]
  ;; Is the data valid according to the spec?
  (if (s/valid? spec data)
    ;; If so... just say it's okay...
    true

    ;; Otherwise, say why it's not on spec
    (s/explain-data spec data)))
