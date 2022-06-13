(ns odie.sokoban.utils
  "General utility functions"
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint]
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

(defn find-first
  "Find the first item in the collection that matches the predicate"
  [pred coll]
  (some #(when (pred %) %) coll))

(defn on-mac? []
  (= (System/getProperty "os.name")
     "Mac OS X"))

(defn binary-present?
  "Check if a terminal program can be located by name."
  [binary-name]
  (let [result (sh "which" binary-name)]
    (= (:exit result) 0)))

(defn term-notify
  "On OSX, run the `terminal-notifier` tool to pop up a notification."
  [opts]
  (when (and (on-mac?)
             (binary-present? "terminal-notifier"))
    (let [params (reduce (fn [strs kv-pair]
                           (-> strs
                               (conj (str "-" (name (key kv-pair))))
                               (conj (str (val kv-pair)))))
                         ["terminal-notifier"]
                         opts)]
      (apply sh params))))

(defn key-by
  "Like `group-by`, but does not deal with maps returning the same key value.

  Given a sequence of maps, turn them into maps of (f a-map)=>a-map.
  Note that if `f` reports duplicate keys, it is undefined which map would
  remain in the returning result."
  [f seq-of-maps]
  (->> seq-of-maps
       (map (juxt f identity))
       (into {})))

(defn pretty-spit
  [file coll]
  (spit file (with-out-str (clojure.pprint/write coll :dispatch clojure.pprint/code-dispatch))))

(defn merge-into
  "Merge `seq-maps` into the src-map"
  [src-map seq-maps]
  (apply merge src-map seq-maps))

(defn print-file-permission [perm]
  (with-out-str
    (doseq [oct [(bit-shift-right (bit-and perm 2r111000000) 6)
                 (bit-shift-right (bit-and perm 2r000111000) 3)
                 (bit-and perm 2r000000111)]]
      (if (zero? (bit-and oct 2r100))
        (print "-")
        (print "r"))
      (if (zero? (bit-and oct 2r010))
        (print "-")
        (print "w"))
      (if (zero? (bit-and oct 2r001))
        (print "-")
        (print "x")))))

;; Ripped from clojure src
(defmacro  assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                 (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

(defmacro when-let*
  "bindings => binding-form test
  When test is true, evaluates body with binding-form bound to the value of test"
  [bindings & body]
  (assert-args
   (vector? bindings) "a vector for its binding"
   (< 1 (count bindings)) "at least 2 forms in binding vector"
   (= 0 (mod (count bindings) 2)) "even number of forms in binding vector")
  (let [form (bindings 0) tst (bindings 1) more (vec (drop 2 bindings))]
    `(let [temp# ~tst]
       (if (nil? temp#)
         temp#
         (let [~form temp#]
           ~(if (seq more)
              `(aws-when-let* ~more ~@body)
              `(do ~@body)))))))
