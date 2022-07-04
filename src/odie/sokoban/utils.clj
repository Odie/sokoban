(ns odie.sokoban.utils
  "General utility functions"
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.java.shell :refer [sh]]
            [clojure.pprint]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import java.io.StringWriter java.lang.ProcessBuilder$Redirect)
  )


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

(defn ^"[Ljava.lang.String;" string-array [args]
  (into-array String args))

(defn exec-stty
  "Excute the `stty(1)` command to configure TTY options like input echoing, and
  line handling."
  [& args]
  (let [^Process
        process (-> (ProcessBuilder. (string-array (cons "stty" args)))
                    (.redirectInput (ProcessBuilder$Redirect/from (io/file "/dev/tty")))
                    (.start))

        ^StringWriter err (StringWriter.)]
    {:exit (.waitFor process)
     :err @(future
             (with-open [^StringWriter err (StringWriter.)]
               (io/copy (.getErrorStream process) err)
               (.toString err)))
     :out @(future
             (with-open [^StringWriter out (StringWriter.)]
               (io/copy (.getInputStream process) out)
               (.toString out)))}))

(defn raw-terminal-mode [on?]
  (if on?
    (exec-stty "-echo" "-icanon")
    (exec-stty "echo" "icanon")))

(defmacro with-raw-terminal-mode
  "Turn on raw terminal mode, run the given body, then turn the raw terminal mode off,
  returning the result of the body."
  [body]
  `(do
     (raw-terminal-mode true)
     (let [res# ~body]
       (raw-terminal-mode false)
       res#)))

(defn x-lines-up--sol
  "Move up `x` number of lines and move to start of line"
  [x]
  (format "\033[%dF" x))

(defn x-lines-down--sol
  "Move down `x` number of lines and move to start of line"
  [x]
  (format "\033[%dE" x))


(defn vector-or-seq? [coll]
  (or (vector? coll)
      (seq? coll)))

(defmacro doseq-indexed
  "loops over a set of values, binding index-sym to the 0-based index of each value"
  ([[val-sym values index-sym] & code]
   `(loop [vals# (seq ~values)
           ~index-sym (long 0)]
      (if vals#
        (let [~val-sym (first vals#)]
          ~@code
          (recur (next vals#) (inc ~index-sym)))
        nil))))

(defn indexed [a-seq]
  (map-indexed vector a-seq))

(defn write-edn
  "Write the `data` to `target-file` in EDN format"
  [target-file data]
  (pretty-spit target-file data))

(defn read-edn
  "Read the target edn file"
  [target-file]
  (edn/read-string (slurp target-file)))

(defn re-count [re s]
  (count (re-seq re s)))

(defmacro collect [& bind-names]
  (into {}
        (map (juxt keyword identity))
        (->> (select-keys &env bind-names)
             keys)))

(defmacro interp
  [^String string]
  "Like 'format' but with string interpolation"
  (let [-re #"#\{(.*?)\}"
        fstr (str/replace string -re "%s")
        fargs (map #(read-string (second %)) (re-seq -re string))]
    `(format ~fstr ~@fargs)))

(defn fmt [string params]
  "Like 'format' but with string interpolation"
  (let [-re #"#\{(.*?)\}"
        fstr (str/replace string -re "%s")
        fargs (map #(get params (keyword (second %))) (re-seq -re string))]
    (apply format fstr fargs)))

(comment

  (let [abc "world"
        msg "Hello #{abc}"]
    (interp "Hello #{abc}"))

  (let [abc "world"
        msg "Hello #{abc}"]
    (fmt msg {:abc "world"}))

  (let [abc "world"
        msg "Hello #{abc}"]
    (fmt msg (collect abc)))

  )
