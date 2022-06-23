(ns odie.sokoban.cli
  (:require [odie.sokoban.utils :as u]
            [colorize.core :refer [green blue]]
            [me.raynes.fs :as fs]))

(defonce ^:dynamic *in-repl* false)

(defn render-choice
  [items selection]

  ;; For each line to be displayed...
  (doseq [[idx item] (u/indexed items)]

    ;; Draw the cursor or just an indent
    (if (= idx selection)
      (print (str " " (green ">") " "))
      (print "   "))

    ;; Selected item should be marked green, the same as the cursor
    (cond->> item
      (= idx selection) (green)
      :finally (println)))
  (count items))

(defn get-user-input
  ([]
   (get-user-input nil))
  ([prompt]

   (when prompt
     (println (blue prompt)))
   (let [input (read-line)]
     (println)
     input)))

(defn choice
  "Let the user pick from a list of choices interactively via the keyboard.
  We're locked in an input loop until the user confirms a selection using the 'enter' key.
  `xs` is expected to be a sequence of maps, each with at least a `:text` key, which
  will be displayed for the user to pick from."
  [prompt xs]

  (let [items (map :text xs)
        selection (atom 0)
        render-cnt (atom 0)
        render (fn []
                 (when-not (zero? @render-cnt)
                   (print (u/x-lines-up--sol (+ (count items) 1))))
                 (when prompt
                   (println prompt))
                 (render-choice items @selection)
                 (.flush System/out)
                 (swap! render-cnt inc))
        read-all (fn [buf]
                   ;; Wait and read the first byte
                   (conj! buf (.read System/in))

                   ;; We want to wait a short duration for the rest to arrive.
                   ;; In particular, we are waiting for a full escape sequence which
                   ;; represents arrow keys from the keyboard.
                   ;; For some reason, the java API has made this extremely clumsy.
                   ;; It would have been better to get a posix style API instead.
                   (let [end-time (+ 10 (System/currentTimeMillis))]
                     (loop []

                       (when (> end-time (System/currentTimeMillis))
                         ;; When the java API reports there is data to be read...
                         (let [cnt (.available System/in)]
                           (when (not= 0 cnt)
                             ;; Read and record those bytes
                             (doseq [_ (range cnt)]
                               (conj! buf (.read System/in)))))

                         ;; Wait a bit for more bytes to arrive or for a timeout
                         (Thread/sleep 1)
                         (recur))

                       ;; Timed-out
                       ;; We assume there are no more bytes to be read.
                       )))]
    (render)

    (u/with-raw-terminal-mode
      (loop []
        (let [buf (transient [])]
          ;; Grab the user input
          ;; Depending on if we're working in the repl or not, we want to deal with
          ;; input retrieval slightly differently
          (if *in-repl*
            (conj! buf (get-user-input nil))
            (read-all buf))
          (let [data (persistent! buf)]
            ;; (println data)
            (cond
              (or
               (= data ["u"])
               (= data [(int \k)])
               (= data [27 91 65])) ;; \033[A - Up arrow
              (do
                (swap! selection #(max 0 (dec %)))
                (render))
              (or
               (= data ["d"])
               (= data [(int \j)])
               (= data [27 91 66])) ;; \033[B - Down arrow
              (do
                (swap! selection #(min (dec (count items)) (inc %)))
                (render))
              (= data [27 91 67]) ;; \033[C - Right arrow
              :no-op
              (= data [27 91 68]) ;; \033[D - Left arrow
              :no-op)

            ;; We're going to keep looping until the user hits the 'enter' key
            (when (and (not= data [10])
                       (not= data [""]))
              (recur))))))
    (println)

    ;; Return the selection the user chose
    (nth xs @selection)))

(defn exists?-or-print-error [file prompt]
  (if-not (fs/exists? file)
    (do
      (if (= 1 (u/re-count #"%s" prompt))
        (println (format prompt file))
        (println prompt))
      false)
    true))

(defn absent?-or-print-error [file prompt]
  (if (fs/exists? file)
    ;; The file exists?
    (do
      (if (= 1 (u/re-count #"%s" prompt))
        (println (format prompt file))
        (println prompt))
      ;; Answer that it is *not* absent
      false)

    ;; Yes, the file is absent
    true))
