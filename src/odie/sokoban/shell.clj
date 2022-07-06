(ns odie.sokoban.shell
  (:require [clojure.java.io :as io]
            [babashka.process :as proc]))
(defn wait
  "Takes a process, waits until is finished and throws if exit code is non-zero."
  [proc]
  (let [start-time (System/nanoTime)
        proc @proc
        end-time (System/nanoTime)
        exit-code (:exit proc)
        err (:err proc)]
    (if (not (zero? exit-code))
      (let [err (if (string? err)
                  err
                  (slurp (:err proc)))]
        (throw (ex-info (if (string? err)
                          err
                          "failed")
                        (assoc proc :type ::error))))
      (-> proc
          (assoc :duration (float (/ (- end-time start-time) 1000000000)))))))

(def default-shell-opts
  {:in :inherit
   :out :inherit
   :err :inherit
   :shutdown proc/destroy-tree})

(defn shell
  "Convenience function around `process` that was originally in `babashka.tasks`.
  Defaults to inheriting I/O: input is read and output is printed
  while the process runs. Throws on non-zero exit codes. Kills all
  subprocesses on shutdown. Optional options map can be passed as the
  first argument, followed by multiple command line arguments. The
  first command line argument is automatically tokenized.

  Differences with process:

  - Does not work with threading for piping output from another
  process.
  - It does not take a vector of strings, but varargs strings.
  - Option map goes first, not last.

  Examples:

  - `(shell \"ls -la\")`
  - `(shell {:out \"/tmp/log.txt\"} \"git commit -m\" \"WIP\")`"
  [cmd & args]
  (let [[prev cmd args]
        (if (and (map? cmd)
                 (:proc cmd))
          [cmd (first args) (rest args)]
          [nil cmd args])
        [opts cmd args]
        (if (map? cmd)
          [cmd (first args) (rest args)]
          [nil cmd args])
        opts (if prev
               (assoc opts :in nil)
               opts)
        _ (assert (string? cmd))
        cmd (if (.exists (io/file cmd))
              [cmd]
              (proc/tokenize cmd))
        cmd (into cmd args)]
    (wait (proc/process prev cmd
                        (merge default-shell-opts opts)))))
