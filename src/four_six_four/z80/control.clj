(ns four-six-four.z80.control
  (:require [taoensso.timbre :as log]
            [four-six-four.z80.fetch :refer [fetch-instruction]]
            [four-six-four.z80.instructions :refer [operation]]
            [four-six-four.pprint :refer [format-decode]]
            [four-six-four.z80.vm :refer [*z80* set-running toggle-running is-running get-pc set-pc inc-refresh write-mem-vector print-z80]]))

;;;; CPU control

(defn execute-program-sequence
  "Execute bound VM running program from `program` sequence. Nb this is only suitable
  for simple programs, i.e. without jumps, because the sequence is not written
  to memory."
  [program]
  (dorun
   (map operation program)))

(defn execute
  "Execute bound VM running program from `start` in memory. Runs until halted."
  [start]
  (log/infof "Z80 execution begin at 0x%x" start)
  (dosync
   (set-pc start)
   (set-running))
  (while (is-running)
    (let [decode (fetch-instruction)]
      (log/trace (format-decode (merge decode {:loc (get-pc)})))
      (dosync
       (inc-refresh))
      (operation (:instr decode)))))

(defn execute-step
  "Execute bound VM running program from `start` in memory step-wise. Runs until
  halted or aborted."
  [start]
  (log/infof "Z80 execution begin at 0x%x" start)
  (dosync
   (set-pc start)
   (set-running))
  (while (is-running)
    (let [decode (fetch-instruction)]
      (println (format-decode (merge decode {:loc (get-pc)})))
      (print "?> ")
      (flush)
      (case (first (read-line))
        \n (do
             (dosync
              (inc-refresh))
             (operation (:instr decode)))
        \q (do
             (println "quitting")
             (dosync
              (toggle-running)
              (print-z80 true)))
        \p (print-z80 true)
        (println "Invalid command")))))

(defn execute-program
  "Execute bound VM running `program` copied into location `start`."
  [program start & args]
  (let [kwargs (apply hash-map args)]
    (dosync
     (write-mem-vector start program))
    (if (:step kwargs)
      (execute-step start)
      (execute start))))

