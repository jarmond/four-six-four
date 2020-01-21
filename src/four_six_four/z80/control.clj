(ns four-six-four.z80.control
  (:require [clojure.tools.logging :as log]
            [four-six-four.z80.fetch :refer [fetch-instruction]]
            [four-six-four.z80.instructions :refer [operation]]
            [four-six-four.pprint :refer [format-instr]]
            [four-six-four.z80.vm :refer [*z80* is-running set-pc inc-refresh memcpy]]))

;;;; CPU control

(defn execute-program-sequence
  "Execute bound VM running program from `program` sequence. Nb this is only suitable
  for simple programs, i.e. without jumps, because the sequence is not written
  to memory."
  [program]
  (dorun
   (map operation program)))

(defn execute
  "Execute provided VM running program from `start` in memory. Runs until halted."
  [z80 start]
  (binding [*z80* z80]
    (log/infof "Z80 execution begin at 0x%x" start)
    (dosync (set-pc start))
    (while (is-running)
      (let [instr (fetch-instruction)]
        (log/trace (format-instr instr))
        (inc-refresh)
        (operation instr)))))

(defn execute-program
  "Execute bound VM running `program` copied into location `start`."
  [program start]
  (memcpy start program)
  (execute *z80* start))

