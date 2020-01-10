(ns four-six-four.z80.control
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.tools.logging :as log]
            [four-six-four.numbers :refer :all]
            [four-six-four.utils :refer [nilmap]]
            [four-six-four.z80.fetch :refer [fetch-instruction]]
            [four-six-four.z80.instructions :refer [operation]]
            [four-six-four.z80.pprint :refer [format-instr]]
            [four-six-four.z80.vm :refer [*z80* is-running set-pc]]))

;;;; CPU control

(defn execute-program
  "Execute VM running program from `program` sequence."
  [program]
  (dorun
   (map operation program)))

(defn execute
  "Execute VM running program from `start` in memory."
  [z80 start]
  (binding [*z80* z80]
    (log/infof "Z80 execution begin at 0x%x" start)
    (dosync (set-pc start))
    (while (is-running)
      (let [instr (fetch-instruction)]
        (log/trace (format-instr instr))
        (operation instr)))))
