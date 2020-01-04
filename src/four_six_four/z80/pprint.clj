(ns four-six-four.z80.pprint
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.string :as str]
            [four-six-four.numbers :refer [format-hex]]))

(defn format-loc
  [{:keys [mode od cond]}]
  (cl-format nil "~:[~;(~]~a~2:*~:[~;)~]"
             (= mode :indirect)
             (if (keyword? od)
               (name od)
               (format-hex od))))

(defn format-instr
  [{:keys [op dest src bla]}]
  (cl-format nil "~4a ~:[~*~;~a,~]~:[~*~;~a~]"
             (name op)
             dest (when dest (format-loc dest))
             src (when src (format-loc src))))

(defn format-decode
  [{:keys [loc opcode instr]}]
  (cl-format nil "~6x ~{~2,'0x~^ ~} ~20,0t~a"
             loc
             opcode
             (format-instr instr)))

(defn format-assembly
  [assembly]
  (doall
   (->> assembly
        (map format-decode)
        (str/join "\n"))))

(defn print-assembly
  [assembly]
  (print (format-assembly assembly)))
