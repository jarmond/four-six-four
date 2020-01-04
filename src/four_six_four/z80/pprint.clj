(ns four-six-four.z80.pprint
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.string :as str]
            [four-six-four.numbers :refer [format-hex]]))

(defn format-loc
  [{:keys [mode od bit jpcond]}]
  (let [dest-str (cond
                   bit (str bit ",")
                   jpcond (str (name jpcond) ",")
                   :else "")
        src-str (cond
                  (keyword? od) (name od)
                  (vector? od) (cl-format nil "~a~[~:;+~:*~a~]" (name (first od)) (second od))
                  :else (format-hex od))]
    (cl-format nil "~:[~;(~]~a~a~3:*~:[~;)~]"
               (= mode :indirect)
               dest-str
               src-str)))

(defn format-instr
  [{:keys [op dest src]}]
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
