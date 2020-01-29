(ns four-six-four.pprint
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [cl-format]]
            [clojure.string :as str]))

;;; Numbers

(defn format-bin
  ([x]
   (when x
     (cl-format nil "~b" x)))
  ([w x]
   (when x
     (cl-format nil "~v,'0b" w x))))

(defn format-hex
  ([x]
   (when x
     (cl-format nil "~x" x)))
  ([w x]
   (when x
     (cl-format nil "~v,'0x" w x))))

;;; Hex dump
(def hex-width 16)

(defn hex-dump
  ([xs]
   (hex-dump xs 0))
  ([xs offset]
   (let [lines (partition-all hex-width xs)
         line-starts (map (partial * hex-width) (range offset (+ offset (count lines))))]
     (doseq [[line start] (map vector lines line-starts)]
       (cl-format true "~8,'0x: ~{~2,'0x~^ ~}~%" start line)))))


;;; Instructions

(defn format-loc
  [{:keys [mode od]}]
  (let [src-str (cond
                  (keyword? od) (name od)
                  (vector? od) (cl-format nil "~a~[~:;+~:*~a~]" (name (first od)) (second od))
                  :else (str (format-hex (if (< od 256) 2 4) od) "h"))]
    (cl-format nil "~:[~;(~]~a~2:*~:[~;)~]"
               (= mode :indirect)
               src-str)))

(defn format-instr
  [{:keys [op dest src]}]
  (cl-format nil "~4a ~:[~*~;~a,~]~:[~*~;~a~]"
             (when op (name op))
             dest (when dest (format-loc dest))
             src (when src (format-loc src))))

(defn format-decode
  [{:keys [loc opcode instr]}]
  (cl-format nil "~6x   ~{~2,'0x~^ ~} ~22,0t~a"
             loc
             opcode
             (format-instr instr)))

(defn format-assembly
  [assembly]
  (doall
   (->> assembly
        (map (comp str/trimr format-decode))
        (str/join "\n"))))

(defn print-assembly
  ([assembly outfile]
   (with-open [w (io/writer outfile)]
     (binding [*out* w]
       (print-assembly assembly))))

  ([assembly]
   (print (format-assembly assembly))))

