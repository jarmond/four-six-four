(ns four-six-four.tools
  (:require [clojure.set :as s]
            [clojure.pprint :refer [*print-base* pprint]]))

;;; Debug and dev tools


(defn filter-instr
  "Filter `ir` for instruction `op`."
  [ir op]
  (filter (comp (partial = op) :op :instr) ir))

(defn decoder? [m]
  (every? number? (keys m)))

(defn find-unused-opcodes
  "Function to find unused opcodes."
  [decoder]
  (letfn [(decoder? [m]
            (every? number? (keys m)))]
    (let [all (set (range 256))
          prefixes (select-keys decoder
                                (for [[k v] decoder :when (decoder? v)] k))]
      (apply s/union
             (s/difference all (keys decoder))
             (for [[k v] prefixes]
               (map #(vector k %)
                    (s/difference all (keys v))))))))

(defn print-unused-opcodes
  [decoder]
  (binding [*print-base* 16]
    (pprint (find-unused-opcodes decoder))))
