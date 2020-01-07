(ns four-six-four.z80.instructions
  (:require [clojure.pprint :refer [cl-format]]))

;;; Instructions

(defn op-dispatch [instr]
  (cond-> (vec (keep (partial get-in instr) [[:op] [:dest :mode] [:src :mode]]))
    (contains? (:src instr) :bit) (conj :bit)
    (contains? (:src instr) :jpcond) (conj :jpcond)))

(defmulti op op-dispatch)

(defmethod op :default [& args]
  (println "op default"))

(defmacro defop
  "Define instruction operation. Requires a `dispatch-val` vector of op keyword,
  dest mode keyword and src mode keyword, and optionally a :bit or :jpcond
  keyword. Accepts 1 or 2 vars in vector `args` with either src or dest and src
  operands."
  [dispatch-val [src-or-dest src] & body]
  {:pre [(<= 2 (count dispatch-val) 4)]}
  (let [instr (gensym)
        dest (when src src-or-dest)
        src  (if-not src src-or-dest src)] ; resolve one or two operands

    `(defmethod op ~dispatch-val [~instr]
       (let [~@(when dest `(~dest (:dest ~instr)))
             ~src (:src ~instr)]
         ~@body))))

(defop [:add :direct :direct] [dest src]
  (println "add direct direct" dest src))

(defop [:add :indirect :direct] [dest src]
  (println "add direct indirect"))
