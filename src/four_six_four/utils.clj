(ns four-six-four.utils)

(defn walk-count
  ([value form]
   (walk-count 0 value form))
  ([acc value form]
   (cond
     (coll? form) (reduce + (map (partial walk-count acc value) form))
     (= form value) 1
     :else 0)))
