(ns four-six-four.utils)

(defn nilmap
  "Create map with keys mapped to nil."
  [keys]
  (zipmap keys (repeat nil)))

(defn walk-count
  "Walk collection `coll` and count entries with `val`"
  ([val form]
   (walk-count 0 val form))
  ([acc val form]
   (cond
     (coll? form) (reduce + (map (partial walk-count acc val) form))
     (= form val) 1
     :else 0)))

(defn walk-member
  "Walk collection `coll` and test for membership of `val`"
  [val form]
  (if (coll? form)
    (some true? (map (partial walk-member val) form))
    (= val form)))

(defn crc32
  "Compute CRC-32"
  [xs]
  (let [arr (byte-array xs)
        crc (java.util.zip.CRC32.)]
    (. crc update arr 0 (count arr))
    (. crc getValue)))
