(ns four-six-four.utils
  (:import java.lang.StackWalker
           java.util.function.Function
           java.util.stream.Stream))

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

(defn crc16
  "Compute CRC-16 for Tape checksum [X-25 polynomial]."
  [xs]
  (letfn [(mask-and-shift [crc]
            (let [mask (- (bit-and crc 1))]
              (bit-xor (bit-shift-right crc 1) (bit-and 0x8408 mask))))
          (division-loop [crc b]
            (let [xcrc (bit-xor crc b)]
              (nth (iterate mask-and-shift xcrc) 8)))]
    (->> xs
         (map int)
         (reduce division-loop 0xFFFF)
         bit-not
         (bit-and 0xFFFF))))

(defn ensure-vector [x]
  (if (coll? x) x (vector x)))


(defn top-of-stacktrace []
  (let [pick-first (reify Function
                     (apply [_ stream]
                       (.. stream
                           (skip 1)
                           (findFirst))))]
    (.. StackWalker
        (getInstance)
        (walk pick-first)
        (get)
        (getMethodName))))
