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
  "Compute CRC-16 for Tape checksum (defaults to X-25 polynomial)."
  ([xs]
   (crc16 xs 0x8408))
  ([xs poly]
   (letfn [(mask-and-shift [crc]
             (let [mask (- (bit-and crc 1))]
               (bit-xor (bit-shift-right crc 1) (bit-and poly mask))))
           (division-loop [crc b]
             (let [xcrc (bit-xor crc b)]
               (nth (iterate mask-and-shift xcrc) 8)))]
     (->> xs
          (map int)
          (reduce division-loop 0xFFFF)
          bit-not
          (bit-and 0xFFFF)))))

(defn crc16-alt
  [xs]
  (letfn [(mask-and-shift [aux]
            (if (= 1 (bit-shift-right (bit-and aux 0x8000) 15))
              (bit-xor (bit-shift-left aux 1) 0x1021)
              (bit-shift-left aux 1)))
          (division-loop [crc b]
            (let [xcrc (bit-xor crc (bit-shift-left b 8))]
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
