(ns four-six-four.numbers
  (:require [clojure.pprint :refer [cl-format]])
  (:import java.lang.Long))

;;; TODO organize or split

;;; Number utilities

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

(defn digit->int [c]
  (- (int c) 48))

(defn le-bytes->int
  "Convert sequence of bytes into integer assuming little-endian order."
  [xs]
  (as-> xs xxs
       (map bit-shift-left xxs (range 0 (* (count xs) 8) 8))
       (reduce bit-or xxs)))

(defn be-bytes->int
  "Convert sequence of bytes into integer assuming big-endian order."
  [xs]
  (le-bytes->int (reverse xs)))

(defn two-bytes->int
  [msb lsb]
  (bit-or (bit-shift-left msb 8) lsb))

(defn binary->int
  "Convert sequence of binary values to integer."
  [xs]
  (->> xs
       reverse
       (map-indexed #(bit-shift-left %2 %1))
       (reduce +)))

(defn binary-string->int
  "Convert binary string to integer. E.g. '101' -> 5."
  [xs]
  (->> xs
       (map digit->int)
       binary->int))

(defn get-bits
  "Specify digits to extract specific bits from number. E.g. '76' gives most
  two significant bits."
  [x digits]
  (let [shift (apply min digits)]
    (as-> digits d
      (reduce bit-set 0 d)
      (bit-and x d)
      (unsigned-bit-shift-right d shift))))

(defn low-nib [x]
  (bit-and 0x0F x))
(defn high-nib [x]
  (unsigned-bit-shift-right x 4))
(defn low-byte [x]
  (bit-and 0xFF x))
(defn high-byte [x]
  (unsigned-bit-shift-right x 8))

(defn twos-comp
  "Two's complement unsigned byte `x`."
  [x]
  (let [mask (bit-shift-left 1 7)]
    (- (bit-and x mask)
       (bit-and 0xFF (bit-and x (bit-not mask))))))

(defn ones-comp
  "One's complement unsigned byte `x`."
  [x]
  (bit-xor 0xFF x))

(defn digit? [c] (<= (int \0) (int c) (int \9)))

(defn floor-log2 [x]
  (- 63 (Long/numberOfLeadingZeros x)))

(defn ceil-log2 [x]
  (- 64 (Long/numberOfLeadingZeros (dec x))))

(defn pop-count-byte [x]
  (Long/bitCount (bit-and 0xFF x)))

(defn rotate-left [x n]
  (Long/rotateLeft x n))

(defn rotate-right [x n]
  (Long/rotateRight x n))
