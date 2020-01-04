(ns four-six-four.numbers
  (:require [clojure.pprint :refer [cl-format]]))

;;; Number utilities

(defn format-bin
  ([x]
   (cl-format nil "~b" x))
  ([w x]
   (cl-format nil "~v,'0b" w x)))

(defn format-hex
  ([x]
   (cl-format nil "~x" x))
  ([w x]
   (cl-format nil "~v,'0x" w x)))

(defn digit->int [c]
  (- (int c) 48))

(defn bytes->int-le
  "Convert sequence of bytes into integer assuming little-endian order."
  [xs]
  (->> xs
       (map-indexed #(bit-shift-left %2 (* 8 %1)))
       (reduce +)))

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
  (bit-shift-right x 4))

(defn twos-comp
  "Interpret unsigned number `x` of size `bits` as two's complement."
  [bits x]
  (let [mask (bit-shift-left 1 (dec bits))]
    (+ (bit-and x (bit-not mask))
       (- (bit-and x mask)))))

(defn digit? [c] (<= (int \0) (int c) (int \9)))

