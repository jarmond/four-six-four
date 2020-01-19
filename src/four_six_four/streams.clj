(ns four-six-four.streams
  (:require [four-six-four.numbers :refer [be-bytes->int le-bytes->int]]
            [four-six-four.utils :refer [ensure-vector]]))

;;; Reading binary values from streams

(defn normalize-type [type]
  (case type
    :byte [:byte 1]
    :word [:byte 2]
    :dword [:byte 4]
    (ensure-vector type)))

(defn read-value-dispatch-fn [val & args]
  (if (vector? val)
    (mapv (fn [x]
            (if (keyword? x) x (class x)))
          val)
    (if (keyword? val)
      val
      (class val))))
(defmulti read-value read-value-dispatch-fn)

(defmethod read-value [:byte Number] [[_ n] stream] ; little-endian
  (le-bytes->int (repeatedly n #(.read stream))))

(defmethod read-value :byte [_ stream]
  (read-value [:byte 1] stream))

(defmethod read-value :word [_ stream]
  (read-value [:byte 2] stream))

(defmethod read-value :dword [_ stream]
  (read-value [:byte 4] stream))

(defmethod read-value [:be-byte Number] [[_ n] stream] ; Big-endian
  (be-bytes->int (repeatedly n #(.read stream))))

(defmethod read-value [:byte-array Number] [[_ n] stream]
  (let [array (byte-array n)]
    (.read stream array)
    (mapv #(bit-and 0xFF (int %)) array))) ; convert to unsigned

(defmethod read-value [:word-array Number] [[_ n] stream]
  (vec (repeatedly n #(read-value [:byte 2] stream))))

(defmethod read-value [:char Number] [[_ n] stream]
  (apply str (map char (read-value [:byte-array n] stream))))

