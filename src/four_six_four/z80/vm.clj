(ns four-six-four.z80.vm
  (:require [clojure.pprint :refer [cl-format]]
            [taoensso.timbre :as log]
            [four-six-four.numbers :refer :all]
            [four-six-four.utils :refer [nilmap crc32]]))

;;;; Z80 emulation

(def ^:dynamic *debug* true)

;;; Config

(defonce +memory-size+ (* 1024 64))

;;; State
(def ^:dynamic *z80*)

(defrecord Z80 [running? pc iff registers memory])
(defn make-z80 []
  (map->Z80
   {:running? (ref false)
    :pc (ref nil)
    :iff (ref [true true]) ;; Interrupt enable flip flops
    :im (ref nil)          ;; Interrupt mode
    :registers (ref
                (nilmap
                 [;; 16-bit indexes IX, IY
                  :ix
                  :iy
                  ;; 16-bit stack pointer
                  :sp
                  ;; 8-bit interrupt page address register I
                  :i
                  ;; 8-bit memory refresh register
                  :r
                  ;; General purpose registers
                  :a ; 8-bit accumulators A and 8-bit flags F
                  :f :b :c :d :e :h :l
                  ;; Alt pairs.
                  :af' :bc' :de' :hl']))
    :memory (ref nil)}))

(defmacro with-z80
  "Convenience for binding *z80* to a value."
  [z80 & body]
  `(binding [*z80* ~z80]
     ~@body))


(defn reset []
  (dosync
   (ref-set (:running? *z80*) false)
   (ref-set (:pc *z80*) 0)
   (ref-set (:im *z80*) 0)
   (ref-set (:iff *z80*) [false false])
   (commute (:registers *z80*) #(into {} (for [k (keys %)] [k 0])))
   (ref-set (:memory *z80*) (vec (repeat +memory-size+ 0))))
  nil)

;;; Interrupts

(defn test-iff [n]
  {:pre [(<= 0 n 1)]}
  (@(:iff *z80*) n))

(defn set-iff [n]
  {:pre [(<= 0 n 1)]}
  (alter (:iff *z80*) update n (constantly true)))

(defn reset-iff [n]
  {:pre [(<= 0 n 1)]}
  (alter (:iff *z80*) update n (constantly false)))

(defn set-im [n]
  {:pre [(<= 0 n 2)]}
  (ref-set (:im *z80*) n))

;;; Flags

(def flags {:c 0 :n 1 :pv 2 :h 4 :z 6 :s 7})

(defn test-flag
  [flag]
  (bit-test (@(:registers *z80*) :f) (flags flag)))

(defmacro define-flagop
  [name op]
  `(defn ~name [flag#]
     (alter (:registers *z80*) update :f #(~op % (flags flag#)))))

(define-flagop set-flag bit-set)
(define-flagop reset-flag bit-clear)
(define-flagop toggle-flag bit-flip)

(defn cond-flag
  [cond flag]
  (if cond
    (set-flag flag)
    (reset-flag flag)))

;;; Registers

(def reg-pairs {:af [:a :f] :bc [:b :c] :de [:d :e] :hl [:h :l]})
(defn read-reg
  [reg]
  (if-let [pair (reg-pairs reg)]
    (let [[msb lsb] (mapv (partial read-reg) pair)]
      (two-bytes->int msb lsb))
    (@(:registers *z80*) reg)))

(defn write-reg
  [reg val]
  (if-let [[high low] (reg-pairs reg)]
    (let [msb (high-byte val)
          lsb (low-byte val)]
      (write-reg high msb)
      (write-reg low lsb))
    (alter (:registers *z80*) assoc reg val)))

(defn alter-reg
  [reg f & args]
  (if-let [pair (reg-pairs reg)]
    (let [val (read-reg reg)]
      (write-reg reg (apply f val args)))
    (alter (:registers *z80*) apply update reg f args)))

;;; Memory

(defn read-mem
  [loc]
  {:pre (< -1 loc +memory-size+)}
  (@(:memory *z80*) loc))

(defn write-mem
  [loc val]
  {:pre (< -1 loc +memory-size+)}
  (alter (:memory *z80*) assoc loc val))

(defn splice [v start splice-vec]
  (vec (concat
        (subvec v 0 start)
        splice-vec
        (subvec v (+ start (count splice-vec))))))

(defn write-mem-vector
  [loc bytes]
  (alter (:memory *z80*) splice loc bytes))

(defn read-mem-vector
  [loc len]
  (subvec @(:memory *z80*) loc (+ loc len)))

;;; Program counter

(defn inc-pc
  ([]
   (inc-pc 1))
  ([n]
   (alter (:pc *z80*) + n)))

(defn set-pc
  [n]
  (ref-set (:pc *z80*) n))

(defn get-pc
  []
  @(:pc *z80*))

(defn read-pc-byte
  []
  (let [byte (read-mem @(:pc *z80*))]
    (inc-pc)
    byte))

(defn is-running
  []
  @(:running? *z80*))

(defn set-running
  []
  (ref-set (:running? *z80*) true))

(defn toggle-running
  []
  (commute (:running? *z80*) not))

(defn inc-refresh
  []
  (commute (:registers *z80*) update :r inc))

(defn print-z80
  ([print? z80]
   (with-z80 z80
     (print-z80 print?)))
  ([print?]
   (cl-format print?
              (str "#Z80[~:[H~;R~]@~4,'0x~%"
                   "    mem size ~:d crc32 ~8,'0x~%"
                   "    reg A  F  B  C  D  E  H  L  I  R  IX   IY   SP~%"
                   "        ~{~2,'0x ~}~{~4,'0x ~}~%"
                   "    alt A  F  B  C  D  E  H  L~%"
                   "        ~{~2,'0x ~}~%"
                   "    flags C  N  PV H  Z  S~%"
                   "          ~{~:[0~;1~]  ~}~%"
                   "]~%")
              @(:running? *z80*)
              @(:pc *z80*)
              +memory-size+
              (crc32 @(:memory *z80*))
              (map read-reg [:a :f :b :c :d :e :h :l :i :r])
              (map read-reg [:ix :iy :sp])
              (mapcat #(as-> (read-reg %) v [(high-byte v) (low-byte v)]) [:af' :bc' :de' :hl'])
              (map test-flag [:c :n :pv :h :z :s]))))

(defmethod clojure.core/print-method Z80 [x ^java.io.Writer writer]
  (.write writer (print-z80 false x)))

