(ns four-six-four.z80.vm
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.tools.logging :as log]
            [four-six-four.numbers :refer :all]
            [four-six-four.utils :refer [nilmap]]))

;;;; Z80 emulation

(def ^:dynamic *debug* true)

;;; Config

;(defonce +memory-size+ (* 1024 64))
(def +memory-size+ 32)

;;; State
(def ^:dynamic *z80*)

(defrecord Z80 [running? program-counter iff registers memory])
(defn make-z80 []
  (map->Z80
   {:running? (ref false)
    :program-counter (ref nil)
    :iff (ref [true true]) ;; Interrupt enable flip flops
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


(defn reset []
  (dosync
   (ref-set (:running? *z80*) false)
   (ref-set (:program-counter *z80*) 0)
   (ref-set (:iff *z80*) [true true])
   (commute (:registers *z80*) #(into {} (for [k (keys %)] [k 0])))
   (ref-set (:memory *z80*) (vec (repeat +memory-size+ 0))))
  *z80*)

;;; Interrupts

(defn test-iff [n]
  {:pre [(<= 0 n 1)]}
  (@(:iff *z80*) n))


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

;;; Program counter

(defn inc-pc
  ([]
   (inc-pc 1))
  ([n]
   (alter (:program-counter *z80*) + n)))

(defn set-pc
  [n]
  (ref-set (:program-counter *z80*) n))

(defn read-pc-byte
  []
  (let [byte (read-mem @(:program-counter *z80*))]
    (inc-pc)
    byte))

(defn is-running
  []
  @(:running? *z80*))

(defn toggle-running
  []
  (commute (:running? *z80*) not))


