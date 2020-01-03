(ns four-six-four.z80
  (:require [clojure.pprint :refer [cl-format]]
            [four-six-four.numbers :refer :all]))

;;;; Z80 emulation

(def ^:dynamic *debug* true)

;;; Config

;(defonce +memory-size+ (* 1024 64))
(def +memory-size+ 32)

;;; State

(defn nilmap
  "Create map with keys mapped to nil."
  [keys]
  (zipmap keys (repeat nil)))

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
                  :af ; 8-bit accumulators A and 8-bit flags F
                  :bc ; 16-bit general purpose pair BC
                  :de
                  :hl
                  ;; Alt pairs.
                  :af' :bc' :de' :hl']))
    :memory (ref nil)}))

(defn reset [z80]
  (dosync
   (ref-set (:running? z80) false)
   (ref-set (:program-counter z80) 0)
   (ref-set (:iff z80) [true true])
   (commute (:registers z80) #(into {} (for [k (keys %)] [k 0])))
   (ref-set (:memory z80) (vec (repeat +memory-size+ 0))))
  z80)

(defn read-mem
  [z80 loc]
  {:pre (< -1 loc +memory-size+)}
  (@(:memory z80) loc))

(defn write-mem
  [z80 loc val]
  {:pre (< -1 loc +memory-size+)}
  (commute (:memory z80) assoc loc val))

(defn inc-pc
  ([z80]
   (inc-pc z80 1))
  ([z80 n]
   (alter (:program-counter z80) + n)))

(defn set-pc
  [z80 n]
  (ref-set (:program-counter z80) n))

(defn read-pc-byte
  [z80]
  (let [byte (read-mem z80 @(:program-counter z80))]
    (inc-pc z80)
    byte))

(defn is-running
  [z80]
  @(:running? z80))

(defn toggle-running
  [z80]
  (commute (:running? z80) not))



