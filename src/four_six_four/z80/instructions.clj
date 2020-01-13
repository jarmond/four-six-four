(ns four-six-four.z80.instructions
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.tools.logging :as log]
            [four-six-four.numbers
             :refer
             [low-nib pop-count-byte rotate-left rotate-right]]
            [four-six-four.z80.vm
             :refer
             [cond-flag
              read-mem
              read-reg
              reset-flag
              set-flag
              test-flag
              test-iff
              toggle-flag
              write-mem
              write-reg]]))

;;; Instructions

(defmulti operation (fn [instr & args] (:op instr)))

(defmethod operation :default [instr]
  (log/warn "unknown instruction" (name (:op instr)) "dest" (:dest instr) "src" (:src instr)))

(defmacro defop
  "Define instruction operation. Requires a `dispatch-val` vector of op keyword,
  dest mode keyword and src mode keyword, and optionally a :bit or :jpcond
  keyword. Accepts 1 or 2 vars in vector `args` with either src or dest and src
  operands."
  [dispatch-val [src-or-dest src] & body]
  {:pre [(keyword? dispatch-val)]}
  (let [instr (gensym)
        dest (when src src-or-dest)
        src  (if-not src src-or-dest src)] ; resolve one or two operands

    `(defmethod operation ~dispatch-val [~instr]
       (let [~@(when dest `(~dest (:dest ~instr)))
             ~@(when src `(~src (:src ~instr)))]
         ~@body))))

(declare read-val)
(defn resolve-indirect [operand]
  (cond
    (keyword? operand) (read-val {:mode :direct :od operand})
    (vector? operand) (let [[loc val] operand]
                        (+ (read-val {:mode :direct :od loc}) val))
    :else operand))

;; TODO handle bit cases?
(defn read-val [loc]
  (let [{mode :mode operand :od} loc]
    (case mode
      :imm operand
      :direct (read-reg operand)
      :indirect (read-mem (resolve-indirect operand)))))

(defn write-val [loc val]
  (let [{mode :mode operand :od} loc]
    (case mode
      :direct (write-reg operand val)
      :indirect (write-mem (resolve-indirect operand) val))))

(defn reg-8bit?
  "True if `reg` is an 8-bit register."
  [reg]
  (= 1 (count (name reg))))

(defn reg-wrap
  "Wrap value to register size to simulate overflow for 8 or 16 bit registers."
  [reg x]
  (bit-and x (if (reg-8bit? reg) 0xFF 0xFFFF)))

(defn reg-overflow
  "True if `x` is not a valid two's complement number for register `reg`."
  [reg x]
  (not
   (if (reg-8bit? reg)
     (<= -0x80 x 0x7F)
     (<= -0x8000 x 0x7FFF))))

;;; Loads

(defop :ld [dest src]
  (dosync
   (let [val (read-val src)]
     (write-val dest val)
     (when (and (= :direct (:mode src))
                (or (= :r (:od src)) (= :i (:od src))))
       (cond-flag (test-iff 0) :pv)
       (cond-flag (zero? val) :z)
       (cond-flag (bit-test val 7) :s)))))


;;; Arithmetic and logical group.

(defmacro defarithop
  "Convenience to define similar arithmetic operations. Options in map are
    :add-carry   add carry flag to result
    :set-test    whether to set carry flag (sets half-carry regardless)
    :carry-test  one of :sub or :add, selects carry test expression
    :setn        whether to set N flag for subtract operation
    :store       whether to store result in dest
    :by-one      add one to src and store in-place
    :implicit    operation works on implicit accumulator"
  [mnemonic op-fn
   {carry? :add-carry
    carry-test :carry-test
    setn :setn
    store? :store
    set-carry :set-carry
    by-one :by-one
    implicit :implicit}]
  (let [[x y r1 r2 msbit src is-8bit] (repeatedly gensym)
        dest (cond
               by-one src
               implicit accumulator
               :else (gensym))
        src (if by-one {:mode :imm :od 1} src)]
    `(defop ~mnemonic [~@(when-not (or by-one implicit) `(~dest)) ~src]
       (dosync
        ;; Compute result.
        (let [~x (read-val ~dest)
              ~y (read-val ~src)
              ~is-8bit (reg-8bit? (:od ~dest))
              ~msbit (if ~is-8bit 7 15)
              ~r1 (~op-fn ~x ~y ~(if carry?
                                   `(if (test-flag :c) 1 0)
                                   0))
              ~r2 (reg-wrap (:od ~dest) ~r1)]

          ;; Set flags.
          ~(if setn '(set-flag :n) '(reset-flag :n))
          ~(when set-carry
             `(cond-flag ~(case carry-test
                            :add `(bit-test ~r1 (inc ~msbit))
                            :sub `(>= ~x ~y))
                         :c));        Carry flag
          (when (or ~is-8bit ~carry?) ;       Only 8-bit instructions, ADC and SBC set these.
            (when ~is-8bit
              (cond-flag ~(case carry-test
                            :add `(bit-test ~r1 (inc (bit-shift-right~ msbit 1)))
                            :sub `(>= (low-nib ~x) (low-nib ~y)))
                         :h))
            (cond-flag (reg-overflow (:od ~dest) ~r1) :pv) ; Overflow flag
            (cond-flag (bit-test ~r2 ~msbit) :s);            Sign flag
            (cond-flag (zero? ~r2) :z)) ;                    Zero flag

          ~(when store? `(write-val ~dest ~r2)))))))

(def accumulator {:mode :direct :od :a})

(defarithop :add +
  {:set-carry true, :carry-test :add, :store true})
(defarithop :adc +
  {:set-carry true, :carry-test :add, :add-carry true, :store true})
(defarithop :sub -
  {:set-carry true, :carry-test :sub, :setn true, :store true :implicit true})
(defarithop :sbc -
  {:set-carry true, :carry-test :sub, :setn true, :add-carry true, :store true})
(defarithop :cp  -
  {:set-carry true, :carry-test :sub, :setn true, :add-carry true, :store false :implicit true})
(defarithop :inc +
  {:set-carry false, :carry-test :add, :store true, :in-place true})
(defarithop :dec -
  {:set-carry false, :carry-test :sub, :store true, :in-place true})

(defop :neg []
  (dosync
   (let [x (read-val accumulator)
         r (- x)]
     (cond-flag (zero? r) :z)
     (cond-flag (reg-overflow :a) :pv)
     (set-flag :n)

     ;; FIXME Not sure how carry is implemented for NEG. Assuming it is as if A = 0-A
     (cond-flag (pos? x) :c)
     (cond-flag (pos? (low-nib x)) :h)
     (write-val accumulator r))))

(defop :scf []
  (dosync
   (set-flag :c)
   (reset-flag :n)
   (reset-flag :h)))

(defop :ccf []
  (dosync
   (toggle-flag :c)
   (reset-flag :n)))


(defmacro deflogop
  "Convenience to define logical operations."
  [mnemonic op-fn {half :half}]
  `(defop ~mnemonic [src#]
     (dosync
      (let [x# (read-val accumulator)
            y# (read-val src#)
            r# (~op-fn x# y#)]

        (cond-flag (zero? r#) :z)                   ; Zero flag
        (cond-flag (even? (pop-count-byte r#)) :pv) ; Parity flag

        ;; Constantly reset flags.
        (reset-flag :c)
        (reset-flag :n)

        ;; For AND, H is set, otherwise reset.
        ~(when half
           '(set-flag :h)
           '(reset-flag :h))
        (cond-flag (bit-test r# 8) :s)))));           Sign flag

(deflogop :and bit-and {:half true})
(deflogop :or  bit-or  {:half false})
(deflogop :xor bit-xor {:half false})

;; Rotates and shifts

(defmacro defrotop
  "Convenience to define rotate and shift operations. Options are:
  :accumulator  operate on accumulator
  :lr           either :left or :right, whether to set carry to msb or lsb"
  [mnemonic op-fn {acc? :accumulator lr :lr carry? :carry}]
  (let [[src x r c] (repeatedly gensym)
        ;; Bit to copy for carry: msb or lsb for left or right rotates.
        out (case lr
              :left 7
              :right 0)
        in (case lr
             :left 0
             :right 7)]

    `(defop ~mnemonic [~@(when-not acc? `(~src))]
       (dosync
        (let [~x (read-val ~(if acc? accumulator src))
              ~r (~op-fn ~x)]
          ;; If a carry instruction, copy new flag, otherwise copy old carry.
          ~(if carry?
             `(if (bit-test ~x ~out)
                (bit-set ~r ~in)
                (bit-clear ~r ~in))
             `(if (test-flag :c)
                (bit-set ~r ~in)
                (bit-clear ~r ~in)))

          ;; Set carry flag.
          (cond-flag (bit-test ~x ~out) :c)

          ;; Set zero and parity flags for non-accumulator operands.
          ~(when-not acc?
             `((cond-flag (zero? ~r) :z)
               (cond-flag (even? (pop-count-byte ~r)) :pv))) ; Parity flag

          ;; Constantly reset flags.
          (reset-flag :h)
          (reset-flag :n)
          (cond-flag (bit-test ~r 7) :s) ; Sign flag

          ;; Store value
          (write-val ~(if acc? accumulator src) ~r))))))

(defn rotate-left1 [x]
  (-> (rotate-left x 1)
      (bit-and 0xFF)))
(defn rotate-right1 [x]
  (-> (rotate-right x 1)
      (bit-and 0xFF)))
(defn shift-left1 [x]
  (-> (bit-shift-left x 1)
      (bit-and 0xFF)))
(defn shift-right1 [x]
  (-> (bit-shift-right x 1)
      (bit-and 0xFF)))
(defn arith-shift-right1 [x]
  (-> (bit-or 0xFFFFFF00)
      (bit-shift-right 1)
      (bit-and 0xFF)))

(defrotop :rla  rotate-left1  {:accumulator true, :lr :left})
(defrotop :rra  rotate-right1 {:accumulator true, :lr :right})
(defrotop :rlca rotate-left1  {:accumulator true, :lr :left, :carry true})
(defrotop :rrca rotate-right1 {:accumulator true, :lr :right, :carry true})
(defrotop :rl   rotate-left1  {:lr :left})
(defrotop :rr   rotate-right1 {:lr :right})
(defrotop :rlc  rotate-left1  {:lr :left, :carry true})
(defrotop :rrc  rotate-right1 {:lr :right, :carry true})
(defrotop :sra  arith-shift-right1 {:lr :right})
(defrotop :srl  shift-right1  {:lr :right})
(defrotop :sla  shift-left1   {:lr :left})

;; General-purpose arithmetic



(defop :cpl []
  (dosync
   (let [a (read-val accumulator)]
     (write-val accumulator
                (bit-xor 0xFF a))
     (set-flag :n)
     (set-flag :h))))
