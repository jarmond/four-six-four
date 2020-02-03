(ns four-six-four.z80.instructions
  (:require [clojure.pprint :refer [cl-format]]
            [taoensso.timbre :as log]
            [four-six-four.numbers
             :refer
             [low-nib high-nib pop-count-byte rotate-left rotate-right ones-comp byte->signed
             high-byte low-byte two-bytes->int]]
            [four-six-four.z80.vm
             :refer
             [cond-flag
              read-mem
              read-mem-vector
              read-reg
              reset-flag
              set-flag
              test-flag
              set-iff
              reset-iff
              test-iff
              set-im
              get-pc
              set-pc
              inc-pc
              toggle-flag
              write-mem
              write-mem-vector
              write-reg
              alter-reg
              call-trap
              push-val
              pop-val
              toggle-running]]))

;;; Instructions

(defmulti operation (fn [instr & args] (:op instr)))

(defmethod operation :default [instr]
  (log/warn (cl-format false "unknown instruction ~a dest ~a src ~a"
                       (some-> (:op instr) name)
                       (:dest instr)
                       (:src instr))))

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
         (dosync
          ~@body)))))

;;; Loads and stores

(declare read-val)
(defn resolve-indirect [operand]
  (cond
    (keyword? operand) (read-val {:mode :direct :od operand})
    (vector? operand) (let [[loc val] operand]
                        (+ (read-val {:mode :direct :od loc}) val))
    :else operand))

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


;;; Register helpers

(defn reg-8bit?
  "True if `reg` is an 8-bit register."
  [reg]
  (and (keyword? reg)
       (= 1 (count (name reg)))))

(defn reg-wrap
  "Wrap value to register size to simulate overflow for 8 or 16 bit registers."
  [is-8bit? x]
  (bit-and x (if is-8bit? 0xFF 0xFFFF)))

(defn reg-overflow?
  "True if `x` is not a valid two's complement number for register `reg`."
  [is-8bit? x]
  (not
   (if is-8bit?
     (<= -0x80 x 0x7F)
     (<= -0x8000 x 0x7FFF))))

;;; Emulator trap

(defop :trap []
  (call-trap))

;;; Load group

(defop :ld [dest src]
  (let [val (read-val src)]
    (write-val dest val)
    (when (and (= :direct (:mode src))
               (or (= :r (:od src)) (= :i (:od src))))
      (cond-flag (test-iff 0) :pv)
      (cond-flag (zero? val) :z)
      (cond-flag (bit-test val 7) :s))))

(defop :push [src]
  (push-val (read-val src)))

(defop :pop [src]
  (write-val src (pop-val)))

;;; Arithmetic group.
(def accumulator {:mode :direct :od :a})

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
               :else (gensym))]
    `(defop ~mnemonic [~@(when-not (or by-one implicit) `(~dest)) ~src]
       ;; Compute result.
       (let [~x (read-val ~dest)
             ~y ~(if by-one 1 `(read-val ~src))
             ~is-8bit (reg-8bit? (:od ~dest))
             ~msbit (if ~is-8bit 7 15)
             ~r1 (~op-fn ~x ~y ~(if carry?
                                  `(if (test-flag :c) 1 0)
                                  0))
             ~r2 (reg-wrap ~is-8bit ~r1)]

         ;; Set flags.
         ~(if setn '(set-flag :n) '(reset-flag :n))
         ~(when set-carry
            `(cond-flag ~(case carry-test
                           :add `(bit-test ~r1 (inc ~msbit))
                           :sub `(>= ~y ~x))
                        :c));        Carry flag
         (when (or ~is-8bit ~carry?) ;       Only 8-bit instructions, ADC and SBC set these.
           (when ~is-8bit
             (cond-flag ~(case carry-test
                           :add `(bit-test ~r1 4)
                           :sub `(>= (low-nib ~y) (low-nib ~x)))
                        :h))
           (cond-flag (reg-overflow? ~is-8bit ~r1) :pv) ; Overflow flag
           (cond-flag (bit-test ~r2 ~msbit) :s);            Sign flag
           (cond-flag (zero? ~r2) :z)) ;                    Zero flag

         ~(when store? `(write-val ~dest ~r2))))))


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
  {:set-carry false, :carry-test :add, :store true, :by-one true})
(defarithop :dec -
  {:set-carry false, :carry-test :sub, :store true, :by-one true})

;;; Logical operations

(defmacro deflogop
  "Convenience to define logical operations."
  [mnemonic op-fn {half :half}]
  `(defop ~mnemonic [src#]
     (let [x# (read-reg :a)
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
       (cond-flag (bit-test r# 8) :s);           Sign flag
       (write-reg :a r#))))

(deflogop :and bit-and {:half true})
(deflogop :or  bit-or  {:half false})
(deflogop :xor bit-xor {:half false})

;;; Rotates and shifts

(defmacro defrotop
  "Convenience to define rotate and shift operations. Options are:
  :accumulator  operate on accumulator
  :lr           either :left or :right, whether to set carry to msb or lsb
  :carry        carry logic, one of :shift, :carry, :rotate"
  [mnemonic op-fn {acc? :accumulator lr :lr carry :carry}]
  (let [[src x r r2 c] (repeatedly gensym)
        ;; Bit to copy for carry: msb or lsb for left or right rotates.
        out (case lr
              :left 7
              :right 0)
        in (case lr
             :left 0
             :right 7)]

    `(defop ~mnemonic [~@(when-not acc? `(~src))]
       (let [~x (read-val ~(if acc? accumulator src))
             ~r (~op-fn ~x)
             ;; If a carry instruction, copy new flag, otherwise copy old carry.
             ~r2 ~(case carry
                    :carry `(if (bit-test ~x ~out)
                              (bit-set ~r ~in)
                              ~r)
                    :rotate `(if (test-flag :c)
                               (bit-set ~r ~in)
                               ~r)
                    :shift r)]

         ;; Set carry flag.
         (cond-flag (bit-test ~x ~out) :c)

         ;; Set zero and parity flags for non-accumulator operands.
         ~@(when-not acc?
            `((cond-flag (zero? ~r2) :z)
              (cond-flag (even? (pop-count-byte ~r2)) :pv) ; Parity flag
              (cond-flag (bit-test ~r2 7) :s))) ; Sign flag

         ;; Constantly reset flags.
         (reset-flag :h)
         (reset-flag :n)

         ;; Store value
         (write-val ~(if acc? accumulator src) ~r2)))))

#_(defn rotate-left1 [x]
  (-> (rotate-left x 1)
      (bit-and 0xFF)))
#_(defn rotate-right1 [x]
  (-> (rotate-right x 1)
      (bit-and 0xFF)))
(defn shift-left1 [x]
  (-> (bit-shift-left x 1)
      (bit-and 0xFF)))
(defn shift-right1 [x]
  (-> (bit-shift-right x 1)
      (bit-and 0xFF)))
(defn arith-shift-right1 [x]
  (-> (bit-or 0xFFFFFF00 x)
      (bit-shift-right 1)
      (bit-and 0xFF)))

(defrotop :rla  shift-left1  {:accumulator true, :lr :left, :carry :rotate})
(defrotop :rra  shift-right1 {:accumulator true, :lr :right, :carry :rotate})
(defrotop :rlca shift-left1  {:accumulator true, :lr :left, :carry :carry})
(defrotop :rrca shift-right1 {:accumulator true, :lr :right, :carry :carry})
(defrotop :rl   shift-left1  {:lr :left, :carry :rotate})
(defrotop :rr   shift-right1 {:lr :right, :carry :rotate})
(defrotop :rlc  shift-left1  {:lr :left, :carry :carry})
(defrotop :rrc  shift-right1 {:lr :right, :carry :carry})
(defrotop :sra  arith-shift-right1 {:lr :right, :carry :shift})
(defrotop :srl  shift-right1  {:lr :right, :carry :shift})
(defrotop :sla  shift-left1   {:lr :left, :carry :shift})

(def reg-indirect-hl {:mode :indirect :od :hl})
(defop :rld []
  (let [hl (read-val reg-indirect-hl)
        acc (read-reg :a)]
    (write-val reg-indirect-hl (bit-or (bit-and 0xFF (bit-shift-left hl 4))
                                       (low-nib acc)))
    (write-reg :a (bit-or (bit-and 0xF0 acc)
                          (high-nib hl)))))

(defop :rrd []
  (let [hl (read-val reg-indirect-hl)
        acc (read-reg :a)]
    (write-val reg-indirect-hl (bit-or (bit-shift-right hl 4)
                                       (bit-shift-left (low-nib acc) 4)))
    (write-reg :a (bit-or (bit-and 0xF0 acc)
                          (low-nib hl)))))


;;; General-purpose arithmetic

(defn daa-add [high low]
  (if (test-flag :c)
    (if (or (test-flag :h) (<= 0xA low 0xF))
      [0x66 true]
      [0x60 true])
    (if (test-flag :h)
      (if (<= 0 high 9)
        [0x06 false]
        [0x66 true])
      (if (<= 0 low 9)
        (if (<= 0 high 9)
          [0 false]
          [0x60 true])
        (if (<= 0 high 8)
          [0x06 false]
          [0x66 true])))))

(defn daa-sub [high low]
  (if (test-flag :c)
    (if (test-flag :h)
      [0x9A true]
      [0xFA true])
    (if (test-flag :h)
      [0xFA false]
      [0 false])))

(defop :daa []
  (let [acc (read-reg :a)
        low (low-byte acc)
        high (high-byte acc)
        [adj carry] (if (test-flag :n)
                      (daa-sub high low)
                      (daa-add high low))]
    (write-reg :a (+ acc adj))
    (cond-flag carry :c)))

(defop :neg []
  (let [x (read-reg :a)
        r (bit-and 0xFF (- x))]
    (cond-flag (zero? r) :z)
    (cond-flag (reg-overflow? true x) :pv)
    (set-flag :n)
    (cond-flag (not (zero? x)) :c)
    (cond-flag (pos? (low-nib x)) :h)
    (write-reg :a r)))

(defop :scf []
  (set-flag :c)
  (reset-flag :n)
  (reset-flag :h))

(defop :ccf []
  (toggle-flag :c)
  (reset-flag :n))

(defop :cpl []
  (set-flag :h)
  (set-flag :n)
  (-> :a read-reg ones-comp (write-reg :a)))

(defop :nop [])

(defop :di []
  (reset-iff 0)
  (reset-iff 1))

(defop :ei []
  (set-iff 0)
  (set-iff 1))

(defop :halt []
  (log/info "CPU HALTED")
  (toggle-running))

(defop :im [src]
  (set-im (read-val src)))

;;; Bit group
(defop :bit [dest src]
  (let [{b :od} dest
        x (read-val src)]
    (cond-flag (not (bit-test x b)) :z)))

(defop :set [dest src]
  (let [{b :od} dest
        x (read-val src)]
    (write-val src (bit-set x b))))

(defop :res [dest src]
  (let [{b :od} dest
        x (read-val src)]
    (write-val src (bit-clear x b))))

;;; Jump group

(defn test-jpcond [jpcond]
  (case jpcond
    :nz (not (test-flag :z))
    :z  (test-flag :z)
    :nc (not (test-flag :c))
    :c  (test-flag :c)
    :po (not (test-flag :pv))
    :pe (test-flag :pv)
    :p  (not (test-flag :pv))
    :m  (test-flag :pv)))


(defmacro defjumpop
  "Convenience to jump operations."
  [mnemonic jump-fn relative?]
  (let [target (gensym)]
    `(defop ~mnemonic [dest# src#]
       (let [~target (read-val (or src# dest#))
             ~@(when relative?
                 `(~target (byte->signed ~target)))
             jpcond# (when (= (:mode dest#) :jpcond)
                       (:od dest#))]
         (when (or (nil? jpcond#) (test-jpcond jpcond#))
           (~jump-fn ~target))))))


(defjumpop :jp set-pc false)
(defjumpop :jr inc-pc true)


(def register-b {:mode :direct :od :b})
(defop :djnz [src]
  (let [target (byte->signed (read-val src))
        b (dec (read-val register-b))]
    (write-val register-b b)
    (when (pos? b)
      (inc-pc target))))

;;; Call and return group

(def stack-pointer {:mode :direct :od :sp})
(defn push-pc-and-jump
  [target]
  (let [pc (get-pc)]
    (push-val pc)
    (set-pc target)))

(defjumpop :call push-pc-and-jump false)

(defop :ret [src]
  (let [jpcond (:jpcond src)]
    (when (or (nil? jpcond) (test-jpcond jpcond))
      (set-pc (pop-val)))))

(defop :reti []
  (log/warn "STUB: RETI"))

(defop :retn []
  (log/warn "STUB: RETN"))

(defop :rst [dest src]
  (push-val (get-pc))
  (when dest
    (push-val (read-val dest)))
  (let [target (read-val src)]
    (set-pc target)))

;;; Input/output group

(defop :in [dest src]
  (log/warn "STUB IN"))

(defop :ini []
  (log/warn "STUB INI"))

(defop :inir []
  (log/warn "STUB INIR"))

(defop :ind [src]
  (log/warn "STUB IND"))

(defop :indr [src]
  (log/warn "STUB INDR"))

(defop :out [dest src]
  (log/warn "STUB OUT"))

(defop :outi []
  (log/warn "STUB OUTI"))

(defop :outir []
  (log/warn "STUB OUTIR"))

(defop :outd [src]
  (log/warn "STUB OUTD"))

(defop :outdr [src]
  (log/warn "STUB OUTDR"))


;;; Exchange and block transfer/search group

(defop :ex [dest src]
  (let [tmp (read-val src)]
    (write-val src (read-val dest))
    (write-val dest tmp)))

(defop :exx []
  (operation {:op :exx :dest {:mode :direct :od :bc} :src {:mode :direct :od :bc'}})
  (operation {:op :exx :dest {:mode :direct :od :de} :src {:mode :direct :od :de'}})
  (operation {:op :exx :dest {:mode :direct :od :hl} :src {:mode :direct :od :hl'}}))

;; macro on dec/inc for ldd
(defn transfer-byte
  "Implementation of LDI/LDD."
  [succ]
  (let [src (read-reg :hl)
        dest (read-reg :de)]
    (write-mem dest (read-mem src))
    (alter-reg :hl succ)
    (alter-reg :de succ)
    (alter-reg :bc dec)
    (cond-flag (pos? (read-reg :bc)) :pv)
    (reset-flag :n)
    (reset-flag :h)))

(defop :ldi []
  (transfer-byte inc))

(defop :ldd []
  (transfer-byte dec))

(defop :ldir []
  (transfer-byte inc)
  (when (test-flag :pv)
    (inc-pc -2)))

(defop :lddr []
  (transfer-byte dec)
  (when (test-flag :pv)
    (inc-pc -2)))

(defn search-byte [succ]
  [succ]
  (let [acc (read-reg :a)
        val (read-val reg-indirect-hl)
        cmp (- acc val)]
    (cond-flag (zero? cmp) :z)
    (cond-flag (neg? cmp) :s)
    (alter-reg :hl succ)
    (alter-reg :bc dec)
    (cond-flag (pos? (read-reg :bc)) :pv)
    (set-flag :n)))

(defop :cpi []
  (search-byte inc))

(defop :cpd []
  (search-byte dec))

(defop :cpir []
  (search-byte inc)
  (when (and (not (test-flag :z)) (test-flag :pv))
    (inc-pc -2)))

(defop :cpdr []
  (search-byte dec)
  (when (and (not (test-flag :z)) (test-flag :pv))
    (inc-pc -2)))
