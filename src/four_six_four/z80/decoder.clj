(ns four-six-four.z80.decoder
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk postwalk-replace]]
            [four-six-four.numbers :refer :all]
            [four-six-four.pprint :refer [format-bin]]
            [four-six-four.utils :refer [walk-count]]))

;;; Decoder

(defn instr?
  "Test if map is an instruction."
  [m]
  (contains? m :op))

(def count-decoders (partial walk-count :op))

(defn merge-decoders
  "Merges opcode decode maps, preserving two nested levels. Raises exception if any
  instruction lost indicating duplicated opcodes."
  [& maps]
  (let [pre-count (reduce + (map count-decoders maps))
        merged (apply merge-with (partial merge-with merge) maps)
        post-count (count-decoders merged)]
    (if (not= pre-count post-count)
      (throw (IllegalArgumentException.
              (cl-format nil "pre (~a) /= post (~a)" pre-count post-count)))
      merged)))


(defn bit-pattern
  "Replace placeholders in bit-string pattern and evaluate. NB If value is too big to fit in
  pattern, it will silently expand the string."
  [pattern & values]
  (let [places (->> pattern
                    (partition-by #(or (digit? %) (int %)))
                    (filter (comp not digit? first))
                    (map (partial apply str)))
        binary-vals (map format-bin (map count places) values)
        replacements (zipmap places binary-vals)]
    (->> replacements
         (reduce-kv str/replace-first pattern)
         (binary-string->int))))

(defmacro for->map
  [& body]
  `(into {} (for ~@body)))

(defn generic-pattern-reg
  "Generate map across all possible reg operands using a bit `pattern` and a
  `form` with the placeholder :reg to be replaced."
  [pattern reg form]
  (for->map [[k v] reg]
            {(bit-pattern pattern k)
             (postwalk-replace {:reg v} form)}))

(def opcode-reg
  "Single register representations in opcodes."
  {0 :b, 1 :c, 2 :d, 3 :e, 4 :h, 5 :l, 7 :a})

(defn pattern-reg1
  [pattern form]
  (generic-pattern-reg pattern opcode-reg form))

(defn pattern-regpair-dd
  [pattern form]
  (generic-pattern-reg pattern {0 :bc, 1 :de, 2 :hl, 3 :sp} form))

(defn pattern-regpair-qq
  [pattern form]
  (generic-pattern-reg pattern {0 :bc, 1 :de, 2 :hl, 3 :af} form))

(defn pattern-regpair-pp
  [pattern form]
  (generic-pattern-reg pattern {0 :bc, 1 :de, 2 :ix, 3 :af} form))

(defn pattern-regpair-rr
  [pattern form]
  (generic-pattern-reg pattern {0 :bc, 1 :de, 2 :iy, 3 :af} form))

(defn pattern-jpcond
  [pattern form]
  (generic-pattern-reg pattern {0 :nz, 1 :z, 2 :nc, 3 :c, 4 :po, 5 :pe, 6 :p, 7 :m} form))

(defn generic-pattern-reg2
  "Generate map across all possible pairs of reg operands using a bit `pattern` and
  a `form` with the placeholders :reg1 and :reg2 to be replaced."
  [pattern reg1 reg2 form]
  (for->map [[k1 v1] reg1 [k2 v2] reg2]
            {(bit-pattern pattern k1 k2)
             (postwalk-replace {:reg1 v1 :reg2 v2} form)}))

(defn pattern-reg2
  [pattern form]
  (generic-pattern-reg2 pattern opcode-reg opcode-reg form))


(defn gen-index-pair
  "Generate pair of decodes with prefix bytes 0xDD and 0xFD for IX and IY respectively. Replaces
  placeholder :index."
  [lsb form]
  {0xDD {lsb (postwalk-replace {:index :ix} form)}
   0xFD {lsb (postwalk-replace {:index :iy} form)}})

(def eight-bit-load-group
  "Table 7.0-1: 8-bit load group."
  (merge-decoders
   ;; LD r, r'
   (pattern-reg2
    "01dddsss"
    {:op :ld :dest {:mode :direct :od :reg1} :src {:mode :direct :od :reg2}})
   ;; LD r, n
   (pattern-reg1
    "00ddd110"
    {:op :ld :dest {:mode :direct :od :reg} :src {:mode :imm :od :arg1}})
   ;; LD r, (HL)
   (pattern-reg1
    "01ddd110"
    {:op :ld :dest {:mode :direct :od :reg} :src {:mode :indirect :od :hl}})
   ;; LD r, (IX+d)
   {0xDD
    (pattern-reg1
     "01ddd110"
     {:op :ld :dest {:mode :direct :od :reg} :src {:mode :indirect :od [:ix :arg1]}})}
   ;; LD r, (IY+d)
   {0xFD
    (pattern-reg1
     "01ddd110"
     {:op :ld :dest {:mode :direct :od :reg} :src {:mode :indirect :od [:iy :arg1]}})}
   ;; LD (HL), r
   (pattern-reg1
    "01110sss"
    {:op :ld :dest {:mode :indirect :od :hl} :src {:mode :direct :od :reg}})
   ;; LD (IX+d), r
   {0xDD
    (pattern-reg1
     "01110sss"
     {:op :ld :dest {:mode :indirect :od [:ix :arg1]} :src {:mode :direct :od :reg}})}
   ;; LD (IX+d), r
   {0xFD
    (pattern-reg1
     "01110sss"
     {:op :ld :dest {:mode :indirect :od [:iy :arg1]} :src {:mode :direct :od :reg}})}
   ;; LD (HL), n
   {0x36
    {:op :ld :dest {:mode :indirect :od :hl} :src {:mode :imm :od :arg1}}}
   ;; LD (I?+d), n
   (gen-index-pair
    0x36
     {:op :ld :dest {:mode :indirect :od [:index :arg1]} :src {:mode :imm :od :arg2}})
   ;; LD A, (BC)
   {0x0A
    {:op :ld :dest {:mode :direct :od :a} :src {:mode :indirect :od :bc}}}
   ;; LD A, (DE)
   {0x1A
    {:op :ld :dest {:mode :direct :od :a} :src {:mode :indirect :od :de}}}
   ;; LD A, (nn)
   {0x3A
    {:op :ld :dest {:mode :direct :od :a} :src {:mode :indirect :od :argword}}}
   ;; LD (BC), A
   {0x02
    {:op :ld :dest {:mode :indirect :od :bc} :src {:mode :direct :od :a}}}
   ;; LD (DE), A
   {0x12
    {:op :ld :dest {:mode :indirect :od :de} :src {:mode :direct :od :a}}}
   ;; LD (nn), A
   {0x32
    {:op :ld :dest {:mode :indirect :od :argword} :src {:mode :direct :od :a}}}

   {0xED
    {;; LD A, I
     0x57 {:op :ld :dest {:mode :direct :od :a} :src {:mode :direct :od :i}}
     ;; LD A, R
     0x5F {:op :ld :dest {:mode :direct :od :a} :src {:mode :direct :od :r}}
     ;; LD I, A
     0x47 {:op :ld :dest {:mode :direct :od :i} :src {:mode :direct :od :a}}
     ;; LD R, A
     0x4F {:op :ld :dest {:mode :direct :od :r} :src {:mode :direct :od :a}}}}))

(def sixteen-bit-load-group
  "Table 7.0-2: 16-bit load group."
  (merge-decoders
   ;; LD dd, nn
   (pattern-regpair-dd
    "00dd0001"
    {:op :ld :dest {:mode :direct :od :reg} :src {:mode :imm :od :argword}})
   ;; LD I?, nn
   (gen-index-pair
    0x21
    {:op :ld :dest {:mode :direct :od :index} :src {:mode :imm :od :argword}})
   ;; LD HL, (nn)
   {0x2A
    {:op :ld :dest {:mode :direct :od :hl} :src {:mode :indirect :od :argword}}}
   ;; LD mm, (nn)
   {0xED
    (pattern-regpair-dd
     "01dd1011"
     {:op :ld :dest {:mode :direct :od :reg} :src {:mode :indirect :od :argword}})}
   ;; LD I?, (nn)
   (gen-index-pair
    0x2A
    {:op :ld :dest {:mode :direct :od :ix} :src {:mode :indirect :od :argword}})
   ;; LD (nn), HL
   {0x22
    {:op :ld :dest {:mode :indirect :od :argword} :src {:mode :direct :od :hl}}}
   ;; LD (nn), mm
   {0xED
    (pattern-regpair-dd
     "01dd0011"
     {:op :ld :dest {:mode :indirect :od :argword} :src {:mode :direct :od :reg}})}
   ;; LD (nn), I?
   (gen-index-pair
    0x22
    {:op :ld :dest {:mode :indirect :od :argword} :src {:mode :direct :od :index}})
   ;; LD SP, HL
   {0xF9
    {:op :ld :dest {:mode :direct :od :sp} :src {:mode :direct :od :hl}}}
   ;; LD SP, I?
   (gen-index-pair
    0xF9
    {:op :ld :dest {:mode :direct :od :sp} :src {:mode :direct :od :ix}})
   ;; PUSH qq
   (pattern-regpair-qq
    "11qq0101"
    {:op :push :src {:mode :direct :od :reg}})
   ;; PUSH IX
   (gen-index-pair
    0xE5
    {:op :push :src {:mode :direct :od :index}})
   ;; POP qq
   (pattern-regpair-qq
    "11qq0001"
    {:op :pop :src {:mode :direct :od :reg}})
   ;; POP I?
   (gen-index-pair
    0xE1
    {:op :pop :src {:mode :direct :od :index}})))

(def exchange-and-block-group
  "Table 7.0-3: Exchange group and block transfer and search group."
  (merge-decoders
   ;; EX DE, HL
   {0xEB
    {:op :ex :dest {:mode :direct :od :de} :src {:mode :direct :od :hl}}}
   ;; EX AF, AF'
   {0x08
    {:op :ex :dest {:mode :direct :od :af}}}
   ;; EXX
   {0xD9
    {:op :exx}}
   ;; EX (SP), HL
   {0xE3
    {:op :ex :dest {:mode :indirect :od :sp} :src {:mode :direct :od :hl}}}
   ;; EX (SP), I?
   (gen-index-pair
    0xE3
    {:op :ex :dest {:mode :indirect :od :sp} :src {:mode :direct :od :index}})
   ;; LDI
   {0xED
    {0xA0 {:op :ldi}
     ;; LDIR
     0xB0 {:op :ldir}
     ;; LDD
     0xA8 {:op :ldd}
     ;; LDDR
     0xB8 {:op :lddr}
     ;; CPI
     0xA1 {:op :cpi}
     ;; CPIR
     0xB1 {:op :cpir}
     ;; CPD
     0xA9 {:op :cpd}
     ;; CPDR
     0xB9 {:op :cpdr}}}))

(defn assoc-if
  "Assoc `key` `value` pair into `map` if `cond` is true, otherwise leaves map untouched."
  [cond map key val]
  (if cond
     (assoc map key val)
     map))

(defn gen-arithmetic-group
  "Generates arithmetic and logical instruction decoders which follow a consistent pattern."
  [mnemonic pat]
  (let [common (binary-string->int (str "10" pat "110"))
        explicit (#{:add :adc :sbc} mnemonic)] ; Explicit dest only for these instructions.
    (merge-decoders
     ;; ADD A, r
     (pattern-reg1
      (str "10" pat "rrr")
      (assoc-if explicit
                {:op mnemonic :src {:mode :direct :od :reg}}
                :dest {:mode :direct :od :a}))
     ;; ADD A, n
     {(binary-string->int (str "11" pat "110"))
      (assoc-if explicit
                {:op mnemonic :src {:mode :imm :od :arg1}}
                :dest {:mode :direct :od :a})}
     ;; ADD A, (HL)
     {common
      (assoc-if explicit
                {:op mnemonic :src {:mode :indirect :od :hl}}
                :dest {:mode :direct :od :a})}
     ;; ADD A, (IX+d)
     (gen-index-pair
      common
      (assoc-if explicit
                {:op mnemonic :src {:mode :indirect :od [:index :arg1]}}
                :dest {:mode :direct :od :a})))))

(defn gen-increment-group
  "Generates inc/dec instructions."
  [mnemonic pat]
  (let [common (binary-string->int (str "00110" pat))]
    (merge-decoders
     ;; INC r
     (pattern-reg1
      (str "00rrr" pat)
      {:op mnemonic :src {:mode :direct :od :reg}})
     ;; INC (HL)
     {common
      {:op mnemonic :src {:mode :indirect :od :hl}}}
     ;; INC (IX+d)
     (gen-index-pair
      common
      {:op mnemonic :src {:mode :indirect :od [:index :arg1]}}))))

(def eight-bit-arithmetic-group
  "Table 7.0-4: 8-bit arithmetic and logical group."
  (merge-decoders
   (gen-arithmetic-group :add "000")
   (gen-arithmetic-group :adc "001")
   (gen-arithmetic-group :sub "010")
   (gen-arithmetic-group :sbc "011")
   (gen-arithmetic-group :and "100")
   (gen-arithmetic-group :or  "110")
   (gen-arithmetic-group :xor "101")
   (gen-arithmetic-group :cp  "111")
   (gen-increment-group  :inc "100")
   (gen-increment-group  :dec "101")))


(def gp-arithmetic-cpu-control-group
  "Table 7.0-5: General purpose arithmetic operations and cpu control groups."
  (merge-decoders
   {0x27
    {:op :daa}}
   {0x2F
    {:op :cpl}}
   {0x3F
    {:op :ccf}}
   {0x37
    {:op :scf}}
   {0
    {:op :nop}}
   {0x76
    {:op :halt}}
   {0xF3
    {:op :di}}
   {0xFB
    {:op :ei}}
   {0xED
    {0x44 {:op :neg}
     0x46 {:op :im :src {:mode :imm :od 0}}
     0x56 {:op :im :src {:mode :imm :od 1}}
     0x5E {:op :im :src {:mode :imm :od 2}}}}))

(def sixteen-bit-arithmetic-group
  "Table 7.0-6: 16 arithmetic group."
  (merge-decoders
   ;; ADD HL, ss
   (pattern-regpair-dd
    "00ss1001"
    {:op :add :dest {:mode :direct :od :hl} :src {:mode :direct :od :reg}})
   {0xED
    (merge-decoders
     ;; ADC HL, ss
     (pattern-regpair-dd
      "01ss1010"
      {:op :adc :dest {:mode :direct :od :hl} :src {:mode :direct :od :reg}})
     ;; SBC HL, ss
     (pattern-regpair-dd
      "01ss0010"
      {:op :sbc :dest {:mode :direct :od :hl} :src {:mode :direct :od :reg}}))}
   ;; ADD IX, pp
   {0xDD
    (pattern-regpair-pp
     "00pp1001"
     {:op :add :dest {:mode :direct :od :ix} :src {:mode :direct :od :reg}})}
   ;; ADD IY, rr
   {0xFD
    (pattern-regpair-pp
     "00rr1001"
     {:op :add :dest {:mode :direct :od :iy} :src {:mode :direct :od :reg}})}
   ;; INC ss
   (pattern-regpair-dd
    "00ss0011"
    {:op :inc :src {:mode :direct :od :reg}})
   ;; INC I?
   (gen-index-pair
    0x23
    {:op :inc :src {:mode :direct :od :index}})
   ;; DEC ss
   (pattern-regpair-dd
    "00ss1011"
    {:op :dec :src {:mode :direct :od :reg}})
   ;; DEC I?
   (gen-index-pair
    0x2B
    {:op :dec :src {:mode :direct :od :index}})))

(defn gen-rotate-shift-group
  [mnemonic pat]
  "Generates rotate and shift instruction decoders."
  (let [common (binary-string->int (str "00" pat "110"))]
    (merge-decoders
     {0xCB
      (merge-decoders
       ;; RLC r
       (pattern-reg1
        (str "00" pat "rrr")
        {:op mnemonic :src {:mode :direct :od :reg}})
       ;; RLC (HL)
       {common
        {:op mnemonic :src {:mode :indirect :od :hl}}})}
     (gen-index-pair
      0xCB
      {common {:op mnemonic :src {:mode :indirect :od [:index :arg1]}}}))))

(def rotate-shift-group
  "Table 7.0-7: Rotate and shift group."
  (merge-decoders
   ;; RLCA
   {0x07 {:op :rlca}}
   ;; RLA
   {0x17 {:op :rla}}
   ;; RRCA
   {0x0F {:op :rrca}}
   ;; RRA
   {0x1F {:op :rra}}
   {0xED
    ;; RLD
    {0x6F {:op :rld}
     ;; RRD
     0x67 {:op :rrd}}}
   (gen-rotate-shift-group :rlc "000")
   (gen-rotate-shift-group :rl  "010")
   (gen-rotate-shift-group :rrc "001")
   (gen-rotate-shift-group :rr  "011")
   (gen-rotate-shift-group :sla "100")
   (gen-rotate-shift-group :sra "101")
   (gen-rotate-shift-group :srl "111")))

(def bit-map (vec (for [x (range 8)] [x x])))
(defn gen-set-reset-group
  [mnemonic pat]
  "Generate bit set and reset instruction decoders."
  (merge-decoders
   ;; SET b, r
   {0xCB
    (generic-pattern-reg2 (str pat "bbbrrr") bit-map opcode-reg
                          {:op mnemonic :src {:mode :direct :od :reg2} :dest {:mode :bit :od :reg1}})}
   ;; SET b, (HL)
   {0xCB
    (generic-pattern-reg (str pat "bbb110") bit-map
                         {:op mnemonic :src {:mode :indirect :od :hl} :dest {:mode :bit :od :reg}})}
   ;; SET b, (IX+d)
   {0xDD
    {0xCB
     (generic-pattern-reg (str pat "bbb110") bit-map
                          {:op mnemonic :src {:mode :indirect :od [:ix :arg1]} :dest {:mode :bit :od :reg}})}}
   ;; SET b, (IY+d)
   {0xFD
    {0xCB
     (generic-pattern-reg (str pat "bbb110") bit-map
                          {:op mnemonic :src {:mode :indirect :od [:iy :arg1]} :dest {:mode :bit :od :reg}})}}))

(def bit-set-group
  "Table 7.0-8: Bit set, reset and test group."
  (merge-decoders
   {0xCB
    (merge-decoders
     ;; BIT b, r
     (generic-pattern-reg2 "01bbbrrr" bit-map opcode-reg
                           {:op :bit :dest {:mode :bit :od :reg1} :src {:mode :direct :od :reg2}})
     ;; BIT b, [HL]
     (generic-pattern-reg "01bbb110" bit-map
                          {:op :bit :dest {:mode :bit :od :reg} :src {:mode :indirect :od :hl}}))}
   ;; BIT b, [IX+d]
   {0xDD
    {0xCB
     (generic-pattern-reg "01bbb110" bit-map
                          {:op :bit :dest {:mode :bit :od :reg} :src {:mode :indirect :od [:ix :arg1]}})}}

   (gen-set-reset-group :set "11")
   (gen-set-reset-group :res "10")))

(def jump-group
  "Table 7.0-9: Jump group"
  (merge-decoders
   ;; JP cc, nn
   (pattern-jpcond
    "11ccc010"
    {:op :jp :src {:mode :imm :od :argword} :dest {:mode :jpcond :od :reg}})
   ;; JP nn
   {0xC3 {:op :jp :src {:mode :imm :od :argword}}
    ;; JR e
    0x18 {:op :jr :src {:mode :imm :od :arg1}}
    ;; JR C, e
    0x38 {:op :jr :src {:mode :imm :od :arg1} :dest {:mode :jpcond :od :c}}
    ;; JR NC, e
    0x30 {:op :jr :src {:mode :imm :od :arg1} :dest {:mode :jpcond :od :nc}}
    ;; JR Z, e
    0x28 {:op :jr :src {:mode :imm :od :arg1} :dest {:mode :jpcond :od :z}}
    ;; JR NZ, e
    0x20 {:op :jr :src {:mode :imm :od :arg1} :dest {:mode :jpcond :od :nz}}
    ;; JP (HL) NB this does PC <- HL, so it's really using direct address from reg.
    0xE9 {:op :jp :src {:mode :direct :od :hl}}}
   ;; JP (I?)
   (gen-index-pair
    0xE9
    {:op :jp :src {:mode :direct :od :index}})
   ;; DJNZ e
   {0x10 {:op :djnz :src {:mode :imm :od :arg1}}}))

;; Amstrad CPC specifics:
;; RSTs 1, 2, 3 and 5 (0x08, 0x10, 0x18, and 0x28) take a 2-byte address
;; following instruction
(defn pattern-restart
  [pattern form]
  (merge-decoders
   (generic-pattern-reg pattern {0 0x00, 4 0x20, 6 0x30, 7 0x38} form)
   (generic-pattern-reg pattern {1 0x08, 2 0x10, 3 0x18, 5 0x28}
                        (merge form {:dest {:mode :imm :od :argword}}))))

(def call-return-group
  "Table 7.0-10: Call and return group."
  (merge-decoders
   ;; CALL nn
   {0xCD
    {:op :call :src {:mode :imm :od :argword}}
    ;; RET
    0xC9
    {:op :ret}}
   ;; CALL cc, nn
   (pattern-jpcond "11ccc100"
                   {:op :call :src {:mode :imm :od :argword} :dest {:mode :jpcond :od :reg}})
   ;; RET cc
   (pattern-jpcond "11ccc000"
                   {:op :ret :src {:mode :jpcond :od :reg}})
   ;; RETI
   {0xED
    {0x4D {:op :reti}
     ;; RETN
     0x45 {:op :retn}}}
   ;; RST p
   (pattern-restart "11ttt111"
                    {:op :rst :src {:mode :imm :od :reg}})))

(def input-output-group
  "Table 7.0-11: Input and output group."
  (merge-decoders
  ;; IN A, (n)
  {0xDB
   {:op :in :dest {:mode :direct :od :a} :src {:mode :indirect :od :arg1}}}
  ;; OUT (n), A
  {0xD3
   {:op :in :src {:mode :direct :od :a} :dest {:mode :indirect :od :arg1}}}
  ;; IN r, (C)
  {0xED
   (merge-decoders
    (pattern-reg1 "01rrr000"
                  {:op :in :dest {:mode :direct :od :reg} :src {:mode :indirect :od :c}})
    ;; OUT (C), r
    (pattern-reg1 "01rrr001"
                  {:op :out :src {:mode :direct :od :reg} :dest {:mode :indirect :od :c}}))}
  {0xED
   ;; IN flags, (C)
   {0x70
    {:op :in :dest {:mode :direct :od :f} :src {:mode :indirect :od :c}}
    ;; INI
    0xA2 {:op :ini}
    ;; INIR
    0xAA {:op :ind}
    ;; INDR
    0xBA {:op :indr}
    ;; OUTI
    0xA3 {:op :outi}
    ;; OTIR
    0xB3 {:op :otir}
    ;; OUTD
    0xAB {:op :outd}
    ;; OTDR
    0xBB {:op :otdr}}}))

;;; Fake instructions to control emulator.
(def emulator-group
  {0xCB
   {0x32
    ;; TRAP (call a trap function identified by PC)
    {:op :trap}}})

(def decoder
  "Merged nested map of decodes. First level is first byte, etc.."
  (merge-decoders
   eight-bit-load-group
   sixteen-bit-load-group
   exchange-and-block-group
   eight-bit-arithmetic-group
   gp-arithmetic-cpu-control-group
   sixteen-bit-arithmetic-group
   rotate-shift-group
   bit-set-group
   jump-group
   call-return-group
   input-output-group
   emulator-group))

(defn decode-opcode
  "Decode opcode into a symbolic form. Following Tables 7.0-*.
  Output is a map with key :op and optionally :src and :dest. The values of
  latter two are maps specifying a :mode (one of :imm :direct :indirect :bit)
  and :od (operand) to which the mode applies. The operand can be a numeric
  value, a keyword (register). For :imm, a vector of numeric values representing
  a 16-bit value is permitted. For :indirect, a vector of register and offset is
  permitted. An additional key is :bit which specifies a single bit position for
  the operand. Examples:

  LD A, B -> {:op :ld :dest {:mode :direct :od :a} :src {:mode :direct :od :b}}
  "
  [opcode-bytes]
  (let [[msb lsb arg1erand last-byte] opcode-bytes
        instr (decoder msb)]
    (when instr
      (if-not (instr? instr)
        (instr lsb)
        instr))))


(defn decode-operands
  "Complete decode of `instr`uction by filling in operand values."
  [instr [operand1 operand2 :as operands]]
  (->> instr
       ;; Replace arg keys.
       (postwalk-replace {:arg1 operand1
                          :arg2 operand2
                          :argword (when operand2
                                     (two-bytes->int operand2 operand1))})


       ;; Two's complement IX/Y offsets.
       (postwalk (fn [x]
                   (if (vector? x)
                     (let [[reg val] x]
                       (if-let [ix (#{:ix :iy} reg)]
                         [ix (byte->signed val)]
                         x))
                     x)))))

(defn decode
  "Convenience function for when both opcode and operands are available."
  ([opcode-bytes]
   (decode opcode-bytes nil))
  ([opcode-bytes operands]
   (-> opcode-bytes
       decode-opcode
       (decode-operands operands))))
