(ns four-six-four.basic
  (:require [four-six-four.numbers :refer [le-bytes->int]]
            [four-six-four.streams :refer [read-value]]
            [clojure.string :as str])
  (:import java.io.ByteArrayInputStream))

(def basic-tokens
  (merge
   {0x01	":"
    0x02	:var-def    ; integer variable definition (defined with "%" suffix)
    0x03	:var-def ; string variable definition (defined with "$" suffix)
    0x04	:var-def  ; floating point variable definition (defined with "!" suffix)
    0x05	"var?"
    0x06	"var?"
    0x07	"var?"
    0x08	"var?"
    0x09	"var?"
    0x0a	"var?"
    0x0b  :var-def    ; variable definition (no suffix)
    0x0c	:var-def
    0x0d	:var-def
    0x0e	"0"
    0x0f	"1"
    0x10	"2"
    0x11	"3"
    0x12	"4"
    0x13	"5"
    0x14	"6"
    0x15	"7"
    0x16	"8"
    0x17	"9"
    0x18	"10"
    0x19	:byte     ; 8-bit integer decimal value
    0x1a	:word     ; 16-bit integer decimal value
    0x1b	:word ; 16-bit integer binary value (with "&X" prefix)
    0x1c	:word ; 16-bit integer hexadecimal value (with "&H" or "&" prefix)
    0x1d	:word  ; 16-bit BASIC program line memory address pointer (see notes)
    0x1e	:word; 16-bit integer BASIC line number
    0x1f	:float ; floating point value
    0x20	" " ; (space) symbol
    0x21	"!" ; symbol
    0x22	:string ; quoted string value
    0x7c	"|"
    0x80	"AFTER"
    0x81	"AUTO"
    0x82	"BORDER"
    0x83	"CALL"
    0x84	"CAT"
    0x85	"CHAIN"
    0x86	"CLEAR"
    0x87	"CLG"
    0x88	"CLOSEIN"
    0x89	"CLOSEOUT"
    0x8a	"CLS"
    0x8b	"CONT"
    0x8c	"DATA"
    0x8d	"DEF"
    0x8e	"DEFINT"
    0x8f	"DEFREAL"
    0x90	"DEFSTR"
    0x91	"DEG"
    0x92	"DELETE"
    0x93	"DIM"
    0x94	"DRAW"
    0x95	"DRAWR"
    0x96	"EDIT"
    0x97	"ELSE"
    0x98	"END"
    0x99	"ENT"
    0x9a	"ENV"
    0x9b	"ERASE"
    0x9c	"ERROR"
    0x9d	"EVERY"
    0x9e	"FOR"
    0x9f	"GOSUB"
    0xa0	"GOTO"
    0xa1	"IF"
    0xa2	"INK"
    0xa3	"INPUT"
    0xa4	"KEY"
    0xa5	"LET"
    0xa6	"LINE"
    0xa7	"LIST"
    0xa8	"LOAD"
    0xa9	"LOCATE"
    0xaa	"MEMORY"
    0xab	"MERGE"
    0xac	"MID$"
    0xad	"MODE"
    0xae	"MOVE"
    0xaf	"MOVER"
    0xb0	"NEXT"
    0xb1	"NEW"
    0xb2	"ON"
    0xb3	"ON BREAK"
    0xb4	"ON ERROR GOTO"
    0xb5	"ON SQ"
    0xb6	"OPENIN"
    0xb7	"OPENOUT"
    0xb8	"ORIGIN"
    0xb9	"OUT"
    0xba	"PAPER"
    0xbb	"PEN"
    0xbc	"PLOT"
    0xbd	"PLOTR"
    0xbe	"POKE"
    0xbf	"PRINT"
    0xc0	"'" ; (same function as REM keyword)
    0xc1	"RAD"
    0xc2	"RANDOMIZE"
    0xc3	"READ"
    0xc4	"RELEASE"
    0xc5	"REM"
    0xc6	"RENUM"
    0xc7	"RESTORE"
    0xc8	"RESUME"
    0xc9	"RETURN"
    0xca	"RUN"
    0xcb	"SAVE"
    0xcc	"SOUND"
    0xcd	"SPEED"
    0xce	"STOP"
    0xcf	"SYMBOL"
    0xd0	"TAG"
    0xd1	"TAGOFF"
    0xd2	"TROFF"
    0xd3	"TRON"
    0xd4	"WAIT"
    0xd5	"WEND"
    0xd6	"WHILE"
    0xd7	"WIDTH"
    0xd8	"WINDOW"
    0xd9	"WRITE"
    0xda	"ZONE"
    0xdb	"DI"
    0xdc	"EI"
    0xdd	"FILL"
    0xde	"GRAPHICS"
    0xdf	"MASK"
    0xe0	"FRAME"
    0xe1	"CURSOR"
    0xe3	"ERL"
    0xe4	"FN"
    0xe5	"SPC"
    0xe6	"STEP"
    0xe7	"SWAP"
    0xea	"TAB"
    0xeb	"THEN"
    0xec	"TO"
    0xed	"USING"
    0xee	">"
    0xef	"="
    0xf0	">="
    0xf1	"<"
    0xf2	"<>"
    0xf3	"=<"
    0xf4	"+"
    0xf5	"-"
    0xf6	"*"
    0xf7	"/"
    0xf8	"^"
    0xf9	"\\"
    0xfa	"AND"
    0xfb	"MOD"
    0xfc	"OR"
    0xfd	"XOR"
    0xfe	"NOT"
    0xff	:prefix}

   ;; ASCII printable symbols
   (into {}
         (map #(vector % (str (char %))) (range 0x23 0x7b)))))

(def basic-tokens-ff
  "Basic tokens prefixed by 0xFF."
  {0x00	"ABS"
   0x01	"ASC"
   0x02	"ATN"
   0x03	"CHR$"
   0x04	"CINT"
   0x05	"COS"
   0x06	"CREAL"
   0x07	"EXP"
   0x08	"FIX"
   0x09	"FRE"
   0x0a	"INKEY"
   0x0b	"INP"
   0x0c	"INT"
   0x0d	"JOY"
   0x0e	"LEN"
   0x0f	"LOG"
   0x10	"LOG10"
   0x11	"LOWER$"
   0x12	"PEEK"
   0x13	"REMAIN"
   0x14	"SGN"
   0x15	"SIN"
   0x16	"SPACE$"
   0x17	"SQ"
   0x18	"SQR"
   0x19	"STR$"
   0x1a	"TAN"
   0x1b	"UNT"
   0x1c	"UPPER$"
   0x1d	"VAL"
   0x40	"EOF"
   0x41	"ERR"
   0x42	"HIMEM"
   0x43	"INKEY$"
   0x44	"PI"
   0x45	"RND"
   0x46	"TIME"
   0x47	"XPOS"
   0x48	"YPOS"
   0x49	"DERR"
   0x71	"BIN$"
   0x72	"DEC$"
   0x73	"HEX$"
   0x74	"INSTR"
   0x75	"LEFT$"
   0x76	"MAX"
   0x77	"MIN"
   0x78	"POS"
   0x79	"RIGHT$"
   0x7a	"ROUND"
   0x7b	"STRING$"
   0x7c	"TEST"
   0x7d	"TESTR"
   0x7e	"COPYCHR$"
   0x7f	"VPOS"})

(def var-definition [:byte :word :msb-term-string])

(def tokens-multiple-operands
  {0x02 var-definition
   0x03 var-definition
   0x04 var-definition
   0x0b var-definition
   0x0c var-definition
   0x0d var-definition
   0x22 [:delim-string]
   0x19 [:byte]
   0x1a [:word]
   0x1b [:word]
   0x1c [:word]
   0x1d [:word]
   0x1e [:word]
   0x1f [:float]
   0x7c [:byte :msb-term-string]})   ; RSX

(defn basic-operands
  "Operand specification following token."
  [token]
  (get tokens-multiple-operands token 1))


;; This is the BASIC float.
(defn disassemble-float
  [[m2 m1 m0 sm e]]
  (let [e (- e 128)
        m3 (bit-or 0x80 (bit-and 0x7F sm))
        s (bit-and 0x80 sm)
        v (* (le-bytes->int [m3 m2 m1 m0]) (Math/pow  2 e))]
    (if (pos? s)
      (- v)
      v)))

(defn last-name-byte?
  "Predicate for last byte of name string, i.e. has bit 7 set."
  [c]
  (bit-test c 7))

(defn var-type->str
  [type]
  (case type
    2 "%"
    3 "$"
    4 "!"))

(defn disassemble-var-def
  [[var-type off1 off2 & xs]]
  (let [offset (le-bytes->int [off1 off2])
        [prefix remain] (split-with (complement last-name-byte?) xs)]
    [(apply str (conj prefix (first remain) (var-type->str var-type)))
     (rest remain)]))


(defn disassemble-one-token
  [[x & xs] token-map]
  (let [t (token-map x)]
    ;; Disassemble current token consuming as necessary.
    (case t
      :prefix (disassemble-one-token xs basic-tokens-ff)
      :var-def (disassemble-var-def xs)
      :word [(str (le-bytes->int [(first xs) (second xs)]))
             (nnext xs)]
      :byte [(str (first xs)) (next xs)]
      :string (let [[s xxs] (split-with (partial = 0x22) xs)]
                [(str \" (apply str (butlast s)) \") xxs])
      :float [(str (disassemble-float (take 5 xs)))
              (drop 5 xs)]
      nil [nil nil]
      [t xs])))

(defn disassemble-tokens
  ([xs]
   (disassemble-tokens xs nil))

  ([tokens line]
   (loop [[x & xs :as tokens] tokens
          line line]
     (if (or (nil? x) (zero? x))
       line
       (let [[append xxs] (disassemble-one-token tokens basic-tokens)]
         (recur xxs (str/join [line append])))))))


(defn disassemble-line
  [stream]
  (let [length (read-value :word stream)]
    (when (pos? length)
      (let [line-num (read-value :word stream)
            line-tokens (read-value [:byte-array (- length 4)] stream)]
        (apply str line-num " " (disassemble-tokens line-tokens))))))

(defn disassemble
  [bs]
  (let [stream (ByteArrayInputStream. (byte-array bs))]
    (take-while string? (repeatedly (partial disassemble-line stream)))))
