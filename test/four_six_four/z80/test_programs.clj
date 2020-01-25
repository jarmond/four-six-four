(ns four-six-four.z80.test-programs)

(def memcpy
  {:object [0x78 0xB1 0x28 0x08 0x1A 0x77 0x13 0x23 0x0B 0xC3 0x00 0x10 0x76]
   :source "
; memcpy --
; Copy a block of memory from one location to another.
;
; Entry registers
;      BC - Number of bytes to copy
;      DE - Address of source data block
;      HL - Address of target data block
;
; Return registers
;      BC - Zero

        org     1000h       ;Origin at 1000h
loop:   ld      a,b         ;Test BC,
        or      c           ;If BC = 0,
        jr      z,exit      ;Terminate
        ld      a,(de)      ;Load A from (DE)
        ld      (hl),a      ;Store A into (HL)
        inc     de          ;Increment DE
        inc     hl          ;Increment HL
        dec     bc          ;Decrement BC
        jp      loop        ;Repeat the loop
exit:
        halt
"
   :url "https://en.wikipedia.org/wiki/Zilog_Z80#Example_code"
   :origin 0x1000
   :asm [{:op :ld   :dest {:mode :direct :od :a} :src {:mode :direct :od :b}}
         {:op :or   :src  {:mode :direct :od :c}}
         {:op :jr   :dest {:cond :z} :src {:mode :imm :od 0x8}}
         {:op :ld   :dest {:mode :direct :od :a} :src {:mode :indirect :od :de}}
         {:op :ld   :dest {:mode :indirect :od :hl} :src {:mode :direct :od :a}}
         {:op :inc  :src  {:mode :direct :od :de}}
         {:op :inc  :src  {:mode :direct :od :hl}}
         {:op :dec  :src  {:mode :direct :od :bc}}
         {:op :jp   :src  {:mode :imm :od [0x00 0x10]}}
         {:op :halt}]})


(def bubble-sort
  (let [DATA [0x2600]
        FLAG 0]
    {:url "Zilog_Z-80_CPU_Technical_Manual.pdf?page=73"
     :source "
; BUBBLE LISTING PAGE 1 SOURCE STATEMENT
; *** STANDARD EXCHANGE (BUBBLE) SORT ROUTINE ***
;
; AT ENTRY:
; HL CONTAINS ADDRESS OF DATA
; C CONTAINS NUMBER OF ELEMENTS TO BE SORTED (1<C<256)
;
; AT EXIT: DATA SORTED IN ASCENDING ORDER
;
; USE OF REGISTERS
;
; REGISTER CONTENTS
;
; A        TEMPORARY STORAGE FOR CALCULATIONS
; B        COUNTER FOR DATA ARRAY
; C        LENGTH OF DATA ARRAY
; D        FIRST ELEMENT IN COMPARISON
; E        SECOND ELEMENT IN COMPARISON
; H        FLAG TO INDICATE EXCHANGE
; L        UNUSED
; IX       POINTER INTO DATA ARRAY
; IY       UNUSED
;
SORT:      LD (DATA), HL
LOOP:      RES FLAG,H
           LD B,C
           DEC B
           LD IX, (DATA)
NEXT:      LD A, (IX)
           LD D,A
           LD E,(IX+1)
           SUB E
           JR NC, NOEX-$
           LD (IX), E
           LD (IX+1), D
           SET FLAG,H
NOEX:      INC IX
           DJNZ NEXT-$

           BIT FLAG,H
           JR NZ, LOOP-$
           HALT
;
FLAG:      EQU 0
DATA:      DEFS 2
           END
"
     :origin 0x1000
     :object
     [0x22 0x26 0x00
      0xCB 0x84
      0x41
      0x05
      0xDD 0x2A 0x26 0x00
      0xDD 0x7E 0x00
      0x57
      0xDD 0x5E 0x01
      0x93
      0x30 0x08
      0xDD 0x73 0x00
      0xDD 0x72 0x01
      0xCB 0xC4
      0xDD 0x23
      0x10 0xEA
      0xCB 0x44
      0x20 0xDE
      0x76]
     :asm
     [{:op :ld   :dest {:mode :indirect :od DATA} :src {:mode :direct :od :hl}} ; LD (DATA), HL
      {:op :res  :src  {:mode :direct :od :h} :dest {:mode :bit :od FLAG}}
      {:op :ld   :dest {:mode :direct :od :b} :src {:mode :direct :od :c}}
      {:op :dec  :src  {:mode :direct :od :b}}
      {:op :ld   :dest {:mode :direct :od :idx} :src {:mode :indirect :od DATA}}
      {:op :ld   :dest {:mode :direct :od :a} :src {:mode :indirect :od :ix}}
      {:op :ld   :dest {:mode :direct :od :d} :src {:mode :direct :od :a}}
      {:op :ld   :dest {:mode :direct :od :e} :src {:mode :indirect :od [:ix 1]}}
      {:op :sub  :src {:mode :direct :od :e}}
      {:op :jr   :src {:mode :imm :od 10} :dest {:mode :jpcond :od :nc}}
      {:op :ld   :dest {:mode :indirect :od :ix} :src {:mode :direct :od :e}}
      {:op :ld   :dest {:mode :indirect :od [:ix 1]} :src {:mode :direct :od :d}}
      {:op :set  :src {:mode :direct :od :h} :dest {:mode :bit :od FLAG}}
      {:op :inc  :src {:mode :direct :od :ix}}
      {:op :djnz :src {:mode :imm :od -20}}
      {:op :bit  :src {:mode :direct :od :h} :dest {:mode :bit :od FLAG}}
      {:op :jr   :src {:mode :imm :od -32} :dest {:mode :jpcond :od :nz}}
      {:op :halt}]}))

(def multiply
  {:url "Zilog_Z-80_CPU_Technical_Manual.pdf?page=73"
   :source "
MULT:; UNSIGNED SIXTEEN BIT INTEGER MULTIPLY.
;      ON ENTRANCE: MULTIPLIER IN DE.
;                   MULTIPLICAND IN HL.
;      ON EXIT: RESULT IN HL.
;      REGISTER USES:
;      H HIGH ORDER PARTIAL RESULT
;      L LOW ORDER PARTIAL RESULT
;      D HIGH ORDER MULTIPLICAND
;      E LOW ORDER MULTIPLICAND
;      B COUNTER FOR NUMBER OF SHIFTS
;      C HIGH ORDER BITS OF MULTIPLIER
;      A LOW ORDER BITS OF MULTIPLIER
;
       LD B,16
       LD C,D
       LD A,E
       EX DE,HL
       LD HL,O
MLOOP: SRL C
       RRA
       JR NC, NOADD-$
       ADD HL,DE
NOADD: EX DE,HL
       ADD HL,HL
       EX DE,HL
       DJNZ MLOOP-$
       HALT
       END
"
   :origin 0x1000
   :object [0x06 0x10 0x4a 0x7b 0xeb 0x21 0x00 0x00 0xcb 0x39 0x1f 0x30 0x01 0x19 0xeb 0x29 0xeb 0x10 0xf5 0x76]
   :asm [{:op :ld, :dest {:mode :direct, :od :b}, :src {:mode :imm, :od 16}}
         {:op :ld, :dest {:mode :direct, :od :c}, :src {:mode :direct, :od :d}}
         {:op :ld, :dest {:mode :direct, :od :a}, :src {:mode :direct, :od :e}}
         {:op :ex, :dest {:mode :direct, :od :de}, :src {:mode :direct, :od :hl}}
         {:op :ld, :dest {:mode :direct, :od :hl}, :src {:mode :imm, :od 0}}
         {:op :srl, :src {:mode :direct, :od :c}}
         {:op :rra}
         {:op :jr, :src {:mode :imm, :od 1}, :dest {:mode :jpcond, :od :nc}}
         {:op :add, :dest {:mode :direct, :od :hl}, :src {:mode :direct, :od :de}}
         {:op :ex, :dest {:mode :direct, :od :de}, :src {:mode :direct, :od :hl}}
         {:op :add, :dest {:mode :direct, :od :hl}, :src {:mode :direct, :od :hl}}
         {:op :ex, :dest {:mode :direct, :od :de}, :src {:mode :direct, :od :hl}}
         {:op :djnz, :src {:mode :imm, :od 245}}
         {:op :halt}]})
