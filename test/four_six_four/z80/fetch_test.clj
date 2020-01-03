(ns four-six-four.z80.fetch-test
  (:require  [clojure.test :refer :all]
             [four-six-four.z80.fetch :refer :all]))

(def program
  {:object [0x78 0xB1 0xC8 0x1A 0x77 0x13 0x23 0x0B 0xC3 0x00 0x10]
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

org     1000h               ;Origin at 1000h
memcpy  public
loop    ld      a,b         ;Test BC,
        or      c           ;If BC = 0,
        ret     z           ;Return
        ld      a,(de)      ;Load A from (DE)
        ld      (hl),a      ;Store A into (HL)
        inc     de          ;Increment DE
        inc     hl          ;Increment HL
        dec     bc          ;Decrement BC
        jp      loop        ;Repeat the loop
"
   :url "https://en.wikipedia.org/wiki/Zilog_Z80#Example_code"
   :asm [{:op :ld   :dest {:mode :direct :od :a} :src {:mode :direct :od :b}}
         {:op :or   :src  {:mode :direct :od :c}}
         {:op :ret  :src  {:cond :z}}
         {:op :ld   :dest {:mode :direct :od :a} :src {:mode :indirect :od :de}}
         {:op :ld   :dest {:mode :indirect :od :hl} :src {:mode :direct :od :a}}
         {:op :inc  :src  {:mode :direct :od :de}}
         {:op :inc  :src  {:mode :direct :od :hl}}
         {:op :dec  :src  {:mode :direct :od :bc}}
         {:op :jp   :src  {:mode :imm :od [0x00 0x10]}}]})


#_(deftest assembler-test
  (testing "Assemble memcpy"))

(deftest disassembler-test
  (testing "Disassembler"
    (is (:asm program) (disassemble (:object program)))))
