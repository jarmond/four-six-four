(ns four-six-four.z80.instructions-test
  (:require [clojure.test :refer :all]
            [four-six-four.z80.control :refer [execute-program-sequence]]
            [four-six-four.z80.vm
             :refer
             [*z80* make-z80 read-reg read-mem-vector read-mem reset-z80 flags set-flag test-flag print-z80 with-z80]]
            [four-six-four.z80.parser :refer [parse-assembly]]
            [clojure.string :as str]))

(defn z80-fixture [f]
  (with-z80 (make-z80)
    (reset-z80)
    (f)))

(use-fixtures :each z80-fixture)

(defn get-state
  "Return state to match expected."
  [expected]
  (reduce (fn [m [k v]]
            (assoc m k (case k
                         :acc (read-reg :a)
                         :set-flags (into #{} (keep #(when (test-flag %) %) v))
                         :reset-flags (into #{} (keep #(when-not (test-flag %) %) v))
                         :mem [(first v) (read-mem (first v))]
                         :mem-block [(first v) (second v) (read-mem-vector (first v) (second v))]
                         (read-reg k))))
          {}
          expected))

(defn run-test-program
  [program]
  (let [asm (if (vector? program) (str/join "\n" program) program)]
    (let [ast (parse-assembly asm)]
      (reset-z80)
      (execute-program-sequence ast))))

(defmacro test-program
  [program expected & body]
  (let [asm (if (vector? program) (str/join "\n" program) program)
        msg (if (vector? program) (last program) program)]
    `(do
       (run-test-program ~asm)
       ~@body
       (is (= ~expected (get-state ~expected)) ~msg))))


(deftest arithmetic-test
  (testing "ADD"
    (test-program ["ld a, 44h"
                   "ld c, 11h"
                   "add a, c"]
                  {:acc 0x55})
    (test-program ["ld a, 23h"
                   "add a, 33h"]
                  {:acc 0x56})
    (test-program ["ld a, A0h"
                   "ld hl, 0Ah"
                   "ld (0Ah), 08h"
                   "add a, (hl)"]
                  {:acc 0xA8})
    (test-program ["ld a, 11h"
                   "ld ix, 1000h"
                   "ld (1005h), 22h"
                   "add a, (ix+5h)"]
                  {:acc 0x33})
    (test-program ["ld a, 11h"
                   "ld iy, 1000h"
                   "ld (1005h), 22h"
                   "add a, (iy+5h)"]
                  {:acc 0x33}))
  (testing "ADC"
    (test-program ["ld a, 16h"
                   "scf"
                   "ld hl, 6666h"
                   "ld (hl), 10h"
                   "adc a, (hl)"]
                  {:acc 0x27}))
  (testing "SUB"
    (test-program ["ld a, 29h"
                   "ld d, 11h"
                   "sub d"]
                  {:acc 0x18}))
  (testing "SBC"
    (test-program ["ld a, 16h"
                   "scf"
                   "ld hl, 3433h"
                   "ld (hl), 05h"
                   "sbc a, (hl)"]
                  {:acc 0x10}))
  (testing "CP"
    (test-program ["ld a, 63h"
                   "ld hl, 6000h"
                   "ld (6000h), 60h"
                   "cp (hl)"]
                  {:reset-flags #{:pv :z :s :c :h}}))
  (testing "INC"
    (test-program ["ld d, 28h"
                   "inc d"]
                  {:d 0x29}))
  (testing "INC"
    (test-program ["ld ix, 2020h"
                   "ld (2030h), 34h"
                   "inc (ix+10h)"]
                  {:mem [0x2030 0x35]})

    (test-program ["ld iy, 2020h"
                   "ld (2030h), 34h"
                   "inc (iy+10h)"]
                  {:mem [0x2030 0x35]}))
  (testing "DEC"
    (test-program ["ld d, 2ah"
                   "dec d"]
                  {:d 0x29})))

(deftest logical-test
  (testing "AND"
    (test-program ["ld b, 7bh"
                   "ld a, c3h"
                   "and b"]
                  {:acc 0x43}))
  (testing "OR"
    (test-program ["ld h, 48h"
                   "ld a, 12h"
                   "or h"]
                  {:acc 0x5a}))
  (testing "XOR"
    (test-program ["ld a, 96h"
                   "xor 5dh"]
                  {:acc 0xcb})))

(deftest general-purpose-arithmetic-and-contol-test
  (testing "NEG"
    (test-program ["ld a, 98h"
                   "neg"]
                  {:acc 0x68}))
  (testing "DAA"
    (test-program ["ld a, 0x15"
                   "add a, 0x27"
                   "daa"]
                  {:acc 0x42}))
  (testing "CCF"
    (test-program ["ld a, 0xff"
                   "add a, 0xff"
                   "ccf"]
                  {:reset-flags #{:c}}))
  (testing "SCF"
    (test-program ["scf"]
                  {:set-flags #{:c}}))

  )

(deftest rotate-shift-test
  (testing "RLCA"
    (test-program ["ld a, 0x88"
                   "rlca"]
                  {:acc 0x11 :set-flags #{:c}}))
  (testing "RLA"
    (test-program ["ld a, 0x76"
                   "scf"
                   "rla"]
                  {:acc 0xed :reset-flags #{:c}}))
  (testing "RRCA"
    (test-program ["ld a, 0x11"
                   "rrca"]
                  {:acc 0x88 :set-flags #{:c}}))
  (testing "RRA"
    (test-program ["ld a, 0xe1"
                   "rcf"
                   "rra"]
                  {:acc 0x70 :set-flags #{:c}}))
  (testing "RLC"
    (test-program ["ld b, 0x88"
                   "rlc b"]
                  {:b 0x11 :set-flags #{:c}}))
  (testing "RL"
    (test-program ["ld d, 0x8f"
                   "rcf"
                   "rl d"]
                  {:d 0x1e :set-flags #{:c}}))
  (testing "RR"
    (test-program ["ld c, 0xdd"
                   "rcf"
                   "rr c"]
                  {:c 0x6e :set-flags #{:c}}))
  (testing "SLA"
    (test-program ["ld l, 0xb1"
                   "sla l"]
                  {:l 0x62 :set-flags #{:c}}))
  (testing "SRA"
    (test-program ["ld b, 0xb8"
                   "sra b"]
                  {:b 0xdc :reset-flags #{:c}}))
  (testing "SRL"
    (test-program ["ld b, 0x8f"
                   "srl b"]
                  {:b 0x47 :set-flags #{:c}})) 
  (testing "RLD"
    (test-program ["ld hl, 0x5000"
                   "ld (hl), 0x31"
                   "ld a, 0x7a"
                   "rld"]
                  {:acc 0x73 :mem [0x5000 0x1a]}))
  (testing "RRD"
    (test-program ["ld hl, 0x5000"
                   "ld (hl), 0x20"
                   "ld a, 0x84"
                   "rrd"]
                  {:acc 0x80 :mem [0x5000 0x42]}))
  )

(defn run-test-program-standalone
  [program]
  (let [asm (if (vector? program) (str/join "\n" program) program)]
    (z80-fixture (fn []
                   (run-test-program asm)
                   *z80*))))
