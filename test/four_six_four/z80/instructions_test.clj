(ns four-six-four.z80.instructions-test
  (:require [clojure.test :refer :all]
            [four-six-four.z80.control :refer [execute-program]]
            [four-six-four.z80.vm
             :refer
             [*z80* make-z80 read-reg read-mem reset flags set-flag test-flag print-z80]]
            [four-six-four.z80.parser :refer [parse-assembly]]
            [clojure.string :as str]))

(defn z80-fixture [f]
  (binding [*z80* (make-z80)]
    (reset)
    (f)))

(use-fixtures :each z80-fixture)

(defn get-state
  "Return state to match expected."
  [expected]
  (reduce (fn [m [k v]]
            (assoc m k (case k
                         :acc (read-reg :a)
                         :set-flags (into #{} (keep #(when (test-flag %) %) (keys flags)))
                         :reset-flags (into #{} (keep #(when-not (test-flag %) %) (keys flags)))
                         :mem [(first v) (read-mem (second v))]
                         (read-reg k))))
          {}
          expected))


(defmacro test-program
  [program expected & body]
  (let [asm (if (vector? program) (str/join "\n" program) program)
        msg (if (vector? program) (last program) program)]
    `(let [ast# (parse-assembly ~asm)]
       (reset)
       (execute-program ast#)
       ~@body
       (is (= ~expected (get-state ~expected)) ~msg))))


(deftest single-op-test
  (testing "Arithmetic"
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
                    {:mem [0x2030 0x35]}))

    )

  (testing "Logical"
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
  
  )

