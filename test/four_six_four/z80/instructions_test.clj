(ns four-six-four.z80.instructions-test
  (:require [clojure.test :refer :all]
            [four-six-four.z80.control :refer [execute-program]]
            [four-six-four.z80.vm
             :refer
             [*z80* make-z80 read-reg reset flags set-flag test-flag print-z80]]
            [four-six-four.z80.parser :refer [parse-assembly]]
            [clojure.string :as str]))

(defn z80-fixture [f]
  (binding [*z80* (make-z80)]
    (reset)
    (println "Z80 reset")
    (f)))

(use-fixtures :each z80-fixture)

(defn get-state
  "Return state to match expected."
  [expected]
  (reduce (fn [m k]
            (assoc m k (case k
                         :acc (read-reg :a)
                         :set-flags (keep #(when (test-flag %) %) flags)
                         :reset-flags (keep #(when-not (test-flag %) %) flags))))
          {}
          (keys expected)))


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
                    {:acc 0x27})
    )))

