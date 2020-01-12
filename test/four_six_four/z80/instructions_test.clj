(ns four-six-four.z80.instructions-test
  (:require [clojure.test :refer :all]
            [four-six-four.z80.control :refer [execute-program]]
            [four-six-four.z80.vm
             :refer
             [*z80* make-z80 read-reg reset flags set-flag test-flag]]
            [four-six-four.z80.parser :refer [parse-assembly]]
            [clojure.string :as str]))

(defn z80-fixture [f]
  (binding [*z80* (make-z80)]
    (reset)
    (println "Z80 reset")
    (f)))

(use-fixtures :each z80-fixture)

#_(defn get-state
  "Return accumulator and flags."
  ([]
   {:acc (read-reg :a)})
  ([flags]
   (merge (get-state) {:flags (->> (map test-flag flags)
                                   (zipmap flags))})))

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
  [program expected]
  `(let [ast# (parse-assembly ~program)
         msg# (-> ~program str/split-lines last str/trim)]
     (execute-program ast#)
     (is (= ~expected (get-state ~expected)) msg#)))


(deftest single-op-test
  (testing "Arithmetic"
    (testing "ADD"
      (test-program " ld a, 0x44\n ld c, 0x11\n add a, c"
                    {:acc 0x55})
      (test-program " ld a, 0x23\n  add a, 0x33"
                    {:acc 0x56})
      (test-program " ld a, 0xA0\n ld hl, 0x0A\n ld (0x0A), 0x08\n add a, (hl)"
                    {:acc 0xA8})
      (test-program " ld a, 0x11\n ld ix, 0x05\n ld (0x05), 0x22\n add a, (ix+5)"
                    {:acc 0x33})
      (test-program " ld a, 0x11\n ld iy, 0x05\n ld (0x05), 0x22\n add a, (iy+5)"
                    {:acc 0x33})
    )))

