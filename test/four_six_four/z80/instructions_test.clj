(ns four-six-four.z80.instructions-test
  (:require [clojure.test :refer :all]
            [four-six-four.z80.control :refer [execute-program]]
            [four-six-four.z80.vm
             :refer
             [*z80* make-z80 read-reg reset set-flag test-flag]]))

(defn z80-fixture [f]
  (binding [*z80* (make-z80)]
    (reset)
    (println "Z80 reset")
    (f)))

(use-fixtures :each z80-fixture)

;; change to get state function
(defn get-state
  "Return accumulator and flags."
  ([]
   {:acc (read-reg :a)})
  ([flags]
   (merge (get-state) {:flags (->> (map test-flag flags)
                                   (zipmap flags))})))

(defn load-imm
  [reg imm]
  {:op :ld :dest {:mode :direct :od reg} :src {:mode :imm :od imm}})

(defn load-mem
  [loc val]
  (list
   (load-imm :a val)
   {:op :ld :dest {:mode :indirect :od loc} :src {:mode :direct :od :a}}
   {:op :xor :src {:mode :direct :od :a}}))

(defmacro test-program
  [regvals memvals setflags instr & body]
  `(let [program# [~@(concat (mapcat (fn [[loc val]] (load-mem loc val)) memvals)
                             (map (fn [[d s]] (load-imm d s)) regvals))
                   ~instr]]
     ~(when setflags
        `(dosync
          ~@(map #(list 'set-flag %) setflags)))
     (execute-program program#)
     ~@body))

(deftest single-op-test
  (testing "Arithmetic"
    (testing "ADD"
      (test-program {:a 0x44 :c 0x11} nil nil
                    {:op :add :dest {:mode :direct :od :a} :src {:mode :direct :od :c}}
                    (is (= {:acc 0x55} (get-state)) "ADD A, C"))
      (test-program {:a 0x23} nil nil
                    {:op :add :dest {:mode :direct :od :a} :src {:mode :imm :od 0x33}}
                    (is (= {:acc 0x56} (get-state)) "ADD A, 0x33"))
      (test-program {:a 0xA0 :hl 0x0A}
                    {0x0A 0x08} nil
                    {:op :add :dest {:mode :direct :od :a} :src {:mode :indirect :od :hl}}
                    (is (= {:acc 0xA8} (get-state)) "ADD A, (HL)"))
      (test-program {:a 0x11 :ix 0x05}
                    {0x0A 0x22} nil
                    {:op :add :dest {:mode :direct :od :a} :src {:mode :indirect :od [:ix 5]}}
                    (is (= {:acc 0x33} (get-state)) "ADD A, (IX+5)"))
      (test-program {:a 0x11 :iy 0x05}
                    {0x0A 0x22} nil
                    {:op :add :dest {:mode :direct :od :a} :src {:mode :indirect :od [:iy 5]}}
                    (is (= {:acc 0x33} (get-state)) "ADD A, (IY+5)")))

    (testing "ADC"
      (test-program {:a 0x16 :hl 0x04}
                    {0x04 0x10}
                    [:c]
                    {:op :adc :dest {:mode :direct :od :a} :src {:mode :indirect :od :hl}}
                    (is (= {:acc 0x27} (get-state)) "ADC A, (HL)")))

    (testing "SUB"
      (test-program {:a 0x29 :d 0x11} nil nil
                    {:op :sub :dest {:mode :direct :od :a} :src {:mode :indirect :od :d}}
                    (is (= {:acc 0x18} (get-state)) "SUB A, D")))


    )
  )

