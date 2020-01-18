(ns four-six-four.z80.fetch-test
  (:require [clojure.test :refer :all]
            [four-six-four.z80.test-programs :refer [bubble-sort memcpy]]
            [four-six-four.z80.fetch :refer :all]))

(deftest disassembler-test
  (testing "Disassembler"
    (testing "memcpy"
      (is (:asm memcpy) (disassemble (:object memcpy))))
    (testing "bubble sort"
      (is (:asm bubble-sort) (disassemble (:object bubble-sort))))))
