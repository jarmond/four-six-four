(ns four-six-four.z80.decoder-test
  (:require [clojure.test :refer :all]
            [four-six-four.z80.decoder :refer :all]))

(deftest decode-test
  (testing "Testing load instructions"
    (is (= {:op :ld :dest {:mode :direct :od :c} :src {:mode :direct :od :b}}
           (decode [0x48])))
    (is (= {:op :ld :dest {:mode :indirect :od :hl} :src {:mode :direct :od :d}}
           (decode [0x72])))
    (is (= {:op :ld :dest {:mode :direct :od :e} :src {:mode :indirect :od [:ix 8]}}
           (decode [0xDD 0x5E] [8])))
    (is (= {:op :ld :dest {:mode :direct :od :a} :src {:mode :indirect :od 0x6F32}}
           (decode [0x3A] [0x32 0x6F])))
    (is (= {:op :ld :dest {:mode :direct :od :h} :src {:mode :imm :od 0x36}}
           (decode [0x26] [0x36])))
    (is (= {:op :ld :dest {:mode :indirect :od [:ix -15]} :src {:mode :imm :od 0x21}}
           (decode [0xDD 0x36] [0xF1 0x21])))
    (is (= {:op :ld :dest {:mode :direct :od :de} :src {:mode :imm :od 0x0659}}
           (decode [0x11] [0x59 0x06]))))
  (testing "Testing stack instructions"
    (is (= {:op :push :src {:mode :direct :od :af}}
           (decode [0xF5])))
    (is (= {:op :pop :src {:mode :direct :od :hl}}
           (decode [0xE1]))))
  (testing "Testing exchange instructions"
    (is (= {:op :ex :dest {:mode :direct :od :af}}
           (decode [0x08])))
    (is (= {:op :exx}
           (decode [0xD9]))))
  (testing "Testing block transfer and search instructions")
  (testing "Testing arithmetic and logical instructions"
    (is (= {:op :and :src {:mode :imm :od 0x07}}
           (decode [0xE6] [07])))
    (is (= {:op :daa}
           (decode [0x27])))
    (is (= {:op :cpl}
           (decode [0x2F])))
    (is (= {:op :neg}
           (decode [0xED 0x44])))
    (is (= {:op :ccf}
           (decode [0x3F])))
    (is (= {:op :scf}
           (decode [0x37]))))
  (testing "Testing rotate and shift instructions")
  (testing "Testing bit manipulation instructions")
  (testing "Testing control flow instructions"
    (is (= {:op :jp :src {:mode :imm :od 0x3E32}}
           (decode [0xC3] [0x32 0x3E])))
    (is (= {:op :rst :src {:mode :imm :od 0}}
           (decode [0xC7]))))
  (testing "Testing input/output instructions"
    (is (= {:op :in :dest {:mode :direct :od :a} :src {:mode :indirect :od 0xA0}}
           (decode [0xDB] [0xA0]))))
  (testing "Testing CPU control instructions"
    (is (= {:op :nop}
           (decode [0])))
    (is (= {:op :halt}
           (decode [0x76])))
    (is (= {:op :di}
           (decode [0xF3])))
    (is (= {:op :ei}
           (decode [0xFB])))))
