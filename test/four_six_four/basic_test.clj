(ns test.four-six-four.basic-test
  (:require [clojure.test :refer :all]
            [four-six-four.basic :as b]))

(deftest basic-diassembly-test
  (testing "Disassemble line"
    (is (= ["10 GOSUB 200"] (b/disassemble [9 0 10 0 0x9f 0x1e 0xc8 0 0])))
    (is (= ["50 GOSUB 200"] (b/disassemble [9 0 50 0 0x9f 0x1e 0xc8 0 0])))))
