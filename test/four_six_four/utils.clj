(ns four-six-four.utils-test
  (:require [four-six-four.utils :refer :all]
            [clojure.test :refer :all]))

(deftest crc-test
  (testing "CRC16"
    (is (= 0x02CE (crc16 (map int "CRCTEST")))))
  (testing "CRC32"
    (is (= 0x7934775B (crc32 (map int "CRCTEST"))))))
