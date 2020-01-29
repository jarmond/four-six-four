(ns four-six-four.z80.control-test
  (:require [clojure.test :refer :all]
            [four-six-four.z80.control :refer :all]
            [four-six-four.z80.test-programs :as p]
            [four-six-four.z80.vm :refer [read-mem-vector write-mem-vector *z80* make-z80 with-z80 print-z80 reset-z80 write-reg read-reg]]))

(defn z80-fixture [f]
  (with-z80 (make-z80)
    (reset-z80)
    (f)))

(use-fixtures :each z80-fixture)

(def test-string (mapv int "Amstrad CPC 464"))

(deftest program-test
  (testing "memcpy"
    (dosync
     (write-mem-vector 0x200 test-string)
     (write-reg :bc (count test-string))
     (write-reg :de 0x200)
     (write-reg :hl 0x100))
    (execute-program (:object p/memcpy) (:origin p/memcpy))
    (is (= test-string (read-mem-vector 0x100 (count test-string)))))

  (testing "bubble-sort"
    (let [n 10
          data (repeatedly n #(rand-int 255))
          loc 0x100]
      (reset-z80)
      (dosync
       (write-mem-vector loc data)
       (write-reg :hl loc)
       (write-reg :c n))
      (execute-program (:object p/bubble-sort) (:origin p/bubble-sort))
      (is (= (reverse (sort data)) (read-mem-vector loc n)))))

  (testing "multiply"
    (let [x 1024
          y 60]
      (reset-z80)
      (dosync
       (write-reg :de x)
       (write-reg :hl y))
      (execute-program (:object p/multiply) (:origin p/multiply))
      (is (= (* x y) (read-reg :hl))))))


(deftest block-instructions-test
  (testing "LDDR"
    (let [src [0xa5 0x36 0x88]]
      (reset-z80)
      (dosync
       (write-reg :hl 0x1114)
       (write-reg :de 0x2225)
       (write-reg :bc 3)
       (write-mem-vector 0x1112 src)
       (write-mem-vector 0x2223 [0xc5 0x59 0x66]))
      (execute-program [0xed 0xb8 0x76] 0)
      (is (= src (read-mem-vector 0x2223 3))))))

