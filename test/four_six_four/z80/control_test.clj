(ns four-six-four.z80.control-test
  (:require [clojure.test :refer :all]
            [four-six-four.z80.control :refer :all]
            [four-six-four.z80.test-programs :as p]
            [four-six-four.z80.vm :refer [read-mem-vector write-mem-vector *z80* make-z80 with-z80 print-z80 reset write-reg read-reg]]))

(defn z80-fixture [f]
  (with-z80 (make-z80)
    (reset)
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
      (reset)
      (dosync
       (write-mem-vector loc data)
       (write-reg :hl loc)
       (write-reg :c n))
      (execute-program (:object p/bubble-sort) (:origin p/bubble-sort))
      (is (= (reverse (sort data)) (read-mem-vector loc n)))))

  (testing "multiply"
    (let [x 1234
          y 786]
      (reset)
      (dosync
       (write-reg :de x)
       (write-reg :hl y))
      (execute-program (:object p/multiply) (:origin p/multiply))
      (is (= (* x y) (read-reg :hl))))))

