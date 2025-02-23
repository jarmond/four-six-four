(ns four-six-four.cassette
  (:require [four-six-four.cdt :as cdt]
            [four-six-four.firmware :refer :all]
            [four-six-four.z80.vm :as z80]))

(defn make-cas [] (atom {:tape nil
                         :pos 0}))

(defn insert-tape [cas cdt]
  (swap! cas assoc :tape cdt))

;;; CAS - Cassette manager
(stub cas-initialise 0xbc65)
(stub cas-set-speed 0xbc68)
(stub cas-noisy 0xbc6b)
(stub cas-start-motor 0xbc6e)
(stub cas-stop-motor 0xbc71)
(stub cas-restore-motor 0xbc74)
(stub cas-in-open 0xbc77)
(stub cas-in-close 0xbc7a)
(stub cas-in-abandon 0xbc7d)
(stub cas-in-char 0xbc80)
(stub cas-in-direct 0xbc83)
(stub cas-return 0xbc86)
(stub cas-test-eof 0xbc89)
(stub cas-out-open 0xbc8c)
(stub cas-out-close 0xbc8f)
(stub cas-out-abandon 0xbc92)
(stub cas-out-char 0xbc95)
(stub cas-out-direct 0xbc98)
(stub cas-catalog 0xbc9b)
(stub cas-write 0xbc9e)

;;; TODO abstract mapping reg to vars
(defkernfn cas-read [cas] 0xbca1
  (let [sync (z80/read-reg :a)
        de (z80/read-reg :de)
        len (if (zero? de) 65536 de)
        addr (z80/read-reg :hl)
        data (cdt/cdt-data (:cdt @cas) n)]
    ;; TODO disable interruprs
    ;; TODO save motor state, turn on
    (if (> len (count data))
      (do
        (z80/reset-flag :c)
        (z80/write-reg :a 1))
      (do
        (z80/write-mem-vector addr (subvec 0 len))
        (z80/set-flag :c)
        (z80/write-reg :a 0)))))
;; TODO restore motor state, turn off
;; TODO move pointer along (tape pos)
 ;; TODO enable interruprs


(stub cas-check 0xbca4)

