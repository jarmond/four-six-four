(ns dev
  (:require [clojure.java.io :as io]
            [four-six-four.cdt :refer :all]
            [four-six-four.cpc :refer :all]
            [four-six-four.pprint :refer :all]
            [four-six-four.tools :refer :all]
            [four-six-four.utils :refer :all]
            [four-six-four.z80.control :refer :all]
            [four-six-four.z80.fetch :refer [disassemble]]
            [four-six-four.z80.test-programs :as p]
            [four-six-four.z80.vm :refer :all]))

(def root-dir "/Users/jarmond/OneDrive/notes/cpc464")

(def spy-vs-spy (str root-dir "/tapes/Spy Vs Spy.cdt"))
(def spy-cdt (read-cdt spy-vs-spy))

(def welcome (str root-dir "/tapes/Welcome To Amstrad (Cara A).cdt"))
(def welcome-cdt (read-cdt welcome))

(def dizzy (str root-dir "/tapes/Dizzy V - Spellbound Dizzy.cdt"))
(def dizzy-cdt (read-cdt dizzy))
(def dizzy-entry (get-in dizzy-cdt [:blocks 1 :data :entry-address]))
(def dizzy-ir (disassemble (cdt-extract dizzy-cdt "SPELLBND") dizzy-entry))
(def dizzy-calls (filter-instr :call dizzy-ir))
(def dizzy-rsts (filter-instr :rst dizzy-ir))

(def basic-rom-file (str root-dir "/roms/basic.rom"))
(def basic-rom (slurp-bytes basic-rom-file))

(def os-rom-file (str root-dir "/roms/os.rom"))
(def os-rom (slurp-bytes os-rom-file))
(def os-ir (disassemble os-rom))

(def dev80 (make-z80))
(defn dizzy-load []
  (with-z80 dev80
    (reset-z80)
    (dosync
     (write-mem-vector dizzy-entry (cdt-extract dizzy-cdt "SPELLBND")))))

(defn dizzy-run []
  (with-z80 dev80
    (execute-step dizzy-entry)))

(def devcpc (make-cpc))

(defn trap-test
  [cpc]
  (with-z80 (:cpu cpc)
    (execute-program [0xcd 0x0 0x0 0x76] 0x1000 :step true)))

(defn rst-test
  [cpc]
  (with-z80 (:cpu cpc)
    (execute-program [0xcf 0xff 0x0 0x76] 0x1000 :step true)))




