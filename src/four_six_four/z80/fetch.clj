(ns four-six-four.z80.fetch
  (:require
   byte-streams
   [four-six-four.utils :refer [walk-member]]
   [four-six-four.z80 :refer [read-pc-byte]]
   [four-six-four.z80.decoder :refer [decode-opcode decode-operands]]))

;;; Fetcher

(defn opcode-size
  "Determine number of bytes to encode opcode with given `prefix`. Most
  instructions are encoded by 1 or 2 bytes, optionally followed by 1 or 2
  operands - these are read as 1/2 byte opcodes so the [[decode-opcode]] can be
  used to figure out how many operands are required."
  [prefix]
  (if (#{0xCB 0xDD 0xED 0xFD} prefix)
    2 1))

(defn operand-size
  "Determine number of operand bytes from decode `instr`uction."
  [instr]
  (condp walk-member instr
    :arg2 2
    :arg1 1
    0))


(defn fetcher
  "Fetch and decode instruction. Fetch-Decode sequence is as follows:
    1. Read first opcode byte.
    2. Call [[opcode-size]].
    3. If size is 2 bytes, read next opcode byte.
    4. If second byte is CB, read an internal operand byte and a final opcode byte.
    5. Call [[decode-opcode]] to decode opcode bytes.
    6. If we don't have internal operand
      a. Call [[operand-size]] on decoded instruction to determine number of operands.
      b. Read operand bytes.
    7. Call [[decode-operands]] to fill in operand in argument slots.

  Program counter is autoincremented. Some instructions, notably
  rotate/shifts/tests, violate the 1/2 byte opcode pattern and encode the
  operand within a set of 4 bytes, so these are special-cased here to be read as
  a 4 byte opcode."
  [reader]
  (when-let [prefix1 (reader)]
    (let [prefix2 (when (= (opcode-size prefix1) 2)
                    (reader))
          [internal-operand last] (when (= prefix2 0xCB)
                                    [(reader) (reader)])

          instr (decode-opcode [prefix1 prefix2 last])
          operands (if internal-operand
                     [internal-operand]
                     (repeatedly (operand-size instr) reader))

          ;; Reconstruct opcode as seen in memory.
          original-opcode
          (if (seq internal-operand)
            [prefix1 prefix2 internal-operand last]
            (vec (keep identity (concat [prefix1 prefix2] operands))))]
      {:opcode original-opcode :instr (decode-operands instr operands)})))

(defn fetch-instruction
  "Fetch and decode instruction from memory at PC."
  [z80]
  (dosync
   (fetcher (partial read-pc-byte z80))))

;; FIXME use clojure.java.io.reader
(defn disassemble
  "Disassemble bytes to assembly."
  [bytes]
  (let [n (count bytes)
        pc (atom 0)]
    (letfn [(get-byte []
              (when (< @pc n)
                (let [b (bytes @pc)]
                  (swap! pc inc)
                  b)))]
      (take-while (comp seq :instr)
                  (map #(assoc %2 :loc %1)
                       (repeatedly (fn [] @pc))
                       (repeatedly #(fetcher get-byte)))))))

(defn disassemble-file
  "Disassemble file to assembly."
  [filename]
  (->
   (java.io.File. filename)
   byte-streams/to-byte-array
   vec
   disassemble))
