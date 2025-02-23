(ns four-six-four.firmware
  (:require [four-six-four.z80.vm :as z80]
            [taoensso.timbre :as log]))

;; Trap is followed by RET to avoid returns in handlers.
;; This makes code reusable eg. for BASIC.
(defn write-trap
  [loc]
  (let [[op1 op2] z80/+trap-opcode+]
    (z80/write-mem loc op1)
    (z80/write-mem (+ loc 1) op2)
    (z80/write-mem (+ loc 2) 0xc9))) ; ret

(def trap-map
  "Map of addresses to kernel routines. Filled in by `defkernfn`."
  {})

(defmacro defkernfn
  "Construct a kernel routine and register in trap map."
  [name args addr & body]
  `(do
     (defn ~name [~@args]
       ~@body)
     (alter-var-root #'trap-map assoc ~addr #'~name)))

(defn stub-msg [msg]
  (log/warn "STUB:" msg))

(defmacro stub [stub-name addr]
  `(defkernfn ~stub-name [k#] ~addr
     (stub-msg ~(name stub-name))
     (ret)))


