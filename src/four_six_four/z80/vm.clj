(ns four-six-four.z80.vm
  (:require [clojure.pprint :refer [cl-format]]
            [taoensso.timbre :as log]
            [four-six-four.numbers :refer :all]
            [four-six-four.utils :refer [nilmap zero-vector crc32]]))

;;;; Z80 emulation


;;; Config

(defonce +memory-size+ (* 1024 64))
(defonce +lower-rom-start+ 0)
(defonce +lower-rom-end+ 0x3fff)
(defonce +upper-rom-start+ 0xc000)
(defonce +upper-rom-end+ 0xffff)
(defonce +trap-opcode+ [0xcb 0x32])

;;; State
(def ^:dynamic *z80*)

(defrecord Z80 [running? pc iff im registers refresh memory roms traps])
(defn make-z80 []
  (map->Z80
   {:running? (ref false)
    :pc (ref nil)
    :iff (ref [true true]) ;; Interrupt enable flip flops
    :im (ref nil)          ;; Interrupt mode
    :registers (ref
                (nilmap
                 [;; 16-bit indexes IX, IY
                  :ix
                  :iy
                  ;; 16-bit stack pointer
                  :sp
                  ;; 8-bit interrupt page address register I
                  :i
                  ;; General purpose registers
                  :a ; 8-bit accumulators A and 8-bit flags F
                  :f :b :c :d :e :h :l
                  ;; Alt pairs.
                  :af' :bc' :de' :hl']))
    :refresh (atom nil) ; 8-bit memory refresh register
    :memory (ref nil)
    :roms (atom nil)
    :traps (atom nil)}))

(defmacro with-z80
  "Convenience for binding *z80* to a value."
  [z80 & body]
  `(binding [*z80* ~z80]
     ~@body))

(defn reset-z80 []
  (dosync
   (ref-set (:running? *z80*) false)
   (ref-set (:pc *z80*) 0)
   (ref-set (:im *z80*) 0)
   (ref-set (:iff *z80*) [false false])
   (commute (:registers *z80*) #(into {} (for [k (keys %)] [k 0])))
   (ref-set (:memory *z80*) (zero-vector +memory-size+))
   (reset! (:refresh *z80*) 0)
   (reset! (:traps *z80*) {})
   (reset! (:roms *z80*) {:lower nil :upper nil})
  nil))

;;; Interrupts

(defn test-iff [n]
  {:pre [(<= 0 n 1)]}
  (@(:iff *z80*) n))

(defn set-iff [n]
  {:pre [(<= 0 n 1)]}
  (alter (:iff *z80*) update n (constantly true)))

(defn reset-iff [n]
  {:pre [(<= 0 n 1)]}
  (alter (:iff *z80*) update n (constantly false)))

(defn set-im [n]
  {:pre [(<= 0 n 2)]}
  (ref-set (:im *z80*) n))

;;; Flags

(def flags {:c 0 :n 1 :pv 2 :h 4 :z 6 :s 7})

(defn test-flag
  [flag]
  (bit-test (@(:registers *z80*) :f) (flags flag)))

(defmacro define-flagop
  [name op]
  `(defn ~name [flag#]
     (alter (:registers *z80*) update :f #(~op % (flags flag#)))))

(define-flagop set-flag bit-set)
(define-flagop reset-flag bit-clear)
(define-flagop toggle-flag bit-flip)

(defn cond-flag
  [cond flag]
  (if cond
    (set-flag flag)
    (reset-flag flag)))

;;; Registers

(def reg-pairs {:af [:a :f] :bc [:b :c] :de [:d :e] :hl [:h :l]})
(defn read-reg
  [reg]
  (if-let [pair (reg-pairs reg)]
    (let [[msb lsb] (mapv (partial read-reg) pair)]
      (two-bytes->int msb lsb))
    (@(:registers *z80*) reg)))

(defn write-reg
  [reg val]
  (if-let [[high low] (reg-pairs reg)]
    (let [msb (high-byte val)
          lsb (low-byte val)]
      (write-reg high msb)
      (write-reg low lsb))
    (alter (:registers *z80*) assoc reg val)))

(defn alter-reg
  [reg f & args]
  (if-let [pair (reg-pairs reg)]
    (let [val (read-reg reg)]
      (write-reg reg (apply f val args)))
    (alter (:registers *z80*) update reg #(apply f % args))))

;;; Memory

(defn read-mem
  [loc]
  {:pre (< -1 loc +memory-size+)}
  ;; Check lower ROM select state.
  (if-let [rom (and (<= loc +lower-rom-end+)
                    (:lower @(:roms *z80*)))]
    (rom loc)
    ;; Check upper ROM select state.
    (if-let [rom (and (<= +upper-rom-start+ loc +upper-rom-end+)
                      (:upper @(:roms *z80*)))]
      (rom loc)
      (@(:memory *z80*) loc))))

;; NOTE RBB-vectors might be useful here
(defn write-mem
  [loc val]
  {:pre (< -1 loc +memory-size+)}
  (alter (:memory *z80*) assoc loc val))

(defn splice [v start splice-vec]
  (vec (concat
        (subvec v 0 start)
        splice-vec
        (subvec v (+ start (count splice-vec))))))

(defn write-mem-vector
  [loc bytes]
  (alter (:memory *z80*) splice loc bytes))

(defn read-mem-vector
  [loc len]
  (subvec @(:memory *z80*) loc (+ loc len)))

;;; Stack usage

(defn push-val [val]
  (let [sp (read-reg :sp)]
    ;; writing 16-bit val
    (write-mem (- sp 1) (high-byte val))
    (write-mem (- sp 2) (low-byte val))
    (alter-reg :sp - 2)))

(defn pop-val []
  (let [sp (read-reg :sp)
        ;; reading 16-bit val
        val (two-bytes->int (read-mem (inc sp)) (read-mem sp))]
    (alter-reg :sp + 2))
  val)

;;; Rom selectors

(defn set-lower-rom [rom]
  (swap! (:roms *z80*) assoc :lower rom))

(defn set-upper-rom [rom]
  (swap! (:roms *z80*) assoc :upper rom))

;;; Program counter

(defn inc-pc
  ([]
   (inc-pc 1))
  ([n]
   (alter (:pc *z80*) + n)))

(defn set-pc
  [n]
  (ref-set (:pc *z80*) n))

(defn get-pc
  []
  @(:pc *z80*))

(defn read-pc-byte
  []
  (let [byte (read-mem @(:pc *z80*))]
    (inc-pc)
    byte))

;;; Running state

(defn is-running
  []
  @(:running? *z80*))

(defn set-running
  []
  (ref-set (:running? *z80*) true))

(defn toggle-running
  []
  (commute (:running? *z80*) not))

(defn inc-refresh
  []
  (swap! (:refresh *z80*) (comp #(mod % 256) inc)))

;;; Trapping

(defn register-trap [addr f]
  (swap! (:traps *z80*) assoc addr f))

(defn call-trap []
  (let [addr (- (get-pc) 2) ; addr of trap instruction
        handler (get @(:traps *z80*) addr (fn [] (log/warn "Unhandled trap: " addr)))
        sp (read-reg :sp)
        ;; 16-bit return addr
        ret (two-bytes->int (read-mem (inc sp)) (read-mem sp))]
    (handler)
    (alter-reg :sp + 2)
    (set-pc ret)))

;;; Debugging

(defn print-z80
  ([print? z80]
   (with-z80 z80
     (print-z80 print?)))
  ([print?]
   (cl-format print?
              (str "#Z80[~:[H~;R~]@~4,'0x R~2,'0x~%"
                   "    mem size ~:d crc32 ~8,'0x LROM ~:[0~;1~] UROM ~:[0~;1~]~%"
                   "    reg A  F  B  C  D  E  H  L  I  IX   IY   SP~%"
                   "        ~{~2,'0x ~}~{~4,'0x ~}~%"
                   "    alt A  F  B  C  D  E  H  L~%"
                   "        ~{~2,'0x ~}~%"
                   "    flags C  N  PV H  Z  S~%"
                   "          ~{~:[0~;1~]  ~}~%"
                   "]~%")
              @(:running? *z80*)
              @(:pc *z80*)
              @(:refresh *z80*)
              +memory-size+
              (crc32 @(:memory *z80*))
              (:lower @(:roms *z80*))
              (:upper @(:roms *z80*))
              (map read-reg [:a :f :b :c :d :e :h :l :i])
              (map read-reg [:ix :iy :sp])
              (mapcat #(as-> (read-reg %) v [(high-byte v) (low-byte v)]) [:af' :bc' :de' :hl'])
              (map test-flag [:c :n :pv :h :z :s]))))

(defmethod clojure.core/print-method Z80 [x ^java.io.Writer writer]
  (.write writer (print-z80 false x)))

