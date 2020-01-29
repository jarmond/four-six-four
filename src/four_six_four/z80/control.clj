(ns four-six-four.z80.control
  (:require [clojure.string :as str]
            [four-six-four.pprint :refer [format-decode format-hex hex-dump]]
            [four-six-four.utils :refer [flip]]
            [four-six-four.z80.fetch :refer [fetch-instruction]]
            [four-six-four.z80.instructions :refer [operation]]
            [four-six-four.z80.vm
             :refer
             [*z80*
              get-pc
              inc-refresh
              is-running
              print-z80
              read-mem
              read-mem-vector
              set-pc
              set-running
              toggle-running
              write-mem-vector]]
            [taoensso.timbre :as log]))

;;;; CPU control

(defn execute-program-sequence
  "Execute bound VM running program from `program` sequence. Nb this is only suitable
  for simple programs, i.e. without jumps, because the sequence is not written
  to memory."
  [program]
  (dorun
   (map operation program)))

(defn execute
  "Execute bound VM running program from `start` in memory. Runs until halted."
  [start]
  (if *z80*
    (do
      (log/infof "Z80 execution begin at 0x%x" start)
      (dosync
       (set-pc start)
       (set-running))
      (while (is-running)
        (let [decode (fetch-instruction)]
          (log/trace (format-decode (merge decode {:loc (get-pc)})))
          (dosync
           (inc-refresh))
          (operation (:instr decode)))))
    (log/fatal "No bound Z80"))):


(defn guess-radix
  [s]
  (let [t (str/trim s)]
    (cond
      (str/starts-with? t "0x") [16 (subs t 2)]
      (str/ends-with? t "h") [16 (subs t (dec (count t)))]
      :else [10 t])))


(defn parse-long
  "Safely parse long in dec, hex ."
  [s]
  (when s
    (let [[radix digits] (guess-radix s)]
      (try
        (if (= radix 10)
          (Long/parseLong (or digits ""))
          (Long/parseUnsignedLong (or digits "") radix))
        (catch NumberFormatException e nil)))))

(defn debugger-loop
  "User interface loop for debugging during stepwise execution."
  [decode]
  (loop []
    (newline)
    (println (format-decode (merge decode  ; subtract off opcode to show start of opcode
                                   {:loc (- (get-pc) (count (:opcode decode)))})))
    (print "?> ")
    (flush)
    (let [input (read-line)]
      (case (first input)
        ;; execute next
        \n nil
        ;; quit
        \q (do
             (println "quitting")
             (dosync
              (toggle-running))
             (print-z80 true)
             :quit)
        ;; print z80 state
        \p (do
             (print-z80 true)
             (recur))
        ;; print IR
        \i (do
             (print decode)
             (recur))
        ;; dump memory location
        \x (let [[_ x y] (str/split input #" ")]
             (if-let [loc (parse-long x)]
               (if-let [len (parse-long y)]
                 (hex-dump (read-mem-vector loc len) loc)
                 (do (newline)
                     (println (format-hex (read-mem loc)))))
               (print "must provide location"))
             (recur))
        ;; else
        (do
          (print "invalid command")
          (recur))))))


(defn execute-step
  "Execute bound VM running program from `start` in memory step-wise. Runs until
  halted or aborted."
  [start]
  (log/infof "Z80 execution begin at 0x%x" start)
  (dosync
   (set-pc start)
   (set-running))
  (while (is-running)
    (let [decode (fetch-instruction)]
      (when-not (= (debugger-loop decode) :quit)
        (inc-refresh)
        (operation (:instr decode))))))

(defn execute-program
  "Execute bound VM running `program` copied into location `start`."
  [program start & args]
  (let [kwargs (apply hash-map args)]
    (dosync
     (write-mem-vector start program))
    (if (:step kwargs)
      (execute-step start)
      (execute start))))
