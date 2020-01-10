(ns four-six-four.cdt
  (:require [clojure.pprint :refer [cl-format]]
            [clojure.walk :refer [postwalk]]
            [four-six-four.numbers :refer [ceil-log2]])
  (:import java.io.FileInputStream))

;;;; CDT tape image format stream.
;;;; See spec at http://www.cpcwiki.eu/index.php/Format:CDT_tape_image_file_format
;;;; and at: https://www.worldofspectrum.org/TZXformat.html#TZXFORMAT

;;; Block definitions

(defn make-vector [x]
  (if (coll? x) x (vector x)))

(defn normalize-type [type]
  (case type
    :byte [:byte 1]
    :word [:byte 2]
    :dword [:byte 4]
    (make-vector type)))

(defn patch-references [fields this type]
  (if (keyword? type)
    (normalize-type type)
    [(first type)
     (postwalk (fn [x]
                 (cond
                   (keyword? x) (normalize-type x)
                   (and (symbol? x) (some #{x} fields)) `(~(keyword x) ~this)
                   :else x))
               (second type))]))

(defn clause->read-value [fields this stream [field type]]
  `(assoc ~this ~(keyword field) (read-value ~(patch-references fields this type) ~stream)))

(def block-id-map {})
(defmacro defblock
  "Define CDT block."
  [name id & clauses]
  {:pre [(even? (count clauses))]}
  (let [clause-pairs (partition 2 clauses)
        fields (map first clause-pairs)
        constructor (symbol (str "read->" name))
        [proto record stream] (repeatedly gensym)]

    `(do
       (defrecord ~name [~@fields])
       (defn ~constructor
         [~stream]
         (as-> {} ~record
           ~@(map (partial clause->read-value fields record stream) clause-pairs)
           (~(symbol (str "map->" name)) ~record)))
       ~(when id
          `(alter-var-root #'block-id-map assoc ~id #'~constructor)))))


(defblock Header nil
  magic [:char 7]
  eot :byte
  major :byte
  minor :byte)

(defblock StandardDataBlock 0x10
  pause-after :word
  length :word
  data [:byte-array length])

(defblock TurboDataBlock 0x11
  pilot-pulse :word
  sync-first-pulse :word
  sync-second-pulse :word
  zero-pulse :word
  one-pulse :word
  pilot-tone :word
  last-byte-bits-used :byte
  pause-after :word
  length [:byte 3]
  data [:byte-array length])

(defblock PureToneBlock 0x12
  pulse-length :word
  pulse-count :word)

(defblock PulseSequenceBlock 0x13
  pulse-count :byte
  pulse-lengths [:word-array pulse-count])

(defblock PureDataBlock 0x14
  zero-pulse-length :word
  one-pulse-length :word
  last-byte-bits-used :byte
  pause-after :word
  length [:byte 3]
  data [:byte-array length])

(defblock DirectRecordingBlock 0x15
  tstates-per-sample :word
  pause-after :word
  last-byte-bits-used :byte
  length [:byte 3]
  data [:byte-array length])

(defblock CSWRecording 0x18
  length :dword
  pause-after :word
  sampling-rate [:byte 3]
  compression-type :byte
  pulse-count :dword
  data [:byte-array (- length 10)])

(def ^:dynamic *symdef-max-pulse*)
(defblock SymdefStruct nil
  flags :byte
  pulse-lengths [:word-array *symdef-max-pulse*])

(defblock PrleStruct nil
  symbol-rep :byte
  repetitions :word)

(defblock GeneralizedDataBlock 0x19
  length :dword
  pause-after :word
  pilotsync-symbol-count :dword
  max-pilotsync-pulses :byte
  pilotsync-alphabet :byte
  data-symbol-count :dword
  max-data-pulses :byte
  data-alphabet :byte
  pilotsync-symbols [:symdef pilotsync-alphabet max-pilotsync-pulses]
  pilotsync-data [:prle data-symbol-count]
  data-symbols [:symdef data-alphabet max-data-pulses]
  data [:byte (Math/ceil (* (ceil-log2 data-alphabet) (/ data-symbol-count 8)))])

(defblock PauseBlock 0x20
  pause-duration :word)

(defblock GroupStartBlock 0x21
  name-length :byte
  name [:char name-length])

(defblock GroupEndBlock 0x22)

(defblock JumpToBlock 0x23
  relative-jump :word) ; interpreted as signed short

(defblock LoopStartBlock 0x24
  repetitions :word)

(defblock LoopEndBlock 0x25)

(defblock CallSequenceBlock 0x26
  call-count :word
  call-blocks [:word call-count])

(defblock ReturnFromSequenceBlock 0x27)

(defblock SelectStruct nil
  offset :byte
  length :byte
  description [:char length])

(defblock SelectBlock 0x28
  length :word
  selection-count :byte
  selections [:select selection-count])

(defblock StopTapeIf48KSpectrumBlock 0x2A
  length :word)

(defblock SetSignalLevelBlock 0x2B
  length :word
  signal-level :byte)

(defblock TextDescriptionBlock 0x30
  length :byte
  description [:char length])

(defblock MessageBlock 0x31
  time :byte
  length :byte
  message [:char length])

(defblock TextStruct nil
  text-id :byte
  length :byte
  text [:char length])

(defblock ArchiveInfo 0x32
  length :word
  text-count :byte
  texts [:text text-count])

(defblock HwInfoStruct nil
  hardware-type :byte
  hardware-id :byte
  hardware-info :byte)

(defblock HardwareTypeBlock 0x33
  hw-count :byte
  hw [:hwinfo hw-count])

(defblock CustomInfoBlock 0x35
  id-string [:char 10]
  length :dword
  info [:byte length])

(defblock GlueBlock 0x5A
  magic [:byte 9])

;; Containing structure.
(defrecord Cdt [header blocks])

;;; Reading binary values.

(ns-unmap *ns* 'read-value)
;; tranlsate :word :dword to [:byte 2] etc
(defn read-value-dispatch-fn [val & args]
  (mapv (fn [x]
          (if (keyword? x) x (class x)))
       val))
(defmulti read-value read-value-dispatch-fn)

(defmethod read-value [:byte Number] [[_ n] stream] ; little-endian
  (le-bytes->int (repeatedly n #(.read stream))))

(defmethod read-value [:byte-array Number] [[_ n] stream]
  (let [array (byte-array n)]
    (.read stream array)
    (mapv #(bit-and 0xFF (int %)) array))) ; convert to unsigned

(defmethod read-value [:word-array Number] [[_ n] stream]
  (repeatedly n #(read-value [:byte 2] stream)))

(defmethod read-value [:char Number] [[_ n] stream]
  (apply str (map char (read-value [:byte-array n] stream))))

(defmethod read-value [:symdef Number Number] [[_ n maxn] stream]
  (binding [*symdef-max-pulse* maxn]
    (read->SymdefStruct stream)))

(defmethod read-value [:prle Number] [[_ n] stream]
  (read->PrleStruct stream))

(defmethod read-value [:select Number] [[_ n] stream]
  (read->SelectStruct stream))

(defmethod read-value [:text Number] [[_ n] stream]
  (read->TextStruct stream))

(defmethod read-value [:hwinfo Number] [[_ n] stream]
  (read->HwInfoStruct stream))


;;; Read CDT

(defn block-seq
  "Return CDT blocks as a lazy sequence of blocks."
  [stream]
  (when-let [id (.read stream)]
    (when-let [reader (block-id-map id)]
      (cons (reader stream) (block-seq stream)))))

(defn read-cdt
  "Read CDT tape image."
  [filename]
  (with-open [stream (FileInputStream. filename)]
    (let [cdt (->Cdt (read->Header stream) [])]
      ;; TODO check magic
      (assoc cdt :blocks (vec (block-seq stream))))))

(defn summary-cdt
  "Summary printing of CDT image."
  [{header :header blocks :blocks}]
  (cl-format true "Header: ~a v~a.~a~%" (:magic header) (:major header) (:minor header))
  (cl-format true "Blocks: ~{~a~^, ~}~%" (map type blocks)))
