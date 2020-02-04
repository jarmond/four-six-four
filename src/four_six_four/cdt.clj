(ns four-six-four.cdt
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [cl-format pprint]]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk]]
            [four-six-four.numbers :refer [be-bytes->int ceil-log2 le-bytes->int]]
            [four-six-four.pprint :refer [hex-dump print-assembly]]
            [four-six-four.streams :as stream]
            [four-six-four.utils :refer [crc16 ensure-vector]]
            [four-six-four.z80.fetch :refer [disassemble]])
  (:import [java.io ByteArrayInputStream FileInputStream]))

;;;; CDT tape image format stream.
;;;; See spec at http://www.cpcwiki.eu/index.php/Format:CDT_tape_image_file_format
;;;; and at: https://www.worldofspectrum.org/TZXformat.html#TZXFORMAT

;;; Block definitions

(defn patch-references
  "Replaces labels referring to other fields with the correct values."
  [fields this type]
  (if (keyword? type)
    (stream/normalize-type type)
    [(first type)
     (postwalk (fn [x]
                 (if (and (symbol? x) (some #{x} fields))
                   `(~(keyword x) ~this)
                   x))
               (second type))]))

(defn clause->read-value [fields this stream [field type]]
  `(assoc ~this ~(keyword field) (stream/read-value ~(patch-references fields this type) ~stream)))

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


;;; CPC Tape format

;; 256-bytes


(defblock TapeHeader 0x2C
  filename [:char 16]
  block-number :byte
  last-block? :byte
  filetype :byte
  length :word
  location :word
  first-block? :byte
  logical-length :word
  entry-address :word
  unallocated [:byte-array 36]
  padding [:byte-array 192]
  checksum [:be-byte 2]
  trailer :word)

(defblock TapeDataSegment nil
  data [:byte-array 256]
  checksum [:be-byte 2])

;; N*256 bytes for N=1,...,8
(def ^:dynamic *segment-count*)
(defblock TapeData 0x16
  blocks [:tape-segment *segment-count*]
  trailer :word)

;; Containing structure.
(defrecord Cdt [header blocks])

;;; Extending read-value for reading CDT structs.

(defmethod stream/read-value [:symdef Number Number] [[_ n maxn] stream]
  (binding [*symdef-max-pulse* maxn]
    (read->SymdefStruct stream)))

(defmethod stream/read-value [:prle Number] [[_ n] stream]
  (read->PrleStruct stream))

(defmethod stream/read-value [:select Number] [[_ n] stream]
  (read->SelectStruct stream))

(defmethod stream/read-value [:text Number] [[_ n] stream]
  (read->TextStruct stream))

(defmethod stream/read-value [:hwinfo Number] [[_ n] stream]
  (read->HwInfoStruct stream))


;;; Read CPC Tape blocks


(defmulti read-tape-block class)

(derive StandardDataBlock ::TapeBlockContainer)
(derive TurboDataBlock ::TapeBlockContainer)

(defmethod read-tape-block ::TapeBlockContainer [blk]
  (let [data (byte-array (:data blk))
        checksum (crc16 (rest data))
        stream (ByteArrayInputStream. data)
        id (.read stream)]
    (case id
      0x2C (assoc blk :data (read->TapeHeader stream))
      0x16 (assoc blk :data (binding [*segment-count* (mod (:length blk) 256)]
                              (->> stream
                                   read->TapeData
                                   ;; TODO check checksum
                                   :blocks
                                   (mapcat :data)
                                   vec)))
      blk))) ; If not recognised sync byte, leave untouched.

(defmethod read-tape-block :default [blk] blk)

(defmethod stream/read-value [:tape-segment Number] [[_ n] stream]
  (vec (repeatedly n #(read->TapeDataSegment stream))))

;;; Pretty-printing blocks

(defn trimr-nul
  "Trim trailing NULs from string."
  [s]
  (->> s
       (take-while (complement (partial = \u0000)))
       (apply str)))

(defn format-field [[k v]]
  (str (name k)
       " "
       (cond
         (vector? v) (cl-format false "#~4,'0d" (count v))
         (record? v) (cl-format false "Header{~a}" (str/join " " (map format-field v)))
         :else (str v))))

(defn format-block [blk]
  (str (type blk) " " (str/join " " (map format-field blk))))


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
      (->> stream
           block-seq
           (mapv read-tape-block)
           (assoc cdt :blocks)))))

(defn summary-cdt
  "Summary printing of CDT image."
  [{header :header blocks :blocks}]
  (cl-format true "Header: ~a v~a.~a~%" (:magic header) (:major header) (:minor header))
  (cl-format true "Blocks:~%~{>~a~^~%~}~%" (map format-block blocks)))


(defn cdt-ls
  "List files in CDT."
  [cdt]
  (->> (:blocks cdt)
       (keep #(get-in % [:data :filename]))
       (map trimr-nul)
       (into #{})))

(defn padr-nul
  "Pad on right with NULs"
  [s len]
  (let [pad (- len (count s))]
    (str/join [s (apply str (repeat pad \u0000))])))

(defn cdt-data
  "Extract data block from CDT by index."
  [cdt n]
  (:data (nth (:blocks cdt) n)))

;;; FIXME this should work by reading the header and going until last-block? = 255
(defn cdt-extract
  "Extract file from CDT."
  [cdt filename]
  (let [padded (padr-nul filename 16)]
    (->> (:blocks cdt)
         (partition 2 1) ; Pair each block with the following
         (filter (fn [[a b]] ; Keep pairs where first is a header with right filename
                   (= padded (get-in a [:data :filename]))))
         (sort-by (comp :block-number :data first))
         (map second) ; Extract data blocks
         (mapcat :data)
         vec)))


(defn cdt-cat
  "Print content of file in various forms."
  ([cdt filename-or-block format outfile]
   (with-open [w (io/writer outfile)]
     (binding [*out* w]
       (cdt-cat cdt filename-or-block format))))
  ([cdt filename-or-block format]
   (let [data (if (number? filename-or-block)
                (cdt-data cdt filename-or-block)
                (cdt-extract cdt filename-or-block))]
     (case format
       :disassemble (print-assembly (disassemble data))
       :text (print (apply str (map char data)))
       :ir (pprint (disassemble data))
       :hex (hex-dump data)))))
