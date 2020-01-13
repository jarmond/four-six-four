(ns four-six-four.z80.parser
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [instaparse.core :as insta]))

;;; Context-free grammar for Z80 assembly

(def z80-cfg
  (insta/parser
   "<program> = {statement? (<space> | <comment>)? <newline>}

    statement = (label <':'>)? <space> instr | instr
    <instr> = op (<space> arguments)? <space?> <comment?>
    <arguments> = src | dest <space?> <','> <space?> src

    (* keys matching the IR *)
    op = alpha
    dest = direct/jpcond/indirect
    src = direct/jpcond/imm/indirect

    imm = number|identifier
    direct = register
    jpcond = flag
    indirect = <'('> expression <')'>

    <expression> = location | location operator (number|identifier)
    <operator> = '+'|'-'
    <location> = register / identifier / number
    register = 'a'|'b'|'c'|'d'|'e'|'f'|'hl'|'ix'|'iy'|'af'|'bc'|'de'
    flag = 'nz'|'z'|'nc'|'c'|'po'|'pe'|'p'|'m'
    identifier = alpha|'$'
    <number> = dec|hex

    newline = '\\n'
    label = alpha
    space = #'[ \\t]+'
    dec = digits
    hex = hexdigits <'h'> | <'0x'> hexdigits
    <digits> = #'-?[0-9]+'
    <hexdigits> = #'-?[0-9a-f]+'
    <alpha> = #'[a-z]+'
    comment = #';.*'
    "))

(defn operand->ir
  [[mode [type val :as od]]]
  {:mode mode :od (case type
                    :register (keyword val)
                    :flag (keyword val)
                    :hex (Long/parseLong val 16)
                    :dec (Long/parseLong val)
                    :identifier val)})

(defn statement->ir
  [[stmt & nodes]]
  (when (= stmt :statement)
    (let [astmt (reduce (fn [m [k v]] (assoc m k v)) {} nodes)]
      (cond-> astmt
        (contains? astmt :op) (update :op keyword)
        (contains? astmt :src) (update :src operand->ir)
        (contains? astmt :dest) (update :dest operand->ir)))))


(defn parse-assembly [source]
  (let [source (str/lower-case (str source "\n"))]
    (-> source
        z80-cfg
        ast->ir)))

