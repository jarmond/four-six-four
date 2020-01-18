(ns four-six-four.z80.parser
  (:require [clojure.string :as str]
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
    dest = direct/jpcond/bit/indirect
    src = direct/jpcond/imm/indirect

    imm = number|identifier
    direct = register
    jpcond = flag
    indirect = <'('> (location | expression) <')'>
    bit = bitdigit

    expression = location <space>? operator <space>? (number|identifier)
    <operator> = '+'|'-'
    <location> = register / identifier / number
    register = 'a'|'b'|'c'|'d'|'e'|'f'|'h'|'l'|'hl'|'ix'|'iy'|'af'|'bc'|'de'
    flag = 'nz'|'z'|'nc'|'c'|'po'|'pe'|'p'|'m'
    identifier = alpha|'$'
    <number> = dec|hex

    newline = '\\n'
    label = alpha
    space = #'[ \\t]+'
    dec = digits
    hex = hexdigits <'h'> | <'0x'> hexdigits
    <digits> = #'-?[0-9]+'
    bitdigit = #'[0-7]'
    <hexdigits> = #'-?[0-9a-f]+'
    <alpha> = #'[a-z]+'
    comment = #';.*'
    "))

(declare expr->ir)
(defn val->ir
  [[type val :as operand]]
  (case type
    :register (keyword val)
    :flag (keyword val)
    :hex (Long/parseLong val 16)
    :dec (Long/parseLong val)
    :bitdigit (Long/parseLong val)
    :identifier val
    :expression (expr->ir operand)))

(defn expr->ir
  [[expr loc operator val]]
  (let [loc-ir (val->ir loc)
        val-ir (val->ir val)
        op-fn (-> operator symbol resolve)]
    (if (number? loc-ir)
      (op-fn loc-ir val-ir)      ; eval constant expr
      [loc-ir (op-fn val-ir)]))) ; bake operator into const value

(defn operand->ir
  [[mode od]]
  {:mode mode :od (val->ir od)})

(defn statement->ir
  [[stmt & nodes]]
  (when (= stmt :statement)
    (let [astmt (reduce (fn [m [k v]] (assoc m k v)) {} nodes)]
      (cond-> astmt
        (contains? astmt :op) (update :op keyword)
        (contains? astmt :src) (update :src operand->ir)
        (contains? astmt :dest) (update :dest operand->ir)))))

(defn ast->ir
  [ast]
  (map statement->ir ast))

(defn parse-assembly [source]
  (let [source (str/lower-case (str source "\n"))]
    (-> source
        z80-cfg
        ast->ir)))

