(ns four-six-four.cpc
  (:require [four-six-four.z80 :refer [make-z80]]))

(defrecord CPC [kernel cpu video])
(defn make-cpc []
  (map->CPC {:cpu (make-z80)}))

