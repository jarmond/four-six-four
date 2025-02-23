(ns four-six-four.cpc
  (:require [four-six-four.kernel :refer [init-kernel make-kernel]]
            [four-six-four.z80.vm :as z80]))

(defrecord CPC [kernel cpu video])
(defn make-cpc []
  (map->CPC {:cpu (make-z80)
             :kernel (make-kernel)}))

(defn init-cpc [cpc]
  (with-z80 (:cpu cpc)
    (reset-z80)
    (init-kernel (:kernel cpc))))

