(ns four-six-four.kernel
  (:require [taoensso.timbre :as log]))


;; Consider defining special instruction for kernel trap.
;; Install it in the kernel ROM locations, each call diverts to here.

(def trap-map
  "Map of addresses to kernel routines."
  {})

(defmacro defkernfn
  "Construct a kernel routine and register in trap map."
  [name addr & body]
  `(do
     (defn ~name []
       ~@body)
     (alter-var-root #'trap-map assoc ~addr #'~name)))

(defn stub-msg [msg]
  (log/warn "STUB:" msg))

(defmacro stub [stub-name addr]
  `(defkernfn ~stub-name ~addr
     (stub-msg ~(name stub-name))))

;;; Low Kernel Jumpblock

(stub reset-entry 0) ; RST 0
(stub low-jump 0x8) ; RST 1
(stub kl-low-pchl 0xB)
(stub pcbc-instruction 0xE)
(stub side-call 0x10) ; RST 2
(stub kl-side-pchl 0x13)
(stub pcde-instruction 0x16)
(stub far-call 0x18) ; RST 3
(stub kl-far-call 0x18)
(stub pchl-instruction 0x1E)
(stub ram-lam 0x20) ; RST 4
(stub kl-far-icall 0x23)
(stub firm-jump 0x28) ; RST 5
(stub user-restart 0x30) ; RST 6
(stub interrupt-entry 0x38) ; RST 7
(stub ext-interrupt 0x3B)





;;; Kernel routines

(stub kl-choke-off 0xBCC8)
(stub kl-rom-walk 0xBCCB)
(stub kl-init-back 0xBCCE)
(stub kl-log-ext 0xBCD1)
(stub kl-find-command 0xBCD4)
(stub kl-new-frame-fly 0xBCD7)
(stub kl-add-frame-fly 0xBCDA)
(stub kl-del-frame-fly 0xBCDD)
(stub kl-new-fast-ticker 0xBCE0)
(stub kl-add-fast-ticker 0xBCE3)
(stub kl-del-fast-ticker 0xBCE6)
(stub kl-add-ticker 0xBCE9)
(stub kl-del-ticker 0xBCEC)
(stub kl-init-event 0xBCEF)
(stub kl-event 0xBCF2)
(stub kl-sync-reset 0xBCF5)



(defn init-kernel []
  ;; Intiialize kernel variables in memory
  )
