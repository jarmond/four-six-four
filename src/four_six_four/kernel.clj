(ns four-six-four.kernel
  (:require [four-six-four.utils :refer [zero-vector]]
            [four-six-four.z80.instructions :refer [operation]]
            [four-six-four.z80.vm :as z80]
            [taoensso.timbre :as log]))

;;;; Kernel state and routines. All functions expect *z80* to be bound to a CPU.

;;; Config

(defonce +trap-opcode+ [0xcb 0x32])
(defonce +stack-top+ 0xBFFF) ; grows down
(defonce +rom-size+ (* 16 1024))

;(defonce +ret-addr+ 0x6)

;;; State

(defn make-kernel []
  (map->Kernel (atom {})))

(defn set-rom-select
  "Change selected ROM."
  [k end id]
  (swap! @k assoc-in [end :select] id))

(defn rom-select
  [k end]
  (get-in @k [end :select]))

(defn set-rom-state
  [k end state]
  (let [setter (case end
                 :lower-rom z80/set-lower-rom
                 :upper-rom z80/set-upper-rom)
        select (get-in @k [end :select])
        rom (get-in @k [end :roms select])]
    (swap! @k assoc-in [end :state state])
    (setter rom)))

(defn rom-state
  [k end]
  (get-in @k [end :state]))

(def trap-map
  "Map of addresses to kernel routines. Filled in by `defkernfn`."
  {})

;;; Init

;; FIXME trap should be followed by RET to avoid handling return here
;; This makes code reusable eg. for BASIC.
(defn write-trap
  [loc]
  (z80/write-mem-vector loc +trap-opcode+))

(defn init-kernel [k]
  (dosync
   (reset! k {:lower-rom {:roms [(zero-vector +rom-size+)] :state false :select 0}
              :upper-rom {:roms [(zero-vector +rom-size+)] :state false :select 0}})
   ;; Register trap handlers
   (doseq [[loc f] trap-map]
     (write-trap loc)
     ;; Associate handler curried with kernel object
     (z80/register-trap loc (partial f k)))

   ;; Auxillary RET location to return to firmware caller
   ;(write-mem +ret-addr+ 0xC9)

   ;; Set up stack
   (z80/write-reg :sp +stack-top+)))




;;; Kernel routines

(defn ret
  "Execute a RET instruction."
  []
  (operation {:op :ret}))

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


;;; Internal kernel implementation

(defkernfn low-jump-ret [k] 0x6
  (let [select (z80/pop-val)
        state (z80/pop-val)]
    (set-rom-select k :upper-rom select)
    (set-rom-state k :upper-rom (bit-test state 1))
    (set-rom-state k :lower-rom (bit-test state 0))
    (z80/pop-val) ; Pop padding
    (z80/pop-val)
    (ret)))

(defn encode-rom-state [k]
  (+
   (if (rom-state k :lower-rom) 1 0)
   (if (rom-state k :upper-rom) 2 0)))

;;; Low Kernel Jumpblock

(defkernfn reset-entry [k] 0 ; RST 0
  (z80/reset-z80)
  (init-kernel k))


(defkernfn low-jump [k] 0x8  ; RST 1
  (let [low-addr (z80/pop-val)
        addr (bit-and 0x3f low-addr)]
    (z80/push-val 0) ; Padding
    (z80/push-val 0)
    ;; Push current ROM state/select in two bytes.
    (z80/push-val (encode-rom-state k))
    (z80/push-val (rom-select k :upper-rom))
    ;; Set new ROM state
    (set-rom-state :lower-rom (not (bit-test low-addr 14)))
    (rom-select :upper-rom 0)
    (set-rom-state :upper-rom (not (bit-test low-addr 15)))
    (z80/push-val +ret-addr+)  ; After jump return to RET to return caller
    (z80/set-pc low-addr)))


(stub kl-low-pchl 0xB)
(stub pcbc-instruction 0xe)
(stub side-call 0x10) ; RST 2
(stub kl-side-pchl 0x13)
(stub pcde-instruction 0x16)
(stub far-call 0x18) ; RST 3
(stub kl-far-call 0x18)
(stub pchl-instruction 0x1e)
(stub ram-lam 0x20) ; RST 4
(stub kl-far-icall 0x23)
(stub firm-jump 0x28) ; RST 5
(stub user-restart 0x30) ; RST 6
(stub interrupt-entry 0x38) ; RST 7
(stub ext-interrupt 0x3b)

;;; Main jumpblock

;;; KM - Key manager
(stub km-initialise 0xbb00)
(stub km-reset 0xbb03)
(stub km-wait-char 0xbb06)
(stub km-read-char 0xbb09)
(stub km-char-return 0xbb0c)
(stub km-set-expand 0xbb0f)
(stub km-get-expand 0xbb12)
(stub km-exp-buffer 0xbb15)
(stub km-wait-key 0xbb18)
(stub km-read-key 0xbb1b)
(stub km-test-key 0xbb1e)
(stub km-get-state 0xbb21)
(stub km-get-joystick 0xbb24)
(stub km-set-translate 0xbb27)
(stub km-get-translate 0xbb2a)
(stub km-set-shift 0xbb2d)
(stub km-get-shift 0xbb30)
(stub km-set-control 0xbb33)
(stub km-get-control 0xbb36)
(stub km-set-repeat 0xbb39)
(stub km-get-repeat 0xbb3c)
(stub km-set-delay 0xbb3f)
(stub km-get-delay 0xbb42)
(stub km-arm-break 0xbb45)
(stub km-disarm-break 0xbb48)
(stub km-break-event 0xbb4b)

;;; TXT - Text VDU
(stub txt-initialise 0xbb4e)
(stub txt-reset 0xbb51)
(stub txt-vdu-enable 0xbb54)
(stub txt-vdu-disable 0xbb57)
(stub txt-output 0xbb5a)
(stub txt-wr-char 0xbb5d)
(stub txt-rd-char 0xbb60)
(stub txt-set-graphic 0xbb63)
(stub txt-win-enable 0xbb66)
(stub txt-get-window 0xbb69)
(stub txt-clear-window 0xbb6c)
(stub txt-set-column 0xbb6f)
(stub txt-set-row 0xbb72)
(stub txt-set-cursor 0xbb75)
(stub txt-get-cursor 0xbb78)
(stub txt-cur-enable 0xbb7b)
(stub txt-cur-on 0xbb81)
(stub txt-cur-off 0xbb84)
(stub txt-validate 0xbb87)
(stub txt-place-cursor 0xbb8a)
(stub txt-remove-cursor 0xbb8d)
(stub txt-set-pen 0xbb90)
(stub txt-get-pen 0xbb93)
(stub txt-set-paper 0xbb96)
(stub txt-get-paper 0xbb99)
(stub txt-inverse 0xbb9c)
(stub txt-set-back 0xbb9f)
(stub txt-get-back 0xbba2)
(stub txt-get-matrix 0xbba5)
(stub txt-set-matrix 0xbba8)
(stub txt-set-m-table 0xbbab)
(stub txt-get-m-table 0xbbae)
(stub txt-get-controls 0xbbb1)
(stub txt-str-select 0xbbb4)
(stub txt-swap-streams 0xbbb7)
(stub txt-ask-state 0xbd40)

;;; GRA - Graphics VDU
(stub gra-initialise 0xbbba)
(stub gra-reset 0xbbbd)
(stub gra-move-absolute 0xbbc0)
(stub gra-move-relative 0xbbc3)
(stub gra-ask-cursor 0xbbc6)
(stub gra-set-origin 0xbbc9)
(stub gra-get-origin 0xbbcc)
(stub gra-win-width 0xbbcf)
(stub gra-win-height 0xbbd2)
(stub gra-get-w-width 0xbbd5)
(stub gra-get-w-height 0xbbd8)
(stub gra-clear-window 0xbbdb)
(stub gra-set-pen 0xbbde)
(stub gra-get-pen 0xbbe1)
(stub gra-set-paper 0xbbe4)
(stub gra-get-paper 0xbbe7)
(stub gra-plot-absolute 0xbbea)
(stub gra-plot-relative 0xbbed)
(stub gra-test-absolute 0xbbf0)
(stub gra-test-relative 0xbbf3)
(stub gra-line-absolute 0xbbf6)
(stub gra-line-relative 0xbbf9)
(stub gra-wr-char 0xbbfc)
(stub gra-default 0xbd43)
(stub gra-set-back 0xbd46)
(stub gra-set-first 0xbd49)
(stub gra-set-line-mask 0xbd4c)
(stub gra-from-user 0xbd4f)
(stub gra-fill 0xbd52)

;;; SCR - Screen pack
(stub scr-initialise 0xbbff)
(stub scr-reset 0xbc02)
(stub scr-set-offset 0xbc05)
(stub scr-set-base 0xbc08)
(stub scr-get-location 0xbc0b)
(stub scr-set-mode 0xbc0e)
(stub scr-get-mode 0xbc11)
(stub scr-clear 0xbc14)
(stub scr-char-limits 0xbc17)
(stub scr-char-position 0xbc1a)
(stub scr-dot-position 0xbc1d)
(stub scr-next-byte 0xbc20)
(stub scr-prev-byte 0xbc23)
(stub scr-next-line 0xbc26)
(stub scr-prev-line 0xbc29)
(stub scr-ink-encode 0xbc2c)
(stub scr-ink-decode 0xbc2f)
(stub scr-set-ink 0xbc32)
(stub scr-get-ink 0xbc35)
(stub scr-set-border 0xbc38)
(stub scr-get-border 0xbc3b)
(stub scr-set-flashing 0xbc3e)
(stub scr-get-flashing 0xbc41)
(stub scr-fill-box 0xbc44)
(stub scr-flood-box 0xbc47)
(stub scr-char-invert 0xbc4a)
(stub scr-hw-roll 0xbc4d)
(stub scr-sw-roll 0xbc50)
(stub scr-unpack 0xbc53)
(stub scr-repack 0xbc56)
(stub scr-access 0xbc59)
(stub scr-pixels 0xbc5c)
(stub scr-horizontal 0xbc5f)
(stub scr-vertical 0xbc62)
(stub scr-set-position 0xbd55)

;;; CAS - Cassette manager
(stub cas-initialise 0xbc65)
(stub cas-set-speed 0xbc68)
(stub cas-noisy 0xbc6b)
(stub cas-start-motor 0xbc6e)
(stub cas-stop-motor 0xbc71)
(stub cas-restore-motor 0xbc74)
(stub cas-in-open 0xbc77)
(stub cas-in-close 0xbc7a)
(stub cas-in-abandon 0xbc7d)
(stub cas-in-char 0xbc80)
(stub cas-in-direct 0xbc83)
(stub cas-return 0xbc86)
(stub cas-test-eof 0xbc89)
(stub cas-out-open 0xbc8c)
(stub cas-out-close 0xbc8f)
(stub cas-out-abandon 0xbc92)
(stub cas-out-char 0xbc95)
(stub cas-out-direct 0xbc98)
(stub cas-catalog 0xbc9b)
(stub cas-write 0xbc9e)
(stub cas-read 0xbca1)
(stub cas-check 0xbca4)


;;; Sound manager
(stub sound-reset 0xbca7)
(stub sound-queue 0xbcaa)
(stub sound-check 0xbcad)
(stub sound-arm-event 0xbcb0)
(stub sound-release 0xbcb3)
(stub sound-hold 0xbcb6)
(stub sound-continue 0xbcb9)
(stub sound-ampl-envelope 0xbcbc)
(stub sound-tone-envelope 0xbcbf)
(stub sound-a-address 0xbcc2)
(stub sound-t-address 0xbcc5)

;;; KL- Kernel

(stub kl-choke-off 0xbcc8)
(stub kl-rom-walk 0xbccb)
(stub kl-init-back 0xbcce)
(stub kl-log-ext 0xbcd1)
(stub kl-find-command 0xbcd4)
(stub kl-new-frame-fly 0xbcd7)
(stub kl-add-frame-fly 0xbcda)
(stub kl-del-frame-fly 0xbcdd)
(stub kl-new-fast-ticker 0xbce0)
(stub kl-add-fast-ticker 0xbce3)
(stub kl-del-fast-ticker 0xbce6)
(stub kl-add-ticker 0xbce9)
(stub kl-del-ticker 0xbcec)
(stub kl-init-event 0xbcef)
(stub kl-event 0xbcf2)
(stub kl-sync-reset 0xbcf5)
(stub kl-del-synchronous 0xbcf8)
(stub kl-next-sync 0xbcfb)
(stub kl-do-sync 0xbcfe)
(stub kl-done-sync 0xbd01)
(stub kl-event-disable 0xbd04)
(stub kl-event-enable 0xbd07)
(stub kl-disarm-event 0xbd0a)
(stub kl-time-please 0xbd0d)
(stub kl-time-set 0xbd10)
(stub kl-bank-switch 0xbd5b)

;;; MC - Machine pack
(stub mc-boot-program 0xbd13)
(stub mc-start-program 0xbd16)
(stub mc-wait-flyback 0xbd19)
(stub mc-set-mode 0xbd1c)
(stub mc-screen-offset 0xbd1f)
(stub mc-clear-inks 0xbd22)
(stub mc-set-inks 0xbd25)
(stub mc-reset-printer 0xbd28)
(stub mc-print-char 0xbd2b)
(stub mc-busy-printer 0xbd2e)
(stub mc-send-printer 0xbd31)
(stub mc-sound-register 0xbd34)
(stub mc-print-translation 0xbd58)

(stub jump-restore 0xbd37)

;;; KM - Keyboard manager
(stub km-set-locks 0xbd3a)
(stub km-flush 0xbd3d)




