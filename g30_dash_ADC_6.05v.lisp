; G30 dashboard compability lisp script
; UART Wiring: red=5V black=GND yellow=COM-TX (UART-HDX) green=COM-RX (button)+3.3V with 1K Resistor
; Tested on VESC 6.05 using G30 BLE with spintend ubox Lite 100 100
; Edited by Sharkboy; Thanks to AKA13 and 1zuna for original script

(def software-adc 1)
(def min-adc-thr 0.55)
(def min-adc-brake 0.55)

(def min-speed 1)
(def button-safety-speed (/ 0.1 3.6))

(def eco-speed (/ 7 3.6))
(def eco-current 0.6)
(def eco-watts 2000)
(def eco-fw 0)

(def drive-speed (/ 17 3.6))
(def drive-current 0.7)
(def drive-watts 2000)
(def drive-fw 0)

(def sport-speed (/ 25 3.6))
(def sport-current 1.0)
(def sport-watts 2000)
(def sport-fw 0)

; -> Code starts here (DO NOT CHANGE ANYTHING BELOW THIS LINE IF YOU DON'T KNOW WHAT YOU ARE DOING)
; Load VESC CAN code serer
(import "pkg@://vesc_packages/lib_code_server/code_server.vescpkg" 'code-server)
(read-eval-program code-server)

; Packet handling
(uart-start 115200 'half-duplex)
(gpio-configure 'pin-rx 'pin-mode-in-pu)
(gpio-configure 'pin-ppm 'pin-mode-out); break_light

(define tx-frame (array-create 15))
(bufset-u16 tx-frame 0 0x5AA5) ;Ninebot protocol
(bufset-u8 tx-frame 2 0x06) ;Payload length is 5 bytes
(bufset-u16 tx-frame 3 0x2021) ; Packet is from ESC to BLE
(bufset-u16 tx-frame 5 0x6400) ; Packet is from ESC to BLE
(def uart-buf (array-create 64))

; Button handling
(def presstime (systime))
(def presses 0)

; Mode states
(def off 1)
(def lock 0)
(def speedmode 1)
(def light 0)
(def mode-changed 0)

; timeout
(define secs-left 0)
(define last-action-time (systime))

;cruise
(def last-throttle-updated-at-time (systime))
(def last-throttle-dead-min 0)
(def last-throttle-dead-max 0)
(def cruise-after-sec 5)
(def cruise-dead-zone 0.1)
(def cruise-enabled 0)
(def thr 0)
(def real-thr-pos 0)
(def light-times 0)

;break-light
(def break-light-enabled 0)
(def brake 0)

; Sound feedback
(def feedback 0)
(def beep-time 1)

(if (= software-adc 1)
    (app-adc-detach 3 1)
    (app-adc-detach 3 0)
)

(defun beep(time count)
    {
        (set 'beep-time time)
        (set 'feedback count)
    }
)

(defun enable_brake()
    {
        (gpio-write 'pin-ppm 1)
    }
)

(defun disable_brake()
    {
        (gpio-write 'pin-ppm 0)
    }
)

(defun switch_brake()
    {
        (gpio-write 'pin-ppm (bitwise-xor (gpio-read 'pin-ppm) 1))
    }
)

(defun disable-cruise()
    (if (= cruise-enabled 1)
        {
            (setvar 'cruise-enabled 0)
            (app-adc-override 3 0)
        }
    )
)

(defun enable-cruise(thr)
    (if (> (get-speed) min-speed)
        {
            (setvar 'cruise-enabled 1)
            (app-adc-override 3 thr)
            (set 'light-times 6)
            (beep 2 2)
        }
    )
)

;breaklight logic
(loopwhile-thd 100 t {
        (if (and (> off 0) (= break-light-enabled 1) (> brake min-adc-brake) (> (get-speed) 1))
            {
                (disable_brake)
                (sleep (/ 1.0 10))
                (enable_brake)
                (sleep (*(/ 1.0 10)0.25))
            }
        )

        (if (> secs-left (* 7 60));timeout
            (if (= off 0)
                (shut-down-ble)
            )
        )

        (sleep 0.05)
})

(defun adc-input(buffer) ; Frame 0x65
    {
        (set 'last-throttle-dead-min (- thr cruise-dead-zone))
        (set 'last-throttle-dead-max (+ thr cruise-dead-zone))
        (set 'brake (/(bufget-u8 uart-buf 6) 77.2))
        (set 'thr (/(bufget-u8 uart-buf 5) 77.2))
        (set 'real-thr-pos (/(bufget-u8 uart-buf 5) 77.2))

        (if (<= thr min-adc-thr)
            (setvar 'last-throttle-updated-at-time (systime))
        )

        (if (>= thr last-throttle-dead-max)
            (setvar 'last-throttle-updated-at-time (systime))
        )

        (if (<= thr last-throttle-dead-min)
            (setvar 'last-throttle-updated-at-time (systime))
        )

        (if (>= brake min-adc-brake)
            {
                (disable-cruise)
                (set 'thr 0)
            }
        )

        (if (= cruise-enabled 1)
            (if (< last-throttle-dead-min min-adc-thr)
                (if (> thr min-adc-thr)
                    (disable-cruise)
                )
            )
        )

        (let ((current-speed (* (get-speed) 3.6))) ; 255/3.3 = 77.2

            {
                (if (< thr 0)
                    (setf thr 0))
                (if (> thr 3.3)
                    (setf thr 3.3))
                (if (< brake 0)
                    (setf brake 0))
                (if (> brake 3.3)
                    (setf brake 3.3))

                ; Pass through throttle and brake to VESC
                (app-adc-override 0 thr)
                (app-adc-override 1 brake)

                ; time-out
                (if (= off 0)
                    (if (> thr min-adc-thr)
                            (setvar 'last-action-time (systime))
                    )
                )

                (if (= off 0)
                    (if (> current-speed 1)
                            (setvar 'last-action-time (systime))
                    )
                )

                 (if (= off 0)
                    (if (> brake min-adc-brake)
                            (setvar 'last-action-time (systime))
                    )
                )
                (if (= off 0)
                    (setvar 'secs-left (secs-since last-action-time))
                )
            }
        )

        (if (and (!= cruise-enabled 1) (> (secs-since last-throttle-updated-at-time) cruise-after-sec) (> (* (get-speed) 3.6) min-speed))
            (enable-cruise thr)
        )
    }
)

(defun handle-features()
    {
        (if (or (or (= off 1) (= lock 1) (< (* (get-speed) 3.6) min-speed)))
            (if (not (app-is-output-disabled)) ; Disable output when scooter is turned off
                {
                    (app-adc-override 0 0)
                    (app-adc-override 1 0)
                    (app-disable-output -1)
                    (set-current 0)
                }

            )
            (if (app-is-output-disabled) ; Enable output when scooter is turned on
                (app-disable-output 0)
            )
        )

        (if (= lock 1)
            {
                (set-current-rel 0) ; No current input when locked
                (if (> (abs (* (get-speed) 3.6)) min-speed)
                    (set-brake-rel 1) ; Full power brake
                    (set-brake-rel 0) ; No brake
                )
            }
        )
    }
)

(defun update-dash(buffer) ; Frame 0x64
    {
        (var current-speed (* (l-speed) 3.6))
        (var battery (*(get-batt) 100))

        ; mode field (1=drive, 2=eco, 4=sport, 8=charge, 16=off, 32=lock)
        (if (= off 1)
            (bufset-u8 tx-frame 7 16)
            (if (= lock 1)
                (bufset-u8 tx-frame 7 32) ; lock display
                (if (or (> (get-temp-fet) 60) (> (get-temp-mot) 60)) ; temp icon will show up above 60 degree
                    (bufset-u8 tx-frame 7 (+ 128 speedmode))
                    (bufset-u8 tx-frame 7 speedmode)
                )
            )
        )

        ; batt field
        (if (> mode-changed 0)
            {
                (bufset-u8 tx-frame 8 0)
                (set 'mode-changed (- mode-changed 1))
            }
            {
                (if (= lock 1)
                    {
                        (bufset-u8 tx-frame 8 0)
                    }
                    {
                        (if (and (> brake min-adc-brake) (< (abs current-speed) 0.1))
                            (bufset-u8 tx-frame 8 0)
                            (bufset-u8 tx-frame 8 battery)
                        )
                    }
                )
            }
        )

        ; light field
        (if (> light-times 0)
            {
                (set 'light (bitwise-xor light 1))
                (bufset-u8 tx-frame 9 light)
                (define light-times (- light-times 1))
            }
            {
               (if (= off 0)
                    (bufset-u8 tx-frame 9 light)
                    (bufset-u8 tx-frame 9 0)
               )
            }
        )

        ; speed field
               (if (= lock 1)
                    {
                        (bufset-u8 tx-frame 11 battery)
                    }
                    {
                         (if (> current-speed 1)
                            (bufset-u8 tx-frame 11 current-speed)
                            {
                                (if (> brake min-adc-brake)
                                    {
                                        (if (> real-thr-pos min-adc-thr)
                                            (bufset-u8 tx-frame 11 (/ (get-dist) 1000))
                                            (bufset-u8 tx-frame 11 (* (/ (get-vin) (conf-get 'si-battery-cells)) 10))
                                        )
                                    }
                                    (bufset-u8 tx-frame 11 battery)
                                )
                            }
                        )
                    }
               )

        ; error field
        (if (= (get-fault) 0)
            {
                (if (= lock 1)
                    {
                        (bufset-u8 tx-frame 12 0)
                    }
                    {
                        (if (> brake min-adc-brake)
                            (bufset-u8 tx-frame 12 (get-temp-fet 0))
                        )
                    }
                )
            }
            {
                (bufset-u8 tx-frame 12 (get-fault))
                (beep 2 2)
            }
        )

        ; beep field
        (if (= lock 1)
            (if (> (abs current-speed) min-speed)
                (bufset-u8 tx-frame 10 1) ; beep lock
                (bufset-u8 tx-frame 10 0)
            )
            (if (> feedback 0)
                {
                    (bufset-u8 tx-frame 10 beep-time)
                    (set 'feedback (- feedback 1))
                }
                (bufset-u8 tx-frame 10 0)
            )
        )

        ; calc crc

        (var crcout 0)
        (looprange i 2 13
        (set 'crcout (+ crcout (bufget-u8 tx-frame i))))
        (set 'crcout (bitwise-xor crcout 0xFFFF))
        (bufset-u8 tx-frame 13 crcout)
        (bufset-u8 tx-frame 14 (shr crcout 8))

        ; write
        (uart-write tx-frame)
    }
)

(defun read-frames()
    (loopwhile t
        {
            (uart-read-bytes uart-buf 3 0)
            (if (= (bufget-u16 uart-buf 0) 0x5aa5)
                {
                    (var len (bufget-u8 uart-buf 2))
                    (var crc len)
                    (if (and (> len 0) (< len 60)) ; max 64 bytes
                        {
                            (uart-read-bytes uart-buf (+ len 6) 0) ;read remaining 6 bytes + payload, overwrite buffer

                            (let ((code (bufget-u8 uart-buf 2)) (checksum (bufget-u16 uart-buf (+ len 4))))
                                {
                                    (looprange i 0 (+ len 4) (set 'crc (+ crc (bufget-u8 uart-buf i))))

                                    (if (= checksum (bitwise-and (+ (shr (bitwise-xor crc 0xFFFF) 8) (shl (bitwise-xor crc 0xFFFF) 8)) 65535)) ;If the calculated checksum matches with sent checksum, forward comman
                                        (handle-frame code)
                                    )
                                }
                            )
                        }
                    )
                }
            )
        }
    )
)

(defun handle-frame(code)
    {
        (if (and (= code 0x65) (= software-adc 1))
            (adc-input uart-buf)
        )

        (if(= code 0x64)
            (update-dash uart-buf)
        )
    }
)

(defun turn-on-ble()
    {
        (app-adc-override 3 0) ; disable cruise button
        (set 'speedmode 2)
        (set 'break-light-enabled 1)
        (enable_brake)
        (apply-mode) ; Apply mode on start-up
        (set 'last-action-time (systime))
        (beep 1 1)
        (set 'off 0) ; turn on
    }
)

(defun handle-button()
    (if (= presses 1) ; single press
        (if (= off 1) ; is it off? turn on scooter again
            {
                (turn-on-ble)
            }
            {
                (if (= lock 0)
                    (set 'light (bitwise-xor light 1)) ; toggle light
                )
            }
        )
        (if (>= presses 2) ; double press
            {
                (if (> (get-adc-decoded 1) min-adc-brake) ; if brake is pressed
                    (if (and (> (get-adc-decoded 0) min-adc-thr))
                        {
                            (beep 1 2)
                            (apply-mode)
                        }
                        {
                            (apply-mode)
                            (set 'lock (bitwise-xor lock 1)) ; lock on or off
                            (beep 1 1) ; beep feedback
                        }
                    )
                    {
                        (if (= lock 0)
                            {
                                (cond
                                    ((= speedmode 1) (set 'speedmode 4))
                                    ((= speedmode 2) (set 'speedmode 1))
                                    ((= speedmode 4) (set 'speedmode 2))
                                )
                                (apply-mode)
                            }
                        )
                    }
                )
            }
        )
    )
)

(defun shut-down-ble()
    {
        (if (= (+ lock off) 0) ; it is locked and off?
            {
                (app-adc-override 3 0) ; disable cruise button
                (apply-mode)
                (set 'break-light-enabled 0)  ; disable break light
                (disable_brake)
                (set 'light 0) ; turn off light
                (beep 2 1) ; beep feedback
                (set 'secs-left 0)
                (set 'off 1) ; turn off
            }
        )
    }
)

(defun reset-button()
    {
        (set 'presstime (systime)) ; reset press time again
        (set 'presses 0)
    }
)

; Speed mode implementation

(defun apply-mode()
    (if (= speedmode 1)
        (configure-speed drive-speed drive-watts drive-current drive-fw)
        (if (= speedmode 2)
            (configure-speed eco-speed eco-watts eco-current eco-fw)
            (if (= speedmode 4)
                (configure-speed sport-speed sport-watts sport-current sport-fw)
            )
        )
    )
)

(defun configure-speed(speed watts current fw)
    {
        (set-param 'max-speed speed)
        (set-param 'l-watt-max watts)
        (set-param 'l-current-max-scale current)
        (set-param 'foc-fw-current-max fw)
    }
)

(defun set-param (param value)
    {
        (conf-set param value)
        (loopforeach id (can-list-devs)
            (looprange i 0 5 {
                (if (eq (rcode-run id 0.1 `(conf-set (quote ,param) ,value)) t) (break t))
                false
            })
        )
    }
)

(defun l-speed()
    {
        (var l-speed (get-speed))
        (loopforeach i (can-list-devs)
            {
                (var l-can-speed (canget-speed i))
                (if (< l-can-speed l-speed)
                    (set 'l-speed l-can-speed)
                )
            }
        )

        l-speed
    }
)

(defun button-logic()
    {
        ; Assume button is not pressed by default
        (var buttonold 0)
        (loopwhile t
            {
                (var button (gpio-read 'pin-rx))
                (sleep 0.05) ; wait 50 ms to debounce
                (var buttonconfirm (gpio-read 'pin-rx))
                (if (not (= button buttonconfirm))
                    (set 'button 0)
                )

                (if (> buttonold button)
                    {
                        (set 'presses (+ presses 1))
                        (set 'presstime (systime))
                    }
                    (button-apply button)
                )

                (set 'buttonold button)
                (handle-features)
            }
        )
    }
)

(defun button-apply(button)
    {
        (var time-passed (- (systime) presstime))
        (var is-active (or (= off 1) (<= (get-speed) button-safety-speed)))

        (if (> time-passed 2500) ; after 2500 ms
            (if (= button 0) ; check button is still pressed
                (if (> time-passed 6000) ; long press after 6000 ms
                    {
                        (if is-active
                            (shut-down-ble)
                        )
                        (reset-button) ; reset button
                    }
                )
                (if (> presses 0) ; if presses > 0
                    {
                        (if is-active
                            (handle-button) ; handle button presses
                        )
                        (reset-button) ; reset button
                    }
                )
            )
        )
    }
)

; Apply mode on start-up
(apply-mode)
(disable_brake)
(turn-on-ble)

; Spawn UART reading frames thread
(spawn 150 read-frames)
(button-logic) ; Start button logic in main thread - this will block the main thread