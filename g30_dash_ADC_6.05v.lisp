; G30 dashboard compability lisp script v0.9 by AKA13 and 1zuna
; UART Wiring: red=5V black=GND yellow=COM-TX (UART-HDX) green=COM-RX (button)+3.3V with 1K Resistor
; Guide (German): https://rollerplausch.com/threads/vesc-controller-einbau-1s-pro2-g30.6032/
; Tested on VESC 6.05 BETA on G30D w/ MP2

(define boot-time (systime))
(def feedback 0)
(def beep-time 1)

(def hi-temp-fet 60)
(def hi-temp-motor 115)

; -> User parameters (change these to your needs)
(def software-adc 1)
(def min-adc-throttle 0.1)
(def min-adc-brake 0.1)
(def min-brake-val 0.55)
(def min-thr-val 0.55)

(def show-batt-in-idle 1)
(def min-speed 1)
(def button-safety-speed (/ 0.1 3.6)) ; disabling button above 0.1 km/h (due to safety reasons)

; Speed modes (km/h, watts, current scale)
(def eco-fw 0)
(def drive-fw 0)
(def sport-fw 0)
(define eco-speed (/ 15 3.6))
(define eco-current 0.8)
(define eco-watts 1500)
(define drive-speed (/ 25 3.6))
(define drive-current 1)
(define drive-watts 2000)
(define sport-speed (/ 28 3.6))
(define sport-current 1.0)
(define sport-watts 2500)

(define def-motor-max 90)
(define def-motor-abs 140)

; Secret speed modes. To enable, press the button 2 times while holding break and throttle at the same time.
(def secret-enabled 1)
(def secret-eco-fw 0)
(def secret-drive-fw 0)
(def secret-sport-fw 15)

(define secret-enabled 1)
(define secret-eco-speed (/ 40 3.6))
(define secret-eco-current 1)
(define secret-eco-watts 1500)

(define secret-drive-speed (/ 99 3.6))
(define secret-drive-current 1)
(define secret-drive-watts 2000)

(define secret-sport-speed (/ 99 3.6))
(define secret-sport-current 1.0)
(define secret-sport-watts 3000)

(define secret-motor-max 110)
(define secret-motor-abs 170)

;backlights
(define back-enabled 1)



; -> Code starts here (DO NOT CHANGE ANYTHING BELOW THIS LINE IF YOU DON'T KNOW WHAT YOU ARE DOING)

; Load VESC CAN code serer
(import "pkg@://vesc_packages/lib_code_server/code_server.vescpkg" 'code-server)
(read-eval-program code-server)
(app-adc-override 3 0)

; Packet handling
(uart-start 115200 'half-duplex)
(gpio-configure 'pin-rx 'pin-mode-in-pu)
(gpio-configure 'pin-swdio 'pin-mode-out); deck_light
(gpio-configure 'pin-swclk 'pin-mode-out); break_light

(define tx-frame (array-create 15))
(bufset-u16 tx-frame 0 0x5AA5) ;Ninebot protocol
(bufset-u8 tx-frame 2 0x06) ;Payload length is 5 bytes
(bufset-u16 tx-frame 3 0x2021) ; Packet is from ESC to BLE
(bufset-u16 tx-frame 5 0x6400) ; Packet is from ESC to BLE
(def uart-buf (array-create 64))

; Button handling

(def presstime (systime))



; Mode states


(def lock 0)
(def speedmode 4)
(def light 0)
(def unlock 0)
(def presses 0)
(def off 0)

(def thr 0)
(def brake 0)

;cruise
(def last-throttle-updated-at-time (systime))
(def last-throttle-dead-min 0)
(def last-throttle-dead-max 0)
(def cruise-after-sec 5)
(def cruise-dead-zone 0.1)
(def cruise-enabled 0)

(if (= software-adc 1)
    (app-adc-detach 3 1)
    (app-adc-detach 3 0)
)

(defun printf(msg)
    {
        (var res (secs-since boot-time))
        (print (str-merge (str-from-n res "%.2fs") ": " msg))
    }
)

(loopwhile-thd 100 t {
        (if (= back-enabled 1) ; it is locked and off?
            {
                (if (> brake min-brake-val)
                    {
                        (gpio-write 'pin-swclk (bitwise-xor (gpio-read 'pin-swclk) 1))
                        (sleep (/ 1.0 10))
                        (gpio-write 'pin-swclk (bitwise-xor (gpio-read 'pin-swclk) 1))
                        (sleep (*(/ 1.0 10)0.25))
                    }
                )
                (if (< brake min-brake-val)
                    {
                        (gpio-write 'pin-swclk 1)
                    }
                )
            }
        )

        (sleep 0.05)
})

(defun beep(time count)
    {
        ;(printf (str-merge "beep time(" (str-from-n time) ") count(" (str-from-n count) ")"))
        (set 'beep-time time)
        (set 'feedback count)
    }
)

(defun disable-cruise(handler)
    (if (= cruise-enabled 1)
        {
            (setvar 'cruise-enabled 0)
            (app-adc-override 3 0)
            (printf (str-merge "DISABLE cruise by " handler))
        }
    )
)

(defun enable-cruise(thr)
    {
        (printf "enable cruise")
        (setvar 'cruise-enabled 1)
        (app-adc-override 3 thr)
        (beep 2 2)
    }
)

(defun adc-input(buffer) ; Frame 0x65
    {
        (set 'last-throttle-dead-min (- thr cruise-dead-zone))
        (set 'last-throttle-dead-max (+ thr cruise-dead-zone))

        (set 'brake (/(bufget-u8 uart-buf 6) 77.2))
        (set 'thr (/(bufget-u8 uart-buf 5) 77.2)) ; 255/3.3 = 77.2

        (if (>= brake min-brake-val)
            (disable-cruise "brake")
        )

        (if (<= thr min-thr-val)
            (setvar 'last-throttle-updated-at-time (systime))
        )

        (if (>= thr last-throttle-dead-max)
            (setvar 'last-throttle-updated-at-time (systime))
        )

        (if (<= thr last-throttle-dead-min)
            (setvar 'last-throttle-updated-at-time (systime))
        )

        (if (= cruise-enabled 1)
            (if (< last-throttle-dead-min min-thr-val)
                (if (> thr min-thr-val)
                    (disable-cruise "throttle push")
                )
            )
        )


        (let ((current-speed (* (get-speed) 3.6)))
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
            }
        )

         (if (> (secs-since last-throttle-updated-at-time) cruise-after-sec)
            (if (!= cruise-enabled 1)
                (enable-cruise thr)
            )
        )
    }
)

(defun handle-features()
    {
        (if (or (= off 1) (= lock 1))
            (if (not (app-is-output-disabled)) ; Disable output when scooter is turned off
                {
                    (app-adc-override 0 0)
                    (app-adc-override 1 0)
                    (app-disable-output -1)
                    (set-current 0)
                    ;(loopforeach i (can-list-devs)
                    ;    (canset-current i 0)
                    ;)
                }

            )
            (if (app-is-output-disabled) ; Enable output when scooter is turned on
                (app-disable-output 0)
            )
        )

        (if (= lock 1)
            {
                (set-current-rel 0) ; No current input when locked
                (if (> (* (get-speed) 3.6) min-speed)
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
                (if (or (> (get-temp-fet) hi-temp-fet) (> (get-temp-mot) hi-temp-motor)) ; temp icon will show up above 60 degree
                    (bufset-u8 tx-frame 7 (+ 128 speedmode))
                    (bufset-u8 tx-frame 7 speedmode)
                )
            )
        )

        ; batt field
        (bufset-u8 tx-frame 8 battery)

        ; light field
        (if (= off 0)
            (bufset-u8 tx-frame 9 light)
            (bufset-u8 tx-frame 9 0)
        )

        ; beep field
        (if (= lock 1)
            (if (> current-speed min-speed)
                (bufset-u8 tx-frame 10 beep-time) ; beep lock
                (bufset-u8 tx-frame 10 0))
            (if (> feedback 0)
                {
                    (bufset-u8 tx-frame 10 beep-time)
                    (set 'feedback (- feedback 1))
                }
                (bufset-u8 tx-frame 10 0)
            )
        )

        ; speed field
        (if (> current-speed 1)
            (bufset-u8 tx-frame 11 current-speed)
            (bufset-u8 tx-frame 11 battery)
        )

        ; error field
        (bufset-u8 tx-frame 12 (get-fault))

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
        (beep 1 1)
        (gpio-write 'pin-swdio 1) ; enable deck light
        (gpio-write 'pin-swclk 1) ; enable break light
        (set 'unlock 0)
        (setvar 'back-enabled 1)
        (setvar 'off 0) ; turn on
        (apply-mode) ; Apply mode on start-up
    }
)


(defun handle-button()
    (if (= presses 1) ; single press
        (if (= off 1) ; is it off? turn on scooter again
            (turn-on-ble)
            {
                (if (<= (get-speed) button-safety-speed)
                    (if (<= brake 0.52)
                        (set 'light (bitwise-xor light 1)) ; toggle light
                    )
                )
            }
        )
        (if (>= presses 2) ; double press
            {
                (if (> (get-adc-decoded 1) min-adc-brake) ; if brake is pressed
                    (if (and (= secret-enabled 1) (> (get-adc-decoded 0) min-adc-throttle))
                        {
                             (set 'unlock (bitwise-xor unlock 1))
                             (if (= unlock 0)
                                (beep 2 1)
                                (beep 1 2)
                             )

                            (apply-mode)
                        }
                        {
                            (setvar 'lock (bitwise-xor lock 1)) ; lock on or off
                            (beep 1 1)
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
        (app-adc-override 3 0) ; disable cruise button
        (set 'unlock 0) ; Disable unlock on turn off
        (apply-mode)
        (set 'off 1) ; turn off
        (gpio-write 'pin-swdio 0) ; disable deck light
        (gpio-write 'pin-swclk 0) ; disable break light
        (set 'back-enabled 0)  ; disable break light
        (set 'light 0) ; disable front light
        (beep 2 1)
        (set 'off 1) ; turn off


        ;(setvar 'secs-left 0)
        ;(conf-store)
    }
)

(defun handle-holding-button()
    {
        (if (= (+ lock off) 0) ; it is locked and off?
            (shut-down-ble)
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
    (if (= unlock 0)
        (if (= speedmode 1)
            (configure-speed drive-speed drive-watts drive-current drive-fw def-motor-max def-motor-abs)
            (if (= speedmode 2)
                (configure-speed eco-speed eco-watts eco-current eco-fw def-motor-max def-motor-abs)
                (if (= speedmode 4)
                    (configure-speed sport-speed sport-watts sport-current sport-fw def-motor-max def-motor-abs)
                )
            )
        )
        (if (= speedmode 1)
            (configure-speed secret-drive-speed secret-drive-watts secret-drive-current secret-drive-fw def-motor-max def-motor-abs)
            (if (= speedmode 2)
                (configure-speed secret-eco-speed secret-eco-watts secret-eco-current secret-eco-fw def-motor-max def-motor-abs)
                (if (= speedmode 4)
                    (configure-speed secret-sport-speed secret-sport-watts secret-sport-current secret-sport-fw secret-motor-max secret-motor-abs)
                )
            )
        )
    )
)

(defun configure-speed(speed watts current fw motor-max motor-abs)
    {
        (set-param 'max-speed speed)
        (set-param 'l-watt-max watts)
        (set-param 'l-current-max-scale current)
        (set-param 'l-current-max motor-max) ; motor current max
        (set-param 'l-abs-current-max motor-abs) ; motor abs max
        (set-param 'foc-fw-current-max fw)

        (printf (str-merge "configure-speed=> motorMax:" (str-from-n (conf-get 'l-current-max)) " motorAbs:" (str-from-n (conf-get 'l-abs-current-max))
        " fw:" (str-from-n (conf-get 'foc-fw-current-max))
        ))

        (if (!= beep-time 2)
             (beep 1 1)
        )
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
        (var is-active (or (= off 1) (and (<= (get-speed) button-safety-speed) (< (abs (get-current 0)) 0.1))))
        ;(var is-active (= off 1))

        (if (> time-passed 2500) ; after 2500 ms
            (if (= button 0) ; check button is still pressed
                (if (> time-passed 6000) ; long press after 6000 ms
                    {
                        (if (= off 0)
                            (handle-holding-button)
                        )
                        (reset-button) ; reset button
                    }
                )
                (if (> presses 0) ; if presses > 0
                    {
                        (handle-button) ; handle button presses
                        (reset-button) ; reset button
                    }
                )
            )
        )
    }
)

; Apply mode on start-up
(apply-mode)

; Spawn UART reading frames thread
(spawn 150 read-frames)
(button-logic) ; Start button logic in main thread - this will block the main thread