; UART Wiring: red=5V black=GND yellow=COM-TX (UART-HDX) green=COM-RX (button)+3.3V with 1K Resistor
;servo pin-ppm backlight

; **** User parameters ****
;Calibrate throttle min max
(define cal-thr-lo 41.0)
(define cal-thr-hi 150.0)
(define deadzone 0.05)

;Calibrate brake min max
(define cal-brk-lo 41.0)
(define cal-brk-hi 122.0)

(define min-speed 1)

; Speed modes with MAX KM/H and MAX WATTS
(define eco-speed (/ 25 3.6))
(define eco-current 0.8)
(define eco-watts 1200)
(define drive-speed (/ 27 3.6))
(define drive-current 1)
(define drive-watts 1500)
(define sport-speed (/ 27 3.6))
(define sport-current 1.0)
(define sport-watts 1500)
(define motor-max 60)
(define motor-abs 100)


(define secret-enabled 1)
(define secret-eco-speed (/ 27 3.6))
(define secret-eco-current 1)
(define secret-eco-watts 1500)

(define secret-drive-speed (/ 44 3.6))
(define secret-drive-current 1)
(define secret-drive-watts 1500)

(define secret-sport-speed (/ 90 3.6))
(define secret-sport-current 1.0)
(define secret-sport-watts 2500)

(define secret-motor-max 90)
(define secret-motor-abs 140)

;timeout
(define ble-timeout (* 30 60))
(define last-action-time (systime))
(define secs-left 0)


(uart-start 115200 'half-duplex)
(gpio-configure 'pin-rx 'pin-mode-in-pu)
(gpio-configure 'pin-swdio 'pin-mode-out); deck_light
(gpio-configure 'pin-swclk 'pin-mode-out); break_light


(define tx-frame (array-create 15))
(bufset-u16 tx-frame 0 0x5AA5) ;Ninebot protocol
(bufset-u8 tx-frame 2 0x06) ;Payload length is 5 bytes
(bufset-u16 tx-frame 3 0x2021) ; Packet is from ESC to BLE
(bufset-u16 tx-frame 5 0x6400) ; Packet is from ESC to BLE

(define uart-buf (array-create type-byte 64))

(define buttonold 0)
(define light 0)
(define c-out 0)
(define code 0)

; Button handling
(define presstime (systime))
(define presses 0)

; Mode states
(define off 0)
(define lock 0)
(define speedmode 4)
(define unlock 0)

;backlights
(define back-enabled 1)

; Sound feedback
(define feedback 0)
(define beep-time 1)

; cruise
(define last-throttle-updated-at-time (systime))
(define last-throttle-pos 0)
(define last-cruise-pos 0)
(define last-throttle-dead-min 0)
(define last-throttle-dead-max 0)
(define cruise-after-sec 5)
(define cruise-dead-zone 5)
(define cruise-enabled 0)



(define current-speed 0)
(define throttle-in 0)
(define throttle 0)
(define brake-in 0)
(define brake 0)

(define boot-time (systime))

(defun printf(msg)
    (progn
        (def res (secs-since boot-time))
        (print (str-merge (str-from-n res "%.2fs") ": " msg))
    )
)

(defun beep(time count)
    (progn
        (printf (str-merge "beep time(" (str-from-n time) ") count(" (str-from-n count) ")"))
        (setvar 'beep-time time)
        (setvar 'feedback count)  
    )
)

(defun disable-cruise(handler)
    (if (= cruise-enabled 1)
        (progn
            (printf (str-merge "DISABLE cruise by " handler))
            (setvar 'cruise-enabled 0)
        )
    )
)

(defun enable-cruise()
    (progn
        (printf "enable cruise")
        (beep 2 2)
        (setvar 'last-throttle-pos (bufget-u8 uart-buf 5))
        (setvar 'last-cruise-pos (bufget-u8 uart-buf 5))
        (setvar 'cruise-enabled 1)
    )
)


(defun adc-input(buffer) ; Frame 0x65
    (progn
        (setvar 'current-speed (* (get-speed) 3.6))
        
        (setvar 'last-throttle-dead-min (- throttle-in cruise-dead-zone))
        (setvar 'last-throttle-dead-max (+ throttle-in cruise-dead-zone))
        
        (if (<= (bufget-u8 uart-buf 5) 40)
            (setvar 'last-throttle-updated-at-time (systime))
        )
        
        (if (> (bufget-u8 uart-buf 5) last-throttle-dead-max)
             (setvar 'last-throttle-updated-at-time (systime))
        )
        
        (if (= cruise-enabled 1)
          (if (!= last-throttle-pos 40)
             (setvar 'last-throttle-pos (bufget-u8 uart-buf 5))
          )
        )
        
        (if (> (secs-since last-throttle-updated-at-time) cruise-after-sec)
            (if (!= cruise-enabled 1)
                (enable-cruise)
            )
        )
        
        (if (= cruise-enabled 1)
            (if (= last-throttle-pos 40)
                (if (> (bufget-u8 uart-buf 5) 40)
                    (disable-cruise "throttle push")
                )
            )   
        )
       
        ; Throttle
        (if (= cruise-enabled 0)
            (setvar 'throttle-in (bufget-u8 uart-buf 5))
        )
        (if (= cruise-enabled 0)
            (setvar 'throttle (/(- throttle-in cal-thr-lo) cal-thr-hi))
        )
        
        (if (< throttle deadzone)
            (setvar 'throttle 0)
        )
            
        (if (> throttle 1)
            (setvar 'throttle 1)
        )
      
        ; Brake
        (setvar 'brake-in (bufget-u8 uart-buf 6))
        (setvar 'brake (/(- brake-in cal-brk-lo) cal-brk-hi))
        
        (if(< brake deadzone)
            (setvar 'brake 0)
        )
        (if (<= current-speed 1) ;brk-minspeed=4
            (setvar 'brake 0)
        )
        (if (> brake 1)
            (setvar 'brake 1)
        )
        
        (if (>= brake-in cal-brk-lo)
            (disable-cruise "brake push")
        )
        
        ; time-out
        (if (= off 0)
            (if (> throttle 0)
                    (setvar 'last-action-time (systime))
            )
        )
        
        (if (= off 0)
            (if (> current-speed 1)
                    ;(print "speed")
                    (setvar 'last-action-time (systime))
            )
        )
        
        (if (= off 0)
            (if (> brake-in cal-brk-lo)
                    ;(print brake-in)
                    (setvar 'last-action-time (systime))
            )
        )
        (if (= off 0)
            (setvar 'secs-left (secs-since last-action-time))
        )
        
        ; make
        (if (= (+ off lock) 0)
            (progn ; Driving mode
                (if (<= brake-in cal-brk-lo)
                    (set-current-rel throttle)
                    (set-current-rel 0)
                )
                ;(set-current-rel throttle)
                (if (not (= brake 0))
                    (set-brake-rel brake)
                )
            )
            (progn
                (set-current-rel 0) ; No throttle input when off or locked
                
                (if (= lock 1) ; Check if it is locked
                    (if (> current-speed min-speed) ; Brake when being pushed while locked
                        (set-brake-rel 1) ; Full power brake
                        (set-brake-rel 0) ; No brake
                    )
                    (set-brake-rel 0) ; No brake input when off
                )
            )
        )
    )
)

(defun update-dash(buffer) ; Frame 0x64
    (progn
        ; mode field (1=drive, 2=eco, 4=sport, 8=charge, 16=off, 32=lock)
        (if (= off 1)
            (bufset-u8 tx-frame 7 16)
            (if (= lock 1)
                (bufset-u8 tx-frame 7 32) ; lock display
                (if (< (get-temp-fet) 60) ; temp icon will show up above 60 degree
                    (bufset-u8 tx-frame 7 speedmode)
                    (bufset-u8 tx-frame 7 (+ 128 speedmode))
                )
                
            )
        )
        
        ; batt field
        (bufset-u8 tx-frame 8 (*(get-batt) 100))

        ; light field
        (if (= off 0)
            (bufset-u8 tx-frame 9 light)
            (bufset-u8 tx-frame 9 0)
        )
        
        ; beep field
         (if (= lock 1)
            (if (> (* (get-speed) 3.6) min-speed)
                (bufset-u8 tx-frame 10 beep-time) ; beep lock
                (bufset-u8 tx-frame 10 0))
            (if (> feedback 0)
                (progn
                    (bufset-u8 tx-frame 10 beep-time)
                    (setvar 'feedback (- feedback 1))
                    ;(setvar 'beep-time 1)
                )
                (bufset-u8 tx-frame 10 0)
            )
        )

        ; speed field
        (if (> (* (get-speed) 3.6) 1)
                (bufset-u8 tx-frame 11 (* (get-speed) 3.6))
                (bufset-u8 tx-frame 11 (+ (*(get-batt) 100) 6))
        )
        
        ; error field
        (bufset-u8 tx-frame 12 (get-fault))

        ; calc crc

        (setvar 'crcout 0)
        (looprange i 2 13
        (setvar 'crcout (+ crcout (bufget-u8 tx-frame i))))
        (setvar 'crcout (bitwise-xor crcout 0xFFFF))
        (bufset-u8 tx-frame 13 crcout)
        (bufset-u8 tx-frame 14 (shr crcout 8))
        (uart-write tx-frame)
    )
)

(defun read-frames()
    (loopwhile t
        (progn
            (uart-read-bytes uart-buf 3 0)
            (if (= (bufget-u16 uart-buf 0) 0x5aa5)
                (progn
                    (setvar 'len (bufget-u8 uart-buf 2))
                    (setvar 'crc len)
                    (if (and (> len 0) (< len 60)) ; max 64 bytes
                        (progn
                            (uart-read-bytes uart-buf (+ len 6) 0) ;read remaining 6 bytes + payload, overwrite buffer
                            (setvar 'bCmd (bufget-u8 uart-buf 2)) ;save bCmd
                            (setvar 'wChecksumLE (bufget-u16 uart-buf (+ len 4)))
                            (looprange i 0 (+ len 4) ;prepare checksum, add values of bSrcAddr bDstAddr bCmd bArg bPayload to len
                            (setvar 'crc (+ crc (bufget-u8 uart-buf i))))
                            ;little-endian is broken, do 0xFFFF xor (16-bit sum of bytes <len bSrcAddr bDstAddr bCmd bArg bPayload[]>)
                            (setvar 'wChecksumLECalculated (bitwise-and (+ (shr (bitwise-xor crc 0xFFFF) 8) (shl (bitwise-xor crc 0xFFFF) 8)) 65535))
                            (if (= wChecksumLE wChecksumLECalculated);If the calculated checksum matches with sent checksum, forward comman
                                (handle-frame bCmd)
                            )
                        )
                    )
                )
            )
        )
    )
)


(defun handle-frame(code)
    (progn
        (if(= code 0x65)
            (adc-input uart-buf)
        )

        (if(= code 0x64)
            (update-dash uart-buf)
        )
    )
)

(defun shut-down-ble()
    (progn
        (disable-cruise "ble shut down")
        (setvar 'unlock 0) ; Disable unlock on turn off
        (apply-mode)
        (gpio-write 'pin-swdio 0)
        (setvar 'back-enabled 0)
        (gpio-write 'pin-swclk 0)
        (setvar 'off 1) ; turn off
        (setvar 'light 0)
        (beep 2 1)
        (setvar 'secs-left 0)
        (conf-store)
    )
)

(defun turn-on-ble()
    (progn
        (disable-cruise "ble turn on")
        (setvar 'last-action-time (systime))
        (beep 1 1)
        (gpio-write 'pin-swdio 1)
        (setvar 'off 0) ; turn on
        (setvar 'back-enabled 1)
        (gpio-write 'pin-swclk 1)
        (setvar 'unlock 0) ; Disable unlock on turn off
        (apply-mode) ; Apply mode on start-up
    )
)

(defun handle-button()
    (if (= presses 1) ; single press
        (if (= off 1) ; is it off? turn on scooter again
            (turn-on-ble)
            (setvar 'light (bitwise-xor light 1)) ; toggle light
        )
        (if (>= presses 2) ; double press
            (progn
                (if (> (/(- brake-in cal-brk-lo) cal-brk-hi) deadzone) ; if brake is pressed
                    (if (and (= secret-enabled 1) (> (/(- throttle-in cal-thr-lo) cal-thr-hi) deadzone))
                        (progn
                             (setvar 'unlock (bitwise-xor unlock 1))
                             (if (= unlock 0)
                                (beep 2 1)
                                (beep 1 2)
                             )
                             
                            (apply-mode)
                        )
                        (progn
                            (setvar 'lock (bitwise-xor lock 1)) ; lock on or off
                            (beep 1 1)
                        )
                    )
                    (progn
                        (if (= speedmode 1)
                            (progn
                                (setvar 'speedmode 4)
                            )
                            (if (= speedmode 2)
                                (progn
                                    (setvar 'speedmode 1)
                                )
                                (if (or (= speedmode 4))
                                    (setvar 'speedmode 2)
                                )
                            )
                        )
                        (apply-mode)
                    )
                )
            )
        )
    )
)

(defun handle-holding-button()
    (progn
        (if (= (+ lock off) 0) ; it is locked and off?
           (shut-down-ble)
        )
    )
)

(defun reset-button()
    (progn
        (setvar 'presstime (systime)) ; reset press time again
        (setvar 'presses 0)
    )
)

; Speed mode implementation
; mode field (1=drive, 2=eco, 4=sport, 8=charge, 16=off, 32=lock)
(defun apply-mode()

    (if (= unlock 0)
        (if (= speedmode 1)
            (configure-speed drive-speed drive-watts drive-current motor-max motor-abs)
            (if (= speedmode 2)
                (configure-speed eco-speed eco-watts eco-current motor-max motor-abs)
                (if (= speedmode 4)
                    (configure-speed sport-speed sport-watts sport-current motor-max motor-abs)
                )
            )
        )
        (if (= speedmode 1)
            (configure-speed secret-drive-speed secret-drive-watts secret-drive-current motor-max motor-abs)
            (if (= speedmode 2)
                (configure-speed secret-eco-speed secret-eco-watts secret-eco-current motor-max motor-abs)
                (if (= speedmode 4)
                    (configure-speed secret-sport-speed secret-sport-watts secret-sport-current secret-motor-max secret-motor-abs)
                )
            )
        )
    )
)

(defun configure-speed(speed watts current motor-max motor-abs)
    (progn
        (if (!= feedback 2)
             (beep 1 1)
        )
        ;l-in-current-max - battery current max
        
        (conf-set 'max-speed speed)
        (conf-set 'l-watt-max watts)
        (conf-set 'l-current-max-scale current)
        (conf-set 'l-current-max motor-max) ; motor current max
        (conf-set 'l-abs-current-max motor-abs) ; motor abs max
        
        (printf (str-merge "configure-speed=> motorMax:" (str-from-n (conf-get 'l-current-max)) " motorAbs:" (str-from-n (conf-get 'l-abs-current-max))   ))
        
        ;(print (conf-get 'l-current-max))
        ;(print (conf-get 'l-abs-current-max))
    )
)

; Apply mode on start-up
(apply-mode)
(gpio-write 'pin-swdio 1)


; Spawn UART reading frames thread
(spawn 150 read-frames) 

(defun ppm-sig()
    (if(= back-enabled 1)
        (progn	
            (if (> brake 0)
                (progn
                    (gpio-write 'pin-swclk 1)
                    (sleep (/ 1.0 10))
                    (gpio-write 'pin-swclk 0)
                    (sleep (*(/ 1.0 10)0.25))
                )
            )
            (if (<= brake 0)
                (progn
                    (gpio-write 'pin-swclk 1)
                )
            )      
        )
    )
)

(defun timeout-check()
    (progn
         (if (> secs-left ble-timeout)
            (if (= off 0)
                (shut-down-ble)
            )
         )
    )    
)

(loopwhile t
    (progn
        (if (> buttonold (gpio-read 'pin-rx))
            (progn
                (setvar 'presses (+ presses 1))
                (setvar 'presstime (systime))
            )
            (if (> (- (systime) presstime) 4500) ; after 2500 ms
                (if (= (gpio-read 'pin-rx) 0) ; check button is still pressed
                    (if (> (- (systime) presstime) 8000) ; long press after 8000 ms
                        (progn
                            (if (<= (get-speed) (/ 0.1 3.6)) ;button-safety-speed
                                (handle-holding-button)
                            )
                            (reset-button) ; reset button
                        )
                    )
                    (progn ; when button not pressed
                        (if (> presses 0) ; if presses > 0
                            (progn
                                (handle-button) ; handle button presses
                                (reset-button) ; reset button
                            )
                        )
                    )
                )
            )
        )

        (setvar 'buttonold (gpio-read 'pin-rx))
        (sleep 0.05) ; Recude load on the CPU
        (ppm-sig)
        (timeout-check)
    )
)