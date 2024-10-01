## Difference beetween https://github.com/m365fw/vesc_m365_dash

![image](guide/imgs/cruise.png)
- [x] 0km/h start speed !!!!BE CAREFULL!!!!
- [x] Add timeout shutdown for BLE after 7 mins
- [x] Always show battery percentage on idle. Show speed only when you are running.
- [x] Long beep after shutdown. Small beep when turn on.
- [x] Break lights flashing from vesc. Should be connected to servo 'pin-ppm (also you may need LR7843 – MOSFET module controller if you dont have dc-dc preinstalled) 
- [x] Secret mode - double beep. Default mode single beep
- [x] You can change gears (speed mode) while running.
- [x] Regenerative brake enables after 1 km\h
- [x] Disable option to shut down BLE while you are running
- [x] Force front light disabling after shutdown
- [x] [6.05] When current speed < 0 will show the distance traveled since start in meters when brake and throttle pressed
- [x] [6.05] When current speed < 0 will show the 1 cell battery voltage when brake pressed at the same time (example: you got 3.843 volts on each cell, you will see 38 on dash)
- [x] [6.05] Visual code A5 on speedometer dash and zero of battery when secret mode activated? also with double beep. Default mode single beep and 0 code on speedometer dash
- [x] [6.05] Cruise control after 5 sec (disable after pressing brake or throttle) (box should be enabled in App Settings -> ADC -> General -> Button Inputs Enable Cruise Control)
- [x] [6.05] Will show mosfet controller temperature as error code (red numbers)

# VESC M365 Dash
Allows you to connect your NINEBOT G30 display to VESC controller.

## How
Do you want to use your  NineBot BLE with a VESC controller? This is the right place for you! \
Read one of the guides below to get started.

## Which version should I use?

If you are running **VESC 6.02**, use these:
- **G30**: https://github.com/Rebell81/vesc_g30_dash/blob/main/g30_dash.lisp
- **How-To** Video: None (Use the more detailed guides linked above)

If you are running **VESC 6.05**, use these:
- **G30**: https://github.com/Rebell81/vesc_g30_dash/blob/main/g30_dash_ADC_6.05v.lisp
- **How-To** [Video:](https://www.youtube.com/watch?v=kX8PsaxfoXQ)

## How do I wire it?
<span style="color:rgb(184, 49, 47);">Red </span>to 5V \
<span style="color:rgb(209, 213, 216);">Black </span>to GND \
<span style="color:rgb(250, 197, 28);">Yellow </span>to TX (UART-HDX) \
<span style="color:rgb(97, 189, 109);">Green </span>to RX (Button) \
1k Ohm Resistor from <span style="color:rgb(251, 160, 38);">3.3V</span> to <span style="color:rgb(97, 189, 109);">RX (Button)</span>

![image](guide/imgs/23999.png)

## Implemented


- [x] Add speed modes (double tap on button)
- [x] Add secret speed mode (hold throttle and brake, double press)
- [x] Add lock mode with beeping and braking (double press while braking)
- [x] Add min-speed feature (makes it more secure)
- [x] Add shutdown feature (turn it off by long press and back on by single tap)
- [x] Add battery in idle feature
- [x] Add separate ADC version
- [x] Add temperature notification icon (60°C)

Features to be added:
- [ ] App communication
- [ ] More unlock combinations

## Fixed to be done
- [x] ~~Figure out why 0x64 packets are not being read. (on my setup)~~ (Can be ignored due to the fact that we do not have to receive any 0x64 packets to sent our own 0x64 back)
- [x] ~~Figure out why button reading is randomly~~ (can be fixed with 470R resistor between 3.3v and RX and capacitor on 3.3v+GND)

## Tested on
### BLEs
- Original G30 BLE Dashboard

### VESCs
- 75100 Alu PCB:
    - Flipsky 75100 Alu PCB ([AliExpress](https://s.click.aliexpress.com/e/_DEXNhX3) - 151€)

#### Requirements on VESC
Requires 6.2\6.5 VESC firmware. 
Can be found here: https://vesc-project.com/

## Worth to check out!
https://github.com/Koxx3/SmartESC_STM32_v2 (VESC firmware for Xiaomi ESCs)
