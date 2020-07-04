#!/bin/sh
xrandr --output eDP1 --primary --mode 1920x1080 --pos 1680x0 --rotate normal --output DP1 --off --output DP2 --off --output DP3 --off --output HDMI1 --mode 1680x1050 --pos 0x30 --rotate normal --output HDMI2 --off --output HDMI3 --off --output VIRTUAL1 --off
setxkbmap -layout "us,cz(qwerty)" -option "grp:alt_shift_toggle" &
