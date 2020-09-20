#!/bin/sh
xrandr --output eDP-1 --mode 1920x1080 --pos 1680x0 --rotate normal --output DP-1 --off --output HDMI-1 --mode 1680x1050 --pos 0x0 --rotate normal --output DP-2 --off --output HDMI-2 --off --output DP-3 --off --output HDMI-3 --off
setxkbmap -layout "us,cz(qwerty)" -option "grp:alt_shift_toggle" &
