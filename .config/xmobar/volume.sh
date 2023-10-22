#!/bin/bash

bars=$(pamixer --get-volume-human | awk -F'[]%[]' '/%/ {if ($0 == "muted") {
print "MM" } else if ($1 > 100) { print 10 } else { print int($1 / 10) }}')

case $bars in
  0)  bar='[----------]' ;;
  1)  bar='[#---------]' ;;
  2)  bar='[##--------]' ;;
  3)  bar='[###-------]' ;;
  4)  bar='[####------]' ;;
  5)  bar='[#####-----]' ;;
  6)  bar='[######----]' ;;
  7)  bar='[#######---]' ;;
  8)  bar='[########--]' ;;
  9)  bar='[#########-]' ;;
  10) bar='[##########]' ;;
  *)  bar='[----!!----]' ;;
esac

src="Volume"
if pamixer --list-sinks | grep bluez > /dev/null; then
    src="Bt"
    for line in $(upower -e); do
      if [[ "$line" =~ ^/org/freedesktop/UPower/devices/headset ]]; then
        batt=" Batt: $(upower -i "$line" | grep "percentage:" | awk '{print $2}')"
        break
      fi
    done
fi

echo "$src: $bar$batt"
