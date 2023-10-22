#!/usr/bin/bash

set -exuo pipefail
for device in $(xrandr --query | grep '\bconnected\b' | grep -v 'eDP-1' | cut -d' ' -f1); do
    xrandr --output "$device" --set "Broadcast RGB" "Full"
done
