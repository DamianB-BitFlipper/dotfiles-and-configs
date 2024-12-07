#!/usr/bin/env sh

SSID="$(ipconfig getsummary en0 | awk -F ' SSID : '  '/ SSID : / {print $2}')"

if [ -z "$SSID" ]; then
  sketchybar --set $NAME label="" icon=󰖪
else
  sketchybar --set $NAME label="$SSID" icon=
fi
