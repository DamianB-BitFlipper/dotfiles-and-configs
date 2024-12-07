#!/usr/bin/env sh

SSID="$(system_profiler SPAirPortDataType | awk -F ': ' '/Current Network Information:/ { getline; print substr($0, 13, (length($0) - 13)); exit }')"

if [ -z "$SSID" ]; then
  sketchybar --set $NAME label="Disconnected" icon=󰖪
else
  sketchybar --set $NAME label="$SSID" icon=
fi
