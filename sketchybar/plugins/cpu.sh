#!/bin/bash
PERCENT=$(iostat -c 2 | tail -1 | awk '{ printf "%02d", 100 - $6 }')
if [ "$PERCENT" -lt 50 ]; then
	sketchybar --set "$NAME" drawing=off
else
	sketchybar --set "$NAME" drawing=on label="${PERCENT}%" icon=
fi
