#!/bin/bash
PERCENT=$(memory_pressure | grep "System-wide memory free percentage:" | awk '{ gsub(/%/,""); printf "%02d", 100-$5 }')
if [ "$PERCENT" -lt 30 ]; then
	sketchybar --set "$NAME" drawing=off
else
	sketchybar --set "$NAME" drawing=on label="${PERCENT}%"
fi
