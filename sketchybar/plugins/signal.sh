#!/bin/bash
BADGE=$(lsappinfo info -only StatusLabel "Signal" 2>/dev/null | grep -o '"label"="[^"]*"' | cut -d'"' -f4)

# Only show if badge is a number (true notification)
if [[ "$BADGE" =~ ^[0-9]+$ ]]; then
	sketchybar --set "$NAME" drawing=on icon=
else
	sketchybar --set "$NAME" drawing=off
fi
