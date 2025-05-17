#!/usr/bin/env sh

# Get container count, not just container IDs
N_CONTAINERS=$(/usr/local/bin/docker ps -q | wc -l | tr -d ' ')

# Check container count and update sketchybar
if [ "$N_CONTAINERS" -eq 0 ]; then
    sketchybar --set "$NAME" drawing=off
else
    sketchybar --set "$NAME" drawing=on label="$N_CONTAINERS" icon=
fi
