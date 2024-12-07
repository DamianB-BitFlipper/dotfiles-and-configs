#!/usr/bin/env sh

# Get Bluetooth data in JSON format
BLUETOOTH_DATA=$(system_profiler SPBluetoothDataType -json -detailLevel basic)

# Use jq to check if there are any connected headphones
# We look for entries that have both device_services and device_minorType = "Headphones"
HEADPHONE_CHECK=$(echo "$BLUETOOTH_DATA" | jq -r '
  .SPBluetoothDataType[0].device_connected[]
  | select(.[].device_minorType == "Headphones" and .[].device_services != null)
  | if . then "connected" else "disconnected" end
' 2>/dev/null)

if [ -z "$HEADPHONE_CHECK" ]; then
  sketchybar --set $NAME drawing=off
else
  sketchybar --set $NAME drawing=on icon=
fi
