#!/usr/bin/env sh

BTC_USD=$(curl -s "https://api.coingecko.com/api/v3/simple/price?ids=bitcoin&vs_currencies=usd" | jq -r '.bitcoin.usd')

if [ -z "$BTC_USD" ]; then
    sketchybar --set $NAME drawing=off
else
    sketchybar --set $NAME drawing=on label="\$$BTC_USD" icon=
fi
