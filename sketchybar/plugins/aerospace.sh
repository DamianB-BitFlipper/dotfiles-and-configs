#!/usr/bin/env bash

# $1 is the workspace ID this item represents (0-10)
# $FOCUSED_WORKSPACE is passed by the aerospace_workspace_change event

ITEM_WORKSPACE="$1"

# Debug logging
echo "$(date): NAME=$NAME ITEM=$ITEM_WORKSPACE FOCUSED_WORKSPACE=$FOCUSED_WORKSPACE" >>/tmp/aerospace_debug.log

# Determine if the focused workspace is secondary (S*) and extract the number
if [[ "$FOCUSED_WORKSPACE" == S* ]]; then
	FOCUSED_NUM="${FOCUSED_WORKSPACE#S}"
	BG_COLOR="0x66ff8888" # Coral/salmon for secondary
else
	FOCUSED_NUM="$FOCUSED_WORKSPACE"
	BG_COLOR="0x44ffffff" # White for primary
fi

# Debug: log the comparison
echo "  -> FOCUSED_NUM=$FOCUSED_NUM, comparing with ITEM=$ITEM_WORKSPACE, match=$([ "$ITEM_WORKSPACE" = "$FOCUSED_NUM" ] && echo 'YES' || echo 'NO')" >>/tmp/aerospace_debug.log

# Highlight if this item matches the focused workspace number
if [ "$ITEM_WORKSPACE" = "$FOCUSED_NUM" ]; then
	sketchybar --set $NAME background.drawing=on background.color=$BG_COLOR
else
	sketchybar --set $NAME background.drawing=off
fi
