#!/bin/sh

ICON="$(printf '\357\202\240')"
USED="$(df -g /System/Volumes/Data | awk 'NR==2 { print $2 - $4 }')"

if [ -z "$USED" ]; then
  exit 0
fi

sketchybar --set "$NAME" icon="$ICON" label="${USED}GB"
