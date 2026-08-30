#!/bin/sh

ICON="$(printf '\357\213\233')"
IDLE="$(top -l 2 -n 0 -s 1 | grep -Eo '[0-9.]+% idle' | tail -1 | cut -d% -f1)"

if [ -z "$IDLE" ]; then
  exit 0
fi

sketchybar --set "$NAME" icon="$ICON" label="$(awk -v i="$IDLE" 'BEGIN { printf "%.0f%%", 100 - i }')"
