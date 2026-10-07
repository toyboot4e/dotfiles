#!/bin/sh

# AeroSpace numbers monitors left to right, SketchyBar main display first.

aerospace list-monitors --format '%{monitor-id} %{monitor-appkit-nsscreen-screens-id}' |
while read -r monitor display; do
  args=""
  for sid in 1 2 3 4 5 6 7 8 9; do
    if [ "$monitor" = 1 ]; then ws="$sid"; else ws="$monitor-$sid"; fi
    args="$args --set space.$ws display=$display"
  done
  # shellcheck disable=SC2086
  sketchybar $args
done
