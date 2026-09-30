#!/bin/sh

# $1: the workspace this item represents
# Highlights the workspace visible on this item's display, not only the focused one.

if aerospace list-workspaces --monitor all --visible | grep -qx "$1"; then
  sketchybar --set "$NAME" background.drawing=on
else
  sketchybar --set "$NAME" background.drawing=off
fi
