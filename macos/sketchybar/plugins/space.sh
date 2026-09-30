#!/bin/sh

# $1: the workspace this item represents
# $FOCUSED_WORKSPACE: sent by `exec-on-workspace-change` in aerospace.toml

focused="${FOCUSED_WORKSPACE:-$(aerospace list-workspaces --focused)}"

if [ "$1" = "$focused" ]; then
  sketchybar --set "$NAME" background.drawing=on
else
  sketchybar --set "$NAME" background.drawing=off
fi
