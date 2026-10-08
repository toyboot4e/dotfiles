#!/bin/sh
# usage: ws.sh focus|move N
# The main monitor owns workspaces 1..9 and the other one 2-1..2-9 (see aerospace.toml),
# so N resolves within the focused monitor's group.

if [ "$(aerospace list-monitors --focused --format '%{monitor-is-main}')" = true ]; then
  ws="$2"
else
  ws="2-$2"
fi

case "$1" in
  focus) exec aerospace workspace "$ws" ;;
  move)  exec aerospace move-node-to-workspace --focus-follows-window "$ws" ;;
esac
