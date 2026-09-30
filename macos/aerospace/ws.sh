#!/bin/sh
# usage: ws.sh focus|move N
# Monitor 1 owns workspaces 1..9, monitor M>1 owns M-1..M-9 (see aerospace.toml).

m=$(aerospace list-monitors --focused --format '%{monitor-id}')
if [ "$m" = 1 ]; then ws="$2"; else ws="$m-$2"; fi

case "$1" in
  focus) exec aerospace workspace "$ws" ;;
  move)  exec aerospace move-node-to-workspace --focus-follows-window "$ws" ;;
esac
