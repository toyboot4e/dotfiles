#!/bin/sh

# Shows workspaces 1..9 on the main display and s1..s9 on the other one.
# With a single display left, windows of s1..s9 merge into 1..9.

# display_change can arrive before AeroSpace has noticed the new monitor set
sleep 1

monitors=$(aerospace list-monitors --format '%{monitor-is-main}|%{monitor-appkit-nsscreen-screens-id}') || exit

primary=$(printf '%s\n' "$monitors" | awk -F'|' '$1 == "true" { print $2; exit }')
secondary=$(printf '%s\n' "$monitors" | awk -F'|' -v p="$primary" '$2 != p { print $2; exit }')

args=""
for sid in 1 2 3 4 5 6 7 8 9; do
  args="$args --set space.$sid display=$primary"
  if [ -n "$secondary" ]; then
    args="$args --set space.s$sid display=$secondary drawing=on"
  else
    args="$args --set space.s$sid drawing=off"
  fi
done
# shellcheck disable=SC2086
sketchybar $args

[ -n "$secondary" ] && exit

aerospace list-windows --all --format '%{window-id} %{workspace}' |
while read -r id ws; do
  case "$ws" in
    s[1-9]) aerospace move-node-to-workspace --window-id "$id" "${ws#s}" ;;
  esac
done
