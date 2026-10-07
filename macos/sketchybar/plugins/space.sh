#!/bin/sh

# Highlights the workspaces visible on any display with one AeroSpace query and one SketchyBar call.

visible=$(aerospace list-workspaces --monitor all --visible) || exit

args=""
for display in 1 2; do
  for sid in 1 2 3 4 5 6 7 8 9; do
    if [ "$display" = 1 ]; then ws="$sid"; else ws="$display-$sid"; fi
    case "
$visible
" in
      *"
$ws
"*) on=on ;;
      *) on=off ;;
    esac
    args="$args --set space.$ws background.drawing=$on"
  done
done

# shellcheck disable=SC2086
sketchybar $args
