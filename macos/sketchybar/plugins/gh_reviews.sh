#!/bin/bash

# Set auth
# $ gh auth refresh -s notifications

# Get gyn in PATH
export PATH="/etc/profiles/per-user/$USER/bin:$PATH"
COUNT=$(gh search prs --review-requested=@me --state=open --json url --jq 'length' 2>/dev/null || echo "?")

BAR_DEFAULT=0xFFABDBDB  # mint

if [ "$COUNT" -gt 0 ] 2>/dev/null; then
  case "$COUNT" in
    1) BAR_COLOR=0xFFDBC0B0 ;;
    2) BAR_COLOR=0xFFDBAFAF ;;
    3) BAR_COLOR=0xFFDB9E9E ;;
    4) BAR_COLOR=0xFFDB8D8D ;;
    *) BAR_COLOR=0xFFDB7C7C ;;
  esac
  sketchybar --set "$NAME" label="[$COUNT]" label.color=0xffffcc00 \
             --bar color=$BAR_COLOR
else
  sketchybar --set "$NAME" label="[0]" label.color=0xff606060 \
             --bar color=$BAR_DEFAULT
fi
