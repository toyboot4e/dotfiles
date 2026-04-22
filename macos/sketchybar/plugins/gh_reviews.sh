#!/bin/bash

# Set auth
# $ gh auth refresh -s notifications

# Get gyn in PATH
export PATH="/etc/profiles/per-user/$USER/bin:$PATH"
COUNT=$(gh search prs --review-requested=@me --state=open --json url --jq 'length' 2>/dev/null || echo "?")

if [ "$COUNT" -gt 0 ] 2>/dev/null; then
  sketchybar --set "$NAME" label="[$COUNT]" label.color=0xffff6666
else
  sketchybar --set "$NAME" label="[0]" label.color=0xffffffff background.drawing=off
fi
