#!/bin/sh

ICON="$(printf '\357\210\263')"
TOTAL="$(sysctl -n hw.memsize)"

if [ -z "$TOTAL" ]; then
  exit 0
fi

# Used = active + wired + compressed, matching Activity Monitor's "Memory Used".
USED="$(vm_stat | awk -v total="$TOTAL" '
  /page size of/    { for (i = 1; i <= NF; i++) if ($i == "of") page = $(i+1) }
  /Pages active/    { active = $3 }
  /Pages wired/     { wired = $4 }
  /occupied by comp/{ comp = $5 }
  END {
    gsub(/\./, "", active); gsub(/\./, "", wired); gsub(/\./, "", comp)
    printf "%.0f", (active + wired + comp) * page * 100 / total
  }')"

sketchybar --set "$NAME" icon="$ICON" label="${USED}%"
