#!/bin/sh

ICON="$(printf '\357\210\263')"
TOTAL="$(sysctl -n hw.memsize)"

COLOR_OK=0xff606060
COLOR_WARN=0xffb58900
COLOR_BAD=0xffdc322f

if [ -z "$TOTAL" ]; then
  exit 0
fi

# Used = active + wired + compressed, matching Activity Monitor's "Memory Used".
# It stays under 100% by design, so swap and the kernel's pressure level carry the real signal.
USED="$(vm_stat | awk -v total="$TOTAL" '
  /page size of/    { for (i = 1; i <= NF; i++) if ($i == "of") page = $(i+1) }
  /Pages active/    { active = $3 }
  /Pages wired/     { wired = $4 }
  /occupied by comp/{ comp = $5 }
  END {
    gsub(/\./, "", active); gsub(/\./, "", wired); gsub(/\./, "", comp)
    printf "%.0f", (active + wired + comp) * page * 100 / total
  }')"

SWAP_GB="$(sysctl -n vm.swapusage | awk '{ for (i = 1; i <= NF; i++) if ($i == "used") { v = $(i+2); sub(/M$/, "", v); printf "%.0f", v / 1024 } }')"

LABEL="${USED}%"
if [ "${SWAP_GB:-0}" -ge 1 ]; then
  LABEL="$LABEL S${SWAP_GB}G"
fi

case "$(sysctl -n kern.memorystatus_vm_pressure_level)" in
  4) COLOR="$COLOR_BAD" ;;
  2) COLOR="$COLOR_WARN" ;;
  *) COLOR="$COLOR_OK" ;;
esac

sketchybar --set "$NAME" icon="$ICON" label="$LABEL" icon.color="$COLOR" label.color="$COLOR"
