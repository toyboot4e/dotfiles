#!/bin/bash

set -uo pipefail

ICON="$(printf '\357\201\251')"

CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/claude-usage.json"
ENDPOINT="https://api.anthropic.com/api/oauth/usage"

COLOR_OK=0xff606060
COLOR_WARN=0xffb58900
COLOR_BAD=0xffdc322f

render() {
  sketchybar --set "$NAME" icon="$ICON" label="$1" label.color="$2"
}

token="$(security find-generic-password -s 'Claude Code-credentials' -w 2>/dev/null \
         | jq -r '.claudeAiOauth.accessToken // empty' 2>/dev/null)"

if [ -n "$token" ]; then
  fresh="$(curl -sf --max-time 10 "$ENDPOINT" \
             -H "Authorization: Bearer $token" \
             -H "anthropic-beta: oauth-2025-04-20")"

  # Only replace the cache once the payload parses and carries a window.
  if [ -n "$fresh" ] && printf '%s' "$fresh" | jq -e '.five_hour.utilization' >/dev/null 2>&1; then
    mkdir -p "$(dirname "$CACHE")"
    printf '%s' "$fresh" > "$CACHE.tmp" && mv "$CACHE.tmp" "$CACHE"
  fi
fi

if [ ! -s "$CACHE" ]; then
  render "n/a" "$COLOR_BAD"
  exit 0
fi

read -r session week <<<"$(jq -r '
  [(.five_hour.utilization // -1), (.seven_day.utilization // -1)]
  | map(round) | @tsv' "$CACHE")"

# A stale cache is still worth showing, so failure here means malformed JSON.
if [ -z "${session:-}" ] || [ "$session" = "-1" ]; then
  render "n/a" "$COLOR_BAD"
  exit 0
fi

worst="$session"
[ "$week" -gt "$worst" ] 2>/dev/null && worst="$week"

if [ "$worst" -ge 90 ]; then
  color="$COLOR_BAD"
elif [ "$worst" -ge 75 ]; then
  color="$COLOR_WARN"
else
  color="$COLOR_OK"
fi

render "S ${session}% W ${week}%" "$color"
