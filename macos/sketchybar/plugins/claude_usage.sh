#!/bin/bash

set -uo pipefail

ICON="$(printf '\357\201\251')"

CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/claude-usage.json"
ENDPOINT="https://api.anthropic.com/api/oauth/usage"

COLOR_OK=0xff606060
COLOR_WARN=0xffb58900
COLOR_BAD=0xffdc322f

# Each window carries its own color, so session and week live in separate items.
SESSION_ITEM=claude_usage
WEEK_ITEM=claude_usage_week

color_for() {
  if [ "$1" -ge 90 ]; then
    printf '%s' "$COLOR_BAD"
  elif [ "$1" -ge 75 ]; then
    printf '%s' "$COLOR_WARN"
  else
    printf '%s' "$COLOR_OK"
  fi
}

render() {
  sketchybar --set "$SESSION_ITEM" icon="$ICON" label="$1" label.color="$2" \
             --set "$WEEK_ITEM" label="$3" label.color="$4"
}

unavailable() {
  render "n/a" "$COLOR_BAD" "" "$COLOR_BAD"
  exit 0
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

[ -s "$CACHE" ] || unavailable

read -r session week <<<"$(jq -r '
  [(.five_hour.utilization // -1), (.seven_day.utilization // -1)]
  | map(round) | @tsv' "$CACHE")"

# A stale cache is still worth showing, so failure here means malformed JSON.
[ -n "${session:-}" ] && [ "$session" != "-1" ] || unavailable

if [ -n "${week:-}" ] && [ "$week" != "-1" ]; then
  week_label="W ${week}%"
  week_color="$(color_for "$week")"
else
  week_label="W n/a"
  week_color="$COLOR_BAD"
fi

render "S ${session}%" "$(color_for "$session")" "$week_label" "$week_color"
