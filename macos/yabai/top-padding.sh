#!/usr/bin/env sh

# Applies top-padding for non-builtin displays

BAR_HEIGHT=34

builtin_ids=$(
  system_profiler SPDisplaysDataType -json |
    jq -r '[.SPDisplaysDataType[].spdisplays_ndrvs // [] | .[]
      | select(.spdisplays_connection_type == "spdisplays_internal")
      | ._spdisplays_displayID]'
)

yabai -m query --displays |
  jq -r --argjson builtin "$builtin_ids" --arg h "$BAR_HEIGHT" '
    .[] | (if (.id | tostring) as $id | $builtin | index($id) then "0" else $h end) as $pad
    | .spaces[] | "\(.) \($pad)"
  ' |
  while read -r space pad; do
    yabai -m config --space "$space" top_padding "$pad"
  done
