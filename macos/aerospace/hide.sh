#!/bin/sh
# Hide the focused app without AeroSpace following focus to another workspace.
# Finder takes focus first: with no windows of its own, it keeps AeroSpace on this workspace.

ws=$(aerospace list-workspaces --focused)
pid=$(aerospace list-windows --focused --format '%{app-pid}') || exit

osascript -l JavaScript -e "
ObjC.import('AppKit');
\$.NSRunningApplication.runningApplicationsWithBundleIdentifier('com.apple.finder').firstObject.activateWithOptions(0);
\$.NSRunningApplication.runningApplicationWithProcessIdentifier($pid).hide;
" >/dev/null
aerospace workspace "$ws" 2>/dev/null
