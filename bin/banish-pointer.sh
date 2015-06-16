#!/bin/sh

# Get screen size.
#
# `--maxdepth 0` means search root window only.
eval $(xdotool search --maxdepth 0 '.*' getwindowgeometry --shell 2>/dev/null)

xdotool mousemove $WIDTH $HEIGHT
