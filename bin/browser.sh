#!/bin/sh

select_browser () {
	if [ x"$DISPLAY" = x ]; then
		echo w3m
	else
		cat <<EOF | dmenu -l 8 -p "Which browser?"
firefox -private-window
firefox -new-window
firefox -new-tab
firefox
chromium --incognito
chromium
conkeror
EOF
	fi
}

browser="$(select_browser)"

[ x"$browser" = x ] || exec env "BROWSER=$browser" sensible-browser "$@"
