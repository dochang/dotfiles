#!/bin/sh

select_browser () {
	if [ x"$DISPLAY" = x ]; then
		echo w3m
	else
		cat <<EOF | rofi -dmenu -l 8 -p "Which browser?"
firefox -private-window
firefox -new-window
firefox -new-tab
firefox
chromium --incognito
chromium
conkeror
clipboard
EOF
	fi
}

browser="$(select_browser)"

if [ x"$browser" = x"clipboard" -o x"$browser" = x ]; then
	echo "$@" | xclip -selection clipboard
else
	exec env "BROWSER=$browser" sensible-browser "$@"
fi
