#!/bin/sh

case x"$1" in
x-y | x--yes)
	reply=yes
	;;
*)
	reply="$(rofi -normal-window -dmenu -i -p 'Lock the computer? (Press yes)' </dev/null)"
	;;
esac

[ x"$reply" = xyes ] && eval "${XLOCKER:-slock}"
