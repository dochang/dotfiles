#!/bin/sh

case x"$1" in
x-y|x--yes)
	reply=yes
	;;
*)
	reply="$(rofi -dmenu -i -p 'Lock the computer? (Press yes)' < /dev/null)"
	;;
esac

[ -r ~/.config/X/env ] && . ~/.config/X/env

[ x"$reply" = xyes ] && eval "${XLOCKER:-slock}"
