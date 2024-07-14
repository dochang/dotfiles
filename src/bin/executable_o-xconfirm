#!/bin/sh

case $# in
1)
	cmd=$1
	msg="$1 ?"
	;;
2)
	cmd=$1
	msg=$2
	;;
*)
	echo "$0 <command> [ message ]"
	exit 1
	;;
esac

prompt="$(printf '%s (Press yes)' "$msg")"

reply="$(rofi -normal-window -dmenu -i -p "$prompt" </dev/null)"

[ x"$reply" = xyes ] && eval "$cmd"
