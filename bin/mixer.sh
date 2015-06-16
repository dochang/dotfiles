#!/bin/sh

which amixer >/dev/null 2>/dev/null || exit

scontrol_name () {
	case $({ amixer info | sed -n "s|^.*Card default.*'\([^']*\)'/.*$|\1|p" ; } 2>/dev/null) in
	L4500)
		echo PCM
		;;
	*)
		echo Master
		;;
	esac
}

exec amixer sset $(scontrol_name) "$@"
