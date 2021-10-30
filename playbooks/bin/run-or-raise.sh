#!/bin/sh

# http://vickychijwani.github.io/2012/04/15/blazing-fast-application-switching-in-linux/
# http://vedang.me/techlog/2011/08/04/why-i-quit-stumpwm/
# https://gist.github.com/vedang/1130303

# Usage
#
#     run-or-raise.sh [xdotool options]* <pattern> [--] <command> [arguments]*

xdo_opts=''
pattern_given=''

while [ "$#" -gt 0 ]; do
	expr x"$1" : x-- >/dev/null || pattern_given=yes
	xdo_opts="$xdo_opts '$1'"
	shift
	[ x"$pattern_given" != x ] && break
done

[ x"$pattern_given" = x ] && echo "No pattern !!!" && exit 1

while [ x"$1" = x-- ]; do
	shift
done

[ $# -eq 0 ] && echo "No command !!!" && exit 2

eval "xdotool search $xdo_opts windowactivate || $@ &"
