#!/bin/sh

CONF_DIR="${XDG_CONFIG_DIR:-${HOME}/.config}/multi-head"
CONF_FILE="${CONF_DIR}/multi-head.py"

if [ -x "${CONF_FILE}" ] ; then
	xrandr2json | "${CONF_FILE}" | json2xrandr
fi
