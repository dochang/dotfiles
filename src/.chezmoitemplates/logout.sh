{{- template "functions.sh" -}}

# DO NOT `source` it when the shell is invoked as `sh`.
if [ x"$(get_shell)" != xsh ]; then
	# when leaving the console clear the screen to increase privacy
	if [ "$SHLVL" = 1 ]; then
		[ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
	fi
fi
