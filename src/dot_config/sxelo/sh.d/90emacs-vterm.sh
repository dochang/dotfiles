if [ "$INSIDE_EMACS" = vterm ] && [ -n "${EMACS_VTERM_PATH}" ]; then
	if [ -f "${EMACS_VTERM_PATH}/etc/emacs-vterm-${SXELO}.sh" ]; then
		. "${EMACS_VTERM_PATH}/etc/emacs-vterm-${SXELO}.sh"
	fi
fi
