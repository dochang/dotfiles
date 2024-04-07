if command -v direnv >/dev/null 2>&1; then
	case "$SXELO" in
	sh) ;;
	*) eval "$(direnv hook "$SXELO")" ;;
	esac
fi
