if command -v navi >/dev/null 2>&1; then
	case "$SXELO" in
	sh) ;;
	*) eval "$(navi widget "$SXELO")" ;;
	esac
fi
