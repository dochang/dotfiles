PATH="${ANYENV_ROOT}/bin${PATH:+:}$PATH"
[ ! -x "${ANYENV_ROOT}/bin/anyenv" ] || eval "$("${ANYENV_ROOT}/bin/anyenv" init - "$SXELO")"
