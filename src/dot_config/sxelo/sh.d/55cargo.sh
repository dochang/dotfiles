# shellcheck source=/dev/null
[ ! -r "${CARGO_HOME}/env" ] || . "${CARGO_HOME}/env"
if [ -n "${CARGO_INSTALL_ROOT}" ]; then
	PATH="${CARGO_INSTALL_ROOT}/bin${PATH:+:}$PATH"
else
	PATH="${CARGO_HOME}/bin${PATH:+:}$PATH"
fi
