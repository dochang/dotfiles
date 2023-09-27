# shellcheck source=/dev/null

if [ -r "${CARGO_HOME}/env" ]; then
	. "${CARGO_HOME}/env"
elif [ -n "${CARGO_INSTALL_ROOT}" ]; then
	PATH="${CARGO_INSTALL_ROOT}/bin${PATH:+:}$PATH"
else
	PATH="${CARGO_HOME}/bin${PATH:+:}$PATH"
fi
