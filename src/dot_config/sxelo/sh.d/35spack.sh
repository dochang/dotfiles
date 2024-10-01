SPACK_ROOT="${HOME}/spack"
if [ -f "${SPACK_ROOT}/share/spack/setup-env.sh" ]; then
	# shellcheck source=/dev/null
	. "${SPACK_ROOT}/share/spack/setup-env.sh"
else
	PATH="${SPACK_ROOT}/bin${PATH:+:}$PATH"
fi
