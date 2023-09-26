MINICONDA3_ROOT="${HOME}/miniconda3"
if [ -f "${MINICONDA3_ROOT}/etc/profile.d/conda.sh" ]; then
	# shellcheck source=/dev/null
	. "${MINICONDA3_ROOT}/etc/profile.d/conda.sh"
else
	# shellcheck disable=SC2123
	PATH="${MINICONDA3_ROOT}/condabin${PATH:+:}$PATH"
	PATH="${MINICONDA3_ROOT}/bin${PATH:+:}$PATH"
fi
