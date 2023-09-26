# https://asdf-vm.com/guide/getting-started.html#_3-install-asdf

if [ -r "${ASDF_DIR}/completions/asdf.bash" ]; then
	# shellcheck source=/dev/null
	. "${ASDF_DIR}/completions/asdf.bash"
fi
