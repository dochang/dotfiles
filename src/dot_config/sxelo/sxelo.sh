source_dir_if_exist() {
	dirpath="$1"
	suffix="$2"
	if [ -d "$dirpath" ]; then
		for rc in "$dirpath"/*"$suffix"; do
			# shellcheck source=/dev/null
			. "$rc"
		done
	fi
}

sxelo_init_once() {
	if [ -z "$SXELO_INIT" ]; then
		sxelo_dir="${XDG_CONFIG_HOME:-${HOME}/.config}/sxelo"
		for sh in "$@"; do
			dirpath="${sxelo_dir}/${sh}.d"
			source_dir_if_exist "$dirpath" ".$sh"
		done
		SXELO_INIT=1
		# DO NOT export this variables.  They are used to prevent
		# config files being loaded more than once by `profile` and
		# `rc` in one shell process.
	fi
}
