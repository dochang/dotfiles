is_function() {
	type $1 | grep -E ' is (.* )?function' >/dev/null 2>&1
}

get_shell() {
	local sh="$(ps c -p $$ -o 'comm=' 2>/dev/null || true)"
	if ! expr "$sh" : "[a-zA-Z0-9]*sh" >/dev/null; then
		sh="$(head -n 1 $0 | sed -e 's:#![^ ]*/\([^ /]*\)\( .*\|$\):\1:')"
	fi
	echo "$sh"
}

is_command() {
	command -v $1 >/dev/null 2>&1
}

___no_proxy() {
	printf "localhost,127.*.*.*,10.*.*.*,192.168.*.*"
	printf ",$(seq -f '172.%g.*.*' -s , 16 31)"
	# https://github.com/docker/docker/pull/10133
	# https://github.com/docker/docker/pull/9951
	# https://github.com/docker/docker/issues/10192
	# https://github.com/docker/docker/issues/10224
	printf ",/var/run/docker.sock"
	printf ",lvh.me,vcap.me,localtest.me"
	printf ",progn.in"
	if [ -r ~/.no_proxy ]; then
		grep -v '^[ \t]*#' <~/.no_proxy | xargs printf ',%s'
	fi
}
