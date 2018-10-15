
is_function () {
	type $1 | grep -E ' is (.* )?function' >/dev/null 2>&1
}

get_shell () {
	local sh="$(ps c -p $$ -o 'comm=' 2>/dev/null || true)"
	if ! expr "$sh" : "[a-zA-Z0-9]*sh" >/dev/null ; then
		sh="$(head -n 1 $0 | sed -e 's:#![^ ]*/\([^ /]*\)\( .*\|$\):\1:')"
	fi
	echo "$sh"
}

is_command () {
	command -v $1 >/dev/null 2>&1
}


