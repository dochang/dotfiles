get_shell() {
	local sh="$(ps c -p $$ -o 'comm=' 2>/dev/null || true)"
	if ! expr "$sh" : "[a-zA-Z0-9]*sh" >/dev/null ; then
		sh="$(head -n 1 $0 | sed -e 's:#![^ ]*/\([^ /]*\)\( .*\|$\):\1:')"
	fi
	echo "$sh"
}


