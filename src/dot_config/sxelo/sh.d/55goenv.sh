## For goenv
#
# goenv overrides `GOPATH` to avoid mixing multiple versions of golang packages
# at `GOPATH` when using different versions of golang.  To change `GOPATH`, do
# not set `GOPATH`, set `GOENV_GOPATH_PREFIX` instead.
#
# https://github.com/syndbg/goenv/blob/master/ENVIRONMENT_VARIABLES.md
# https://github.com/syndbg/goenv/issues/72#issuecomment-478011438

# goenv prepend `PATH` only if the shims is not in `PATH`.  But others prepend
# their paths unconditionally.  So if `.profile` is invoked many times, goenv
# shims will be overridden by others.  Force to prepend it here.
[ -z "$GOENV_ROOT" ] || {
	# shellcheck disable=SC2123
	PATH="$GOENV_ROOT/shims${PATH:+:}$PATH"
}
