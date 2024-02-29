[ ! -x "${HOMEBREW_ROOT}/bin/brew" ] || eval "$("${HOMEBREW_ROOT}/bin/brew" shellenv)"
if [ -d "${HOMEBREW_ROOT}/share/emacs/site-lisp" ]; then
	EMACSLOADPATH="${HOMEBREW_ROOT}/share/emacs/site-lisp${EMACSLOADPATH:+:}${EMACSLOADPATH}"
fi
