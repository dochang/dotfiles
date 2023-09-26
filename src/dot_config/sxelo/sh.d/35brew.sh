[ ! -x "${HOMEBREW_ROOT}/bin/brew" ] || eval "$("${HOMEBREW_ROOT}/bin/brew" shellenv)"
EMACSLOADPATH="${HOMEBREW_ROOT}/share/emacs/site-lisp${EMACSLOADPATH:+:}${EMACSLOADPATH}"
