if [ -d "${XDG_CONFIG_HOME}/emacs/site-lisp" ]; then
	EMACSLOADPATH="${EMACSLOADPATH}${EMACSLOADPATH:+:}${XDG_CONFIG_HOME}/emacs/site-lisp"
fi
if [ -d "${XDG_DATA_HOME}/emacs/site-lisp" ]; then
	EMACSLOADPATH="${XDG_DATA_HOME}/emacs/site-lisp${EMACSLOADPATH:+:}${EMACSLOADPATH}"
fi
