## For Guix

# _GUIX_PROFILE: `guix pull` profile
_GUIX_PROFILE="${XDG_CONFIG_HOME}/guix/current"
if [ -L "$_GUIX_PROFILE" ]; then
	export PATH="$_GUIX_PROFILE/bin${PATH:+:}$PATH"
	# Export INFOPATH so that the updated info pages can be found
	# and read by both /usr/bin/info and/or $GUIX_PROFILE/bin/info
	# When INFOPATH is unset, add a trailing colon so that Emacs
	# searches 'Info-default-directory-list'.
	export INFOPATH="$_GUIX_PROFILE/share/info:$INFOPATH"
fi

# GUIX_PROFILE: User's default profile
GUIX_PROFILE="$HOME/.guix-profile"
[ -L "$GUIX_PROFILE" ] || return
GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
export GUIX_PROFILE GUIX_LOCPATH

# shellcheck source=/dev/null
[ ! -f "$GUIX_PROFILE/etc/profile" ] || . "$GUIX_PROFILE/etc/profile"

# set XDG_DATA_DIRS to include Guix installations
export XDG_DATA_DIRS="$GUIX_PROFILE/share:${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}"

# DO NOT source `/etc/profile.d/guix.sh`.  That script doesn't support XDG
# vars.

if [ -f "${GUIX_PROFILE}/etc/profile" ]; then
	# shellcheck source=/dev/null
	. "${GUIX_PROFILE}/etc/profile"
fi
if [ -d "${GUIX_PROFILE}/etc/profile.d" ]; then
	for rc in "${GUIX_PROFILE}/etc/profile.d"/*.sh; do
		# shellcheck source=/dev/null
		. "$rc"
	done
fi

expr ":${EMACSLOADPATH}:" : ".*:${GUIX_PROFILE}/share/emacs/site-lisp:" >/dev/null || {
	EMACSLOADPATH="${GUIX_PROFILE}/share/emacs/site-lisp${EMACSLOADPATH:+:}${EMACSLOADPATH}"
}
# Guix only sets `EMACSLOADPATH` if Emacs is installed by Guix.  In case of
# using external Emacs, set `EMACSLOADPATH` manually.
