# shellcheck source=/dev/null

if [ -d "${XDG_CONFIG_HOME}/guix/current/etc/bash_completion.d" ]; then
	for rc in "${XDG_CONFIG_HOME}/guix/current/etc/bash_completion.d"/*; do
		. "$rc"
	done
fi

if [ -r "${GUIX_PROFILE}/share/bash-completion/bash_completion" ]; then
	. "${GUIX_PROFILE}/share/bash-completion/bash_completion"
fi
