for profile in "${XDG_CONFIG_HOME}/guix/current" "${GUIX_PROFILE}"; do
	fpath=("$profile/share/zsh/site-functions" $fpath)
done
