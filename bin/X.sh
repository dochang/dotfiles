#!/bin/sh

systemctl --user import-environment GPG_AGENT_INFO SSH_AGENT_PID SSH_AUTH_SOCK SSH_ASKPASS SUDO_ASKPASS

encfs_mount() {
	src=$1
	dest=$2
	src_basename=$(basename $1 .enc)
	dest_basename=$(basename $2)
	which encfs >/dev/null && \
		{ df | grep "/${dest_basename}\$" ; test $? -eq 1 ; } >/dev/null && \
		test -d $src && \
		{ test -d $dest || mkdir $dest ; } && \
		encfs --idle=720 --extpass="pass show system/encfs/${src_basename}/password" $src $dest
}
encfs_mount ~/Dropbox/org.enc ~/org
multi-head.py
# Run multi head setting after encfs so that the askpass dialog will appear in
# the center of the primary output.

eval "$@"
