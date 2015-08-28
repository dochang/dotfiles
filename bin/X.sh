#!/bin/sh

systemctl --user import-environment GPG_AGENT_INFO SSH_AGENT_PID SSH_AUTH_SOCK SSH_ASKPASS SUDO_ASKPASS
encfs-mount.sh ~/Dropbox/org.enc ~/org
multi-head.py
# Run multi head setting after encfs so that the askpass dialog will appear in
# the center of the primary output.

eval "$@"
