#!/bin/sh

systemctl --user import-environment SSH_AGENT_PID SSH_AUTH_SOCK SSH_ASKPASS SUDO_ASKPASS
multi-head.py
# Run multi head setting after encfs so that the askpass dialog will appear in
# the center of the primary output.

eval "$@"
