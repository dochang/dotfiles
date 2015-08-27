#!/bin/sh

systemctl --user import-environment GPG_AGENT_INFO SSH_AGENT_PID SSH_AUTH_SOCK SSH_ASKPASS SUDO_ASKPASS
eval "$@"
