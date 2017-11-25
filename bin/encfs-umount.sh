#!/bin/sh

set -e

dst=$1

command -v fusermount >/dev/null
mountpoint -q "$dst" || exit 0
fusermount -u "$dst"
