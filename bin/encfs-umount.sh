#!/bin/sh

set -e

dst=$1

which fusermount >/dev/null
mountpoint -q "$dst" || exit 0
fusermount -u "$dst"
