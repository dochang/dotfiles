#!/bin/sh

set -e

src=$1
dst=$2
src_basename="$(basename $1 .enc)"
dst_basename="$(basename $2)"

extpass="$3"
if [ -z "$extpass" ]; then
	extpass="pass show system/encfs/${src_basename}/password"
fi

command -v encfs >/dev/null
! mountpoint -q "$dst" || exit 0
test -d "$src"
{ test -d "$dst" || mkdir "$dst" ; }
encfs --idle=720 --extpass="$extpass" "$src" "$dst"
