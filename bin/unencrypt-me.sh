#!/bin/sh

FILE_DIR="$(dirname `readlink -m $1`)"
FILE_NAME="`basename \"$1\"`"

DEC_FILE_NAME="$FILE_DIR/${FILE_NAME%.*}"

echo
echo ":: Decrytping $1"
openssl enc -d -a -in "$1" -out "$DEC_FILE_NAME"

echo
[ `echo $?` -eq 0 ] && echo done. || echo failed.
