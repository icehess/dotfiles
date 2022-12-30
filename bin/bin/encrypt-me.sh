#!/bin/sh

# read a single character
read_char() {
    stty -icanon -echo
    eval "$1=\$(dd bs=1 count=1 2>/dev/null)"
    stty icanon echo
}

FILE_DIR="$(dirname `readlink -m $1`)"
FILE_NAME="`basename \"$1\"`"

FILE_TO_ENC="$1"

if [ -d $1 ] ; then
    echo; echo -n "Encrypting a directory? "
    read_char do_directory
    if [ xy = x"$do_directory" ]; then
        tar cf "$FILE_DIR/$FILE_NAME.tar.xz" "${1%/}"
        FILE_TO_ENC="$FILE_DIR/$FILE_NAME.tar.xz"
    else
        echo
        echo Either say yes to encrypting directory or sepecify a single file
        echo
        exit 1
    fi
fi

echo
echo ":: Encrytping $1"
openssl aes-256-ctr -a -salt -in "$FILE_TO_ENC" -out "$FILE_TO_ENC".enc

echo
