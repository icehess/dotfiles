#!/bin/sh

if type ssh-add > /dev/null 2>&1 ; then
    FILES="$HOME/.ssh/2600hz_hesaam $HOME/.ssh/hesaam_github"
    for file in $FILES ; do
        if [ -f $file ] ; then
            ssh-add $HOME/.ssh/$file </dev/null
        fi
    done
else
    echo "ssh-add command not found"
    exit 1
fi
