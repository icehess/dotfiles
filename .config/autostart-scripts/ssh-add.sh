#!/bin/sh

if type ssh-add > /dev/null 2>&1 ; then
    if [ -f $HOME/.ssh/hesaam_github ] ; then
        ssh-add $HOME/.ssh/hesaam_github </dev/null
    else
        echo "no github keys were found."
        exit 1
    fi
else
    echo "ssh-add command not found"
    exit 1
fi
