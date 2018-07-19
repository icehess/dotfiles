#!/bin/sh

if type ssh-add > /dev/null 2>&1 ; then
    if [ -f $HOME/.ssh/2600hz_hesaam ] ; then
        ssh-add $HOME/.ssh/2600hz_hesaam </dev/null
    else
        echo "no 2600Hz keys were found."
        exit 1
    fi
else
    echo "ssh-add command not found"
    exit 1
fi
