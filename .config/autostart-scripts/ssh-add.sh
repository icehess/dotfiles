#!/bin/sh
[ type ssh-add > /dev/null 2>&1 ] && [ -f $HOME/.ssh/hesaam_github ] && ssh-add $HOME/.ssh/hesaam_github </dev/null
