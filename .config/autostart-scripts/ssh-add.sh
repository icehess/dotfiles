#!/bin/sh
[ type ssh-add > /dev/null 2>&1 -a -f $HOME/.ssh/id_rsa ] && ssh-add $HOME/.ssh/id_rsa </dev/null
