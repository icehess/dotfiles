#!/bin/bash

if type -P 1password >/dev/null; then
    1password --silent --lock
fi
