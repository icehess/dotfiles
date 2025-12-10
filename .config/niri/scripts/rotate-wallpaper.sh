#!/bin/bash

set -e

# Time between changes (30 minutes)
INTERVAL=$((30 * 60))

# sleep 2
while true; do
    ~/bin/hswitch-wall.sh random
    sleep "${INTERVAL}"
done
