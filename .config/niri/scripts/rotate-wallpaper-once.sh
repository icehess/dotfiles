#!/bin/bash

set -e

# Directory containing your wallpapers
WALLPAPER_DIR="${HOME}/Pictures/backgrounds"

# Get a list of image files
mapfile -t IMAGES < <(find "${WALLPAPER_DIR}" -type f \( -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.png" -o -iname "*.webp" \))

# Exit if no images found
if [ ${#IMAGES[@]} -eq 0 ]; then
    echo "No images found in ${WALLPAPER_DIR}"
    exit 1
fi

# Pick a random image
RANDOM_IMAGE="${IMAGES[RANDOM % ${#IMAGES[@]}]}"

# Start awww daemon if not running
# if ! pgrep -x "awww-daemon" > /dev/null; then
#     echo "Starting awww-daemon..."
#     awww-daemon &
#     sleep 1 # Give the daemon a moment to start
# fi
# awww img "${RANDOM_IMAGE}" \
#     --transition-type "${TRANSITION_TYPE:-random}" \
#     --transition-duration "${TRANSITION_DURATION:-1}" \
#     --resize "${RESIZE:-crop}"



pkill swaybg || true
swaybg -m fill -i "${RANDOM_IMAGE}" &
