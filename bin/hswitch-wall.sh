#!/bin/bash

# Theme Switcher Script
WALLPAPER_DIR="${HOME}/Pictures/wallpapers"
CURRENT_WALLPAPER_FILE_INDEX="$HOME/.cache/current_wallpaper_index"
CURRENT_WALLPAPER_FILE="$HOME/.cache/current_wallpaper"
CURRENT_WALLPAPER_FILE_BLUR="$HOME/.cache/current_wallpaper_blur"

# Collect wallpapers
mapfile -t WALLPAPERS < <(find "$WALLPAPER_DIR" -type f \( -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.png" \) | sort)

if [ ${#WALLPAPERS[@]} -eq 0 ]; then
    notify-send "Theme Switcher" "No wallpapers found in $WALLPAPER_DIR"
    exit 1
fi

# Helpers
get_current_index() {
    [[ -f "$CURRENT_WALLPAPER_FILE_INDEX" ]] && cat "$CURRENT_WALLPAPER_FILE_INDEX" || echo "0"
}

set_current() {
    local wallpaper_path="${1}"
    local index="${2}"

    if [ -n "${wallpaper_path}" ]; then
        ln -sf "${wallpaper_path}" "${CURRENT_WALLPAPER_FILE}"
    else
        rm "${CURRENT_WALLPAPER_FILE}"
    fi

    if [ -n "${index}" ]; then
        echo "${index}" >"${CURRENT_WALLPAPER_FILE_INDEX}"
    else
        rm -f "${CURRENT_WALLPAPER_FILE_INDEX}"
    fi
}

apply_theme() {
    local wallpaper_path="$1"
    local index="$2"

    set_current "${wallpaper_path}" "${index}"
    create_blur "${wallpaper_path}"

    #### awww
    # if ! pgrep -x "awww-daemon" > /dev/null; then
    #     echo "Starting awww-daemon..."
    #     awww-daemon &
    #     sleep 1 # Give the daemon a moment to start
    # fi
    # awww img "${wallpaper_path}" \
    #     --transition-type "${TRANSITION_TYPE:-random}" \
    #     --transition-duration "${TRANSITION_DURATION:-1}" \
    #     --resize "${RESIZE:-crop}"

    #### swaybg
    pkill swaybg || true
    swaybg -m fill -i "${wallpaper_path}" &

    # update_hyprlock_wallpaper "$wallpaper_path"
}

create_blur() {
    local wallpaper_path="${1:-$CURRENT_WALLPAPER_FILE}"
    [ -z "${wallpaper_path}" ] && return
    [ ! -f "${wallpaper_path}" ] && return

    # for apps like hyprlock that require extension

    wallpaper_path="$(realpath "${wallpaper_path}")"
    local extension="${wallpaper_path##*.}"
    local extension="jpg"

    rm -f "${CURRENT_WALLPAPER_FILE_BLUR}"
    convert -scale 10% -blur 0x2.5 -resize 1000% "${wallpaper_path}" "${CURRENT_WALLPAPER_FILE_BLUR}.${extension:-jpg}"
    ln -sf "${CURRENT_WALLPAPER_FILE_BLUR}.${extension:-jpg}" "${CURRENT_WALLPAPER_FILE_BLUR}"
}

update_hyprlock_wallpaper() {
    local wallpaper_path="$1"
    local hyprlock_config="$HOME/.config/hypr/hyprlock.conf"

    [[ ! -f "${hyprlock_config}.backup" ]] && cp "$hyprlock_config" "${hyprlock_config}.backup"

    sed -i "/background {/,/}/{s|path = .*|path = $wallpaper_path|}" "$hyprlock_config"
}

restore_theme() {
    local index
    index=$(get_current_index)
    apply_theme "${WALLPAPERS[$index]}" "$index"
}

# Main
case "${1:-next}" in
"next")
    next_index=$((($(get_current_index) + 1) % ${#WALLPAPERS[@]}))
    apply_theme "${WALLPAPERS[$next_index]}" "$next_index"
    ;;
"get")
    [ ! -f "${CURRENT_WALLPAPER_FILE}" ] && echo nocurrent && exit 1
    exit 2
    ;;
"random")
    random_index=$((RANDOM % ${#WALLPAPERS[@]}))
    apply_theme "${WALLPAPERS[$random_index]}" "$random_index"
    ;;
"set-current")
    apply_theme "${2}"
    ;;
"restore")
    restore_theme
    ;;
"paper-updated")
    set_current "${2}"
    create_blur
    ;;
"list")
    # Show only filenames in wofi
    selected=$(printf "%s\n" "${WALLPAPERS[@]##*/}" | fuzzel --dmenu --prompt "Choose Wallpaper")

    if [ -n "$selected" ]; then
        for i in "${!WALLPAPERS[@]}"; do
            if [[ "${WALLPAPERS[$i]##*/}" == "$selected" ]]; then
                apply_theme "${WALLPAPERS[$i]}" "$i"
                break
            fi
        done
    else
        notify-send "Theme Switcher" "No wallpaper selected."
    fi
    ;;
esac
