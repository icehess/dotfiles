#!/bin/bash

# Get current power profile
get_current_profile() {
    if command -v powerprofilesctl &>/dev/null; then
        powerprofilesctl get
    else
        echo "power-saver" # fallback
    fi
}

# Set power profile
set_profile() {
    case $1 in
    "power-saver")
        if command -v powerprofilesctl &>/dev/null; then
            powerprofilesctl set power-saver
        fi
        ;;
    "balanced")
        if command -v powerprofilesctl &>/dev/null; then
            powerprofilesctl set balanced
        fi
        ;;
    "performance")
        if command -v powerprofilesctl &>/dev/null; then
            powerprofilesctl set performance
        fi
        ;;
    esac
}

# Toggle between profiles
toggle_profile() {
    current=$(get_current_profile)
    case $current in
    "power-saver")
        set_profile "balanced"
        ;;
    "balanced")
        set_profile "performance"
        ;;
    "performance")
        set_profile "power-saver"
        ;;
    esac
}

is_profile_in() {
    current=$(get_current_profile)
    case "${current}" in
    "${1}")
        echo true
        ;;
    *)
        echo false
        ;;
    esac
}

# Display current profile with icon only
display_profile() {
    current=$(get_current_profile)
    case $current in
    "power-saver")
        echo "󰾆" # Battery/efficiency icon
        ;;
    "balanced")
        echo "󰾅" # Balanced icon
        ;;
    "performance")
        echo "󰓅" # Performance/rocket icon
        ;;
    esac
}
display_tooltip_profile() {
    current=$(get_current_profile)
    case $current in
    "power-saver")
        echo "Power saver mode" # Battery/efficiency icon
        ;;
    "balanced")
        echo "Balanced mode" # Balanced icon
        ;;
    "performance")
        echo "Performance mode" # Performance/rocket icon
        ;;
    esac
}

# Handle arguments
case $1 in
"toggle")
    toggle_profile
    display_profile
    ;;
"set")
    set_profile "${2:-balanced}"
    display_tooltip_profile
    ;;
"set-swaync")
    if [ "${SWAYNC_TOGGLE_STATE}" = "true" ]; then
        set_profile "${2:-balanced}"
    else
        toggle_profile
    fi
    maybe_update_swaync "${2:-balanced}" "${3}"
    display_tooltip_profile
    ;;
"is-in")
    is_profile_in "${2}"
    ;;
"tooltip")
    display_tooltip_profile
    ;;
"display" | *)
    display_profile
    ;;
esac
exit 0
