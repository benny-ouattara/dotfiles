#!/usr/bin/env bash
# Polybar dnd module (custom/ipc hook): a bell-off icon while dunst is paused,
# clickable to resume. "dnd.sh toggle" flips the pause and refreshes the module.

if [[ "$1" == toggle ]]; then
    dunstctl set-paused toggle
    polybar-msg action dnd hook 0 >/dev/null 2>&1
    exit 0
fi

if [[ "$(dunstctl is-paused 2>/dev/null)" == true ]]; then
    echo "%{A1:~/.config/polybar/tokyo/dnd.sh toggle:}"$'\U000f009b'"%{A}"
fi
