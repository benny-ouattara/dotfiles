#!/usr/bin/env bash
# Polybar bluetooth module: prints an icon for the adapter state, muted when
# the adapter is off. "bluetooth.sh toggle" turns the adapter on or off.

state=$(timeout 2 bluetoothctl show 2>/dev/null)

if [[ "$1" == toggle ]]; then
    if [[ "$state" == *"Powered: yes"* ]]; then
        timeout 5 bluetoothctl power off
    else
        timeout 5 bluetoothctl power on
    fi >/dev/null
    exit 0
fi

if [[ "$state" != *"Powered: yes"* ]]; then
    muted=$(sed -n 's/^muted = //p' ~/.config/theme/current/polybar.ini)
    echo "%{F${muted}}"$'\U000f00b2'"%{F-}"
elif [[ -n "$(timeout 2 bluetoothctl devices Connected 2>/dev/null)" ]]; then
    echo $'\U000f00b1'
else
    echo $'\U000f00af'
fi
