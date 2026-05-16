#!/bin/bash

update() {
  source "$CONFIG_DIR/icons.sh"
  INFO=$(networksetup -getairportnetwork en0 | sed 's/Current Wi-Fi Network: //')
  IP=$(ipconfig getifaddr en0)
  if [ -n "$IP" ]; then
    ICON="$WIFI_CONNECTED"
    LABEL="$INFO ($IP)"
  else
    ICON="$WIFI_DISCONNECTED"
    LABEL="Disconnected"
  fi

  sketchybar --set $NAME icon="$ICON" label="$LABEL"
}

click() {
  CURRENT_WIDTH="$(sketchybar --query $NAME | jq -r .label.width)"

  WIDTH=0
  if [ "$CURRENT_WIDTH" -eq "0" ]; then
    WIDTH=dynamic
  fi

  sketchybar --animate sin 20 --set $NAME label.width="$WIDTH"
}

case "$SENDER" in
  "wifi_change"|"routine"|"system_woke") update
  ;;
  "mouse.clicked") click
  ;;
esac
