#!/bin/bash

CONFIG_DIR="$HOME/.config/sketchybar"
CURRENT=$(cat "$CONFIG_DIR/.current-theme" 2>/dev/null || echo "catppuccin")
source "$CONFIG_DIR/themes/${CURRENT}.sh"
