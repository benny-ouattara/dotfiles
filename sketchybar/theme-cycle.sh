#!/bin/bash
# Cycle through sketchybar themes

CONFIG_DIR="$HOME/.config/sketchybar"
THEME_DIR="$CONFIG_DIR/themes"
STATE_FILE="$CONFIG_DIR/.current-theme"

THEMES=(catppuccin tokyo-night nord dracula rose-pine gruvbox)

# Read current theme
CURRENT=$(cat "$STATE_FILE" 2>/dev/null || echo "catppuccin")

# Find next theme
for i in "${!THEMES[@]}"; do
  if [ "${THEMES[$i]}" = "$CURRENT" ]; then
    NEXT_INDEX=$(( (i + 1) % ${#THEMES[@]} ))
    break
  fi
done

NEXT="${THEMES[$NEXT_INDEX]}"
echo "$NEXT" > "$STATE_FILE"

# Reload sketchybar to pick up the new theme
sketchybar --reload

# Notify
sketchybar --trigger theme_changed
echo "Theme: $NEXT"
