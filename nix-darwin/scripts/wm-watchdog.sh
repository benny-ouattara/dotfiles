#!/usr/bin/env sh

# Check skhd
if ! pgrep -x "skhd" > /dev/null; then
  echo "$(date): skhd down, restarting..."
  /bin/launchctl kickstart -k gui/$(id -u)/org.nixos.skhd
fi

# Check yabai
if ! pgrep -x "yabai" > /dev/null; then
  echo "$(date): yabai down, restarting..."
  /bin/launchctl kickstart -k gui/$(id -u)/homebrew.mxcl.yabai
fi
