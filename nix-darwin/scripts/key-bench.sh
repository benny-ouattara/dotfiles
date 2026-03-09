#!/usr/bin/env sh

echo "--- macOS Defaults Check ---"
defaults read -g KeyRepeat
defaults read -g InitialKeyRepeat

echo ""
echo "--- Nix-Darwin Target Check ---"
echo "Expected KeyRepeat: 1"
echo "Expected InitialKeyRepeat: 7"

echo ""
echo "--- Real-time Latency Test ---"
echo "Hold down a key for 2 seconds. Press Ctrl+C to stop and check frequency."
# Use hidutil to monitor actual key event frequency if needed
ioreg -r -c AppleHIDKeyboard | grep -E "KeyRepeat|InitialKeyRepeat"
