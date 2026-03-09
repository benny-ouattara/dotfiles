#!/usr/bin/env sh

echo "--- Checking Accessibility Permissions ---"
if sqlite3 "/Library/Application Support/com.apple.TCC/TCC.db" "SELECT client FROM access WHERE service='kTCCServiceAccessibility';" | grep -q "com.apple.Terminal" || \
   sqlite3 "/Library/Application Support/com.apple.TCC/TCC.db" "SELECT client FROM access WHERE service='kTCCServiceAccessibility';" | grep -q "yabai"; then
  echo "✅ Accessibility permissions appear active."
else
  echo "❌ WARNING: yabai/Terminal might be missing Accessibility permissions."
  echo "Check: System Settings > Privacy & Security > Accessibility"
fi

echo ""
echo "--- Checking Scripting Addition ---"
if yabai -m query --config | grep -q "status"; then
  echo "✅ yabai communication is working."
else
  echo "❌ yabai is not responding to queries."
fi
