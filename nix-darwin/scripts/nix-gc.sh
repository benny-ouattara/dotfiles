#!/usr/bin/env bash
# Keep the last 5 generations regardless of age
/run/current-system/sw/bin/nix-env --profile /nix/var/nix/profiles/system --delete-generations +5
# Clean up the store
/run/current-system/sw/bin/nix-collect-garbage -d
