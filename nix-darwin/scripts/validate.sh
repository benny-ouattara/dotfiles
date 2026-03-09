#!/usr/bin/env bash
set -e

echo "🔍 Validating Otter-Ops Fleet..."

# Validate Nix-Darwin (Nix)
if command -v nix >/dev/null; then
    echo "Checking Nix syntax..."
    find nix-darwin -name "*.nix" -exec nix-instantiate --parse {} >/dev/null +
    if [ -f "nix-darwin/flake.nix" ]; then
        nix flake check ./nix-darwin --metadata
    fi
fi

echo "✅ All clear! Proceeding with commit."
