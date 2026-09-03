#!/usr/bin/env bash
set -e

# Git runs hooks from the repo root, but allow direct invocation from anywhere.
cd "$(git rev-parse --show-toplevel)"

echo "🔍 Validating Otter-Ops Fleet..."

# Validate Nix-Darwin (Nix)
if command -v nix >/dev/null; then
    echo "Checking Nix syntax..."
    find nix-darwin -name "*.nix" -exec nix-instantiate --parse {} >/dev/null +
    if [ -f "nix-darwin/flake.nix" ]; then
        echo "Checking flake metadata..."
        # Not `nix flake check`: that evaluates the whole darwin system and is
        # far too slow for a pre-commit hook. Metadata still catches a
        # malformed flake or unresolvable inputs.
        nix flake metadata ./nix-darwin >/dev/null
    fi
fi

echo "✅ All clear! Proceeding with commit."
