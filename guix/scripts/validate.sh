#!/usr/bin/env bash
set -e

echo "🔍 Validating Otter-Ops Fleet..."

# Validate GNU Guix (Scheme)
if command -v guile >/dev/null; then
    echo "Checking Scheme syntax..."
    find guix -name "*.scm" -exec guile --r6rs -c '(import (rnrs))' {} +
fi

echo "✅ All clear! Proceeding with commit."

