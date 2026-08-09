#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
cd "$ROOT"

actionlint
cabal build all
cabal test all --test-show-details=direct
cabal check
bash scripts/check-docs.sh
bash scripts/check-execution-queue.sh
bash scripts/check-examples.sh
nix flake check
git diff --check
