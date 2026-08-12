#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
cd "$ROOT"

if [[ -n "${NIX_CONFIG:-}" ]]; then
  NIX_CONFIG+=$'\n'
fi
NIX_CONFIG+='extra-experimental-features = nix-command flakes'
export NIX_CONFIG

actionlint
cabal build all
cabal test all --test-show-details=direct
cabal check
python3 scripts/test-check-ci-policy.py
python3 scripts/release/test-verify-artifacts.py
bash scripts/check-docs.sh
bash scripts/check-execution-queue.sh
bash scripts/check-examples.sh
nix flake check
if [[ -n "${JAZZ_DIFF_BASE:-}" ]]; then
  git diff --check "$JAZZ_DIFF_BASE...HEAD"
else
  git diff --check
fi
