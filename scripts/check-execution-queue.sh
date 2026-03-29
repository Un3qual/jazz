#!/usr/bin/env bash
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
cd "$ROOT"

if ! command -v python3 >/dev/null 2>&1; then
  printf 'FAIL: python3 is required for scripts/check-execution-queue.sh\n' >&2
  exit 1
fi

python3 "$ROOT/scripts/check-execution-queue.py"
