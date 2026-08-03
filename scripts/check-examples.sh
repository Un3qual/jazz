#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

if ! command -v python3 >/dev/null 2>&1; then
  printf 'FAIL: python3 is required to run checked Jazz examples\n' >&2
  exit 1
fi

exec python3 "$ROOT/scripts/check-examples.py" "$ROOT"
