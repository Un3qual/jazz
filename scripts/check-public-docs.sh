#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel 2>/dev/null)"; then
  :
else
  ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
fi

if ! command -v python3 >/dev/null 2>&1; then
  printf 'FAIL: python3 is required for scripts/check-public-docs.sh\n' >&2
  exit 1
fi

exec python3 "$ROOT/scripts/check-public-docs.py" "$ROOT"
