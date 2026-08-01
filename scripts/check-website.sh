#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"

python3 "$ROOT/scripts/check-website-boundary.py" "$ROOT"
npm --prefix "$ROOT/website" run typecheck
npm --prefix "$ROOT/website" run build
python3 "$ROOT/scripts/check-website-boundary.py" "$ROOT"
