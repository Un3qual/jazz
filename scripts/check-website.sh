#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"

python3 "$ROOT/scripts/check-docs-pages-workflow.py" "$ROOT"
pnpm --dir "$ROOT/website" run test:brand
pnpm --dir "$ROOT/website" run test:search
pnpm --dir "$ROOT/website" run test:experience
pnpm --dir "$ROOT/website" run typecheck
pnpm --dir "$ROOT/website" run build
python3 "$ROOT/scripts/check-website-boundary.py" --build-directory "$ROOT/website/build"
