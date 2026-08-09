#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

if ! command -v python3 >/dev/null 2>&1; then
  printf 'FAIL: python3 is required to run checked Jazz examples\n' >&2
  exit 1
fi

if [[ "$#" -eq 0 ]]; then
  if ! command -v cabal >/dev/null 2>&1; then
    printf 'FAIL: cabal is required to build checked Jazz examples\n' >&2
    exit 1
  fi
  (
    cd "$ROOT"
    cabal build jazz
  )
  jazz_bin="$(
    cd "$ROOT"
    cabal list-bin jazz
  )"
elif [[ "$#" -eq 2 && "$1" == "--jazz-bin" ]]; then
  jazz_bin="$2"
elif [[ "$#" -eq 1 && "$1" == --jazz-bin=* ]]; then
  jazz_bin="${1#--jazz-bin=}"
else
  printf 'usage: scripts/check-examples.sh [--jazz-bin PATH]\n' >&2
  exit 2
fi

if [[ ! -f "$jazz_bin" ]]; then
  printf 'FAIL: Jazz executable does not exist: %s\n' "$jazz_bin" >&2
  exit 1
fi

python3 "$ROOT/scripts/check-examples.py" \
  "$ROOT" --jazz-bin "$jazz_bin"
python3 "$ROOT/scripts/check-public-docs.py" \
  "$ROOT" --jazz-bin "$jazz_bin"
