#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
cd "$ROOT"

JAZZ_CABAL_JOBS="${JAZZ_CABAL_JOBS-1}"
if [[ ! "$JAZZ_CABAL_JOBS" =~ ^[0-9]*[1-9][0-9]*$ ]]; then
  printf 'FAIL: JAZZ_CABAL_JOBS must be a positive integer\n' >&2
  exit 2
fi

# A fresh tree is essential: cached test HIE files could otherwise keep unused
# production code alive, and deleted modules could remain in the graph.
quality_build="$(mktemp -d "${TMPDIR:-/tmp}/jazz-haskell-quality.XXXXXX")"
trap 'rm -r -- "$quality_build"' EXIT

bash scripts/test-weeder-policy.sh
hlint src app test benchmark program-support --hint=.hlint.yaml
cabal build exe:jazz --disable-tests --disable-benchmarks \
  --builddir="$quality_build" --ghc-options=-fwrite-ide-info \
  --jobs="$JAZZ_CABAL_JOBS"
weeder --config=weeder-production.toml --hie-directory="$quality_build" --no-default-fields

# Only after production passes do benchmark/tooling and test entry points enter
# the graph.
cabal build all --enable-tests --enable-benchmarks \
  --builddir="$quality_build" --ghc-options=-fwrite-ide-info \
  --jobs="$JAZZ_CABAL_JOBS"
weeder --config=weeder.toml --hie-directory="$quality_build" --no-default-fields
cabal test generated-invariants-spec \
  --builddir="$quality_build" --ghc-options=-fwrite-ide-info \
  --test-show-details=direct --jobs="$JAZZ_CABAL_JOBS"
