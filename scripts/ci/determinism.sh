#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
cd "$ROOT"

JAZZ_CABAL_JOBS="${JAZZ_CABAL_JOBS-1}"
case "$JAZZ_CABAL_JOBS" in
  "" | 0 | *[!0-9]*)
    printf 'FAIL: JAZZ_CABAL_JOBS must be a positive integer\n' >&2
    exit 2
    ;;
esac

JAZZ_ARTIFACT_ROOT="${JAZZ_ARTIFACT_ROOT:-artifacts/determinism}"
SOURCE="examples/functions/factorial.jz"
mkdir -p "$JAZZ_ARTIFACT_ROOT"

cabal build jazz --jobs="$JAZZ_CABAL_JOBS"
JAZZ_BIN="$(cabal list-bin jazz)"

"$JAZZ_BIN" --run --runtime-stats=json "$SOURCE" \
  >"$JAZZ_ARTIFACT_ROOT/stats-one.stdout" \
  2>"$JAZZ_ARTIFACT_ROOT/stats-one.stderr"
"$JAZZ_BIN" --run --runtime-stats=json "$SOURCE" \
  >"$JAZZ_ARTIFACT_ROOT/stats-two.stdout" \
  2>"$JAZZ_ARTIFACT_ROOT/stats-two.stderr"

"$JAZZ_BIN" --run \
  --runtime-profile="$JAZZ_ARTIFACT_ROOT/profile-one.speedscope.json" \
  "$SOURCE" \
  >"$JAZZ_ARTIFACT_ROOT/profile-one.stdout" \
  2>"$JAZZ_ARTIFACT_ROOT/profile-one.stderr"
"$JAZZ_BIN" --run \
  --runtime-profile="$JAZZ_ARTIFACT_ROOT/profile-two.speedscope.json" \
  "$SOURCE" \
  >"$JAZZ_ARTIFACT_ROOT/profile-two.stdout" \
  2>"$JAZZ_ARTIFACT_ROOT/profile-two.stderr"

cmp "$JAZZ_ARTIFACT_ROOT/stats-one.stdout" "$JAZZ_ARTIFACT_ROOT/stats-two.stdout"
cmp "$JAZZ_ARTIFACT_ROOT/stats-one.stderr" "$JAZZ_ARTIFACT_ROOT/stats-two.stderr"
cmp "$JAZZ_ARTIFACT_ROOT/profile-one.stdout" "$JAZZ_ARTIFACT_ROOT/profile-two.stdout"
cmp "$JAZZ_ARTIFACT_ROOT/profile-one.stderr" "$JAZZ_ARTIFACT_ROOT/profile-two.stderr"
cmp \
  "$JAZZ_ARTIFACT_ROOT/profile-one.speedscope.json" \
  "$JAZZ_ARTIFACT_ROOT/profile-two.speedscope.json"

printf 'Determinism checks passed; artifacts: %s\n' "$JAZZ_ARTIFACT_ROOT"
