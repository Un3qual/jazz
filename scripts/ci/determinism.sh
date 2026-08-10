#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
cd "$ROOT"

JAZZ_ARTIFACT_ROOT="${JAZZ_ARTIFACT_ROOT:-artifacts/determinism}"
SOURCE="examples/functions/factorial.jz"
mkdir -p "$JAZZ_ARTIFACT_ROOT"

cabal build jazz
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
