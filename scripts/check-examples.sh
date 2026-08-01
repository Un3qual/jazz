#!/usr/bin/env bash
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
cd "$ROOT"

cabal build jazz
JAZZ_BIN="$(cabal list-bin jazz)"
TEMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TEMP_DIR"' EXIT

run_example() {
  local name="$1"
  local expected="$2"
  shift 2

  local stdout_path="$TEMP_DIR/$name.stdout"
  local stderr_path="$TEMP_DIR/$name.stderr"
  local expected_path="$TEMP_DIR/$name.expected"

  if ! "$JAZZ_BIN" "$@" >"$stdout_path" 2>"$stderr_path"; then
    printf 'FAIL: %s exited nonzero\n' "$name" >&2
    sed 's/^/  stderr: /' "$stderr_path" >&2
    return 1
  fi
  if [[ -s "$stderr_path" ]]; then
    printf 'FAIL: %s wrote unexpected stderr\n' "$name" >&2
    sed 's/^/  /' "$stderr_path" >&2
    return 1
  fi

  printf '%s\n' "$expected" >"$expected_path"
  if ! cmp -s "$expected_path" "$stdout_path"; then
    printf 'FAIL: %s stdout did not match the checked result\n' "$name" >&2
    diff -u "$expected_path" "$stdout_path" >&2 || true
    return 1
  fi
  printf 'PASS: %s\n' "$name"
}

run_example hello '"Hello, Jazz"' --run examples/hello.jz
run_example factorial '720' --run examples/functions/factorial.jz
run_example result '41' --run examples/patterns/result.jz
run_example module '"Hello from a Jazz module"' \
  --run --entry-module Example::Main --module-root examples/modules/src

echo "Checked Jazz examples passed."
