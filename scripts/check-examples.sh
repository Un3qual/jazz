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

while IFS=$'\t' read -r case_name case_sources case_expected case_args_text; do
  if [[ "$case_name" == "name" ]]; then
    continue
  fi
  if [[ -z "$case_name" || -z "$case_sources" || -z "$case_expected" || -z "$case_args_text" ]]; then
    printf 'FAIL: malformed example case row: %s\n' "$case_name" >&2
    exit 1
  fi

  IFS=',' read -r -a case_source_paths <<< "$case_sources"
  for source_path in "${case_source_paths[@]}"; do
    if [[ ! -f "$source_path" ]]; then
      printf 'FAIL: example case source is missing: %s\n' "$source_path" >&2
      exit 1
    fi
  done

  IFS=' ' read -r -a case_args <<< "$case_args_text"
  run_example "$case_name" "$case_expected" "${case_args[@]}"
done < scripts/example-cases.tsv

echo "Checked Jazz examples passed."
