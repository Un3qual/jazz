#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
STDLIB_DIR="${ROOT}/jazz-next/stdlib"
status=0

for file in "$STDLIB_DIR"/*.jz; do
  [[ -e "$file" ]] || continue
  file_name="$(basename "$file")"
  if [[ "$file_name" == "Prelude.jz" ]]; then
    continue
  fi

  header="$(sed -n '1p' "$file")"
  if [[ "$header" != module\ * ]]; then
    continue
  fi

  if ! awk -v file="$file" '
    { lines[NR] = $0 }
    END {
      failed = 0
      if (lines[1] !~ /^module .*[{]$/) {
        printf "FAIL: %s:1 must be an unindented module header ending in {\n", file > "/dev/stderr"
        failed = 1
      }
      if (lines[NR] != "}") {
        printf "FAIL: %s:%d must be an unindented final }\n", file, NR > "/dev/stderr"
        failed = 1
      }
      for (line = 2; line < NR; line += 1) {
        if (lines[line] ~ /^[[:space:]]*$/) {
          continue
        }
        match(lines[line], /^ */)
        spaces = RLENGTH
        if (spaces < 2 || spaces % 2 != 0) {
          printf "FAIL: %s:%d must use two-space indentation levels\n", file, line > "/dev/stderr"
          failed = 1
        }
      }
      exit failed
    }
  ' "$file"; then
    status=1
  fi
done

if [[ "$status" -ne 0 ]]; then
  exit "$status"
fi

echo "Jazz stdlib formatting checks passed."
