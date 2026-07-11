#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CHECKER="${ROOT}/jazz-next/scripts/check-stdlib-format.sh"

tmpdir="$(mktemp -d)"
cleanup() {
  rm -rf "$tmpdir"
}
trap cleanup EXIT

fixture_root="${tmpdir}/fixture"
fixture_scripts="${fixture_root}/jazz-next/scripts"
fixture_stdlib="${fixture_root}/jazz-next/stdlib"
hostile_bin="${tmpdir}/hostile-bin"
mkdir -p "$fixture_scripts" "$fixture_stdlib" "$hostile_bin"
cp "$CHECKER" "$fixture_scripts/check-stdlib-format.sh"

printf '%s\n' \
  '#!/usr/bin/env bash' \
  'echo "find must not be required by the stdlib format checker" >&2' \
  'exit 64' \
  > "$hostile_bin/find"
chmod +x "$hostile_bin/find"

printf '%s\n' \
  'module Bad {' \
  '   value = 1' \
  '}' \
  > "$fixture_stdlib/Bad.jz"

invalid_output="${tmpdir}/invalid-output.txt"
if PATH="${hostile_bin}:${PATH}" bash "$fixture_scripts/check-stdlib-format.sh" \
  > "$invalid_output" 2>&1; then
  echo "FAIL: malformed stdlib source passed when find was unavailable" >&2
  cat "$invalid_output" >&2
  exit 1
fi

if ! grep -Fq "must use two-space indentation levels" "$invalid_output"; then
  echo "FAIL: malformed stdlib source failed for the wrong reason" >&2
  cat "$invalid_output" >&2
  exit 1
fi

printf '%s\n' \
  'module Good {' \
  '  value = 1' \
  '}' \
  > "$fixture_stdlib/Bad.jz"

valid_output="${tmpdir}/valid-output.txt"
if ! PATH="${hostile_bin}:${PATH}" bash "$fixture_scripts/check-stdlib-format.sh" \
  > "$valid_output" 2>&1; then
  echo "FAIL: valid stdlib source failed when find was unavailable" >&2
  cat "$valid_output" >&2
  exit 1
fi

if ! grep -Fq "Jazz stdlib formatting checks passed." "$valid_output"; then
  echo "FAIL: valid stdlib source did not report success" >&2
  cat "$valid_output" >&2
  exit 1
fi

echo "Stdlib format checker portability tests passed."
