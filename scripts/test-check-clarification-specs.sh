#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CHECKER="$ROOT/scripts/check-clarification-specs.sh"
required_owners=(
  "docs/language/control-flow.md"
  "docs/language/operators.md"
  "docs/reference/runtime-values.md"
  "docs/reference/diagnostics.md"
)

fixture_root="$(mktemp -d)"
trap 'rm -rf "$fixture_root"' EXIT

git -C "$fixture_root" init -q
for owner in "${required_owners[@]}"; do
  if [[ "$owner" == "docs/language/control-flow.md" ]]; then
    continue
  fi
  mkdir -p "$fixture_root/$(dirname "$owner")"
  cp "$ROOT/$owner" "$fixture_root/$owner"
done

set +e
missing_output="$(cd "$fixture_root" && bash "$CHECKER" 2>&1)"
missing_status=$?
set -e

if [[ "$missing_status" -eq 0 ]]; then
  printf 'FAIL: clarification checker accepted a missing required public owner\n' >&2
  exit 1
fi
if ! rg -F 'FAIL: missing required public owner: docs/language/control-flow.md' <<<"$missing_output" >/dev/null; then
  printf 'FAIL: clarification checker did not identify the missing public owner\n' >&2
  printf '%s\n' "$missing_output" >&2
  exit 1
fi

if ! current_output="$(cd "$ROOT" && bash "$CHECKER" 2>&1)"; then
  printf 'FAIL: clarification checker rejected the current repository\n' >&2
  printf '%s\n' "$current_output" >&2
  exit 1
fi
if [[ "$current_output" != "Public clarification contract check passed." ]]; then
  printf 'FAIL: clarification checker emitted unexpected success output\n' >&2
  printf '%s\n' "$current_output" >&2
  exit 1
fi

echo "Public clarification checker regressions passed."
