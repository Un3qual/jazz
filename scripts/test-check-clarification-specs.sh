#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CHECKER="$ROOT/scripts/check-clarification-specs.sh"
required_specs=(
  "docs/spec/control-flow/if-expressions.md"
  "docs/spec/syntax/operators.md"
  "docs/spec/runtime/primitive-semantics.md"
  "docs/spec/tooling/compiler-warning-flags.md"
)

fixture_root="$(mktemp -d)"
trap 'rm -rf "$fixture_root"' EXIT

git -C "$fixture_root" init -q
for spec in "${required_specs[@]}"; do
  if [[ "$spec" == "docs/spec/control-flow/if-expressions.md" ]]; then
    continue
  fi
  mkdir -p "$fixture_root/$(dirname "$spec")"
  cp "$ROOT/$spec" "$fixture_root/$spec"
done

set +e
missing_output="$(cd "$fixture_root" && bash "$CHECKER" 2>&1)"
missing_status=$?
set -e

if [[ "$missing_status" -eq 0 ]]; then
  printf 'FAIL: clarification checker accepted a missing required surviving spec\n' >&2
  exit 1
fi
if ! rg -F 'FAIL: missing required file: docs/spec/control-flow/if-expressions.md' <<<"$missing_output" >/dev/null; then
  printf 'FAIL: clarification checker did not identify the missing surviving spec\n' >&2
  printf '%s\n' "$missing_output" >&2
  exit 1
fi

if ! current_output="$(cd "$ROOT" && bash "$CHECKER" 2>&1)"; then
  printf 'FAIL: clarification checker rejected the current repository\n' >&2
  printf '%s\n' "$current_output" >&2
  exit 1
fi
if [[ "$current_output" != "Spec clarification contract check passed." ]]; then
  printf 'FAIL: clarification checker emitted unexpected success output\n' >&2
  printf '%s\n' "$current_output" >&2
  exit 1
fi

echo "Spec clarification checker regressions passed."
