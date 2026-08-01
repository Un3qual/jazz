#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CHECKER="$ROOT/scripts/check-clarification-specs.sh"
public_owners=(
  "docs/language/algebraic-data-types-and-patterns.md"
  "docs/language/control-flow.md"
  "docs/language/operators.md"
  "docs/standard-library/prelude.md"
  "docs/standard-library/queue.md"
  "docs/reference/expression-grammar.md"
  "docs/reference/runtime-values.md"
  "docs/reference/diagnostics.md"
)
execution_owners=(
  ".codex/execution/README.md"
  ".codex/execution/blocker-contracts.md"
  ".codex/execution/queue.md"
  ".codex/execution/prompts/autonomous-next-batch.md"
  ".codex/execution/prompts/curated-next-batch.md"
)

fixture_root="$(mktemp -d)"
trap 'rm -rf "$fixture_root"' EXIT

copy_complete_fixture() {
  local target="$1"
  mkdir -p "$target"
  git -C "$target" init -q
  for owner in "${public_owners[@]}" "${execution_owners[@]}"; do
    mkdir -p "$target/$(dirname "$owner")"
    cp "$ROOT/$owner" "$target/$owner"
  done
}

missing_owner_root="$fixture_root/missing-owner"
copy_complete_fixture "$missing_owner_root"
rm "$missing_owner_root/docs/language/control-flow.md"

set +e
missing_output="$(cd "$missing_owner_root" && bash "$CHECKER" 2>&1)"
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

obsolete_product_identity='jazz'"-next"
deleted_design_filenames=(
  "2026-07-21-${obsolete_product_identity}-hosted-canonical-core-design.md"
  "2026-07-21-${obsolete_product_identity}-backend-neutral-lowered-ir-design.md"
  "2026-07-22-${obsolete_product_identity}-typed-core-elaboration-design.md"
  "2026-07-30-${obsolete_product_identity}-typed-core-expression-direct-call-design.md"
)

for index in "${!deleted_design_filenames[@]}"; do
  design_fixture_root="$fixture_root/deleted-design-$index"
  copy_complete_fixture "$design_fixture_root"
  printf '\n%s\n' "${deleted_design_filenames[$index]}" >>"$design_fixture_root/.codex/execution/README.md"

  set +e
  deleted_design_output="$(cd "$design_fixture_root" && bash "$CHECKER" 2>&1)"
  deleted_design_status=$?
  set -e

  if [[ "$deleted_design_status" -eq 0 ]]; then
    printf 'FAIL: clarification checker accepted deleted design filename %s\n' "${deleted_design_filenames[$index]}" >&2
    exit 1
  fi
  if ! rg -F 'FAIL: .codex/execution contains a live reference to a deleted design filename' <<<"$deleted_design_output" >/dev/null; then
    printf 'FAIL: clarification checker did not diagnose deleted design filename %s\n' "${deleted_design_filenames[$index]}" >&2
    printf '%s\n' "$deleted_design_output" >&2
    exit 1
  fi
done

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
