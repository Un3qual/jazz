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

replace_literal() {
  python3 - "$1" "$2" "$3" <<'PY'
from pathlib import Path
import sys

path = Path(sys.argv[1])
old = sys.argv[2]
new = sys.argv[3]
text = path.read_text(encoding="utf-8")
if old not in text:
    raise SystemExit(f"fixture text not found in {path}: {old}")
path.write_text(text.replace(old, new, 1), encoding="utf-8")
PY
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

missing_heading_root="$fixture_root/missing-heading"
copy_complete_fixture "$missing_heading_root"
replace_literal \
  "$missing_heading_root/docs/language/control-flow.md" \
  '## Conditionals' \
  '## Conditional expressions'
if missing_heading_output="$(cd "$missing_heading_root" && bash "$CHECKER" 2>&1)"; then
  printf 'FAIL: clarification checker accepted a missing required heading\n' >&2
  exit 1
fi
if ! rg -F 'docs/language/control-flow.md missing required section: conditionals' <<<"$missing_heading_output" >/dev/null; then
  printf 'FAIL: clarification checker did not identify the missing required heading\n' >&2
  printf '%s\n' "$missing_heading_output" >&2
  exit 1
fi

hidden_heading_root="$fixture_root/hidden-heading"
copy_complete_fixture "$hidden_heading_root"
replace_literal \
  "$hidden_heading_root/docs/language/control-flow.md" \
  '## Conditionals' \
  $'<!--\n## Conditionals\n-->\n## Conditional expressions'
if hidden_heading_output="$(cd "$hidden_heading_root" && bash "$CHECKER" 2>&1)"; then
  printf 'FAIL: clarification checker accepted a required heading hidden in Markdown\n' >&2
  exit 1
fi
if ! rg -F 'docs/language/control-flow.md missing required section: conditionals' <<<"$hidden_heading_output" >/dev/null; then
  printf 'FAIL: clarification checker did not identify a required heading hidden in Markdown\n' >&2
  printf '%s\n' "$hidden_heading_output" >&2
  exit 1
fi

missing_contract_root="$fixture_root/missing-contract"
copy_complete_fixture "$missing_contract_root"
replace_literal \
  "$missing_contract_root/docs/language/operators.md" \
  '`True | False` is rejected with `E2003`.' \
  '`True | False` is rejected.'
if missing_contract_output="$(cd "$missing_contract_root" && bash "$CHECKER" 2>&1)"; then
  printf 'FAIL: clarification checker accepted a weakened public contract\n' >&2
  exit 1
fi
if ! rg -F 'docs/language/operators.md missing required contract: pipe is fixity-only and rejected with E2003' <<<"$missing_contract_output" >/dev/null; then
  printf 'FAIL: clarification checker did not identify the weakened public contract\n' >&2
  printf '%s\n' "$missing_contract_output" >&2
  exit 1
fi

hidden_contract_root="$fixture_root/hidden-contract"
copy_complete_fixture "$hidden_contract_root"
replace_literal \
  "$hidden_contract_root/docs/language/operators.md" \
  $'`|` is parser/fixity metadata only. It has no executable built-in type rule;\n`True | False` is rejected with `E2003`.' \
  $'<!--\n`|` is parser/fixity metadata only. It has no executable built-in type rule;\n`True | False` is rejected with `E2003`.\n-->\n`|` executes as a built-in Boolean operator.'
if hidden_contract_output="$(cd "$hidden_contract_root" && bash "$CHECKER" 2>&1)"; then
  printf 'FAIL: clarification checker accepted a public contract hidden in Markdown\n' >&2
  exit 1
fi
if ! rg -F 'docs/language/operators.md missing required contract: pipe is fixity-only and rejected with E2003' <<<"$hidden_contract_output" >/dev/null; then
  printf 'FAIL: clarification checker did not identify a public contract hidden in Markdown\n' >&2
  printf '%s\n' "$hidden_contract_output" >&2
  exit 1
fi

stale_execution_root="$fixture_root/stale-execution-owner"
copy_complete_fixture "$stale_execution_root"
printf '\nSee docs/execution/queue.md.\n' >>"$stale_execution_root/.codex/execution/README.md"
if stale_execution_output="$(cd "$stale_execution_root" && bash "$CHECKER" 2>&1)"; then
  printf 'FAIL: clarification checker accepted the deleted docs/execution owner\n' >&2
  exit 1
fi
if ! rg -F '.codex/execution contains a live reference to a deleted documentation owner' <<<"$stale_execution_output" >/dev/null; then
  printf 'FAIL: clarification checker did not identify the deleted docs/execution owner\n' >&2
  printf '%s\n' "$stale_execution_output" >&2
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
