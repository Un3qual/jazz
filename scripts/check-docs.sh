#!/usr/bin/env bash
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
cd "$ROOT"

fail_count=0

fail() {
  printf 'FAIL: %s\n' "$1" >&2
  fail_count=$((fail_count + 1))
}

require_file() {
  local file="$1"
  [[ -f "$file" ]] || fail "missing required file: $file"
}

require_pattern() {
  local file="$1"
  local label="$2"
  local pattern="$3"
  if ! rg -n -e "$pattern" "$file" >/dev/null 2>&1; then
    fail "$file missing required content: $label"
  fi
}

if ! bash scripts/check-public-docs.sh; then
  fail "scripts/check-public-docs.sh reported public documentation boundary violations"
fi

rfc_index="rfcs/README.md"
rfc_proposals_index="rfcs/proposed/README.md"
required_rfcs=(
  "0001-language-authority-and-change-control"
  "0002-repository-productization"
  "0003-bootstrap-interpreter-profile"
  "0004-hosted-canonical-compiler"
  "0005-typed-core-elaboration"
  "0006-lowered-ir-contract"
  "0007-runtime-host-boundary"
  "0008-parser-scale-and-performance-tiers"
)

require_file "$rfc_index"
require_file "$rfc_proposals_index"
for rfc_name in "${required_rfcs[@]}"; do
  require_file "rfcs/accepted/${rfc_name}.md"
done

if [[ -d "rfcs/accepted" ]]; then
  while IFS= read -r rfc_file; do
    rfc_name="$(basename "$rfc_file" .md)"
    require_pattern "$rfc_file" "accepted status" '^Status: Accepted$'
    require_pattern "$rfc_file" "decision date" '^Date: [0-9]{4}-[0-9]{2}-[0-9]{2}$'
    require_pattern "$rfc_file" "superseded decisions" '^Supersedes: .+$'
    require_pattern "$rfc_file" "decision section" '^## Decision$'
    require_pattern "$rfc_file" "context section" '^## Context$'
    require_pattern "$rfc_file" "consequences section" '^## Consequences$'
    require_pattern "$rfc_index" "accepted RFC ${rfc_name} index entry" "accepted/${rfc_name}\\.md"
  done < <(find rfcs/accepted -maxdepth 1 -type f -name '*.md' -print | sort)

  nested_accepted_rfcs="$(find rfcs/accepted -mindepth 2 -type f -name '*.md' -print | sort)"
  if [[ -n "$nested_accepted_rfcs" ]]; then
    fail "accepted RFC files must live directly below rfcs/accepted/"
    printf '%s\n' "$nested_accepted_rfcs" >&2
  fi
fi

if [[ -d "rfcs/proposed" ]]; then
  while IFS= read -r rfc_file; do
    require_pattern "$rfc_file" "proposed status" '^Status: Proposed$'
    require_pattern "$rfc_file" "decision date" '^Date: [0-9]{4}-[0-9]{2}-[0-9]{2}$'
    require_pattern "$rfc_file" "superseded decisions" '^Supersedes: .+$'
    require_pattern "$rfc_file" "decision section" '^## Decision$'
    require_pattern "$rfc_file" "context section" '^## Context$'
    require_pattern "$rfc_file" "consequences section" '^## Consequences$'
  done < <(find rfcs/proposed -type f -name '[0-9][0-9][0-9][0-9]-*.md' -print | sort)

  if rg -n '^Status: Accepted$' rfcs/proposed --glob '*.md' >/dev/null 2>&1; then
    fail "accepted RFC status is not allowed below rfcs/proposed/"
  fi
fi

require_file "docs/project/status.md"
require_file "docs/project/governance.md"
require_file ".codex/execution/blocker-contracts.md"
require_pattern "docs/project/status.md" "verification date" '^Updated: 2026-07-31$'
require_pattern "docs/project/status.md" "implementation snapshot" '^Implementation snapshot: `b0ff07799029c27728799b817488d5bead85ee72`$'
require_pattern ".codex/execution/blocker-contracts.md" "blocker contract template" '^## Promotion Contract Template'

removed_paths=(
  "docs/spec"
  "docs/feature-status.md"
  "docs/jazz-language-state.md"
  "docs/jazz-improvement-backlog.md"
)
for removed_path in "${removed_paths[@]}"; do
  if [[ -e "$removed_path" ]]; then
    fail "obsolete documentation path still exists: $removed_path"
  fi
done

if rg -n '__kernel_' docs --glob '*.md' >/dev/null 2>&1; then
  fail "public standard-library documentation exposes private kernel names"
fi

documentation_checkers=(
  "scripts/check-spec-authority.sh"
  "scripts/check-clarification-specs.sh"
  "scripts/check-execution-queue.sh"
  "scripts/test-check-execution-queue.sh"
)
for checker in "${documentation_checkers[@]}"; do
  if ! bash "$checker"; then
    fail "$checker reported a documentation contract violation"
  fi
done

if command -v prettier >/dev/null 2>&1 && [[ -n "${IN_NIX_SHELL:-}" ]]; then
  if ! prettier --check README.md docs rfcs .codex/execution .codex/plans >/dev/null 2>&1; then
    fail "prettier --check reported documentation formatting drift"
  fi
elif command -v prettier >/dev/null 2>&1; then
  printf 'WARN: prettier found outside nix shell; skipping format enforcement to avoid tool-version drift.\n' >&2
else
  printf 'WARN: prettier not found; skipping markdown formatting check.\n' >&2
fi

if [[ "$fail_count" -ne 0 ]]; then
  exit 1
fi

echo "Documentation checks passed."
