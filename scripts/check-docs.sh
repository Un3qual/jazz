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

if ! bash scripts/check-public-docs.sh "$@"; then
  fail "scripts/check-public-docs.sh reported public documentation boundary violations"
fi
if ! python3 scripts/test-check-public-docs.py; then
  fail "scripts/test-check-public-docs.py reported public documentation checker regressions"
fi
if ! python3 scripts/test-markdown-visibility.py; then
  fail "scripts/test-markdown-visibility.py reported rendered Markdown visibility regressions"
fi
if ! python3 scripts/test-check-examples.py; then
  fail "scripts/test-check-examples.py reported executable-example checker regressions"
fi
if ! python3 scripts/check-rfcs.py "$ROOT"; then
  fail "scripts/check-rfcs.py reported RFC structure violations"
fi
if ! python3 scripts/test-check-rfcs.py; then
  fail "scripts/test-check-rfcs.py reported RFC checker regressions"
fi
if ! python3 scripts/test-check-spec-authority.py; then
  fail "scripts/test-check-spec-authority.py reported authority checker regressions"
fi
if ! lychee --offline --no-progress --include-fragments=anchor-only README.md; then
  fail "README.md contains a broken local link or image target"
fi

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

require_file "rfcs/README.md"
require_file "rfcs/proposed/README.md"
for rfc_name in "${required_rfcs[@]}"; do
  require_file "rfcs/accepted/${rfc_name}.md"
done

require_file "docs/project/status.md"
require_file "docs/project/governance.md"
require_file ".codex/execution/blocker-contracts.md"
require_pattern "docs/project/status.md" "verification date" '^Updated: 2026-08-12$'
require_pattern "docs/project/status.md" "implementation snapshot" '^Implementation snapshot: `33f3d7c1a7a98d46dd95efcc86cd90ebe9d54dea`$'
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

documentation_checkers=(
  "scripts/check-spec-authority.sh"
  "scripts/check-clarification-specs.sh"
  "scripts/test-check-clarification-specs.sh"
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
