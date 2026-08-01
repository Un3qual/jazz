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

reject_pattern() {
  local label="$1"
  local pattern="$2"
  shift 2
  local rg_output
  if rg_output="$(rg -in -e "$pattern" "$@" 2>&1)"; then
    fail "$label"
  else
    local rg_status=$?
    if [[ "$rg_status" -ne 1 ]]; then
      fail "$label: rg failed: $rg_output"
    fi
  fi
}

require_file "README.md"
require_file "docs/feature-status.md"
require_file "docs/jazz-language-state.md"
require_file "docs/execution/blocker-contracts.md"
require_file "docs/execution/done-archive.md"

require_pattern "README.md" "implemented section heading" '^### Implemented Today \(verified\)'
require_pattern "README.md" "planned section heading" '^### Planned / Aspirational'
require_pattern "README.md" "canonical status link" 'docs/feature-status.md'

require_pattern "docs/feature-status.md" "last verified anchor" 'Last verified against commit'
require_pattern "docs/feature-status.md" "implemented rubric label" '`Implemented Today`'
require_pattern "docs/feature-status.md" "partial rubric label" '`Partially Implemented / Parse-Only`'
require_pattern "docs/feature-status.md" "planned rubric label" '`Planned / Aspirational`'
require_pattern "docs/feature-status.md" "maintenance checklist" '^## Maintenance Checklist'
require_pattern "docs/feature-status.md" "reviewer checklist item" 'Does README status match docs/feature-status.md\?'
require_pattern "docs/execution/blocker-contracts.md" "blocker contract template" '^## Promotion Contract Template'
require_pattern "docs/execution/done-archive.md" "done archive heading" '^# Execution Queue Done Archive'

require_pattern "docs/jazz-language-state.md" "top-level docs contract section" '^## Top-level Docs Contract'
require_pattern "docs/jazz-language-state.md" "feature status reference" 'docs/feature-status.md'
require_pattern "docs/jazz-language-state.md" "item `#5` status update" 'Status update for item `#5`'
require_pattern "docs/feature-status.md" "active compiler path reference" 'src/Jazz/'
require_pattern "README.md" "active compiler path reference" 'src/Jazz/'
archive_tag='archive/pre-root-canonicalization-2026-07-31'
former_package='jazz-''next'
former_reference='jazz-''hs'
former_rewrite='jazz''2'
deleted_tree_pattern="(${former_package}|${former_reference}|${former_rewrite})"
require_pattern "docs/jazz-language-state.md" "legacy evidence archive tag" "$archive_tag"
reject_pattern "active specs must not describe deleted implementation trees as read-only" \
  "(${deleted_tree_pattern}.{0,120}(remain|stay|are|is).{0,80}read[- ]only)|(read[- ]only.{0,120}${deleted_tree_pattern})" \
  docs/spec
generic_legacy_tree_pattern='(legacy|deleted|removed|former).{0,80}(directories|trees|paths)'
reject_pattern "active specs must not generically describe absent legacy trees as read-only" \
  "(${generic_legacy_tree_pattern}.{0,120}(remain|stay|are|is).{0,80}read[- ]only)|(read[- ]only.{0,120}${generic_legacy_tree_pattern})" \
  docs/spec
reject_pattern "active documentation must not link into deleted implementation trees" \
  "\\]\\([^)]*${deleted_tree_pattern}[^)]*\\)" \
  docs/spec docs/jazz-language-state.md
generated_artifact_pattern='generatedjs|generated js|js output|javascript output|javascript generation|codegen placeholder'
reject_pattern "active compiler sources must not reference JavaScript generation artifacts" "$generated_artifact_pattern" src jazz test
reject_pattern "active compile docs must not expose generated-JS artifact naming" "$generated_artifact_pattern" \
  docs/execution/queue.md \
  docs/spec/tooling/compiler-warning-flags.md
if [[ -f "scripts/check-execution-queue.sh" ]]; then
  if ! bash scripts/check-execution-queue.sh; then
    fail "scripts/check-execution-queue.sh reported queue/frontmatter drift"
  fi
else
  fail "missing required file: scripts/check-execution-queue.sh"
fi
if [[ -f "scripts/test-check-execution-queue.sh" ]]; then
  if ! bash scripts/test-check-execution-queue.sh; then
    fail "scripts/test-check-execution-queue.sh reported validator regression drift"
  fi
else
  fail "missing required file: scripts/test-check-execution-queue.sh"
fi
if command -v prettier >/dev/null 2>&1 && [[ -n "${IN_NIX_SHELL:-}" ]]; then
  markdown_files=(
    "README.md"
    "docs/feature-status.md"
    "docs/jazz-language-state.md"
  )
  if [[ "${#markdown_files[@]}" -gt 0 ]]; then
    if ! prettier --check "${markdown_files[@]}" >/dev/null 2>&1; then
      fail "prettier --check reported markdown formatting drift"
    fi
  fi
elif command -v prettier >/dev/null 2>&1; then
  printf 'WARN: prettier found outside nix shell; skipping format enforcement to avoid tool-version drift.\n' >&2
else
  printf 'WARN: prettier not found; skipping markdown formatting check.\n' >&2
fi

if [[ "$fail_count" -ne 0 ]]; then
  exit 1
fi

echo "Docs status checks passed."
