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

require_file "README.md"
require_file "docs/feature-status.md"
require_file "docs/jazz-language-state.md"

require_pattern "README.md" "implemented section heading" '^### Implemented Today \(verified\)'
require_pattern "README.md" "planned section heading" '^### Planned / Aspirational'
require_pattern "README.md" "canonical status link" 'docs/feature-status.md'

require_pattern "docs/feature-status.md" "last verified anchor" 'Last verified against commit'
require_pattern "docs/feature-status.md" "implemented rubric label" '`Implemented Today`'
require_pattern "docs/feature-status.md" "partial rubric label" '`Partially Implemented / Parse-Only`'
require_pattern "docs/feature-status.md" "planned rubric label" '`Planned / Aspirational`'
require_pattern "docs/feature-status.md" "maintenance checklist" '^## Maintenance Checklist'
require_pattern "docs/feature-status.md" "reviewer checklist item" 'Does README status match docs/feature-status.md\?'

require_pattern "docs/jazz-language-state.md" "top-level docs contract section" '^## Top-level Docs Contract'
require_pattern "docs/jazz-language-state.md" "feature status reference" 'docs/feature-status.md'
require_pattern "docs/jazz-language-state.md" "item #5 status update" 'Status update for item #5'

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
