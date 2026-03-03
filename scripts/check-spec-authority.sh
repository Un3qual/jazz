#!/usr/bin/env bash
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
cd "$ROOT"

fail_count=0

fail() {
  printf 'FAIL: %s\n' "$1" >&2
  fail_count=$((fail_count + 1))
}

require_pattern() {
  local file="$1"
  local label="$2"
  local pattern="$3"
  if ! rg -n -i -e "$pattern" "$file" >/dev/null 2>&1; then
    fail "$file missing policy statement: $label"
  fi
}

required_files=(
  "README.md"
  "docs/jazz-language-state.md"
)

for f in "${required_files[@]}"; do
  [[ -f "$f" ]] || fail "missing required file: $f"
done

# Required policy statements in both top-level summary docs.
RE_JAZZ2_NON_NORMATIVE='jazz2.{0,200}(reference[- ]only|non[- ]normative|not (normative|authoritative|source of truth|implementation target))'
RE_AUTHORITY_HIERARCHY='(docs/spec/\*|docs/spec/.{0,160}(canonical|normative|source of truth|authoritative))|(jazz-hs.{0,160}(behavior|tests|temporary authority|source of truth|authoritative|normative))'
RE_CHANGE_CONTROL='(semantic.{0,160}changes?.{0,120}(must|require).{0,120}(rfc|decision record).{0,120}(before implementation))|((non[- ]semantic|internal).{0,160}changes?.{0,120}(implementation-first|may be implementation-first).{0,160}(docs|tests).{0,120}(same change|same commit|same pr))'

for f in "${required_files[@]}"; do
  require_pattern "$f" "jazz2 is reference-only and non-normative" "$RE_JAZZ2_NON_NORMATIVE"
  require_pattern "$f" "authority anchored to docs/spec plus jazz-hs behavior/tests" "$RE_AUTHORITY_HIERARCHY"
  require_pattern "$f" "hybrid semantic-change workflow is documented" "$RE_CHANGE_CONTROL"
done

# Reject unsupported normative claims tied to jazz2 paths in summary docs.
CANDIDATES="$(
  rg -n -i \
    --glob '*.md' \
    --glob '!docs/plans/**' \
    '(jazz2/[[:alnum:]_./-]+.{0,120}\b(is|are|serves as|acts as|defines|specifies|governs)\b.{0,120}\b(authoritative|normative|canonical|source of truth|official)\b)|(\b(authoritative|normative|canonical|source of truth|official)\b.{0,120}jazz2/[[:alnum:]_./-]+)' \
    README.md docs || true
)"

if [[ -n "$CANDIDATES" ]]; then
  UNSUPPORTED="$(
    printf '%s\n' "$CANDIDATES" | rg -v -i \
      '(not[^[:alpha:]]{0,6}(authoritative|normative|canonical|source of truth|official)|non[- ]normative|reference[- ]only|design source|redesign stub|unfinished|placeholder|sketch)' || true
  )"
  if [[ -n "$UNSUPPORTED" ]]; then
    fail "unsupported normative claims about jazz2 paths detected"
    printf '%s\n' "$UNSUPPORTED" >&2
  fi
fi

if [[ "$fail_count" -ne 0 ]]; then
  exit 1
fi

echo "Spec authority policy check passed."
