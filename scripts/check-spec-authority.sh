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
    fail "$file missing authority statement: $label"
  fi
}

governance_file="docs/project/governance.md"
authority_rfc="rfcs/accepted/0001-language-authority-and-change-control.md"

for file in "$governance_file" "$authority_rfc"; do
  [[ -f "$file" ]] || fail "missing required authority file: $file"
done

require_pattern "$governance_file" "public documentation authority" 'curated public language and reference documentation'
require_pattern "$governance_file" "implementation and tests evidence" 'current compiler, standard-library, and test behavior'
require_pattern "$governance_file" "accepted durable decisions" 'accepted durable decision records'
require_pattern "$governance_file" "non-normative roadmap" 'roadmap material.{0,80}non-normative'
require_pattern "$governance_file" "semantic change control" 'semantic language changes require a reviewed decision record before'

require_pattern "$authority_rfc" "public documentation authority" 'public documentation'
require_pattern "$authority_rfc" "implementation and tests evidence" 'implementation and tests'
require_pattern "$authority_rfc" "accepted RFC authority" 'accepted durable decisions'
require_pattern "$authority_rfc" "non-normative roadmap" 'roadmap.{0,100}non-normative'
require_pattern "$authority_rfc" "semantic change control" 'semantic language change.{0,160}(RFC|decision record).{0,160}before implementation'

for removed_path in docs/spec docs/feature-status.md docs/jazz-language-state.md; do
  if [[ -e "$removed_path" ]]; then
    fail "superseded authority path still exists: $removed_path"
  fi
done

if [[ "$fail_count" -ne 0 ]]; then
  exit 1
fi

echo "Documentation authority policy check passed."
