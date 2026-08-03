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
  "docs/spec/governance/spec-authority-policy.md"
)

for file in "${required_files[@]}"; do
  [[ -f "$file" ]] || fail "missing required file: $file"
done

policy_file="docs/spec/governance/spec-authority-policy.md"
require_pattern "$policy_file" "transitional contract path" 'docs/spec/'
require_pattern "$policy_file" "transitional public contract" 'transitional public contract'
require_pattern "$policy_file" "future public language and reference authority" 'docs/language/.{0,160}docs/reference/'
require_pattern "$policy_file" "current implementation evidence" 'src/.{0,160}jazz/.{0,160}test/.{0,160}(behavior|evidence)'
require_pattern "$policy_file" "accepted RFC authority" 'accepted rfcs?.{0,160}(authoritative|authority|durable decisions?)'
require_pattern "$policy_file" "non-normative roadmap" 'roadmap.{0,160}non[- ]normative'
require_pattern "$policy_file" "semantic change control" 'semantic.{0,160}changes?.{0,120}(must|require).{0,120}(rfc|decision record).{0,120}before implementation'

for summary_file in "README.md" "docs/jazz-language-state.md"; do
  require_pattern "$summary_file" "governance policy link" 'docs/spec/governance/spec-authority-policy.md'
  require_pattern "$summary_file" "semantic change control" 'semantic.{0,160}changes?.{0,120}(must|require).{0,120}(rfc|decision record).{0,120}before implementation'
done

# Construct removed identities at runtime so this active audit script does not
# itself present superseded product paths as live repository text.
former_package='jazz-''next'
former_reference='jazz-''hs'
former_rewrite='jazz''2'
obsolete_identity_pattern="(${former_package}|${former_reference}|${former_rewrite})"
authority_claim_pattern="(${obsolete_identity_pattern}.{0,160}(active (compiler|implementation|authority|path)|authoritative|normative|source of truth|implementation target))|((active (compiler|implementation|authority|path)|authoritative|normative|source of truth|implementation target).{0,160}${obsolete_identity_pattern})"

authority_candidates="$({
  rg -n -i \
    --glob '*.md' \
    --glob '!docs/plans/**' \
    --glob '!docs/superpowers/**' \
    --glob '!docs/execution/done-archive.md' \
    -e "$authority_claim_pattern" \
    README.md docs || true
})"

if [[ -n "$authority_candidates" ]]; then
  unsupported_claims="$({
    printf '%s\n' "$authority_candidates" | rg -v -i \
      '(removed|legacy|historical|pre-migration|not[^[:alpha:]]{0,6}(active|authoritative|normative|source of truth|implementation target)|non[- ]normative)' || true
  })"
  if [[ -n "$unsupported_claims" ]]; then
    fail "removed implementation identity claimed as live authority"
    printf '%s\n' "$unsupported_claims" >&2
  fi
fi

if [[ "$fail_count" -ne 0 ]]; then
  exit 1
fi

echo "Spec authority policy check passed."
