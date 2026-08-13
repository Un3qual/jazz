#!/usr/bin/env bash
# shellcheck disable=SC2016 # Contract regexes must remain literal.
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RENDER_CACHE_ROOT="$(mktemp -d)"
trap 'rm -rf "$RENDER_CACHE_ROOT"' EXIT
cd "$ROOT"

fail_count=0

fail() {
  printf 'FAIL: %s\n' "$1" >&2
  fail_count=$((fail_count + 1))
}

cached_rendered_markdown() {
  local mode="$1"
  local file="$2"
  local cache_file="$RENDER_CACHE_ROOT/$mode/$file"
  local pending_file="$cache_file.pending"

  if [[ ! -f "$cache_file" ]]; then
    mkdir -p "$(dirname "$cache_file")"
    if ! python3 "$SCRIPT_DIR/markdown_visibility.py" "--preserve-$mode" "$file" >"$pending_file"; then
      return 1
    fi
    mv "$pending_file" "$cache_file"
  fi

  cat "$cache_file"
}

rendered_markdown() {
  cached_rendered_markdown "inline-code" "$1"
}

rendered_markdown_with_code() {
  cached_rendered_markdown "code" "$1"
}

require_file() {
  local file="$1"
  [[ -f "$file" ]] || fail "missing required public owner: $file"
}

require_pattern() {
  local file="$1"
  local label="$2"
  local pattern="$3"
  if ! rendered_markdown "$file" | rg -n -e "$pattern" >/dev/null 2>&1; then
    fail "$file missing required section: $label"
  fi
}

require_contract_pattern() {
  local file="$1"
  local label="$2"
  local pattern="$3"
  if ! rendered_markdown "$file" | tr '\n' ' ' | rg -n -e "$pattern" >/dev/null 2>&1; then
    fail "$file missing required contract: $label"
  fi
}

require_rendered_code_pattern() {
  local file="$1"
  local label="$2"
  local pattern="$3"
  if ! rendered_markdown_with_code "$file" | tr '\n' ' ' | rg -n -e "$pattern" >/dev/null 2>&1; then
    fail "$file missing required contract: $label"
  fi
}

required_owners=(
  "docs/language/algebraic-data-types-and-patterns.md"
  "docs/language/control-flow.md"
  "docs/language/operators.md"
  "docs/standard-library/prelude.md"
  "docs/standard-library/queue.md"
  "docs/reference/expression-grammar.md"
  "docs/reference/runtime-values.md"
  "docs/reference/diagnostics.md"
)

for file in "${required_owners[@]}"; do
  require_file "$file"
done

require_pattern "docs/language/control-flow.md" "conditionals" '^## Conditionals$'
require_pattern "docs/language/control-flow.md" "cases and guards" '^## Cases and guards$'
require_pattern "docs/language/control-flow.md" "static checks" '^## Static checks$'

require_pattern "docs/language/operators.md" "built-in precedence" '^## Built-in precedence$'
require_pattern "docs/language/operators.md" "operator values and sections" '^## Operator values and sections$'
require_pattern "docs/language/operators.md" "source-local declarations" '^## Source-local declarations$'
require_contract_pattern "docs/language/operators.md" "pipe is not Boolean OR and is rejected with E2003" '`\|` participates in pattern alternatives and list patterns; it is not Boolean[[:space:]]+OR\.[[:space:]]+`True \| False` is rejected with `E2003`\.'
require_contract_pattern "docs/language/operators.md" "executable built-ins are callable values" 'An executable built-in can be used as a callable value\.'
require_contract_pattern "docs/language/operators.md" "supported built-in section families" 'Sections are available[[:space:]]+for arithmetic `\+`, `-`, `\*`, `/`; ordering `<`, `<=`, `>`, `>=`; and equality[[:space:]]+`==`, `!=`\.'
require_contract_pattern "docs/language/operators.md" "dollar is callable but not sectionable" '`\$` is[[:space:]]+callable but not sectionable\.'

require_contract_pattern "docs/standard-library/queue.md" "queuePeek worst-case normalization" '`queuePeek` is `O\(1\)` when the front is populated and `O\(n\)` when it must[[:space:]]+reverse a non-empty rear to find the oldest value\.'
require_contract_pattern "docs/standard-library/queue.md" "repeated queuePeek cost" 'Because `queuePeek` does not return the normalized[[:space:]]+queue, repeated peeks of the same front-empty value repeat that `O\(n\)` work\.'
require_contract_pattern "docs/standard-library/queue.md" "queueDequeue amortization boundary" '`queueDequeue` is amortized `O\(1\)` only across a dequeue sequence that keeps using each returned queue\.'

require_rendered_code_pattern "docs/standard-library/prelude.md" "map signature" 'map :: \(a -> b\) -> \[a\] -> \[b\]\.'
require_rendered_code_pattern "docs/standard-library/prelude.md" "filter signature" 'filter :: \(a -> Bool\) -> \[a\] -> \[a\]\.'
require_rendered_code_pattern "docs/standard-library/prelude.md" "hd signature" 'hd :: \[a\] -> a\.'
require_rendered_code_pattern "docs/standard-library/prelude.md" "tl signature" 'tl :: \[a\] -> \[a\]\.'
require_contract_pattern "docs/standard-library/prelude.md" "map preserves order" '[Aa]pplies a function to every item and[[:space:]]+preserves order'
require_contract_pattern "docs/standard-library/prelude.md" "filter preserves matching items in order" '[Kk]eeps the items whose predicate is[[:space:]]+`True`, preserving order'
require_contract_pattern "docs/standard-library/prelude.md" "partial hd and tl" '`hd` and `tl` are partial:[[:space:]]+an empty list fails fatally with `E3009` or `E3010`, respectively\.'
require_contract_pattern "docs/standard-library/prelude.md" "stub-v1 print behavior" 'In stub-v1, evaluates and returns its argument without emitting output\.'

require_contract_pattern "docs/language/algebraic-data-types-and-patterns.md" "top-level pattern alternatives" 'Pattern alternatives are available at the top level of a case arm or pattern[[:space:]]+lambda parameter\.'
require_contract_pattern "docs/language/algebraic-data-types-and-patterns.md" "nested alternatives and pattern-lambda guards unsupported" 'Nested alternatives and guards on pattern-lambda parameters are not[[:space:]]+currently supported'
require_rendered_code_pattern "docs/reference/expression-grammar.md" "case-arm alternative grammar" 'case-arm-pattern[[:space:]]+:= pattern \("\|" pattern\)\*'
require_contract_pattern "docs/reference/expression-grammar.md" "lambda guards unsupported" 'Lambda parameters do not accept guards\.'

require_pattern "docs/reference/runtime-values.md" "value families" '^## Value families and rendering$'
require_pattern "docs/reference/runtime-values.md" "equality" '^## Equality$'
require_pattern "docs/reference/runtime-values.md" "runtime failures" '^## Runtime failures$'

require_pattern "docs/reference/diagnostics.md" "diagnostic code ranges" '^## Diagnostic model and code ranges$'
require_pattern "docs/reference/diagnostics.md" "warning categories" '^## Warning categories and IDs$'
require_pattern "docs/reference/diagnostics.md" "severity behavior" '^## Output and severity$'

execution_files=(
  ".codex/execution/README.md"
  ".codex/execution/blocker-contracts.md"
  ".codex/execution/queue.md"
  ".codex/execution/prompts/autonomous-next-batch.md"
  ".codex/execution/prompts/curated-next-batch.md"
)

for file in "${execution_files[@]}"; do
  [[ -f "$file" ]] || fail "missing required execution owner: $file"
  require_pattern "$file" "current project status owner" 'docs/project/status\.md'
  require_pattern "$file" "current public language guide owner" 'docs/language/'
  require_pattern "$file" "current public reference owner" 'docs/reference/'
done

require_pattern ".codex/execution/blocker-contracts.md" "hosted compiler RFC" 'rfcs/accepted/0004-hosted-canonical-compiler\.md'
require_pattern ".codex/execution/blocker-contracts.md" "typed-core RFC" 'rfcs/accepted/0005-typed-core-elaboration\.md'
require_pattern ".codex/execution/blocker-contracts.md" "lowered IR RFC" 'rfcs/accepted/0006-lowered-ir-contract\.md'

if rg -n -e 'docs/(execution/|feature-status\.md|spec/|jazz-language-state\.md|jazz-improvement-backlog\.md|superpowers/|plans/)' .codex/execution --glob '*.md' >/dev/null 2>&1; then
  fail ".codex/execution contains a live reference to a deleted documentation owner"
fi

obsolete_product_identity='jazz'"-next"
deleted_design_filenames=(
  "2026-07-21-${obsolete_product_identity}-hosted-canonical-core-design.md"
  "2026-07-21-${obsolete_product_identity}-backend-neutral-lowered-ir-design.md"
  "2026-07-22-${obsolete_product_identity}-typed-core-elaboration-design.md"
  "2026-07-30-${obsolete_product_identity}-typed-core-expression-direct-call-design.md"
)
for deleted_design_filename in "${deleted_design_filenames[@]}"; do
  if rg -n -F "$deleted_design_filename" .codex/execution --glob '*.md' >/dev/null 2>&1; then
    fail ".codex/execution contains a live reference to a deleted design filename"
    break
  fi
done

if rg -n -e 'historical archive docs|done-archive\.md|plan, status, and spec updates' .codex/execution --glob '*.md' >/dev/null 2>&1; then
  fail ".codex/execution contains obsolete documentation-preservation guidance"
fi

if [[ "$fail_count" -ne 0 ]]; then
  exit 1
fi

echo "Public clarification contract check passed."
