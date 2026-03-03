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
  if ! rg -n -i -e "$pattern" "$file" >/dev/null 2>&1; then
    fail "$file missing required section: $label"
  fi
}

required_specs=(
  "docs/spec/control-flow/if-expressions.md"
  "docs/spec/syntax/operators.md"
  "docs/spec/runtime/primitive-semantics.md"
  "docs/spec/tooling/compiler-warning-flags.md"
)

for f in "${required_specs[@]}"; do
  require_file "$f"
done

require_pattern "docs/spec/control-flow/if-expressions.md" "canonical surface form" '^## Canonical Surface Form'
require_pattern "docs/spec/control-flow/if-expressions.md" "canonical desugaring" '^## Canonical Desugaring'
require_pattern "docs/spec/control-flow/if-expressions.md" "typing rules" '^## Typing Rules'

require_pattern "docs/spec/syntax/operators.md" "built-in operator table" '^## Built-in Operator Table'
require_pattern "docs/spec/syntax/operators.md" "section AST contract" '^## Section AST Contract'
require_pattern "docs/spec/syntax/operators.md" "staged extensibility model" '^## Staged Extensibility Model'

require_pattern "docs/spec/runtime/primitive-semantics.md" "primitive contract table" '^## Primitive Contract Table'
require_pattern "docs/spec/runtime/primitive-semantics.md" "equality contract" '^## Equality Contract'
require_pattern "docs/spec/runtime/primitive-semantics.md" "runtime failure model" '^## Runtime Failure Model'

require_pattern "docs/spec/tooling/compiler-warning-flags.md" "warning categories" '^## Warning Categories and IDs'
require_pattern "docs/spec/tooling/compiler-warning-flags.md" "precedence rules" '^## Precedence Rules'
require_pattern "docs/spec/tooling/compiler-warning-flags.md" "migration notes" '^## Migration Notes'

plans=(
  "docs/plans/spec-clarification/2026-03-03/control-flow/14-if-expression-surface-and-semantics.md"
  "docs/plans/spec-clarification/2026-03-03/syntax/15-operator-fixity-and-sections.md"
  "docs/plans/spec-clarification/2026-03-03/runtime/16-primitive-semantics-contract.md"
  "docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md"
)

for plan in "${plans[@]}"; do
  require_file "$plan"
done

require_pattern "docs/plans/spec-clarification/2026-03-03/control-flow/14-if-expression-surface-and-semantics.md" "if spec link" 'docs/spec/control-flow/if-expressions.md'
require_pattern "docs/plans/spec-clarification/2026-03-03/syntax/15-operator-fixity-and-sections.md" "operators spec link" 'docs/spec/syntax/operators.md'
require_pattern "docs/plans/spec-clarification/2026-03-03/runtime/16-primitive-semantics-contract.md" "primitive spec link" 'docs/spec/runtime/primitive-semantics.md'
require_pattern "docs/plans/spec-clarification/2026-03-03/tooling/18-compiler-warning-flags.md" "warning flags spec link" 'docs/spec/tooling/compiler-warning-flags.md'

if [[ "$fail_count" -ne 0 ]]; then
  exit 1
fi

echo "Spec clarification contract check passed."
