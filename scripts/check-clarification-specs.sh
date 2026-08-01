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
  [[ -f "$file" ]] || fail "missing required public owner: $file"
}

require_pattern() {
  local file="$1"
  local label="$2"
  local pattern="$3"
  if ! rg -n -e "$pattern" "$file" >/dev/null 2>&1; then
    fail "$file missing required section: $label"
  fi
}

required_owners=(
  "docs/language/control-flow.md"
  "docs/language/operators.md"
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

require_pattern "docs/reference/runtime-values.md" "value families" '^## Value families and rendering$'
require_pattern "docs/reference/runtime-values.md" "equality" '^## Equality$'
require_pattern "docs/reference/runtime-values.md" "runtime failures" '^## Runtime failures$'

require_pattern "docs/reference/diagnostics.md" "diagnostic code ranges" '^## Diagnostic model and code ranges$'
require_pattern "docs/reference/diagnostics.md" "warning categories" '^## Warning categories and IDs$'
require_pattern "docs/reference/diagnostics.md" "severity behavior" '^## Output and severity$'

if [[ "$fail_count" -ne 0 ]]; then
  exit 1
fi

echo "Public clarification contract check passed."
