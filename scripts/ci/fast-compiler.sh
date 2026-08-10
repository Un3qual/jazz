#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
cd "$ROOT"

test_components=(
  cli-spec
  runtime-observation-spec
  warning-config-spec
  structured-error-diagnostics-spec
  diagnostic-catalog-spec
  signature-rendering-spec
  loader-spec
  module-resolution-spec
  module-exports-spec
  module-pipeline-contract-spec
  prelude-loading-spec
  stdlib-spec
  canonical-lexer-comparison-spec
  canonical-parser-comparison-spec
  canonical-core-comparison-spec
  jazz-lowered-ir-contract-spec
  jazz-typed-core-contract-spec
  jazz-typed-core-expression-direct-call-spec
  parser-core-spec
  jazz-parser-parity-spec
  jazz-parser-scale-spec
  jazz-lexer-parity-spec
  parser-foundation-spec
  binding-signature-coherence-spec
  purity-semantics-spec
  runtime-semantics-spec
  repository-audit-spec
)

actionlint
cabal build all
cabal test "${test_components[@]}" --test-show-details=direct
cabal check
python3 scripts/release/test-verify-artifacts.py
bash scripts/check-examples.sh
if [[ -n "${JAZZ_DIFF_BASE:-}" ]]; then
  git diff --check "$JAZZ_DIFF_BASE...HEAD"
else
  git diff --check
fi
