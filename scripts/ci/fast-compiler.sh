#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
cd "$ROOT"

JAZZ_CABAL_JOBS="${JAZZ_CABAL_JOBS-1}"
case "$JAZZ_CABAL_JOBS" in
  "" | 0 | *[!0-9]*)
    printf 'FAIL: JAZZ_CABAL_JOBS must be a positive integer\n' >&2
    exit 2
    ;;
esac
export JAZZ_CABAL_JOBS

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
cabal build all --jobs="$JAZZ_CABAL_JOBS"
cabal test "${test_components[@]}" \
  --test-show-details=direct \
  --jobs="$JAZZ_CABAL_JOBS"
cabal check
python3 scripts/release/test-verify-artifacts.py
jazz_bin="$(cabal list-bin jazz)"
bash scripts/check-examples.sh --jazz-bin "$jazz_bin"
if [[ -n "${JAZZ_DIFF_BASE:-}" ]]; then
  git diff --check "$JAZZ_DIFF_BASE...HEAD"
else
  git diff --check
fi
