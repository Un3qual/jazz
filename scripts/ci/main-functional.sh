#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
cd "$ROOT"

if [[ -n "${NIX_CONFIG:-}" ]]; then
  NIX_CONFIG+=$'\n'
fi
NIX_CONFIG+='extra-experimental-features = nix-command flakes'
export NIX_CONFIG

JAZZ_MAIN_PHASE="${JAZZ_MAIN_PHASE-all}"
JAZZ_CABAL_JOBS="${JAZZ_CABAL_JOBS-1}"
JAZZ_NIX_JOBS="${JAZZ_NIX_JOBS-1}"
JAZZ_NIX_CORES="${JAZZ_NIX_CORES-1}"

require_positive_integer() {
  local variable_name="$1"
  local value="$2"
  case "$value" in
    "" | 0 | *[!0-9]*)
      printf 'FAIL: %s must be a positive integer\n' "$variable_name" >&2
      exit 2
      ;;
  esac
}

case "$JAZZ_MAIN_PHASE" in
  all | compiler | repository | nix | low-memory) ;;
  *)
    printf 'FAIL: JAZZ_MAIN_PHASE must be one of all, compiler, repository, nix, or low-memory\n' >&2
    exit 2
    ;;
esac

require_positive_integer JAZZ_CABAL_JOBS "$JAZZ_CABAL_JOBS"
require_positive_integer JAZZ_NIX_JOBS "$JAZZ_NIX_JOBS"
require_positive_integer JAZZ_NIX_CORES "$JAZZ_NIX_CORES"
export JAZZ_CABAL_JOBS

run_compiler_phase() {
  cabal build all --jobs="$JAZZ_CABAL_JOBS"
  cabal test all --test-show-details=direct --jobs="$JAZZ_CABAL_JOBS"
  cabal check
  local jazz_bin
  jazz_bin="$(cabal list-bin jazz)"
  bash scripts/check-examples.sh --jazz-bin "$jazz_bin"
}

run_repository_preflight() {
  actionlint
}

run_repository_checks() {
  python3 scripts/test-check-ci-policy.py
  python3 scripts/release/test-verify-artifacts.py
  bash scripts/check-docs.sh
  bash scripts/check-execution-queue.sh
  python3 scripts/test-check-examples.py
  if [[ -n "${JAZZ_DIFF_BASE:-}" ]]; then
    git diff --check "$JAZZ_DIFF_BASE...HEAD"
  else
    git diff --check
  fi
}

run_nix_phase() {
  nix flake check \
    --max-jobs "$JAZZ_NIX_JOBS" \
    --cores "$JAZZ_NIX_CORES"
}

case "$JAZZ_MAIN_PHASE" in
  all)
    run_repository_preflight
    run_compiler_phase
    run_repository_checks
    run_nix_phase
    ;;
  compiler)
    run_compiler_phase
    ;;
  repository)
    run_repository_preflight
    run_repository_checks
    printf 'NOTE: repository verification omits executable Jazz example checks; use the compiler, low-memory, or all phase for those checks.\n' >&2
    ;;
  nix)
    run_nix_phase
    ;;
  low-memory)
    run_repository_preflight
    run_compiler_phase
    run_repository_checks
    printf 'NOTE: low-memory verification omits the Nix flake check; use JAZZ_MAIN_PHASE=all for authoritative main/release coverage.\n' >&2
    ;;
esac
