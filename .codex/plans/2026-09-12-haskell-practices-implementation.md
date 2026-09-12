# Haskell practices implementation

Approved by the maintainer on 2026-09-12; continue on
`codex/simplify-haskell-compiler` from `2a08016c`.
Implements the six concrete recommendations in the
[research record](2026-09-12-haskell-compiler-practices-research.md).
No language behavior changes or deferred framework/toolchain migrations.

## Changes

- [x] Store a statement's optional binder and complete scheme together.
- [x] Share the literal datatype across surface/core ASTs and remove conversion.
- [x] Share argument inference with explicit silent/reporting failure policies.
- [x] Share host observation begin/action/end sequencing.
- [x] Factor signature type heads, preserving fallback and exact errors.
- [x] Fold containers directly where intermediate lists serve no purpose.
- [x] Use named initialization for long records and concise invariant notes.

Preserve binder identity, numeric provenance and hosted encodings; inference
allocation/rollback and diagnostics; host events, exceptions and exit ordering;
parser adjacency, consumption, fallback and error locations. Existing semantic
tests remain authoritative; replace representation-only assertions where the new
datatype makes them unnecessary.

## Verification

Serialize Cabal invocations with `--jobs=1` under the pinned Nix development shell.
Run focused module, runtime, inference, literal and signature suites, including a
temporary old/new signature-parser differential check. Then run the compiler test
matrix and clean Haskell quality gate, package/CLI/example and repository checks.
Record commands, results, commits and production line delta here before closeout.

## Results

Implementation complete; broader verification in progress.

- Pinned Ormolu and HLint passed.
- Twelve focused suites passed: module pipeline contracts, runtime semantics,
  runtime observations, profiling, ADT runtime, binding/signature coherence,
  parser foundation, token parser, module imports, declarations, canonical parser
  comparison and source ranges.
- A temporary differential harness compared the signature parser against
  `2a08016c` on 32,307 generated and targeted sources. ASTs, errors, parser states,
  offsets, consumption, success hints, alternative commitment and complete
  payload adapters were identical. The harness is verification tooling under
  `/tmp/jazz-signature-factor`, not permanent duplicate compiler code.
- Independent static reviews found no regressions in statement scheme/identity
  preservation, direct-fold ordering, inference allocation/rollback/laziness, or
  host observation/exit/exception sequencing.
- `JAZZ_MAIN_PHASE=repository bash scripts/ci/main-functional.sh` passed.

Surface literal `Show` now uses the existing core `L*` constructor names. No
string-based surface `Show` consumers were found; hosted canonical schema strings
are unchanged. Replaced representation-only list/map agreement assertions with
binding presence and resolved identity checks. No semantic test cases removed.
