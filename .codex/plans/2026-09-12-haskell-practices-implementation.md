# Haskell practices implementation

Approved by the maintainer on 2026-09-12; continue on
`codex/simplify-haskell-compiler` from `2a08016c`.
Implements the six concrete recommendations and additional readability conventions
in the [research record](2026-09-12-haskell-compiler-practices-research.md).
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
The maintainer subsequently requested skipping the four full-scale parser tests
for this run. Their components may still compile in the quality gate, but the
remaining verification excludes their workloads. The regular scale suite remains
included.
Record commands, results, commits and production line delta here before closeout.

## Results

Implementation committed in `edf3dce6`; verification completed on 2026-09-12.
Relative to `2a08016c`, production Haskell under `src/Jazz` has 150 added and
171 deleted lines: **21 fewer lines**, including the new literal module and
readability notes/record initializers. No performance improvement is claimed.

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
- The initial all-suite run passed 28 regular suites before being stopped to
  honor the full-scale skip. The first full-scale workload had just started and
  was interrupted without a result. The remaining 34 regular suites passed as
  explicit targets. Comparing passing suite names with `jazz.cabal` confirms all
  62 regular suites passed; no full-scale result is claimed for this commit.
- The clean `JAZZ_CABAL_JOBS=1 bash scripts/ci/haskell-quality.sh` gate passed:
  HLint, production Weeder, fresh compilation of all components, full Weeder and
  generated invariants. Full-scale components compiled but did not execute in
  this gate.
- All five executable examples and public documentation checks passed against
  the freshly built CLI.
- `cabal check`, final changed-file Ormolu checks, execution-queue validation and
  `git diff --check` passed.

Surface literal `Show` now uses the existing core `L*` constructor names. No
string-based surface `Show` consumers were found; hosted canonical schema strings
are unchanged. Replaced representation-only list/map agreement assertions with
binding presence and resolved identity checks. No semantic test cases removed.
