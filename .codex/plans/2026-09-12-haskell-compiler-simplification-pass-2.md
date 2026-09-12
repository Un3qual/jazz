# Haskell compiler simplification: remaining stages

User-authorized second pass, on `codex/simplify-haskell-compiler` above
`e6bbf365`. Do not use Ponytail. Reduce code in `src/Jazz` through shared
operations and clearer functions while preserving functionality, diagnostics,
identity, source order, runtime observations and evaluation behavior.

The empty execution queue supplies no additional feature work. This explicit
request scopes the audit to files not investigated in the first pass. Keep
hosted feature work deferred; do not execute benchmarks or scale suites.

- [x] Share compiler/CLI setup, result handling and required-file loading in
  `Driver.hs` and `CLI/Main.hs`; retain early exits and profiling boundaries.
- [x] Inspect declaration parsing and lowering for duplicated traversal and
  signature operations; retain parser commitment, source spans and node order.
- [x] Simplify pattern checking, coverage witnesses and diagnostic construction
  in the remaining inference/checking files; retain rollback and error priority.
- [x] Share import validation, name-resolution operations and bundled-prelude
  construction where their semantic rules agree.
- [x] Inspect analyzed representations, runtime scope preparation and observation
  support; remove repeated operations without adding a generic pass framework.
- [ ] Independently review each change, run relevant existing correctness suites,
  then the eligible correctness matrix and pinned Haskell quality gate.
- [ ] Record inspected-but-retained areas, exact reductions and verification;
  commit reviewed batches along the way.

Use GHC 9.14.1 / Cabal 3.16.1.0 from the pinned Nix shell. The first-pass
baseline has ten known hosted qualification failures in
`jazz-parser-types-declarations-modules-spec`; compare any affected output to
the second-pass base rather than treating a baseline mismatch as a regression.

## Implemented reductions

| Area | Change | Net lines removed |
| --- | --- | ---: |
| Driver and CLI | Use existing `ExceptT` for ordered early exits; share analyzed-program preparation, result construction, compile/run selection and required-file loading | 135 |
| Declaration parsing and lowering | Share nested-declaration errors, class-header failures, signature-variable folding, span qualification and lambda-body lowering | 100 |
| Module imports, names, exports and prelude | Share dependency lookup, collision rules, qualified reference validation, namespace projections and numeric impl construction | 148 |
| Pattern checking, coverage and inference support | Share list checks, subpattern traversal, witness specialization, diagnostics and existing fresh-variable/type folds | 173 |
| Runtime observations | Share conditional statistics updates and the ordered JSON/human field inventory | 69 |
| **Total** | **19 previously unchanged files; no dependency or test scaffolding added** | **625** |

## Review and verification

- Independent reviews covered parser/lowering, module/import rules, inference
  rollback/allocation, coverage witnesses, driver/CLI order and observations.
  A reviewer caught a lazy-tuple change in runtime result construction; retaining
  the original outer outcome case preserves its evaluation behavior.
- All 20 focused suites passed with the pinned toolchain. Coverage includes
  CLI and profiling, exact observation accounting, declaration/module parsing,
  lowering/source ranges, pattern typing/coverage, numeric and signature
  semantics, structured diagnostics, module contracts, prelude and name identity.
- The other 38 eligible correctness suites also passed (58 unique suites total).
  HLint requested `isNothing` in one coverage check after its redundant alias was
  removed; that equivalent simplification is applied and HLint now passes.
- The hosted suite retained the same ten failures: all test messages match the
  verified original baseline, with only a trailing blank-line difference.
- The full Haskell quality gate remains in progress. A concurrent final Cabal
  rerun failed with `semWait: invalid argument (Bad file descriptor)` before
  running the coverage test; its stalled process was stopped. The isolated
  quality build subsequently reported the same semaphore failure. The quality
  gate and final checks are now running sequentially with one Cabal job.

## Inspected and retained

AST phase distinctions, semantic facts, inference state/builders/interfaces,
module identity/graph/runtime carriers, runtime scope planning, warning reference
accounting, host I/O boundaries, source-signature rendering and forcing helpers
retain meaningful semantic differences. No generic traversal framework or new
compiler representation was warranted. The lexer and parser entry-point loops
also retain their existing commitment/error and context-handling rules.
