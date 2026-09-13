# Haskell compiler simplification: remaining stages

User-authorized second pass, on `codex/simplify-haskell-compiler` above
`e6bbf365`. Do not use Ponytail. Reduce code in `src/Jazz` through shared
operations and clearer functions while preserving functionality, diagnostics,
identity, source order, runtime observations and evaluation behavior.

This is the second-pass historical record. The subtotal below covers the first
two passes only; subsequent test repair removed four more Haskell lines, and the
[Haskell practices implementation](2026-09-12-haskell-practices-implementation.md)
removed another 21. Thus `dfd0bde6..705b09b5` removes 1,494 net lines across 43
`src/Jazz` files. See that implementation record for the latest verification;
the ten hosted failures recorded below were fixed during the intervening repair.

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
- [x] Independently review each change, run relevant existing correctness suites,
      then the eligible correctness matrix and pinned Haskell quality gate.
- [x] Record inspected-but-retained areas, exact reductions and verification;
      commit reviewed batches along the way.

Use GHC 9.14.1 / Cabal 3.16.1.0 from the pinned Nix shell. The first-pass
baseline has ten known hosted qualification failures in
`jazz-parser-types-declarations-modules-spec`; compare any affected output to
the second-pass base rather than treating a baseline mismatch as a regression.

## Implemented reductions

| Area                                             | Change                                                                                                                                                   | Net lines removed |
| ------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------: |
| Driver and CLI                                   | Use existing `ExceptT` for ordered early exits; share analyzed-program preparation, result construction, compile/run selection and required-file loading |               135 |
| Declaration parsing and lowering                 | Share nested-declaration errors, class-header failures, signature-variable folding, span qualification and lambda-body lowering                          |               100 |
| Module imports, names, exports and prelude       | Share dependency lookup, collision rules, qualified reference validation, namespace projections and numeric impl construction                            |               148 |
| Pattern checking, coverage and inference support | Share list checks, subpattern traversal, witness specialization, diagnostics and existing fresh-variable/type folds                                      |               173 |
| Runtime observations                             | Share conditional statistics updates and the ordered JSON/human field inventory                                                                          |                69 |
| **Total**                                        | **19 previously unchanged files; no dependency or test scaffolding added**                                                                               |           **625** |

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
- The full Haskell quality gate passed with one Cabal job: HLint, clean
  production/component builds, both Weeder policies and generated invariants.
  The final coverage rerun also passed. Earlier concurrent Cabal invocations
  encountered semaphore errors; serial execution completed those checks.
- Formatting passed for the committed `e6bbf365..HEAD` source diff. Cabal metadata
  checks, the executable build, all five CLI examples and public-documentation
  checks passed. Benchmarks and scale suites were built for Weeder but not
  executed.

Implementation is committed as `3719cd14` on the original
`codex/simplify-haskell-compiler` branch. This pass shares no changed source
file with the first pass. The first two passes alone (`dfd0bde6..3719cd14`)
remove 1,469 net lines across 36 `src/Jazz` files, excluding the subsequent
test repair and Haskell practices implementation described above.

## Inspected and retained

AST phase distinctions, semantic facts, inference state/builders/interfaces,
module identity/graph/runtime carriers, runtime scope planning, warning reference
accounting, host I/O boundaries, source-signature rendering and forcing helpers
retain meaningful semantic differences. No generic traversal framework or new
compiler representation was warranted. The lexer and parser entry-point loops
also retain their existing commitment/error and context-handling rules.
