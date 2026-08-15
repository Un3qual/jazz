# Task 3 report: CFG tail-position propagation

## Files changed

- `src/Jazz/Compiler/LoweredIR/Lower.hs`
- `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- `.codex/plans/2026-08-14-jazz-typed-core-tail-position-lowering.md`
- `.superpowers/sdd/2026-08-14-jazz-typed-core-tail-position-lowering/task-3-report.md`

## RED/GREEN evidence

All Cabal runs used the required serialized focused command:

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=direct --jobs=1
```

- Conditional RED: exit 1. The exact `conditional-function-parameter`
  expectation required direct branch returns, but lowering still emitted two
  jumps to the result join and returned its block parameter.
- Conditional GREEN: exit 0. Function-result branches terminated directly;
  synthetic-entry/value-position conditional joins remained covered and
  unchanged.
- Scalar-case fixture check: the first RED also exposed an incorrect
  hand-written expression depth in the new expected block IDs. The fixture
  expectation was corrected from `e3$0,0,0` to the produced deterministic
  `e2$0,0` path before the authoritative RED rerun.
- Scalar-case RED: exit 1. With exact block IDs fixed, selected bodies still
  jumped to a shared result join, and the recursive arm contained a
  `LoweredDirectCall` instruction rather than a tail terminator.
- Scalar-case GREEN: exit 0. Literal tests, guarded variable fallthrough,
  unguarded catch-all handling, direct returns, and the recursive direct tail
  call matched exactly. Existing value-position cases retained result joins.
- Final fresh focused verification after formatting: exit 0; all
  `TypedCoreExpressionDirectCall` tests passed.
- Final formatting check:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command ormolu --mode check src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  ```

  Exit 0.

## Commits

- `5720e73a` — `feat: propagate tail position through control flow`
- Task 3 plan progress and this report are committed together in the commit
  containing this report.

## Final-review follow-up

The final branch review identified a Minor exact-coverage gap: existing nested
conditional/case fixtures exercised synthetic module entry, while named
function-result fixtures covered only a top-level conditional or scalar case.

Two source-valid named-function fixtures now compare the complete produced
Lowered IR program, including deterministic block IDs, against hand-written
expectations:

- `nested-tail-if-alternatives`: an `if` in one selected tail-`if` branch and a
  scalar case in the other selected tail-`if` branch.
- `nested-tail-case-bodies`: an `if` in one selected tail scalar-case body and
  another scalar case in the other selected tail scalar-case body.

Every nested selected body terminates directly with `LoweredReturn`; neither
the outer nor nested function-result level contains a result join. The exact
expected programs validate, produced Typed Core validates, and the existing
entry/value-position join coverage remains green in the same focused suite.

The focused direct-call suite passed before the test changes and the new exact
fixtures passed on their first run, so no implementation change was needed.
Final serialized focused verification, Ormolu checking for both touched test
modules, and `git diff --check` all exited 0.

Follow-up commit: `15259559` — `test: cover nested tail control flow`.

## Concerns

None from Task 3 or its final-review follow-up. No source, public docs, queue,
or Task 5 evidence changed in the follow-up.
