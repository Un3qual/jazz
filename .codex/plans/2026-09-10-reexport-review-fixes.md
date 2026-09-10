---
id: JN-MODULE-REEXPORT-REVIEW-001
status: complete
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - src/Jazz/Compiler/ModuleRuntime.hs
  - src/Jazz/Compiler/Runtime.hs
  - src/Jazz/Compiler/Runtime/Outcome.hs
  - src/Jazz/Compiler/ModuleAnalysis.hs
  - src/Jazz/Compiler/ModuleExports.hs
  - src/Jazz/Compiler/ModuleResolver.hs
  - src/Jazz/Compiler/Parser/ModuleDeclaration.hs
  - test/Jazz/Compiler/Modules/Loader/ReexportTests.hs
verification:
  - cabal test loader-spec module-exports-spec module-resolution-spec module-pipeline-contract-spec --jobs=4 --test-show-details=failures
  - JAZZ_CABAL_JOBS=4 bash scripts/ci/haskell-quality.sh
  - bash scripts/check-execution-queue.sh
deliverable: "Fix the four verified re-export review findings while preserving RFC 0018 behavior."
last_verified: 2026-09-10
---

# Re-export review fixes

The maintainer authorized fixing each verified finding. RFC 0018 remains the
contract; bootstrap and new language features remain deferred. Work inline in
the existing isolated worktree and commit verified milestones.

## Implementation

- [x] Reproduce lost facade-owned runtime methods with distinguishable results.
      Publish from the completed environment and merge duplicate class imports
      by implementation identity. Cover disjoint facade impls and repeated paths.
- [x] Reproduce the private-class/module-prefix collision. Match complete class
      keys when rebasing and selecting method metadata.
- [x] Reproduce the same-spelled selector diagnostic error. Retain locations in
      parsed selectors and remove resolver token rescanning.
- [x] Remove the constructor-ownership conversion round trip. Keep constructor
      validation and selection on the shared inventory, preserving visibility
      and original-owner checks. Reuse existing constructor coverage.
- [x] Run focused suites, supported non-bootstrap suites, Haskell quality gates,
      executable examples and repository checks. Record results and close queue.

## Verification record

- Strengthened facade tests failed with `(42, 0, 42)` and `(0, 0, 0)` before
  runtime changes. They now pass, including an original class with no impls.
- A multi-facade graph exposed repeated deferred-binding scope IDs in the old
  per-module pure runner. Removing that duplicate runner gives the whole graph
  one evaluation session and keeps method bodies distinct.
- `loader-spec` and `module-pipeline-contract-spec` passed after runtime fixes.

- Runtime publication/session fixes committed as `47752eab`.
- The private `Lib` class regression failed with E2015 before exact key matching;
  `loader-spec` then passed. Fix committed as `68270f8a`.
- Same-spelled export diagnostics failed at column 24 instead of 37 before
  retaining selector spans. Parser, foundation, resolution, exports and loader
  suites passed after fixture metadata was updated. Fix committed as `77c9406c`.
- Constructor selection now builds an index from existing ownership links rather
  than comparing all types with all constructors and reattaching ownership for
  each selector. Existing export, resolution and loader suites passed unchanged
  in behavior, including hidden and wrong-origin constructors.

- Constructor ownership refactor committed as `05cb803e`.
- All 47 supported non-bootstrap suites passed. After removing the now-unused
  pure-scope and diagnostic-result adapters, runtime semantics, observation,
  loader and module pipeline suites passed again.
- All six executable examples passed with the rebuilt executable.
- The first quality pass reported the two adapters left unused by the runtime
  simplification. Both were removed in `55684943`; the fresh gate passed.
- The initial docs run found plan formatting drift and one checker-fixture
  timeout under parallel build load. Formatting was corrected and the quiet rerun passed all checks.

## Final verification

- `cabal test` passed all 47 supported non-bootstrap suites selected from
  `jazz.cabal` by excluding Bootstrap main modules.
- After the final dead-code cleanup, `runtime-semantics-spec`,
  `runtime-observation-spec`, `loader-spec`, and `module-pipeline-contract-spec`
  passed again.
- `JAZZ_CABAL_JOBS=4 bash scripts/ci/haskell-quality.sh` passed in the pinned
  quality shell: HLint reported no hints; fresh production and full Weeder
  passed; all test/benchmark targets, including opt-in scale suites, built;
  generated-invariant tests passed.
- All six executable examples and public-documentation examples passed.
  `cabal check` reported no errors or warnings.
- `bash scripts/check-docs.sh` passed, including checker regressions, links,
  documentation policy and execution-queue checks. The earlier one-second
  checker-fixture timeout did not recur on the quiet rerun.
- All changed Haskell files were formatted with pinned Ormolu. Final closeout
  checks cover queue state, Markdown formatting and `git diff --check`.
- Production Haskell is 55 lines shorter across the review fixes. Hosted
  bootstrap execution/parity remains deferred; only compilation was verified.
