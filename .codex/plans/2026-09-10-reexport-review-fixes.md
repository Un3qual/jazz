---
id: JN-MODULE-REEXPORT-REVIEW-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - src/Jazz/Compiler/ModuleRuntime.hs
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
- [ ] Run focused suites, supported non-bootstrap suites, Haskell quality gates,
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
