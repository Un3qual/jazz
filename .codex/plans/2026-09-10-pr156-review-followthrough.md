---
id: JN-PR156-REVIEW-001
status: complete
priority: P2
size: S
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Review fixes"
target_paths:
  - src/Jazz/Compiler/ModuleResolver/Imports.hs
  - src/Jazz/Compiler/ModuleResolver/Names.hs
  - src/Jazz/Compiler/ModuleResolver.hs
  - src/Jazz/Compiler/ModuleRuntime.hs
  - test/Jazz/Compiler/Modules/Loader/ReexportTests.hs
verification:
  - cabal test loader-spec module-resolution-spec module-exports-spec module-pipeline-contract-spec --jobs=4 --test-show-details=failures
  - JAZZ_CABAL_JOBS=4 bash scripts/ci/haskell-quality.sh
  - bash scripts/check-docs.sh
deliverable: "Address verified PR 156 feedback within RFC 0018, resolve the captured threads and push once."
last_verified: 2026-09-10
---

# PR 156 review follow-through

The maintainer requested one pass over current feedback, including outside-diff
comments, followed by fixes, resolution and a push. Do not wait for another
review or refresh comments after pushing. Bootstrap work remains deferred.

## Review fixes

- [x] Reject partially overlapping value/constructor imports in either order.
      Reuse the existing collision regression and add the two missing shapes.
- [x] List only owned declarations in invalid bare-export diagnostics.
- [x] Share repeated namespace lookups and runtime export selections; add the
      two requested local signatures without introducing new abstractions.
- [x] Correct the RFC table, expand implementation paths and distinguish public
      class publication from retained private evidence in the blocker contract.
- [x] Preserve hidden datatype metadata. Filtering it to public type names would
      break value and constructor imports; resolver inventories enforce visibility.
- [x] Run focused regressions, the Haskell quality gate and docs checks, commit
      fixes and address all captured threads and top-level findings.

## Feedback snapshot

At `7c3e9a0d`, PR 156 had 12 unresolved threads and two top-level CodeAnt nitpicks.
The three collision threads, two namespace-lookup threads, two runtime-selection
threads and repeated table nitpick are duplicates. Greptile's subscription
message, reviewer summaries and passing analyzer status messages need no code.
The remaining feedback is covered above; it adds no new language features.

## Verification record

- Both behavior regressions failed before the fix: partial expression overlap
  produced no E4008, and the bare-selector error listed the rejected import.
- The expanded collision test now covers three overlap shapes in both import
  orders. It and the bare-selector diagnostic check pass.
- `loader-spec`, `module-resolution-spec`, `module-exports-spec`,
  `module-pipeline-contract-spec` and `runtime-observation-spec` passed. The
  final loader rerun and pinned Ormolu check passed after test cleanup.
- Applying the proposed public-type filter in a temporary source override made
  the existing imported-class-method test fail with E2015 for
  `Lib::Api::Make::make` returning the unselected `Lib::Api::Box` type.
  The unchanged metadata transport passes; a separate private-name probe rejects
  alias access with E4014. No production datatype filtering was added.
- Pinned Prettier, `bash scripts/check-docs.sh` and `git diff --check` passed.
- Code and documentation fixes are committed as `376b5cab`.
- The full Haskell quality gate passed: HLint reported no hints, fresh
  production and all-target builds passed both Weeder checks, and generated
  invariants passed. Hosted/bootstrap execution remains deferred.
- Replied to and resolved all 12 captured threads; GitHub mutation responses
  confirmed each resolution. The two top-level CodeAnt nitpicks were answered
  together. The invalid datatype-filter suggestion was resolved with the
  reproducing test evidence, without changing production behavior.
- Final publication is one branch push after this closeout commit. The
  maintainer explicitly requested no post-push comment refresh or wait.
