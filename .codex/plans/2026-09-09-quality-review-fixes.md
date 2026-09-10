---
id: JN-QUALITY-REVIEW-FIXES-001
status: complete
priority: P2
size: S
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - weeder-production.toml
  - weeder.toml
  - src/Jazz/Compiler/ModuleAnalysis.hs
verification:
  - bash scripts/ci/haskell-quality.sh
deliverable: "Bound Weeder exemptions and remove redundant accessors"
last_verified: 2026-09-09
---

# Quality review fixes

The user approved both review findings against `1409e1c2`. Work inline.
Preserve existing structural APIs while replacing global instance roots with
named retained types. Remove the three private accessor aliases in ModuleAnalysis.

## Implementation

- [x] Narrow both Weeder configurations and document the bounded policy.
- [x] Replace ModuleAnalysis accessor aliases with record selectors.
- [x] Verify fresh production/full quality scans, a dead-type rejection probe,
      module tests, formatting, and repository checks; commit and close the queue.

## Verification

### PR 153 follow-up

The user requested review triage, necessary fixes, and a push, then explicitly
asked to stop without waiting for further comments. The initial snapshot at
`ee3c237b` contained six unresolved threads and one duplicate job-count nitpick.

- Fixed all-zero job counts (`00`, `000`) passing the new quality script's
  positive-integer guard. The regression test failed before the fix; the guard
  now requires a nonzero decimal digit and still accepts `004`.
- Documented that legacy diagnostic helpers intentionally assert starts;
  exact endpoints have dedicated source-range and structured-diagnostic tests.
  The old point-only representation did not assert endpoints.
- Retained the Unicode expectation: the emoji occupies two display columns.
- Retained both token-parser expectations: `lexSource` projects to points,
  while the alternative-consumption test uses the production ranged lexer.
- Retained complete imported data-type metadata: name visibility is separately
  filtered by the resolver's public inventory. This code was moved unchanged.
- Kept the two one-line test projections; their IO and Either entry points
  serve different fixtures, and a shared abstraction adds no useful contract.

Verification passed: all 121 CI-policy tests and the CI-policy checker; six
focused suites (`source-ranges-spec`, `token-parser-spec`,
`structured-error-diagnostics-spec`, `module-resolution-spec`,
`module-exports-spec`, and `module-pipeline-contract-spec`); HLint, Ormolu,
Prettier, shell syntax, and `git diff --check`. Haskell output is recorded in
`/tmp/jazz-pr153-followup-tests.log`. No full runtime or Weeder rebuild was
needed for the shell guard and explanatory test comment.

### Initial quality fixes

Both global structural-instance rules now name retained type heads and source
paths. The production baseline has 44 groups; the full baseline has 43. Existing
inspection, parser, runtime, and benchmark APIs remain retained. A new regression
script verifies retention of a known type and rejection of new unused types in
the same module, including a type whose name extends a retained name.

- Fresh `JAZZ_CABAL_JOBS=4 nix develop .#quality --command bash
scripts/ci/haskell-quality.sh` passed: policy probe, HLint, production build and
  Weeder scan, full test/benchmark/scale build and Weeder scan, generated laws.
  Log: `/tmp/jazz-review-fixes-quality.log`.
- The regression script failed against the previous wildcard configuration for
  the expected reason: it accepted the new unused types.
- Module pipeline, resolution, exports, and prelude suites passed. `cabal check`
  reported no warnings or errors. Log: `/tmp/jazz-review-fixes-focused.log`.
- CI policy checks and all 120 checker regressions passed. Documentation and
  queue checks, shell syntax, Ormolu, Prettier, and `git diff --check` passed.
  Documentation log: `/tmp/jazz-review-fixes-docs.log`.

No public language behavior changed. The three private ModuleAnalysis accessor
aliases were deleted; their callers use the existing record selectors directly.
