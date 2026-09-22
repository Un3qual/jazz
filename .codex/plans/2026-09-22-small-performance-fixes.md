---
id: JN-SMALL-PERFORMANCE-FIXES-001
status: complete
priority: P1
size: S
kind: impl
autonomous_ready: no
depends_on: []
last_verified: 2026-09-22
plan_section: Implementation
target_paths:
  - src/Jazz/Compiler/TypeInference/Capabilities.hs
  - src/Jazz/Compiler/TypeInference/Scope.hs
  - src/Jazz/Compiler/Runtime/Types.hs
verification:
  - cabal test all --jobs=1 --test-show-details=direct
  - bash scripts/ci/haskell-quality.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: Remove the three approved inference and scope-counter costs without changing behavior.
supersedes: []
---

# Three small performance fixes

The user approved findings 1–3 of the [measured audit](2026-09-22-performance-memory-audit.md).
Implement those exact changes, preserving compiler diagnostics, recursive
generalization, runtime scope identity, and host effects.

## Implementation

- [x] Return no primitive constraints immediately when the scheme-variable set
      is empty. Measure the same literal-binding inputs before and after.
- [x] Reuse the existing environment free-variable summary at the two earlier
      inference call sites. Resolve it separately at each solver state and retain
      the existing recursive/intervening-group fallback. Measure constrained bindings.
- [x] Make only the next-scope-ID field strict. Measure block and bare countdowns
      at 50,000 and 200,000 iterations; check that the separate binding-cache
      retention behavior is unchanged.

Use the existing worktree and record progress here. The tasks have no new shared
interfaces. Keep the existing performance probes in temporary storage, preserve
the audit's raw evidence, and serialize builds and measurements. These are
behavior-preserving optimizations: use existing meaningful behavioral tests and
before/after performance evidence rather than adding timing-based correctness
tests or tests that mirror the implementation. Add a semantic regression only
if review identifies an uncovered behavior.

Run focused inference/runtime suites during implementation, then all default
suites, Haskell quality, formatting, and documentation checks. Have an independent
reviewer inspect the final diff. Commit the changes and close the queue entry.

## Evidence and progress

Baseline: `82eddcd0`. Measurements use the audit's pinned toolchain and identical
inputs, with three ordinary runs per input and observations disabled.

Task 1: complete in `cd9c2e5e`. The binding/signature suite passes. At 4,096
literal bindings, allocation falls from 1,764,619,576 to 287,552,848 bytes and
median elapsed time from 0.640 to 0.170 seconds. Sampled maximum live heap rises
from 20.87 to 24.48 MB; this is an allocation/CPU improvement, not a claim of
lower residency for this input. The constrained-binding control is unchanged.
Raw logs and the runner are in `/private/tmp/jazz-small-perf-fixes-20260922/`.

Task 2: complete in `a0b46377`. The binding/signature and recursive-binding suites pass (268
named checks). At 4,096 constrained bindings, allocation falls from
9,004,891,416 bytes after task 1 to 1,458,249,808 bytes, and median elapsed time
from 2.786 to 0.863 seconds. The raw free-variable summary is resolved at the
same two solver states as before; recursive/intervening groups still scan their
adjusted environments. Existing shadowing, captured constraints, rebinding, and
recursive-preview coverage has no identified gap requiring a new test.

Task 3: complete in `0d3065e1`. Runtime, lambda, and observation suites pass (332 named checks).
Block countdown maximum live heap falls from 4,947,376 to 1,922,792 bytes at
200,000 iterations; it is 1,912,920 bytes at 50,000 after the fix. Bare countdowns
remain near 1.89 MB. The scratch-binding cache control remains near 21 MB, as
expected; this change does not alter cache lifetime.

Independent review found no actionable issues in the three production changes
and no missing semantic regression case. All 47 default suites (1,998 named
checks) pass, including the full program corpus and standard library. Ormolu
passes on the three changed production files. The clean-build Haskell quality
script passes HLint, production-only Weeder, full Weeder, and generated
invariants; `cabal check` also passes. No new dependencies, abstractions, or tests
were needed.

Documentation, execution-queue checks, Markdown formatting, and `git diff --check`
pass after closeout. The queue entry is closed.

Final ordinary-build comparison against the fresh baseline (three-run medians):

| Input                      | Allocated before / after | Elapsed before / after | Max live heap before / after |
| -------------------------- | ------------------------ | ---------------------- | ---------------------------- |
| 4,096 literal bindings     | 1.765 / 0.288 GB         | 0.640 / 0.176 s        | 20.87 / 24.32 MB             |
| 4,096 constrained bindings | 9.005 / 1.458 GB         | 2.802 / 0.859 s        | 53.76 / 55.45 MB             |
| 200,000 block countdowns   | 4.214 / 4.209 GB         | 0.887 / 0.883 s        | 4.95 / 1.92 MB               |

All outputs match. The inference probes allocate about 84% less and take 69–73%
less elapsed time, but sampled peak residency is 3–17% higher. The scope-counter
fix reduces block-countdown residency by 61%, with essentially unchanged time.
Allocation is cumulative and residency is sampled live heap, not process RSS;
these local measurements are evidence, not portable performance thresholds.
