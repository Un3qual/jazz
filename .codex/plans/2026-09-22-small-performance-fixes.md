---
id: JN-SMALL-PERFORMANCE-FIXES-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
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

- [ ] Return no primitive constraints immediately when the scheme-variable set
      is empty. Measure the same literal-binding inputs before and after.
- [ ] Reuse the existing environment free-variable summary at the two earlier
      inference call sites. Resolve it separately at each solver state and retain
      the existing recursive/intervening-group fallback. Measure constrained bindings.
- [ ] Make only the next-scope-ID field strict. Measure block and bare countdowns
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
