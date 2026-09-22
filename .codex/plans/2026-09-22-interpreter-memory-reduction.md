---
id: JN-INTERPRETER-MEMORY-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-09-22
plan_section: Implementation
target_paths:
  - src/Jazz/Compiler/Runtime/Semantics.hs
  - src/Jazz/Compiler/Runtime/Engine.hs
  - test/Jazz/Compiler/Stdlib/LinearCollectionsTests.hs
  - PERFORMANCE.md
verification:
  - cabal test all --jobs=1 --test-show-details=direct
  - bash scripts/ci/haskell-quality.sh
  - bash scripts/check-docs.sh
  - bash scripts/check-execution-queue.sh
  - git diff --check
deliverable: Reduce measured queue-workload allocation and residency while preserving runtime behavior.
supersedes: []
---

# Interpreter memory reduction

**Goal:** Reduce the memory used by the existing 50,000-element queue workload.
The user authorized this batch and requested a lean plan on 2026-09-22.

**Approach:** Measure the current implementation, remove the demonstrated
redundant work at its existing owner, then repeat the measurement and behavioral
checks. This is an implementation optimization under RFC 0001, with no language
or library API changes.

## Boundaries

- Keep persistent queue versions, numeric defaulting/overflow checks, nominal
  types, tail calls, and host-effect ordering unchanged.
- Reuse existing runtime type information. No new IR, cache policy, runtime
  framework, dependency, or queue-specific evaluator path.
- Keep the 50,000-element test. Do not solve the problem by shrinking the input
  or raising memory limits. Add a test only for a behavior not already covered.
- Use existing RTS/profiling tools and temporary probes. Run builds and physical
  measurements serially; keep timing and heap numbers out of correctness gates.

## Implementation

- [ ] **Measure and isolate.** Copy the queue test's build/drain program to a
  temporary module and measure 1,000, 4,000, 8,000, and 50,000 elements with the
  pinned GHC 9.14.1 build and `+RTS -s`. Compare the ordinary CLI host path with
  the pure driver used by `runStdlibSource`. Record allocation, maximum live
  residency, elapsed time, and exact `(count, sum)` output. Profile a smaller
  reproduction using `cabal.project.profile-hotspots` if the growth's owner is
  still unclear.
- [ ] **Make one targeted fix.** First investigate repeated deep traversal in
  `attachDefaultBindingIntegerTarget`: queue operations share list tails, but
  this function reconstructs lists/constructors even after type hints have
  checked them. Test reuse of existing closed type hints; retain defaulting for
  polymorphic or unhinted values. Keep only a change demonstrated to improve
  the workload. Inspect every caller before changing its contract.
- [ ] **Verify and close.** Repeat identical before/after inputs and build/RTS
  settings. Run the existing persistence, numeric, recursion, observation, and
  host-I/O tests, then the complete default suite and the checks above. Update
  the measured caveat in `PERFORMANCE.md`, record results below, commit the fix,
  and remove the completed queue row.

The isolated command is the built `jazz` executable followed by:

```sh
--run --entry-module Main --module-root /private/tmp/jazz-memory-20260922/SIZE \
  --module-root jazz/stdlib +RTS -s -RTS
```

## Evidence

Baseline source: `3bc22e6d`. The first ordinary CLI runs return the expected
values; 1,000 elements allocate 293,442,768 bytes and 4,000 allocate
2,196,192,896 bytes. Both have approximately 4.4 MB maximum live residency.
This establishes excess allocation; the reported high residency still needs
reproduction through the pure test path.
