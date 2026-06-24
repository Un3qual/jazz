---
id: JN-MAP-FILTER-COMPAT-CLOSURE-001
status: done
priority: P2
size: S
kind: coordination
autonomous_ready: yes
depends_on:
  - JN-MAP-FILTER-COMPAT-PLAN-001
last_verified: 2026-06-24
completed_on: 2026-06-24
plan_section: "Completed coordination batch: map/filter compatibility closure"
target_paths:
  - docs/plans/spec-cleanup/2026-03-02/decisions/02-map-filter-order.md
  - docs/plans/spec-cleanup/2026-03-02/README.md
  - docs/jazz-language-state.md
  - README.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
verification:
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Close the stale map/filter compatibility path as reference-only: active Jazz uses function-first map/filter calls, collection-first examples remain historical/non-canonical, and no parser alias, runtime adapter, warning path, or compiler implementation row is promoted."
---

# Jazz-Next Map/Filter Compatibility Closure Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the old map/filter compatibility cleanup item as historical
evidence rather than an active executor path.

**Architecture:** Active `jazz-next` collection combinators are
function-first: `map f xs` and `filter p xs`. The compatibility decision is a
hard switch; collection-first calls remain non-canonical historical examples
and are not accepted through aliases, adapters, or deprecated-syntax warnings.

**Tech Stack:** Markdown plan and queue metadata, active `jazz-next`
primitive/runtime semantics, bundled-prelude aliases, and repo-root queue/docs
validators.

---

## Completed coordination batch: map/filter compatibility closure

Completed on `2026-06-24`.

Executor-safe scope:

- Do not edit `jazz-hs/` or `jazz2/`.
- Do not add parser aliases or normalization for collection-first calls.
- Do not add runtime adapter or dual-form behavior.
- Do not add warning/deprecation behavior for collection-first calls.
- Route future collection primitive changes through active `jazz-next`
  primitive, stdlib-boundary, or runtime-product contracts.

Evidence:

- `README.md` now demonstrates function-first `map` and `filter` calls.
- `docs/spec/authoritative-syntax.md` and
  `docs/spec/runtime/primitive-semantics.md` lock function-first order.
- Active `jazz-next` type/runtime tests use function-first calls and validate
  mapper/predicate and collection positions separately.
- No active docs/examples/tests require collection-first compatibility after
  the stale language-state note was removed.

### Task 1: Close legacy map/filter plan text

**Files:**

- Modify: `docs/plans/spec-cleanup/2026-03-02/decisions/02-map-filter-order.md`
- Modify: `docs/plans/spec-cleanup/2026-03-02/README.md`
- Modify: `docs/jazz-language-state.md`
- Verify unchanged current examples: `README.md`

- [x] **Step 1: Add reference-only closure note**

Mark the old map/filter order plan as historical evidence and route active
collection primitive work to current `jazz-next` primitive/stdlib/runtime
contracts.

- [x] **Step 2: Choose final compatibility policy**

Record the hard-switch policy: no collection-first compatibility alias, adapter,
or deprecation-warning path.

- [x] **Step 3: Remove stale active-doc contradiction**

Update `docs/jazz-language-state.md` so it no longer claims the current README
uses collection-first `map`.

### Task 2: Close dispatcher state

**Files:**

- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`

- [x] **Step 1: Update blocker contract**

Mark `JN-MAP-FILTER-COMPAT-PLAN-001` as closed with no remaining candidate
child.

- [x] **Step 2: Update queue**

Remove the map/filter compatibility blocker from `Blocked`, add closure
evidence to `Done`, and seed the next curation target from the parse-only
legacy rebase blocker contract.

### Verification

- [x] Active-doc/test search for collection-first examples
- [x] `bash scripts/check-execution-queue.sh`
- [x] `bash scripts/check-docs.sh`
- [x] `git diff --check`
