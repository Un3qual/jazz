---
id: JN-PURITY-EFFECT-CONTRACT-001
status: done
priority: P2
size: S
kind: coordination
autonomous_ready: yes
depends_on:
  - JN-PURITY-EFFECT-TYPING-PLAN-001
last_verified: 2026-06-24
completed_on: 2026-06-24
plan_section: "Completed coordination batch: purity/effect contract refresh"
target_paths:
  - docs/spec/semantics/purity-bang-stub-v1.md
  - docs/plans/spec-cleanup/2026-03-02/decisions/03-purity-bang-semantics.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
verification:
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Refresh the broader purity/effect blocker without promoting implementation: keep stub-v1 bang-suffix enforcement as the active contract, and leave higher-order purity, effect types, cross-module purity graphs, runtime enforcement, inferred effects, and effect typing in signatures blocked pending solver and module-method clarity."
---

# Jazz-Next Purity/Effect Contract Refresh Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Decide whether the broader effect-system blocker has enough active
evidence to promote a concrete contract beyond stub-v1 purity enforcement.

**Architecture:** The active compiler contract remains name-driven,
analyzer-level, direct-call enforcement for `!`-suffixed impure names. Broader
effect-system work must wait until solver-backed constrained signatures,
defaulting/runtime evidence, and module-method/export semantics are clear
enough to support one concrete child.

**Tech Stack:** Markdown spec/plan/queue metadata and repo-root queue/docs
validators. No compiler implementation is in scope.

---

## Completed coordination batch: purity/effect contract refresh

Completed on `2026-06-24`.

Executor-safe scope:

- Do not edit `jazz-hs/` or `jazz2/`.
- Do not change analyzer/runtime purity behavior.
- Do not implement runtime enforcement.
- Do not add effect types, inferred effects, effect polymorphism, or
  cross-module purity graphs.
- Do not add effect typing to signatures before the type-solver and
  module-method contracts can support it.

Evidence:

- `docs/spec/semantics/purity-bang-stub-v1.md` is the active contract.
- `docs/plans/spec-cleanup/2026-03-02/decisions/03-purity-bang-semantics.md`
  records stub-v1 completion and explicitly lists broader effects as non-goals.
- Current solver docs leave effect typing out of scope while remaining solver
  slices are still blocked.
- Current abstraction/module-method docs leave method export/import behavior,
  runtime evidence, dictionaries, default methods, and inferred constraints as
  future work.

### Task 1: Refresh purity/effect evidence

**Files:**

- Modify: `docs/spec/semantics/purity-bang-stub-v1.md`
- Inspect: `docs/plans/spec-cleanup/2026-03-02/decisions/03-purity-bang-semantics.md`

- [x] **Step 1: Confirm active stub-v1 contract**

Keep `!` bang-suffix purity as the current active contract.

- [x] **Step 2: Confirm broader effect directions remain blocked**

Record that higher-order purity, effect types, cross-module purity graphs,
runtime enforcement, inferred effects, and effect typing in signatures do not
yet have a concrete implementation-ready contract.

### Task 2: Close dispatcher state

**Files:**

- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`

- [x] **Step 1: Update blocker contract**

Mark `JN-PURITY-EFFECT-TYPING-PLAN-001` as still blocked with no current
candidate child.

- [x] **Step 2: Update queue**

Remove the purity/effect curation target, add closure evidence to `Done`, keep
the broader purity/effect blocker in `Blocked`, and seed the next named
source-backed candidate from the module/stdlib blocker.

### Verification

- [x] Purity/effect evidence review
- [x] `bash scripts/check-execution-queue.sh`
- [x] `bash scripts/check-docs.sh`
- [x] `git diff --check`
