---
id: JN-BACKEND-TARGET-LEGACY-CLOSURE-001
status: done
priority: P2
size: S
kind: coordination
autonomous_ready: yes
depends_on:
  - JN-BACKEND-TARGET-LEGACY-REBASE-001
last_verified: 2026-06-24
completed_on: 2026-06-24
plan_section: "Completed coordination batch: backend-target legacy closure"
target_paths:
  - docs/plans/spec-clarification/2026-03-02/runtime/12-backend-target-strategy.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
verification:
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Close the legacy backend-target strategy plan as reference-only: active runtime product work stays on the jazz-next interpreter-backed compile/run/help baseline and no backend implementation, codegen policy, or legacy runtime edits are promoted."
---

# Jazz-Next Backend-Target Legacy Closure Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the old backend-target strategy plan as historical evidence
rather than an active executor path.

**Architecture:** Treat the active `jazz-next` runtime architecture plan as the
current runtime product source of truth. The interpreter-backed compile/run/help
baseline is closed, and additional runtime product work must be selected as a
new concrete `jazz-next` delta with target paths and focused verification.

**Tech Stack:** Markdown plan and queue metadata, the active `jazz-next`
runtime architecture plan, and repo-root queue/docs validators.

---

## Completed coordination batch: backend-target legacy closure

Completed on `2026-06-24`.

Executor-safe scope:

- Do not edit `jazz-hs/` or `jazz2/`.
- Do not add backend implementation or backend abstraction work.
- Do not reopen JavaScript, LLVM, QBE, generated artifact, or codegen policy.
- Do not change active compile/run/help semantics.
- Route future runtime product work through the active runtime product blocker
  with a concrete `jazz-next` delta.

Evidence:

- `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`
  states that the interpreter-first compile/run/help baseline is closed and
  that future runtime product work needs a later concrete delta.
- The old
  `docs/plans/spec-clarification/2026-03-02/runtime/12-backend-target-strategy.md`
  plan contains phases that point at legacy `jazz-hs` runtime/CLI/build files,
  so it is not an executor-safe active plan under current workspace guardrails.
- `docs/execution/queue.md` keeps `JN-RUNTIME-PRODUCTIZE-CLOSURE-PLAN-001`
  blocked on additional runtime product deltas beyond the closed baseline.

### Task 1: Close legacy backend-target plan text

**Files:**

- Modify: `docs/plans/spec-clarification/2026-03-02/runtime/12-backend-target-strategy.md`

- [x] **Step 1: Add reference-only closure note**

Mark the old backend-target strategy as historical evidence and route active
work to the `jazz-next` runtime architecture plan.

- [x] **Step 2: Preserve historical decision context**

Keep the old interpreter-only decision and contradiction inventory visible as
history, but make its later legacy file-edit phases non-executable.

### Task 2: Close dispatcher state

**Files:**

- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`

- [x] **Step 1: Update blocker contract**

Mark `JN-BACKEND-TARGET-LEGACY-REBASE-001` as closed with no remaining
candidate child.

- [x] **Step 2: Update queue**

Remove the backend-target legacy blocker from `Blocked`, add closure evidence
to `Done`, and seed the next curation target from an existing source-backed
blocker contract.

### Verification

- [x] `bash scripts/check-execution-queue.sh`
- [x] `bash scripts/check-docs.sh`
- [x] `git diff --check`
