---
id: JN-RUNTIME-INTERPRETER-LEGACY-CLOSURE-001
status: done
priority: P2
size: S
kind: coordination
autonomous_ready: yes
depends_on:
  - JN-RUNTIME-INTERPRETER-LEGACY-REBASE-001
last_verified: 2026-06-24
completed_on: 2026-06-24
plan_section: "Completed coordination batch: runtime-interpreter legacy closure"
target_paths:
  - docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
verification:
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Close the legacy Haskell interpreter implementation plan as reference-only: active runtime work stays on the jazz-next runtime architecture plan and no jazz-hs implementation, second runtime path, or compile/run contract change is promoted."
---

# Jazz-Next Runtime Interpreter Legacy Closure Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the old Haskell interpreter implementation plan as historical
evidence rather than an active executor path.

**Architecture:** Treat the active `jazz-next` runtime architecture plan as the
current owner of interpreter-backed execution. The legacy `jazz-hs` vertical
slices are superseded; future runtime product work must be a concrete
`jazz-next` delta promoted from the queue.

**Tech Stack:** Markdown plan and queue metadata, the active `jazz-next`
runtime architecture plan, and repo-root queue/docs validators.

---

## Completed coordination batch: runtime-interpreter legacy closure

Completed on `2026-06-24`.

Executor-safe scope:

- Do not edit `jazz-hs/` or `jazz2/`.
- Do not add a second runtime path.
- Do not change active compile/run/help semantics.
- Do not promote legacy runtime implementation phases.
- Route future runtime product work through the active `jazz-next` runtime
  product blocker with a concrete delta.

Evidence:

- `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`
  is the active replacement for this old plan and records the closed
  interpreter-first compile/run/help baseline.
- The old
  `docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md`
  plan targets `jazz-hs` runtime, CLI, and test files, so it is not
  executor-safe under current workspace guardrails.
- `docs/execution/queue.md` keeps `JN-RUNTIME-PRODUCTIZE-CLOSURE-PLAN-001`
  blocked until a future concrete runtime product delta is accepted.

### Task 1: Close legacy interpreter plan text

**Files:**

- Modify: `docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md`

- [x] **Step 1: Add reference-only closure note**

Mark the old interpreter implementation plan as historical evidence and route
active work to the `jazz-next` runtime architecture plan.

- [x] **Step 2: Make old phases non-executable**

Keep the old vertical-slice plan visible as history, but state that its
`jazz-hs` file edits must not be executed as active work.

### Task 2: Close dispatcher state

**Files:**

- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`

- [x] **Step 1: Update blocker contract**

Mark `JN-RUNTIME-INTERPRETER-LEGACY-REBASE-001` as closed with no remaining
candidate child.

- [x] **Step 2: Update queue**

Remove the runtime-interpreter legacy blocker from `Blocked`, add closure
evidence to `Done`, and seed the next curation target from an existing
source-backed blocker contract.

### Verification

- [x] `bash scripts/check-execution-queue.sh`
- [x] `bash scripts/check-docs.sh`
- [x] `git diff --check`
