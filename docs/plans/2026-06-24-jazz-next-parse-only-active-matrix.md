---
id: JN-PARSE-ONLY-ACTIVE-MATRIX-001
status: done
priority: P2
size: S
kind: coordination
autonomous_ready: yes
depends_on:
  - JN-PARSE-ONLY-LEGACY-REBASE-001
last_verified: 2026-06-24
completed_on: 2026-06-24
plan_section: "Completed coordination batch: parse-only active matrix"
target_paths:
  - docs/feature-status.md
  - docs/jazz-language-state.md
  - docs/plans/spec-cleanup/2026-03-02/README.md
  - docs/plans/spec-cleanup/2026-03-02/compiler/06-parse-only-features-resolution.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
verification:
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Close the legacy parse-only cleanup plan as reference-only after an active-path matrix check: module/import v1 is no longer a parse-only cleanup driver, operator parser-only work stays under the operator blocker, and no broad jazz-hs parser/analyzer/codegen parity row is promoted."
---

# Jazz-Next Parse-Only Active Matrix Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rebase the old parse-only cleanup plan away from `jazz-hs` by
checking whether any active `jazz-next` parse-only feature is implementation
ready.

**Architecture:** Active compiler behavior belongs in `jazz-next/`. Legacy
`jazz-hs` parse-only forms are historical evidence only. Parser-only or
parse-mostly active surfaces must route through their owning concrete blocker
instead of a broad parser parity queue item.

**Tech Stack:** Markdown status/plan/queue metadata, active `jazz-next`
module/import specs, existing operator blocker evidence, and repo-root
queue/docs validators.

---

## Completed coordination batch: parse-only active matrix

Completed on `2026-06-24`.

Executor-safe scope:

- Do not edit `jazz-hs/` or `jazz2/`.
- Do not revive legacy analyzer/codegen parity work.
- Do not promote broad parser parity.
- Do not route operator runtime semantics outside the operator blocker.
- Do not reopen closed module/import harness work without a concrete product
  delta.

Evidence:

- `docs/feature-status.md` had one stale `Partially Implemented / Parse-Only`
  label for module/import syntax.
- Current module/import specs and queue metadata show the v1 parser, resolver,
  loader, CLI, and migration harness are closed for the active subset.
- Stage 2 fixed-tier operator declarations remain intentionally parser-only,
  but that work is already owned by `JN-USER-DEFINED-OPERATORS-PLAN-001`.
- No target file names a standalone active `jazz-next` parse-only feature that
  is ready for implementation.

### Task 1: Close legacy parse-only plan text

**Files:**

- Modify: `docs/plans/spec-cleanup/2026-03-02/compiler/06-parse-only-features-resolution.md`
- Modify: `docs/plans/spec-cleanup/2026-03-02/README.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/feature-status.md`

- [x] **Step 1: Mark legacy plan reference-only**

Replace the old `jazz-hs` remove-vs-implement plan with a closure that routes
future active behavior through specific `jazz-next` blockers.

- [x] **Step 2: Update active feature matrix**

Remove the stale parse-only label from module/import syntax and record that the
current v1 module/import harness is implemented for the active subset while
broader module/package growth remains blocked.

- [x] **Step 3: Align language-state authority text**

State that `jazz-hs` parse-only behavior is historical evidence only, not an
active implementation surface.

### Task 2: Close dispatcher state

**Files:**

- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`

- [x] **Step 1: Update blocker contract**

Mark `JN-PARSE-ONLY-LEGACY-REBASE-001` as closed with no remaining candidate
child.

- [x] **Step 2: Update queue**

Remove the parse-only legacy blocker from `Blocked`, add closure evidence to
`Done`, and seed the next curation target from the purity/effect blocker
contract.

### Verification

- [x] Active parse-only matrix evidence review
- [x] `bash scripts/check-execution-queue.sh`
- [x] `bash scripts/check-docs.sh`
- [x] `git diff --check`
