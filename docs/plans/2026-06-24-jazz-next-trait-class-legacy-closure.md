---
id: JN-TRAIT-CLASS-LEGACY-CLOSURE-001
status: done
priority: P2
size: S
kind: coordination
autonomous_ready: yes
depends_on:
  - JN-TRAIT-CLASS-LEGACY-REBASE-001
last_verified: 2026-06-24
completed_on: 2026-06-24
plan_section: "Completed coordination batch: trait/class legacy closure"
target_paths:
  - docs/plans/spec-cleanup/2026-03-02/decisions/04-trait-vs-class-keyword.md
  - docs/plans/spec-cleanup/2026-03-02/README.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Close the legacy trait/class cleanup plan as reference-only: active Jazz syntax uses canonical class/impl declarations, trait declarations remain permanently rejected in jazz-next, and no compatibility alias or deprecation-warning path is created."
---

# Jazz-Next Trait/Class Legacy Closure Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the old trait-vs-class cleanup plan as historical evidence
rather than an active executor path.

**Architecture:** Treat the active `jazz-next` authoritative syntax contract as
the current language source of truth: abstraction declarations use `class` and
`impl`; declaration-shaped `trait` syntax is permanently rejected; and any
future abstraction work must start from active `jazz-next` contracts instead of
legacy `jazz-hs` parser or prelude edits.

**Tech Stack:** Markdown plan and queue metadata, existing `jazz-next` parser
coverage, and repo-root queue/docs validators.

---

## Completed coordination batch: trait/class legacy closure

Completed on `2026-06-24`.

Executor-safe scope:

- Do not edit `jazz-hs/` or `jazz2/`.
- Do not accept `trait` declarations in active `jazz-next`.
- Do not add a compatibility alias for `trait`.
- Do not add a deprecation warning or W0004 surface for rejected `trait`
  declarations.
- Preserve ordinary identifier uses of `trait` where the active parser already
  allows them.
- Route future abstraction semantics through active `jazz-next` class/impl
  contracts.

Evidence:

- `docs/spec/authoritative-syntax.md` states that active abstraction syntax is
  `class` and `impl`, and that `trait` is never accepted as a compatibility
  alias.
- `docs/jazz-language-state.md` records active `jazz-next` class/impl behavior
  and permanent non-canonical `trait` declaration rejection.
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs` locks
  top-level and module-body `trait` declaration rejection.
- The old
  `docs/plans/spec-cleanup/2026-03-02/decisions/04-trait-vs-class-keyword.md`
  plan targeted read-only legacy `jazz-hs` parser/prelude/test edits, so it is
  not an executor-safe active plan under current workspace guardrails.

### Task 1: Close legacy plan text

**Files:**

- Modify: `docs/plans/spec-cleanup/2026-03-02/decisions/04-trait-vs-class-keyword.md`
- Modify: `docs/plans/spec-cleanup/2026-03-02/README.md`

- [x] **Step 1: Add reference-only closure note**

Mark the old trait/class plan as historical evidence and route active work to
`docs/spec/authoritative-syntax.md` plus current `jazz-next` abstraction
contracts.

- [x] **Step 2: Remove active compatibility/deprecation guidance**

Replace the stale staged `trait` compatibility/deprecation path with permanent
active rejection. Keep historical notes clear enough for future readers without
leaving unchecked executor tasks.

- [x] **Step 3: Align spec-cleanup batch summary**

Update the batch README so it no longer claims `trait` becomes a deprecated
alias during migration.

### Task 2: Close dispatcher state

**Files:**

- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`

- [x] **Step 1: Update blocker contract**

Mark `JN-TRAIT-CLASS-LEGACY-REBASE-001` as closed with no remaining candidate
child.

- [x] **Step 2: Update queue**

Remove the trait/class legacy blocker from `Blocked`, add closure evidence to
`Done`, and seed the next curation target from an existing source-backed
blocker contract.

### Verification

- [x] `bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- [x] `bash scripts/check-execution-queue.sh`
- [x] `bash scripts/check-docs.sh`
- [x] `git diff --check`
