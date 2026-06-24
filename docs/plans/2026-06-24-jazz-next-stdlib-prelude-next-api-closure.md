---
id: JN-STDLIB-PRELUDE-NEXT-API-CONTRACT-001
status: done
priority: P2
size: S
kind: coordination
autonomous_ready: yes
depends_on:
  - JN-MODULE-REBASE-PLAN-001
last_verified: 2026-06-24
completed_on: 2026-06-24
plan_section: "Completed coordination batch: stdlib/prelude next API closure"
target_paths:
  - docs/spec/stdlib-boundary.md
  - docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
  - docs/execution/README.md
  - docs/execution/prompts/curated-next-batch.md
  - docs/execution/prompts/autonomous-next-batch.md
  - scripts/check-execution-queue.py
  - scripts/test-check-execution-queue.sh
verification:
  - bash scripts/test-check-execution-queue.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Close the stdlib/prelude next API curation candidate without promoting implementation: no source-backed future stdlib/catalog API or module behavior is ready, and module/import execution stays blocked until a concrete API/runtime contract exists."
---

# Jazz-Next Stdlib/Prelude Next API Closure Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the `JN-STDLIB-PRELUDE-NEXT-API-CONTRACT-001` curation
candidate as not implementation-ready.

**Architecture:** The active module/import and stdlib/prelude subset is already
closed on `jazz-next`. Future stdlib/catalog growth must come from a named
product or language need with a concrete API/runtime contract. This closure does
not add compiler behavior, module resolver behavior, import syntax, prelude
runtime behavior, or a new public stdlib/catalog API.

**Tech Stack:** Markdown execution metadata, stdlib boundary docs, runtime plan
docs, and the repo-root execution queue validator.

---

## Completed coordination batch: stdlib/prelude next API closure

Completed on `2026-06-24`.

Executor-safe scope:

- Do not edit `jazz-hs/` or `jazz2/`.
- Do not modify compiler implementation, parser behavior, analyzer behavior,
  codegen, runtime semantics, `ModuleResolver.hs`, import syntax, or stdlib
  runtime behavior.
- Do not reopen the module graph harness.
- Do not add a new prelude/catalog public API without a named contract.
- Do not add direct public builtin fallback in no-prelude mode.
- Do not define package or module-root semantics.

Evidence:

- `docs/spec/stdlib-boundary.md` marks the current `jazz-next` stdlib/prelude
  ownership boundary closed for the active runtime subset.
- `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`
  marks Milestone 5 closed and routes future stdlib/catalog growth through a
  concrete API/runtime contract.
- `docs/execution/blocker-contracts.md` recommends keeping module/import
  execution closed until a product feature needs new stdlib/catalog surface.
- The checked-in `jazz-next/stdlib/Prelude.jz` and active prelude loading tests
  already cover default `Int`/`Float`/`Bool` capability facts,
  width-specific numeric capability facts, public numeric conversion aliases,
  and no-prelude isolation for public aliases.
- No source-backed future stdlib/catalog API or module behavior is ready to
  promote, and there is no next named curation target after this closure.

### Task 1: Validate the candidate as non-promotable

**Files:**

- Inspect: `docs/spec/stdlib-boundary.md`
- Inspect:
  `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`
- Inspect: `docs/execution/blocker-contracts.md`
- Modify: `docs/plans/2026-06-24-jazz-next-stdlib-prelude-next-api-closure.md`

- [x] **Step 1: Confirm active subset closure**

Verify the stdlib boundary and runtime plan already close the active
module/import and prelude/module graph subset.

- [x] **Step 2: Confirm no source-backed next API**

Verify no future stdlib/catalog API or module behavior is named strongly enough
to promote into `Ready Now`.

- [x] **Step 3: Record no implementation promotion**

Create this completed coordination plan and state that no compiler, runtime,
parser, import, or stdlib implementation changed.

### Task 2: Close dispatcher and blocker state

**Files:**

- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/README.md`
- Modify: `docs/execution/prompts/curated-next-batch.md`
- Modify: `docs/execution/prompts/autonomous-next-batch.md`

- [x] **Step 1: Update blocker contract**

Set `JN-MODULE-REBASE-PLAN-001` to no current candidate child, no target paths
until a concrete API/runtime contract exists, and explicit non-goals for module
resolver, import syntax, module graph harness, prelude/catalog API, no-prelude
public fallback, and package/module-root semantics.

- [x] **Step 2: Update queue**

Remove the stdlib/prelude candidate from `Next Curation Target`, keep the table
valid but empty, update current executor status with the terminal empty-candidate
wording, refresh the module blocker row, and add this closure to `Done`.

- [x] **Step 3: Update terminal-empty execution docs**

Teach the queue validator docs and execution prompts that an empty
`Next Curation Target` is allowed only when the queue explicitly says no
source-backed next curation target and no named candidate currently exists; in
that case, stop instead of scanning broadly or inventing work.

### Task 3: Refresh stale stdlib/runtime wording

**Files:**

- Modify: `docs/spec/stdlib-boundary.md`
- Modify:
  `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`

- [x] **Step 1: Align stdlib boundary facts**

Record the current bundled default aliases, width-specific numeric capability
facts, and no-prelude rejection of numeric conversion aliases.

- [x] **Step 2: Replace stale runtime-plan wording**

Replace historical remaining-stdlib-closure language with closed-active-subset
language and route future growth to new concrete API/runtime contracts.

### Task 4: Run closure verification

**Files:**

- Modify: `scripts/check-execution-queue.py`
- Modify: `scripts/test-check-execution-queue.sh`

- [x] **Step 1: Add validator regression coverage**

Add focused coverage proving terminal empty-candidate queues pass only with
explicit current executor status, while accidental empty `Next Curation Target`
tables continue to fail.

- [x] **Step 2: Run queue validator regression tests**

Run:

```bash
bash scripts/test-check-execution-queue.sh
```

Expected: all validator regression tests pass.

- [x] **Step 3: Run queue, docs, and diff checks**

Run:

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: all commands pass; `check-docs.sh` may print the existing
Prettier-outside-Nix warning and still pass.
