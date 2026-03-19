# Execution Dispatch Scaffolding Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a curated execution-dispatch layer that reduces prompt context waste now and supports a near-term switch to fully autonomous next-batch selection.

**Architecture:** Keep status docs and long-form plans in place, but add a small `docs/execution/` control surface. The queue is authoritative for dispatch, prompt files consume the queue, and open plans can gradually adopt lightweight metadata without rewriting the plan archive.

**Tech Stack:** Markdown docs under `docs/`

---

## Progress

- [x] Task 1 complete: design doc records the curated-first dispatch model and promotion path.
- [x] Task 2 complete: `docs/execution/README.md` defines the queue contract, plan metadata schema, and autonomous promotion checklist.
- [x] Task 3 complete: `docs/execution/queue.md` seeds a small verified work queue instead of relying on broad `docs/` scans.
- [x] Task 4 complete: curated and autonomous prompt files capture the expected execution flow.
- [x] 2026-03-19 re-verification complete: the empty-queue blocker was confirmed and then cleared by reseeding `docs/execution/queue.md` with verified active-path `Ready Now` items.

### Task 1: Record The Dispatch Design

**Files:**
- Create: `docs/plans/2026-03-17-execution-dispatch-design.md`

**Step 1: Write the approved design summary**

Capture the problem, decision, curated-first workflow, and autonomous promotion criteria in a short design doc.

**Step 2: Verify the design doc is standalone**

Run:

```bash
sed -n '1,200p' docs/plans/2026-03-17-execution-dispatch-design.md
```

Expected: the document explains why the queue exists and how it changes executor behavior.

### Task 2: Add Execution Operating Rules

**Files:**
- Create: `docs/execution/README.md`

**Step 1: Define queue ownership and boundaries**

Document:
- what `docs/execution/queue.md` is for,
- what status/roadmap/plan docs are still for,
- how small the `Ready Now` set should remain.

**Step 2: Define the plan metadata schema**

Include a frontmatter template that can be added only to still-open plans during the curated-to-autonomous migration.

**Step 3: Verify the guide is actionable**

Run:

```bash
sed -n '1,260p' docs/execution/README.md
```

Expected: the guide contains queue rules, metadata fields, and explicit autonomous-promotion criteria.

### Task 3: Seed The Canonical Queue

**Files:**
- Create: `docs/execution/queue.md`

**Step 1: Add a compact queue format**

Use `Ready Now`, `Blocked`, and `Done` sections with explicit fields for priority, dependency state, and autonomous readiness.

**Step 2: Seed only verified live work**

Use current docs evidence to avoid adding already-executed historical items.

**Step 3: Verify the queue stays small**

Run:

```bash
sed -n '1,260p' docs/execution/queue.md
```

Expected: `Ready Now` contains only a few items and each item links to a real source plan or roadmap.

### Task 4: Freeze Prompt Text

**Files:**
- Create: `docs/execution/prompts/curated-next-batch.md`
- Create: `docs/execution/prompts/autonomous-next-batch.md`

**Step 1: Write the curated prompt**

Require the executor to read `docs/execution/queue.md` first, inspect only the top `Ready Now` items, and avoid repo-wide plan scans.

**Step 2: Write the autonomous prompt**

Require the executor to pick the highest-priority `autonomous_ready: yes` item with satisfied dependencies.

**Step 3: Verify prompt drift is minimized**

Run:

```bash
sed -n '1,220p' docs/execution/prompts/curated-next-batch.md
sed -n '1,220p' docs/execution/prompts/autonomous-next-batch.md
```

Expected: both prompts use the same queue contract, with autonomous mode adding only the stricter selection rule.

### Task 5: Verify The Scaffolding

**Files:**
- Modify: none

**Step 1: Verify the new file set**

Run:

```bash
rg --files docs/execution docs/plans | rg 'execution-dispatch|docs/execution/'
```

Expected: the design doc, plan doc, queue, README, and prompt files are all present.

**Step 2: Inspect git status**

Run:

```bash
git status --short
```

Expected: only the new dispatch docs appear.
