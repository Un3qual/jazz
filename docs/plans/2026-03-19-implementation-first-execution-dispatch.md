# Implementation-First Execution Dispatch Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make the curated and autonomous execution prompts prefer real implementation batches while still requiring queue, plan, and status docs updates before a milestone is considered complete.

**Architecture:** Extend the execution queue contract so each `Ready Now` entry declares whether it is implementation, docs, or coordination work, which plan section should run next, and which paths should change. Update the curated/autonomous prompt text so docs remain required follow-through but are no longer a valid standalone batch when executable implementation work exists.

**Tech Stack:** Markdown docs under `docs/execution/` and `docs/plans/`, shell verification via `bash scripts/check-docs.sh` and targeted `sed` inspection.

---

## Progress

- [x] Task 1: Updated the execution queue contract and queue template with `kind`, `plan_section`, and `target_paths` so `Ready Now` defaults to executable implementation work.
- [x] Task 2: Updated the curated and autonomous prompt text so docs remain required follow-through but stop counting as a standalone successful batch while implementation work exists.
- [x] Task 3: Verified the changed docs with `bash scripts/check-docs.sh` and targeted `sed` inspection, then recorded the policy shift here.

### Task 1: Extend The Queue Contract

**Files:**
- Modify: `docs/execution/README.md`
- Modify: `docs/execution/queue.md`

**Step 1: Add queue entry metadata that separates implementation from docs work**

Document `kind`, `plan_section`, and `target_paths` in the queue contract and explain how they are used during dispatch.

**Step 2: Make implementation the default `Ready Now` work type**

Document that `kind: impl` is the default for `Ready Now`, that implementation entries must point at at least one non-doc path, and that docs-only or coordination items belong there only when they directly unblock implementation flow.

**Step 3: Update the queue template**

Add the new columns to the `Ready Now` table header and entry template so future queue curation follows the same contract.

### Task 2: Tighten Prompt Selection Rules

**Files:**
- Modify: `docs/execution/prompts/curated-next-batch.md`
- Modify: `docs/execution/prompts/autonomous-next-batch.md`

**Step 1: Require implementation-first batch selection**

Update both prompts so the executor prefers `kind: impl` entries and treats docs updates as required follow-through rather than a standalone successful batch when implementation work is available.

**Step 2: Define the docs-only fallback**

Document that docs-only or coordination work is allowed only when no executable implementation batch exists and the change is the smallest action needed to restore implementation flow.

### Task 3: Verify And Record

**Files:**
- Modify: `docs/plans/2026-03-19-implementation-first-execution-dispatch.md`

**Step 1: Run the docs verification command**

Run:

```bash
bash scripts/check-docs.sh
```

Expected: `Docs status checks passed.`

**Step 2: Inspect the changed execution docs**

Run:

```bash
sed -n '1,240p' docs/execution/README.md
sed -n '1,220p' docs/execution/prompts/curated-next-batch.md
sed -n '1,220p' docs/execution/prompts/autonomous-next-batch.md
sed -n '1,220p' docs/execution/queue.md
```

Expected: the queue contract exposes implementation-vs-docs metadata, both prompts prefer implementation batches, and the queue template matches the new fields.

## Verification Evidence

- `bash scripts/check-docs.sh` -> `Docs status checks passed.` (`prettier` was not installed, so the script skipped that optional check)
- `sed -n '1,260p' docs/execution/README.md`
- `sed -n '1,220p' docs/execution/prompts/curated-next-batch.md`
- `sed -n '1,220p' docs/execution/prompts/autonomous-next-batch.md`
- `sed -n '1,220p' docs/execution/queue.md`
