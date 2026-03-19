# Execution Queue Refresh And PR Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Refill the empty execution queue with concrete `jazz-next` implementation batches that are ready now, preserve the implementation-first dispatch policy, and package the updated prompt/queue changes into a PR.

**Architecture:** Explore the independent active-path plans in parallel, select only batches whose dependencies are already satisfied, and curate a small `Ready Now` set with `kind`, `plan_section`, `target_paths`, deliverables, and exact verification commands. Keep the queue authoritative, update linked plan metadata only if needed for safety, then verify the docs and create a PR from the current branch.

**Tech Stack:** Markdown docs under `docs/execution/` and `docs/plans/`, git/GitHub CLI for branch and PR flow, shell verification via `bash scripts/check-docs.sh` and targeted doc inspection.

---

## Progress

- [x] Task 1: Mined the active ADT, type, runtime, and module/import domains in parallel and kept only the honest `Ready Now` implementation batches.
- [x] Task 2: Refilled `docs/execution/queue.md` with three concrete `kind: impl` entries and replaced the temporary empty-queue blocker with the real module/import rebase blocker.
- [ ] Task 3: Verify the queue/docs state, capture the final prompt text, and create the PR.

### Task 1: Mine Active Plans In Parallel

**Files:**
- Read: `docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md`
- Read: `docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md`
- Read: `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`
- Read: other active-path plan docs as needed

**Step 1: Dispatch one subagent per independent plan domain**

Each subagent should identify at most 1-2 candidate `Ready Now` implementation batches whose dependencies are already satisfied.

**Step 2: Require concrete queue-ready output**

For each candidate, capture:
- queue id
- title
- priority and size
- exact plan section
- target code/test paths
- concrete deliverable
- exact verification commands
- any blocker if the plan is not actually ready

### Task 2: Refill The Queue

**Files:**
- Modify: `docs/execution/queue.md`

**Step 1: Add only executable implementation-first entries**

Populate `Ready Now` with a small set of `kind: impl` items. Keep docs-only work out unless it directly unblocks implementation.

**Step 2: Keep blockers explicit**

If a promising item is not actually executable, leave it out of `Ready Now` and retain or add a `Blocked` note only if it materially explains missing queue capacity.

### Task 3: Verify, Capture Prompt, And Create PR

**Files:**
- Modify: `docs/plans/2026-03-19-execution-queue-refresh-and-pr.md`

**Step 1: Verify the updated docs**

Run:

```bash
bash scripts/check-docs.sh
sed -n '1,240p' docs/execution/queue.md
sed -n '1,220p' docs/execution/prompts/curated-next-batch.md
```

Expected: docs checks pass, `Ready Now` is populated with implementation entries, and the curated prompt still reflects the implementation-first rules.

**Step 2: Record verification evidence and create the PR**

Capture the final prompt text that should be used going forward, then push the branch and open a PR with a concise summary and test plan.

## Verification Evidence

- `bash scripts/check-docs.sh` -> `Docs status checks passed.` (`prettier` was not installed, so the optional formatting check was skipped)
- `sed -n '1,260p' docs/execution/queue.md`
- `sed -n '1,220p' docs/execution/prompts/curated-next-batch.md`
