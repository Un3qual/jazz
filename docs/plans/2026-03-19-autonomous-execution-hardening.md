# Autonomous Execution Hardening Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Backfill execution frontmatter on the still-open active-path plans and add a queue-validation script so autonomous selection can trust the queue/plan contract instead of relying on manual discipline alone.

**Architecture:** Treat the `Ready Now` queue entries as the canonical next-batch view and make each linked active plan carry matching frontmatter for its current executable batch. Add `scripts/check-execution-queue.sh` to validate queue structure, linked plan existence, dependency references, non-doc implementation targets, and queue-to-frontmatter parity, then wire the docs guidance around that script.

**Tech Stack:** Markdown docs under `docs/execution/` and `docs/plans/`, shell validation under `scripts/`, standard-library Python for structured queue/frontmatter checks, repo-root verification via `bash scripts/check-docs.sh` and `bash scripts/check-execution-queue.sh`.

---

## Progress

- [x] Task 1: Added execution frontmatter to the three still-open active-path plans linked from `Ready Now`.
- [x] Task 2: Added `scripts/check-execution-queue.sh`, wired `scripts/check-docs.sh` through it, and updated the execution docs to treat it as the queue contract validator.
- [x] Task 3: Verify the queue/docs state and create the stacked PR.

### Task 1: Backfill Active Plan Frontmatter

**Files:**
- Modify: `docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md`
- Modify: `docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md`
- Modify: `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`

**Step 1: Encode the queue-facing metadata on each open plan**

Add frontmatter with:
- `id`
- `status`
- `priority`
- `size`
- `kind`
- `autonomous_ready`
- `depends_on`
- `last_verified`
- `plan_section`
- `target_paths`
- `verification`
- `deliverable`
- `supersedes`

Use values that match the current `Ready Now` queue entries exactly.

### Task 2: Add Queue Validation

**Files:**
- Create: `scripts/check-execution-queue.sh`
- Modify: `docs/execution/README.md`
- Modify: `scripts/check-docs.sh`

**Step 1: Validate queue structure and links**

Check required queue columns, dependency resolution, linked plan existence, and `last_verified` presence for open queue items.

**Step 2: Validate implementation-entry semantics**

Require every `kind: impl` queue row to include at least one concrete, already-existing, non-doc file path in `target_paths`.

**Step 3: Validate queue-to-plan parity**

For each `Ready Now` row, require the linked plan to expose frontmatter matching the queue row for the current next batch.

**Step 4: Make the docs point at the validator**

Update `docs/execution/README.md` and `scripts/check-docs.sh` so the queue contract no longer relies only on manual review.

### Task 3: Verify And Ship

**Files:**
- Modify: `docs/plans/2026-03-19-autonomous-execution-hardening.md`

**Step 1: Run verification**

Run:

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

Expected: both pass.

**Step 2: Record evidence and create the stacked PR**

Capture the verification evidence here, then commit, push the stacked branch, and open a PR against `codex/implementation-first-queue-refresh`.

## Verification Evidence

- `2026-04-01`: `bash scripts/check-execution-queue.sh` -> `Execution queue checks passed.`
- `2026-04-01`: `bash scripts/check-docs.sh` -> `Execution queue checks passed.` then `Docs status checks passed.` (`prettier` was found outside the Nix shell, so the script skipped format enforcement to avoid version drift)
