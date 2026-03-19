# Execution Dispatch

This directory is the control surface for "what should the next executor do?" decisions.

## File Roles

- `queue.md`: canonical dispatch source of truth
- `prompts/curated-next-batch.md`: use while the queue is curated by a human
- `prompts/autonomous-next-batch.md`: use once enough queue entries are safe for automatic selection

Keep using the rest of `docs/` for their existing roles:

- `docs/feature-status.md`: implementation/status truth
- `docs/plans/*`: plan detail, execution receipts, design records
- `docs/spec/*`: normative behavior and decision contracts

## Queue Rules

1. Read `queue.md` first.
2. Treat `Ready Now` as authoritative for dispatch.
3. Keep `Ready Now` to 2-5 entries and bias it toward executable implementation work.
4. Every `Blocked` entry must name its blocker directly.
5. Remove completed items from `Ready Now` immediately.
6. Do not use a repo-wide `docs/plans/**` scan to choose work unless the selected queue entry is insufficient.
7. When a queue entry points at an older historical plan, add a new active-path plan before doing new implementation work.
8. Treat queue, plan, status, and spec updates as required follow-through for an implementation batch, not as a standalone successful batch while executable implementation work exists.
9. Keep docs-only or coordination items out of `Ready Now` unless they are the smallest verified action that directly restores implementation flow.

## Queue Entry Contract

Each queue entry should capture:

- `id`: stable identifier
- `title`: short task label
- `priority`: `P1`, `P2`, or `P3`
- `size`: `S`, `M`, or `L`
- `kind`: `impl`, `docs`, or `coordination`
- `autonomous_ready`: `yes` or `no`
- `depends_on`: `-` or queue ids
- `plan`: source plan or roadmap link
- `plan_section`: exact task or section to execute next
- `target_paths`: expected files or directories to change in the next batch
- `deliverable`: concrete outcome expected from the next batch
- `verification`: commands or docs-only checks required before closing the item
- `last_verified`: absolute date of the latest manual status check

Guidance:

- `kind: impl` is the default for `Ready Now`.
- `target_paths` for `kind: impl` must include at least one non-doc path.
- `kind: docs` and `kind: coordination` belong in `Ready Now` only when they directly unblock an implementation item or close out a queue with no remaining executable code work.

## Active Plan Metadata Schema

Curated mode can work before plan metadata is backfilled. Fully autonomous mode should require this frontmatter on still-open plans:

```yaml
---
id: JN-ADT-CONSTR-SEM-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: true
depends_on: []
last_verified: 2026-03-19
plan_section: "Task 2"
target_paths:
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
verification:
  - runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
deliverable: Constructor-pattern `case` runtime evaluation works in `jazz-next`.
supersedes: []
---
```

Guidance:

- Backfill only open plans.
- Leave historical archive docs unchanged.
- If a legacy plan is no longer an execution target, keep it as evidence and create a new active-path plan instead of mutating history heavily.

## Promotion Checklist

Stay in curated mode until these are true:

1. `queue.md` is kept current and `Ready Now` stays small.
2. Open queue entries link to the active execution plan, not just a historical roadmap note.
3. Still-open plans have metadata frontmatter or a direct replacement plan exists.
4. At least the top priority items have explicit dependency and verification fields.
5. At least one high-priority executable implementation item carries `kind`, `plan_section`, and `target_paths` that name real non-doc paths.
6. The team can trust `autonomous_ready: yes` as a real signal, not a guess.
7. `bash scripts/check-execution-queue.sh` passes after queue or plan metadata changes.

Switch to the autonomous prompt when:

- the top executable implementation item is unambiguous from `queue.md`,
- at least one high-priority `kind: impl` entry is marked `autonomous_ready: yes`,
- linked plans are detailed enough that the executor does not need a broad docs scan to act safely.

## Queue Validation

Use `bash scripts/check-execution-queue.sh` after queue or open-plan metadata changes. It checks:

- required queue columns exist,
- every dependency id resolves,
- every linked plan path exists,
- `last_verified` is present on non-done items,
- every `kind: impl` entry names at least one non-doc `target_paths` location,
- every `Ready Now` row matches the linked plan frontmatter for the current executable batch.
