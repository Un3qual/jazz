# Execution Dispatch

This directory is the control surface for "what should the next executor do?" decisions.

## File Roles

- `queue.md`: canonical dispatch source of truth
- `blocker-contracts.md`: bounded unblocker contracts for blocked rows
- `done-archive.md`: historical closure evidence moved out of the dispatcher
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
4. If `Ready Now` is empty, treat `Next Curation Target` as the only promotion lane.
5. Keep `Next Curation Target` to 1-3 candidates and refresh it in the same change that empties `Ready Now`.
6. Every `Blocked` entry must name its blocker directly and have a matching section in `blocker-contracts.md`.
7. Remove completed items from `Ready Now` immediately.
8. Move stable completed-row evidence to `done-archive.md`; do not let `queue.md` become a changelog.
9. Do not use a repo-wide `docs/plans/**` scan to choose work unless both `Ready Now` and `Next Curation Target` are insufficient.
10. When a queue entry points at an older historical plan, add a new active-path plan before doing new implementation work.
11. Treat queue, plan, status, and spec updates as required follow-through for an implementation batch, not as a standalone successful batch while executable implementation work exists.
12. Keep docs-only or coordination items out of `Ready Now` unless they are the smallest verified action that directly restores implementation flow.

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
- `target_paths`: expected files to change in the next batch
- `deliverable`: concrete outcome expected from the next batch
- `verification`: commands or docs-only checks required before closing the item
- `last_verified`: absolute date of the latest manual status check

Guidance:

- `kind: impl` is the default for `Ready Now`.
- `target_paths` for every `Ready Now` row must be repo-relative concrete existing file paths, not `.` placeholders or paths outside the repository.
- `target_paths` for `kind: impl` must include at least one concrete, already-existing, non-doc file path.
- Use `-` only as a whole-cell placeholder. Do not mix it with real
  target paths or verification commands.
- Keep `target_paths` and `verification` in the same order between the queue row and linked plan frontmatter; parity treats those as ordered batch definitions, while `depends_on` stays set-like.
- `kind: docs` and `kind: coordination` belong in `Ready Now` only when they directly unblock an implementation item or close out a queue with no remaining executable code work.

## Next Curation Target Contract

`Next Curation Target` is required whenever `Ready Now` is empty. It is a small
promotion lane, not an execution queue. A future curation pass must either:

- promote one candidate into `Ready Now` with matching child-plan frontmatter, or
- replace the candidate with a better source-backed candidate and explain why in
  `blocker-contracts.md`.

Each candidate captures:

- `blocked_id`: existing blocked row that owns the umbrella.
- `candidate_child_id`: stable id for the child to create or promote.
- `kind`: `impl`, `docs`, or `coordination`.
- `source_contract`: link to the exact `blocker-contracts.md` section.
- `why_next`: one-sentence reason this is the next narrow promotion.
- `target_paths`: expected files for the child; `impl` candidates must name at
  least one existing non-doc path, and may also name planned new child files.
- `verification`: exact commands; use `-` only as a whole-cell placeholder,
  never mixed with real commands.
- `promotion_check`: the concrete action required before the candidate becomes a
  real `Ready Now` row.

This section exists to avoid the bad state where the queue validates but has no
next action.

## Blocker Contract Rules

Use `blocker-contracts.md` to keep blocked rows actionable without overloading
the queue table. Each blocked row should have one section that names:

- the smallest unblocker,
- the exact missing decision,
- the recommended default,
- a candidate child id,
- target paths,
- verification commands,
- and explicit non-goals.

If a blocked row cannot name these fields, it is not ready for curation. Do not
promote it by guessing from old plan history.

## Active Plan Metadata Schema

Curated mode can work before plan metadata is backfilled. Fully autonomous mode should require this frontmatter on still-open plans:

```yaml
---
id: JN-ADT-CONSTR-SEM-001
status: ready
priority: P1
size: S
kind: impl
autonomous_ready: yes
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
7. Empty `Ready Now` states always have `Next Curation Target` candidates and matching blocker contracts.
8. `bash scripts/check-execution-queue.sh` passes after queue or plan metadata changes.

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
- target paths are repo-relative and concrete, not repository-root placeholders,
- every `Ready Now` target path resolves to an existing file,
- every `kind: impl` entry names at least one concrete, already-existing, non-doc file path in `target_paths`,
- every `Ready Now` row matches the linked plan frontmatter for the current executable batch.
- empty `Ready Now` states still have 1-3 `Next Curation Target` candidates.
- `impl` curation candidates name at least one concrete, already-existing, non-doc file path, even when they also list planned new child files.
- `target_paths` never mixes the `-` placeholder with real paths.
- `verification` never mixes the `-` placeholder with real commands.
- `done-archive.md` exists before archived id checks are applied.
- archived ids are unique within `done-archive.md`.
- active queue rows and curation candidates do not reuse ids from `done-archive.md`.
- `source_contract` anchors point to the matching `blocker-contracts.md` section.
- a curation row's `candidate_child_id` matches the linked contract section's `Candidate child`.
- `blocker-contracts.md` headings do not create duplicate markdown anchors.

Run `bash scripts/test-check-execution-queue.sh` after validator changes. The
docs status gate also runs this regression harness through
`bash scripts/check-docs.sh`.
