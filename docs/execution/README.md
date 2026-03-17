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
3. Keep `Ready Now` to 2-5 entries.
4. Every `Blocked` entry must name its blocker directly.
5. Remove completed items from `Ready Now` immediately.
6. Do not use a repo-wide `docs/plans/**` scan to choose work unless the selected queue entry is insufficient.
7. When a queue entry points at an older historical plan, add a new active-path plan before doing new implementation work.

## Queue Entry Contract

Each queue entry should capture:

- `id`: stable identifier
- `title`: short task label
- `priority`: `P1`, `P2`, or `P3`
- `size`: `S`, `M`, or `L`
- `autonomous_ready`: `yes` or `no`
- `depends_on`: `-` or queue ids
- `plan`: source plan or roadmap link
- `deliverable`: concrete outcome expected from the next batch
- `verification`: commands or docs-only checks required before closing the item
- `last_verified`: absolute date of the latest manual status check

## Active Plan Metadata Schema

Curated mode can work before plan metadata is backfilled. Fully autonomous mode should require this frontmatter on still-open plans:

```yaml
---
id: JN-RUNTIME-PLAN-001
status: ready
priority: P1
size: S
autonomous_ready: true
depends_on: []
last_verified: 2026-03-17
verification:
  - bash jazz-next/scripts/test-warning-config.sh
deliverable: docs/plans/2026-03-17-jazz-next-runtime-architecture.md
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
5. The team can trust `autonomous_ready: yes` as a real signal, not a guess.

Switch to the autonomous prompt when:

- the top executable item is unambiguous from `queue.md`,
- at least one high-priority entry is marked `autonomous_ready: yes`,
- linked plans are detailed enough that the executor does not need a broad docs scan to act safely.

## Next Hardening Step

The next improvement after adopting this layout is a small validation script, for example `scripts/check-execution-queue.sh`, that checks:

- required queue columns exist,
- every dependency id resolves,
- every linked plan path exists,
- `last_verified` is present on non-done items.
