# Execution Dispatch Design

**Problem:** The current "implement the next documented feature" prompt spends too much context reconstructing priority, dependency, and execution status from `docs/feature-status.md`, roadmap docs, and historical plan files. That cost grows as plan history accumulates.

**Decision:** Introduce a small canonical dispatch layer under `docs/execution/`. The queue becomes the only file an execution prompt reads first. Long-form plans remain the detail layer, and status/roadmap docs remain supporting references rather than dispatch inputs.

**Curated-first model:** Start with a maintainer-curated `docs/execution/queue.md` that keeps only a few verified `Ready Now` entries. Each entry links to the relevant plan and states whether it is safe for autonomous selection.

**Promotion path to autonomous:** Add lightweight metadata to open plans, keep dependency information explicit in the queue, and switch the executor prompt to select the highest-priority `autonomous_ready: yes` item once the queue is stable.

## Operating Rules

1. `docs/execution/queue.md` is the dispatch source of truth.
2. `docs/feature-status.md` is status truth, not a work queue.
3. `docs/plans/*` contain detail and historical receipts, not global priority.
4. The executor should read only the selected queue entry and its linked plans unless that material is insufficient.
5. `Ready Now` should stay small enough that an agent can evaluate it cheaply.

## Immediate Migration

- Create `docs/execution/README.md` with queue rules, plan-metadata schema, and promotion criteria.
- Create `docs/execution/queue.md` with a short initial set of verified candidate batches.
- Store curated and autonomous execution prompts beside the queue so prompt text stops drifting across sessions.
- Backfill plan metadata only for still-open plans; leave the archive alone.

## Success Criteria

- A future executor can choose the next batch by reading `docs/execution/queue.md` plus at most one linked plan.
- Historical plan files no longer need to be searched just to determine priority.
- Switching from curated mode to autonomous mode requires only prompt selection and metadata hygiene, not a second doc restructure.
