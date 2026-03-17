# Execution Queue

Read this file before scanning the rest of `docs/`. It is the dispatch source of truth for next-batch execution.

## Ready Now

| id | title | priority | size | autonomous_ready | depends_on | plan | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `JN-RUNTIME-PLAN-001` | Write a `jazz-next` runtime architecture and interpreter execution plan | `P1` | `S` | `yes` | `-` | [`docs/plans/2026-03-03-jazz-interpreter-roadmap.md`](../plans/2026-03-03-jazz-interpreter-roadmap.md) | Create an active-path plan that replaces legacy-targeted runtime guidance and defines the `jazz-next` runtime pipeline, milestones, and dependency map. | Docs-only review plus queue/roadmap linkage update. | `2026-03-17` |
| `JN-TYPE-PLAN-001` | Rebase the type-grammar plan onto the active `jazz-next` path | `P2` | `M` | `yes` | `-` | [`docs/plans/spec-clarification/2026-03-02/type-system/07-type-grammar-and-arrow-associativity.md`](../plans/spec-clarification/2026-03-02/type-system/07-type-grammar-and-arrow-associativity.md) | Create a new `jazz-next`-targeted plan or explicit replacement note so future execution stops pointing at `jazz-hs` files for new type work. | Docs-only review confirming the replacement plan points only at active-path implementation files. | `2026-03-17` |
| `JN-STDLIB-CLOSE-001` | Finish stdlib boundary closure and approved-kernel reduction | `P2` | `M` | `no` | `-` | [`docs/plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md`](../plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md) | Reduce the remaining hardcoded builtin surface to the approved kernel and close the reproducibility/verification checklist. | `bash jazz-next/scripts/test-warning-config.sh` plus docs/status updates in the same change. | `2026-03-17` |

## Blocked

| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |
| `JN-ADT-REBASE-001` | Rebase ADT and pattern-matching work onto `jazz-next` | `JN-RUNTIME-PLAN-001` | The current plan is still written against `jazz-hs` interpreter files, and the active-path runtime architecture is not yet captured in a replacement plan. | [`docs/plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md`](../plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md) | `2026-03-17` |

## Done

Move completed items here briefly with closure evidence, then prune aggressively once the linked plan or commit history is enough.

## Entry Template

```md
| `ITEM-ID` | Short task name | `P1` | `S` | `yes` | `-` | [linked-plan.md](../plans/example.md) | Concrete next-batch deliverable. | Exact verification command or docs-only check. | `YYYY-MM-DD` |
```
