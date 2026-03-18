# Execution Queue

Read this file before scanning the rest of `docs/`. It is the dispatch source of truth for next-batch execution.

## Ready Now

| id | title | priority | size | autonomous_ready | depends_on | plan | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `JN-STDLIB-CLOSE-001` | Finish stdlib boundary closure and approved-kernel reduction | `P2` | `M` | `no` | `-` | [`docs/plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md`](../plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md) | Reduce the remaining hardcoded builtin surface to the approved kernel and close the reproducibility/verification checklist. | `bash jazz-next/scripts/test-warning-config.sh` plus docs/status updates in the same change. | `2026-03-18` |

## Blocked

| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |
| `JN-ADT-REBASE-001` | Rebase ADT and pattern-matching work onto `jazz-next` | `active-path plan rewrite` | `JN-RUNTIME-PLAN-001` is now closed, but the ADT plan itself has not yet been re-verified or rewritten against the new `jazz-next` runtime architecture. | [`docs/plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md`](../plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md) | `2026-03-18` |

## Done

Move completed items here briefly with closure evidence, then prune aggressively once the linked plan or commit history is enough.

| id | closure evidence | completed_on |
| --- | --- | --- |
| `JN-RUNTIME-PLAN-001` | Added the active-path replacement plan at [`docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`](../plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md), updated the roadmap to point at it, and marked legacy `12a` runtime planning as reference-only. | `2026-03-18` |
| `JN-TYPE-PLAN-001` | Added the active-path replacement plan at [`docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md`](../plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md), updated active-path tracker links, and marked legacy `07` type-grammar planning as reference-only. | `2026-03-18` |

## Entry Template

```md
| `ITEM-ID` | Short task name | `P1` | `S` | `yes` | `-` | [linked-plan.md](../plans/example.md) | Concrete next-batch deliverable. | Exact verification command or docs-only check. | `YYYY-MM-DD` |
```
