# Execution Queue

Read this file before scanning the rest of `docs/`. It is the dispatch source of truth for next-batch execution.

## Ready Now

| id | title | priority | size | autonomous_ready | depends_on | plan | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |

No ready items currently.

## Blocked

| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |
| `JN-ADT-REBASE-001` | Rebase ADT and pattern-matching work onto `jazz-next` | `active-path plan rewrite` | `JN-RUNTIME-PLAN-001` is now closed, but the ADT plan itself has not yet been re-verified or rewritten against the new `jazz-next` runtime architecture. | [`docs/plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md`](../plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md) | `2026-03-18` |

## Done

Move completed items here briefly with closure evidence, then prune aggressively once the linked plan or commit history is enough.

| id | closure evidence | completed_on |
| --- | --- | --- |
| `JN-STDLIB-CLOSE-001` | Re-verified the linked Phase-5 closure plan against the active `jazz-next` builtin-resolution path, ran `bash jazz-next/scripts/test-warning-config.sh`, updated [`docs/spec/stdlib-boundary.md`](../spec/stdlib-boundary.md) and [`docs/jazz-language-state.md`](../jazz-language-state.md) to reflect the verified kernel-only boundary, and closed the linked plan metadata. | `2026-03-18` |
| `JN-RUNTIME-PLAN-001` | Added the active-path replacement plan at [`docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`](../plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md), updated the roadmap to point at it, and marked legacy `12a` runtime planning as reference-only. | `2026-03-18` |
| `JN-TYPE-PLAN-001` | Added the active-path replacement plan at [`docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md`](../plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md), updated active-path tracker links, and marked legacy `07` type-grammar planning as reference-only. | `2026-03-18` |

## Entry Template

```md
| `ITEM-ID` | Short task name | `P1` | `S` | `yes` | `-` | [linked-plan.md](../plans/example.md) | Concrete next-batch deliverable. | Exact verification command or docs-only check. | `YYYY-MM-DD` |
```
