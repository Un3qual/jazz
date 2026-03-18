# Execution Queue

Read this file before scanning the rest of `docs/`. It is the dispatch source of truth for next-batch execution.

## Ready Now

| id | title | priority | size | autonomous_ready | depends_on | plan | deliverable | verification | last_verified |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `JN-ADT-SPEC-001` | Lock active-path ADT and pattern semantics contract | `P1` | `S` | `yes` | `JN-ADT-REBASE-001` | [`docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md`](../plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md) | Publish `docs/spec/adt-pattern-semantics.md` and `docs/spec/pattern-matching-semantics.md`, document the currently-landed parser/core `case` subset plus explicit non-goals, and update plan progress/links in the same change. | `test -f docs/spec/adt-pattern-semantics.md && test -f docs/spec/pattern-matching-semantics.md && rg -n "adt-pattern-semantics\|pattern-matching-semantics" docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md docs/execution/queue.md docs/jazz-language-state.md` | `2026-03-18` |
| `JN-ADT-SIMPLE-SEM-001` | Replace temporary simple-pattern placeholders with real semantics | `P1` | `M` | `yes` | `JN-ADT-PARSER-001` | [`docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md`](../plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md) | Teach analyzer/type/runtime the committed simple-pattern subset (`literal`, `_`, variable binders), add dedicated type/runtime coverage, and retire temporary `E2011` / `E3022` placeholders for that subset while keeping constructor/list patterns deferred. | `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs && runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs && bash jazz-next/scripts/test-warning-config.sh` | `2026-03-18` |

## Blocked

| id | title | blocked_on | reason | plan | last_verified |
| --- | --- | --- | --- | --- | --- |

No blocked items currently.

## Done

Move completed items here briefly with closure evidence, then prune aggressively once the linked plan or commit history is enough.

| id | closure evidence | completed_on |
| --- | --- | --- |
| `JN-ADT-PARSER-001` | Added the first active-path `jazz-next` parser/core `case` slice with literal, wildcard, and variable patterns, lowered it into `EPatternCase`, threaded [`jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs`](../../jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs) into `bash jazz-next/scripts/test-warning-config.sh`, and added deterministic `E2011` / `E3022` placeholder diagnostics for unsupported downstream pattern-case evaluation. | `2026-03-18` |
| `JN-ADT-REBASE-001` | Added the active-path replacement plan at [`docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md`](../plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md), updated the runtime roadmap/runtime-plan links to point at it, and marked legacy `11` ADT planning as reference-only. | `2026-03-18` |
| `JN-STDLIB-CLOSE-001` | Re-verified the linked Phase-5 closure plan against the active `jazz-next` builtin-resolution path, ran `bash jazz-next/scripts/test-warning-config.sh`, updated [`docs/spec/stdlib-boundary.md`](../spec/stdlib-boundary.md) and [`docs/jazz-language-state.md`](../jazz-language-state.md) to reflect the verified kernel-only boundary, and closed the linked plan metadata. | `2026-03-18` |
| `JN-RUNTIME-PLAN-001` | Added the active-path replacement plan at [`docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`](../plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md), updated the roadmap to point at it, and marked legacy `12a` runtime planning as reference-only. | `2026-03-18` |
| `JN-TYPE-PLAN-001` | Added the active-path replacement plan at [`docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md`](../plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md), updated active-path tracker links, and marked legacy `07` type-grammar planning as reference-only. | `2026-03-18` |

## Entry Template

```md
| `ITEM-ID` | Short task name | `P1` | `S` | `yes` | `-` | [linked-plan.md](../plans/example.md) | Concrete next-batch deliverable. | Exact verification command or docs-only check. | `YYYY-MM-DD` |
```
