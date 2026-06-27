# Spec-Cleanup Item #6: Parse-Only Feature Resolution Closure

> **Reference-only closure:** This legacy cleanup plan is closed for active
> execution. Do not execute its old `jazz-hs` parser/analyzer/codegen phases as
> new work.

## Goal

Preserve the historical parse-only cleanup evidence while making the active
`jazz-next` routing rule explicit: a parser-only or parse-mostly feature may
return to execution only through its own concrete active-path contract.

## Closure Summary

Completed by `JN-PARSE-ONLY-ACTIVE-MATRIX-001` on `2026-06-24`.

The original plan targeted `jazz-hs` internals:

- `jazz-hs/src/AST.hs`
- `jazz-hs/src/Parser/Lang.hs`
- `jazz-hs/src/Parser/Lib.hs`
- `jazz-hs/src/Analyzer/TypeInference.hs`
- `jazz-hs/src/CodeGen/Javascript.hs`
- `jazz-hs/test/*`
- Nix/Stack/Node matrix scripts for that legacy compiler

Those targets are not executor-safe under the current workspace policy. Active
compiler implementation belongs in `jazz-next/`; `jazz-hs/` and `jazz2/` remain
read-only historical references unless a user explicitly asks for legacy
maintenance.

## Active-Path Matrix Check

| Candidate surface | Active-path status | Routing decision |
| --- | --- | --- |
| Module/import syntax | Current v1 parser, resolver, loader, CLI, and migration harnesses are implemented and documented for `jazz-next`. Future module/std-lib growth is blocked behind `JN-MODULE-REBASE-PLAN-001` until a concrete API/runtime contract is named. | Do not promote parse-only implementation work from this legacy plan. |
| Stage 2 fixed-tier operator declarations | Parser/fixity metadata is intentionally parser-only for the completed Stage 2 slice. Runtime operator semantics remain blocked behind `JN-USER-DEFINED-OPERATORS-PLAN-001`. | Keep future operator execution work under the operator blocker, not this cleanup item. |
| Legacy `jazz-hs` parse-only forms | Historical evidence only. | Do not edit `jazz-hs/` or revive JS codegen/type-inference parity work. |

No standalone active `jazz-next` parse-only feature was named by the target
docs as implementation-ready. The correct resolution is to close this legacy
cleanup path and leave future work behind the more specific active blockers
that own those semantics.

## Non-Executable Historical Work

The old plan phases are retained only as historical context. In particular, do
not promote work from this file to:

- remove or implement legacy `jazz-hs` AST/parser branches,
- add legacy `jazz-hs` analyzer or JavaScript codegen support,
- create Nix/Stack matrix scripts for `jazz-hs`,
- run a broad parser parity project,
- route operator runtime semantics outside the operator blocker,
- reopen closed module/import harness work without a concrete product delta.

## Active Follow-Up Routing

Future parser-only or parser-mostly surfaces should be handled by their owning
active blocker:

- module/import and stdlib/catalog growth:
  `JN-MODULE-REBASE-PLAN-001`;
- user-defined operator execution:
  `JN-USER-DEFINED-OPERATORS-PLAN-001`;
- pattern forms:
  `JN-PATTERN-FUTURE-FORMS-PLAN-001`;
- type grammar and solver behavior:
  `JN-TYPE-GRAMMAR-CLOSURE-PLAN-001`.

If none of those contracts names a concrete active implementation slice, keep
`Ready Now` empty and seed the next source-backed curation target instead.

## Verification

- [x] Checked the active feature-status matrix for remaining parse-only labels.
- [x] Checked current module/import specs and execution queue blockers.
- [x] `bash scripts/check-execution-queue.sh`
- [x] `bash scripts/check-docs.sh`
- [x] `git diff --check`
