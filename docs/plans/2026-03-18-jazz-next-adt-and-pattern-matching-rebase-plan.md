---
id: JN-ADT-CONSTR-LIST-PARSER-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-03-19
plan_section: "Milestone 2: Rebase parser, surface AST, core AST, and lowering"
target_paths:
  - jazz-next/src/JazzNext/Compiler/Parser.hs
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
  - jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
  - jazz-next/test/JazzNext/Compiler/Parser/IfExpressionParserSpec.hs
verification:
  - runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
  - runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/IfExpressionParserSpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
deliverable: "Surface and core pattern representations accept constructor and list patterns in `case` arms, lowering preserves them into `EPatternCase`, and parser coverage locks accepted/rejected forms without regressing existing `if` / `case` parsing boundaries."
supersedes: []
---

# Jazz-Next ADT And Pattern-Matching Rebase Plan

> Active-path replacement for `docs/plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md`. New ADT/pattern planning and execution work belongs in `jazz-next/`; the older `11` plan remains reference-only because its execution targets are `jazz-hs` files.

**Goal:** move ADT and pattern-matching work onto the active `jazz-next` parser/AST/type/runtime pipeline, define a staged implementation path for `data`, constructor values, and `case` matching, and restore a concrete executable queue item for this domain.

**Architecture:** rebase ADT/pattern work in vertical slices against the existing `jazz-next` interpreter path. Lock the active-path semantics contract first, then extend parser/AST/lowering, then analyzer/type semantics, then runtime execution and diagnostics, while keeping the current boolean-branch `if` path stable throughout the migration.

**Tech Stack:** Haskell modules under `jazz-next/src/JazzNext/Compiler`, `runghc` suites under `jazz-next/test/JazzNext/Compiler`, docs/spec updates under `docs/`, repo-root verification via `bash jazz-next/scripts/test-warning-config.sh`.

---

## Plan Progress

- [x] Verified the legacy `11` plan is still execution-bound to `jazz-hs`.
- [x] Verified active `jazz-next` surface and core ASTs do not yet model `data`, surface `case`, or general patterns.
- [x] Verified current `ECase`, `TypeInference`, and `Runtime` logic only support the boolean-branch semantics inherited from `if`.
- [x] Captured the active-path owner map and replacement-plan scope for `JN-ADT-REBASE-001`.
- [x] Landed the first parser/core `case` slice: surface `case` with literal, wildcard, and variable patterns now lowers to `EPatternCase` without regressing `if`.
- [x] Added `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs` and threaded it into `bash jazz-next/scripts/test-warning-config.sh`.
- [x] Added temporary analyzer/type/runtime plumbing so unsupported `EPatternCase` nodes surface deterministic `E2011` / `E3022` diagnostics instead of falling through shared traversals.
- [x] Published `docs/spec/adt-pattern-semantics.md` and `docs/spec/pattern-matching-semantics.md` to lock the active-path subset and explicit non-goals.
- [x] Milestone 1 complete: core ADT/pattern semantics docs and executable subset are locked for `jazz-next`.
- [x] Replaced the temporary simple-pattern placeholders with real type/runtime semantics for literal, wildcard, and variable patterns, added dedicated type/runtime suites, and threaded them into active verification.
- [ ] Milestone 2 complete: parser, surface AST, core AST, and lowering represent the agreed ADT/case/pattern forms.
- [ ] Milestone 3 complete: analyzer/type semantics cover data declarations, constructors, and branch-local pattern bindings.
- [ ] Milestone 4 complete: runtime execution supports constructor values and pattern-matching evaluation with deterministic diagnostics.
- [ ] Milestone 5 complete: docs, roadmap, and queue state close the rebase and future work no longer points at legacy `11`.

## Current State (after first parser slice)

- `jazz-next/src/JazzNext/Compiler/AST.hs` now carries `Pattern`, `CaseArm`, and `EPatternCase`; the older `ECase Expr Expr Expr` remains the internal boolean branch form used after `if` desugaring.
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`, `Parser.hs`, and `Parser/Lexer.hs` now accept `case <expr> { | <pattern> -> <expr> ... }` with literal, wildcard, and variable patterns only; `data`, constructor patterns, and list patterns are still absent on the active path.
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs` and `jazz-next/src/JazzNext/Compiler/Desugar.hs` preserve the new `EPatternCase` node without changing the existing boolean-only `ECase` contract.
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs` and `TypeInference.hs` now keep variable-pattern binders in branch-local scope, typecheck literal compatibility against the scrutinee, and enforce one result type across the committed simple-pattern subset.
- `jazz-next/src/JazzNext/Compiler/Runtime.hs` preserves existing boolean `ECase` execution while evaluating literal, wildcard, and variable-pattern `EPatternCase` arms with deterministic no-match diagnostics.
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs` now covers accepted/rejected parser forms and runs from the default `bash jazz-next/scripts/test-warning-config.sh` path.
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs` and `AdtPatternRuntimeSpec.hs` now cover the committed simple-pattern subset and run from the default `bash jazz-next/scripts/test-warning-config.sh` path.
- `docs/spec/adt-pattern-semantics.md` and `docs/spec/pattern-matching-semantics.md` now lock the Milestone-1 contract around the landed literal / wildcard / variable `case` subset and the deferred ADT/pattern forms.

## Scope Guardrails

In scope:

- rebase ADT/pattern planning onto `jazz-next` files, tests, and docs
- `data` declarations, constructor values, and `case` pattern matching as the release-critical core slice
- deterministic compile-time and runtime diagnostics for constructor lookup, branch binding, and no-match behavior
- docs/spec work that explicitly defines the active-path subset and staging

Out of scope for the first executable slices:

- advanced exhaustiveness analysis beyond deterministic first-match/no-match diagnostics
- GADT-like semantics, guards, or-patterns, and pattern synonyms
- tuple-pattern runtime support until tuple-value ownership is explicitly planned on the active path
- JS backend parity or any new work under `jazz-hs/` or `jazz2/`

## Active-Path Owner Map

| stage | current owner files | current behavior | required rebase outcome |
| --- | --- | --- | --- |
| Surface parse | `jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs` | Supports surface `case` with literal, wildcard, and variable patterns; no `data`, constructor, or list-pattern parsing yet. | Extend the parser-owned nodes to cover the rest of the agreed ADT/pattern surface with deterministic diagnostics. |
| Core AST + lowering | `jazz-next/src/JazzNext/Compiler/AST.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`, `jazz-next/src/JazzNext/Compiler/Desugar.hs` | Carries `EPatternCase` plus simple `Pattern` / `CaseArm`; `ECase` remains bool-only for `if`. | Expand core forms to cover constructor/list patterns and eventual constructor values without regressing `if`. |
| Binding/type semantics | `jazz-next/src/JazzNext/Compiler/Analyzer.hs`, `jazz-next/src/JazzNext/Compiler/TypeInference.hs` | Supports branch-local variable binders, literal-pattern/scrutinee agreement, and branch-result type agreement for literal / wildcard / variable patterns. | Extend the same pipeline to data declarations, constructor bindings, and constructor/list patterns. |
| Runtime execution | `jazz-next/src/JazzNext/Compiler/Runtime.hs`, `jazz-next/src/JazzNext/Compiler/Driver.hs` | Preserves bool-only `ECase` execution while evaluating literal / wildcard / variable `EPatternCase` arms and emitting deterministic no-match diagnostics. | Add constructor runtime values, case dispatch for constructor/list patterns, and constructor-specific diagnostics in the same interpreter pipeline. |
| Active verification | `jazz-next/test/JazzNext/Compiler/Parser/*.hs`, `jazz-next/test/JazzNext/Compiler/Semantics/*.hs`, `jazz-next/test/JazzNext/CLI/CLISpec.hs` | Parser coverage exists via `AdtPatternParserSpec`; dedicated type/runtime suites now exist via `AdtPatternTypeSpec` and `AdtPatternRuntimeSpec`, and all run from the default script. | Extend the same verification path as constructor/list support lands. |

## Dependency Map

| dependency | why it matters | what it unlocks |
| --- | --- | --- |
| `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md` | The runtime plan already names this domain as Milestone 4 work and identifies the active owner files. | Keeps ADT/pattern implementation attached to the active interpreter pipeline. |
| `docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md` | Constructor typing and pattern typing should consume active-path type structures as they evolve. | Avoids wiring ADT semantics into a temporary representation. |
| Current `if` -> `ECase` desugaring in `jazz-next/src/JazzNext/Compiler/Desugar.hs` | Existing control-flow already relies on the current boolean `ECase` form. | Defines the migration constraint when general case forms land. |
| Tuple follow-up still unresolved in `docs/jazz-language-state.md` | Tuple values/patterns are not part of the current active runtime subset. | Lets the first implementation slices stay focused on constructor/list/literal patterns. |

## Milestone Plan

### Milestone 1: Lock the active-path semantics contract

- [x] Create `docs/spec/adt-pattern-semantics.md` and `docs/spec/pattern-matching-semantics.md`.
- [x] Define the currently committed `jazz-next` `case` slice: literal, wildcard, and variable patterns lower to `EPatternCase`, while `data`, constructor application, and constructor/list patterns remain queued follow-up work.
- [x] Explicitly defer tuple patterns and lambda-pattern parameters until the shared pattern engine exists and tuple ownership is planned.
- [x] Link the new docs from this plan and the relevant roadmap/status trackers.

Primary files:

- `docs/spec/adt-pattern-semantics.md`
- `docs/spec/pattern-matching-semantics.md`
- `docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md`
- `docs/jazz-language-state.md`

Suggested verification:

```bash
test -f docs/spec/adt-pattern-semantics.md
test -f docs/spec/pattern-matching-semantics.md
rg -n "adt-pattern-semantics|pattern-matching-semantics" \
  docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md \
  docs/execution/queue.md \
  docs/jazz-language-state.md
```

### Milestone 2: Rebase parser, surface AST, core AST, and lowering

- [x] Introduce parser-facing nodes for the first general `case`-arm slice with literal, wildcard, and variable patterns.
- [x] Extend core AST, lowering, and desugaring with `EPatternCase` without regressing existing `if` handling.
- [x] Add dedicated parser coverage for accepted and rejected active-path `case` forms and thread it into `bash jazz-next/scripts/test-warning-config.sh`.
- [ ] Add `data` declarations, constructor patterns, and list patterns to the active-path surface/core representation.

Primary files:

- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/Desugar.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/IfExpressionParserSpec.hs`

### Milestone 3: Add analyzer/type semantics

- [x] Thread `EPatternCase` through analyzer/type traversals and emit deterministic `E2011` diagnostics until real semantics land.
- [x] Implement pattern-binding scope rules and branch-local visibility for the committed literal / wildcard / variable subset.
- [x] Typecheck `case` branch result agreement for the committed simple-pattern subset.
- [ ] Register data declarations and constructor signatures in active-path semantic environments.
- [ ] Typecheck constructor application, then extend pattern semantics to constructor/list patterns.
- [ ] Add dedicated semantic coverage for the committed slice.

Primary files:

- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`

### Milestone 4: Implement runtime constructor values and case matching

- [x] Thread `EPatternCase` through runtime dependency helpers and emit deterministic `E3022` diagnostics when evaluation reaches unsupported pattern matching.
- [x] Implement first-match pattern evaluation for the agreed literal / wildcard / variable subset.
- [x] Emit deterministic runtime diagnostics when the committed simple-pattern subset matches no arms.
- [x] Add dedicated runtime coverage and thread the new cases through the active-path default verification script.
- [ ] Add runtime value representation for constructors and constructed data.
- [ ] Extend first-match pattern evaluation to constructor/list patterns.
- [ ] Emit deterministic runtime diagnostics for invalid constructor application paths.

Primary files:

- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`
- `jazz-next/test/JazzNext/CLI/CLISpec.hs`

### Milestone 5: Verification and tracker closure

- [ ] Update language-state, README, runtime roadmap, and queue metadata to point future work at the active-path plan and executed milestones.
- [ ] Replace remaining references that imply legacy `11` is an execution target.
- [ ] Run focused parser/type/runtime/CLI checks plus the active-path default verification script as milestones land.

Suggested verification:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

## Definition of Done

- [ ] Active ADT/pattern work targets only `jazz-next` files, tests, and docs.
- [x] `jazz-next` parser, AST/lowering, analyzer/type, and runtime agree on one ADT/case/pattern representation for the committed simple slice.
- [x] Docs describe the implemented active-path subset and explicit non-goals.
- [ ] Queue, roadmap, and runtime-plan linkage no longer point new ADT work at legacy `jazz-hs` execution files.
