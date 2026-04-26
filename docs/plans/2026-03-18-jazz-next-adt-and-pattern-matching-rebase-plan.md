---
id: JN-ADT-CONSTRUCTOR-SEM-001
status: done
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-26
plan_section: "Milestone 4 / Batch 2: Invalid constructor application runtime diagnostics"
target_paths:
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Runtime constructor over-application emits deterministic E3023 diagnostics with constructor name and expected/received arity, closing the invalid constructor-application follow-up."
supersedes:
  - docs/plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md
---

# Jazz-Next ADT And Pattern-Matching Rebase Plan

> Active-path replacement for `docs/plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md`. New ADT/pattern planning and execution work belongs in `jazz-next/`; the older `11` plan remains reference-only because its execution targets are `jazz-hs` files.

**Goal:** move ADT and pattern-matching work onto the active `jazz-next` parser/AST/type/runtime pipeline, define a staged implementation path for `data`, constructor values, and `case` matching, and restore a concrete executable queue item for this domain.

**Architecture:** rebase ADT/pattern work in vertical slices against the existing `jazz-next` interpreter path. Lock the active-path semantics contract first, then extend parser/AST/lowering, then analyzer/type semantics, then runtime execution and diagnostics, while keeping the current boolean-branch `if` path stable throughout the migration.

**Tech Stack:** Haskell modules under `jazz-next/src/JazzNext/Compiler`, `runghc` suites under `jazz-next/test/JazzNext/Compiler` invoked via `bash jazz-next/scripts/runghc.sh`, docs/spec updates under `docs/`, repo-root verification via `bash jazz-next/scripts/test-warning-config.sh`.

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
- [x] Landed constructor and bracketed-list pattern parsing/lowering in the active `jazz-next` surface/core AST, added accepted/rejected parser coverage plus a constructor-arm boundary regression case, and kept constructor/list type/runtime execution explicitly deferred behind placeholder diagnostics.
- [x] On `2026-04-13`, narrowed the next executable queue target to a single Milestone 2 parser/lowering batch for `data` declarations before any constructor typing/runtime follow-up.
- [x] On `2026-04-13`, landed canonical `data` declaration parsing with dedicated surface/core statement nodes and constructor arity metadata preserved through lowering, plus parser rejection coverage for malformed declaration forms.
- [x] Milestone 2 complete: parser, surface AST, core AST, and lowering represent the agreed ADT/case/pattern forms, and the linked repo-local verification commands rerun cleanly.
- [x] On `2026-04-24`, landed constructor analyzer visibility and expression-position constructor type signatures for `data` declarations while leaving constructor/list pattern semantics deferred.
- [x] On `2026-04-24`, landed constructor pattern type semantics for declared constructors, including payload binder typing, unknown-constructor diagnostics, and arity diagnostics, before the later bracketed-list typing batch closed Milestone 3.
- [x] On `2026-04-24`, landed bracketed-list pattern type semantics, including list-scrutinee checks, element binder typing, and ordinary branch result agreement, before the later runtime matching batch.
- [x] Milestone 3 complete: analyzer/type semantics cover data declarations, constructors, and branch-local pattern bindings.
- [x] On `2026-04-24`, landed first-match runtime evaluation for declared constructor patterns and exact-length bracketed-list patterns, including nested binder propagation.
- [x] On `2026-04-26`, landed arity-specific `E3023` runtime diagnostics for constructor over-application, including direct runtime-helper coverage for the invalid application path that source type checking normally rejects first.
- [x] Milestone 4 complete: runtime execution supports constructor values and pattern-matching evaluation with deterministic diagnostics.
- [ ] Milestone 5 complete: docs, roadmap, and queue state close the rebase and future work no longer points at legacy `11`.

## Current State (after invalid constructor application diagnostics batch)

- `jazz-next/src/JazzNext/Compiler/AST.hs` now carries `Pattern`, `CaseArm`, and `EPatternCase`, including `PConstructor` and `PList`; the older `ECase Expr Expr Expr` remains the internal boolean branch form used after `if` desugaring.
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`, `Parser.hs`, and `Parser/Lexer.hs` now accept canonical top-level `data <TypeName> = <Ctor> | <Ctor> ... .` declarations into dedicated statement nodes while continuing to parse `case <expr> { | <pattern> -> <expr> ... }` with literal, wildcard, variable, uppercase-constructor, and bracketed-list patterns.
- `jazz-next/src/JazzNext/Compiler/AST.hs` and `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs` now preserve data constructor names and arities via dedicated core declaration metadata while keeping the existing boolean-only `ECase` contract and richer `EPatternCase` lowering behavior unchanged.
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs` and `TypeInference.hs` now keep nested pattern binders visible to arm bodies, register `data` constructors as visible names, typecheck constructor values/applications in expression positions, typecheck declared constructor patterns against ADT scrutinees with payload binders scoped to arm bodies, and typecheck bracketed-list patterns against list scrutinees with element binders scoped to arm bodies.
- `jazz-next/src/JazzNext/Compiler/Runtime.hs` preserves existing boolean `ECase` execution while evaluating literal / wildcard / variable / constructor / bracketed-list `EPatternCase` arms and constructor values/applications; constructor over-application now emits deterministic `E3023` diagnostics with the constructor name and expected/received arity.
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs` now covers constructor patterns, bracketed list patterns, malformed list syntax, and constructor-arm `|` boundary handling in addition to the previously landed simple-pattern cases.
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs` and `AdtPatternRuntimeSpec.hs` now cover the committed typed/runtime pattern subset and run from the default `bash jazz-next/scripts/test-warning-config.sh` path.
- `docs/spec/adt-pattern-semantics.md` and `docs/spec/pattern-matching-semantics.md` now lock the parser/core constructor-list slice while keeping full ADT/type/runtime semantics staged.

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
| Surface parse | `jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs` | Supports canonical top-level `data` declarations plus surface `case` with literal, wildcard, variable, uppercase-constructor, and bracketed-list patterns. | Reuse the parser-owned declaration and pattern nodes when constructor-expression parsing and semantic registration land. |
| Core AST + lowering | `jazz-next/src/JazzNext/Compiler/AST.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`, `jazz-next/src/JazzNext/Compiler/Desugar.hs` | Carries `EPatternCase`, `PConstructor` / `PList`, and dedicated `SData` declaration metadata with constructor arities; `ECase` remains bool-only for `if`. | Consume data declarations and richer pattern forms in later analyzer/runtime milestones without regressing `if`. |
| Binding/type semantics | `jazz-next/src/JazzNext/Compiler/Analyzer.hs`, `jazz-next/src/JazzNext/Compiler/TypeInference.hs` | Supports branch-local binder visibility for nested pattern shapes, constructor expression typing, declared constructor pattern typing, and bracketed-list pattern typing. | Milestone 3 type semantics are complete for the currently represented constructor/list pattern forms. |
| Runtime execution | `jazz-next/src/JazzNext/Compiler/Runtime.hs`, `jazz-next/src/JazzNext/Compiler/Driver.hs` | Preserves bool-only `ECase` execution while evaluating literal / wildcard / variable / constructor / bracketed-list `EPatternCase` arms plus constructor values/applications. | Milestone 4 runtime semantics are complete for the active ADT/pattern subset, including deterministic invalid constructor over-application diagnostics. |
| Active verification | `jazz-next/test/JazzNext/Compiler/Parser/*.hs`, `jazz-next/test/JazzNext/Compiler/Semantics/*.hs`, `jazz-next/test/JazzNext/CLI/CLISpec.hs` | Parser coverage includes constructor/list forms and case-boundary regressions; semantic coverage now includes constructor values/applications, constructor and bracketed-list pattern typing, constructor/list pattern runtime matching, and invalid constructor over-application runtime diagnostics. | Keep future ADT/pattern work in focused `jazz-next` suites before broadening the default warning-config run. |

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
- [x] Add constructor patterns and bracketed list patterns to the active-path surface/core representation.
- [x] Add `data` declarations to the active-path surface/core representation.

#### Batch 1: Data declaration surface/core AST and lowering

This batch landed on `2026-04-13`. Constructor typing/runtime work stays out of scope until a narrower Milestone 3 semantic batch is selected.

- [x] Reserve and lex `data` on the active path, then parse canonical top-level `data <TypeName> = <Ctor> | <Ctor> ... .` declarations into dedicated surface statement nodes.
- [x] Extend the core AST and `Parser/Lower.hs` so constructor names and arities survive lowering without changing current `EPatternCase` typing/runtime behavior.
- [x] Add accepted and rejected declaration coverage in `AdtPatternParserSpec.hs` for canonical declarations, empty constructor lists, malformed `|` placement, and missing terminators.

Batch 1 files:

- `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs`

Batch 1 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

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
- [x] Register data declarations and constructor signatures in active-path semantic environments.
- [x] Typecheck constructor value/application arity in expression positions.
- [x] Extend pattern type semantics to declared constructor patterns.
- [x] Extend pattern type semantics to bracketed-list patterns.
- [x] Add dedicated semantic coverage for the committed constructor-signature slice.

#### Batch 1: Constructor visibility and expression type signatures

This batch landed on `2026-04-24`. It stops at compile-time constructor visibility/type signatures; runtime constructor values and constructor/list pattern typing remain queued follow-up work.

- [x] Register `data` constructors as analyzer-visible names after their declaration.
- [x] Add first-order constructor result types in `TypeInference.hs`, with fresh argument placeholders per constructor use.
- [x] Add source-pipeline coverage for constructor values, constructor application, and over-applied nullary constructors in `AdtPatternTypeSpec.hs`.

Batch 1 files:

- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`

Batch 1 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

Primary files:

- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`

#### Batch 2: Constructor pattern typing

This batch landed on `2026-04-24`. It adds compile-time constructor pattern semantics for declared `data` constructors while leaving list pattern typing and runtime constructor/list pattern matching queued as separate follow-up work.

- [x] Typecheck constructor patterns against the scrutinee ADT type using constructor metadata registered from preceding `data` declarations.
- [x] Bind constructor payload variables with fresh arm-local types so arm bodies can typecheck against payload usage.
- [x] Reject unknown constructor patterns and constructor pattern arity mismatches with deterministic `E2011` diagnostics.
- [x] Include declared constructor arms in ordinary `E2012` branch result agreement checks.

Batch 2 files:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`

Batch 2 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 3: Bracketed-list pattern typing

This batch landed on `2026-04-24`. It adds compile-time type semantics for bracketed-list patterns; runtime matching landed in the later Milestone 4 batch.

- [x] Typecheck bracketed-list patterns against list scrutinee types.
- [x] Bind list element variables with the scrutinee element type so arm bodies can typecheck against element usage.
- [x] Reject bracketed-list patterns for non-list scrutinees with deterministic `E2011` diagnostics.
- [x] Include bracketed-list arms in ordinary `E2012` branch result agreement checks.

Batch 3 files:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`

Batch 3 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

### Milestone 4: Implement runtime constructor values and case matching

- [x] Thread `EPatternCase` through runtime dependency helpers and emit deterministic `E3022` diagnostics when evaluation reaches unsupported pattern matching.
- [x] Implement first-match pattern evaluation for the agreed literal / wildcard / variable subset.
- [x] Emit deterministic runtime diagnostics when the committed simple-pattern subset matches no arms.
- [x] Add dedicated runtime coverage and thread the new cases through the active-path default verification script.
- [x] Add runtime value representation for constructors and constructed data.
- [x] Extend first-match pattern evaluation to constructor/list patterns.
- [x] Emit deterministic runtime diagnostics for invalid constructor application paths.

Primary files:

- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`
- `jazz-next/test/JazzNext/CLI/CLISpec.hs`

#### Batch 1: Constructor and list runtime pattern matching

This batch landed on `2026-04-24`. It adds runtime first-match pattern evaluation for the constructor/list pattern forms already represented and typed by the active pipeline.

- [x] Match saturated constructor values against constructor patterns with the same constructor name and payload arity.
- [x] Bind constructor payload variables recursively in selected arm bodies.
- [x] Match bracketed-list patterns against exact-length runtime lists.
- [x] Bind list element variables recursively in selected arm bodies while preserving no-match fallback behavior for non-matching shapes.

Batch 1 files:

- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs`

Batch 1 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 2: Invalid constructor application runtime diagnostics

This batch landed on `2026-04-26`. It closes the Milestone 4 runtime diagnostic follow-up for constructor application paths that source type checking normally rejects before evaluation.

- [x] Preserve deterministic `E3023` for runtime constructor over-application.
- [x] Include the constructor name plus expected and received argument counts in the runtime diagnostic text.
- [x] Add direct runtime-helper coverage for an over-applied constructor AST so the invalid runtime path remains covered independently of source type checking.

Batch 2 files:

- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Batch 2 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

### Milestone 5: Verification and tracker closure

- [ ] Update language-state, README, runtime roadmap, and queue metadata to point future work at the active-path plan and executed milestones.
- [ ] Replace remaining references that imply legacy `11` is an execution target.
- [ ] Run focused parser/type/runtime/CLI checks plus the active-path default verification script as milestones land.

Suggested verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

## Definition of Done

- [ ] Active ADT/pattern work targets only `jazz-next` files, tests, and docs.
- [x] `jazz-next` parser, AST/lowering, analyzer/type, and runtime agree on one ADT/case/pattern representation for the committed simple slice.
- [x] Docs describe the implemented active-path subset and explicit non-goals.
- [ ] Queue, roadmap, and runtime-plan linkage no longer point new ADT work at legacy `jazz-hs` execution files.
