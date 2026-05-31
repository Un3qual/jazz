---
id: JN-ADT-GENERIC-CONSTRUCTOR-TYPES-PLAN-001
status: blocked
priority: P2
size: S
kind: docs
autonomous_ready: no
depends_on: []
last_verified: 2026-05-31
plan_section: "Future generic ADT and pattern-form contracts"
target_paths:
  - docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md
  - docs/execution/queue.md
verification:
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Define named ADT type parameters, fresh per-use constructor type schemes, the monomorphic binding boundary, diagnostics, target paths, and focused verification before expanding beyond the closed monomorphic ADT/pattern subset."
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
- [x] On `2026-05-22`, locked tuple-shaped case patterns to an explicit parser diagnostic while keeping tuple-pattern semantics deferred.
- [x] On `2026-05-23`, recorded tuples as an active core runtime feature and landed tuple literals, concrete tuple signature types, heterogenous tuple inference, and runtime tuple rendering.
- [x] On `2026-05-25`, landed fixed-arity tuple case pattern semantics across parser/core AST, lowering, analyzer/module reference traversal, type inference, and runtime matching.
- [x] On `2026-05-25`, landed cons-like list case pattern semantics across parser/core AST, lowering, analyzer/module reference traversal, type inference, and runtime matching.
- [x] On `2026-05-25`, landed lambda parameter pattern semantics by lowering pattern-shaped parameters through ordinary unary lambdas and internal single-arm `EPatternCase` bodies.
- [x] Milestone 5 complete: docs, roadmap, and queue state close the rebase and future work no longer points at legacy `11`.
- [x] On `2026-05-30`, closed the active ADT/pattern rebase metadata around the implemented constructor/list/tuple/lambda-parameter pattern subset and kept generic ADT type schemes plus future pattern forms blocked as separate active-path planning items.
- [x] On `2026-05-31`, recorded the next future-contract decisions: the first generic ADT slice is constructor type schemes with fresh per-use instantiation only, and the first additional pattern form is `name @ pattern` as-patterns.

## Current State (after lambda-parameter pattern semantics)

- `jazz-next/src/JazzNext/Compiler/AST.hs` now carries `Pattern`, `CaseArm`, and `EPatternCase`, including `PConstructor`, `PList`, and `PTuple`; it also carries tuple expression and concrete tuple signature nodes for active runtime values. The older `ECase Expr Expr Expr` remains the internal boolean branch form used after `if` desugaring.
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`, `Parser.hs`, and `Parser/Lexer.hs` now accept canonical top-level `data <TypeName> = <Ctor> | <Ctor> ... .` declarations into dedicated statement nodes while continuing to parse `case <expr> { | <pattern> -> <expr> ... }` with literal, wildcard, variable, uppercase-constructor, bracketed-list, cons-like list, and tuple patterns. Tuple literals such as `(1, True)` and concrete tuple signature types such as `(Int, Bool)` are parsed into structured nodes.
- `jazz-next/src/JazzNext/Compiler/AST.hs` and `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs` now preserve data constructor names and arities via dedicated core declaration metadata, lower tuple literals to core tuple values, lower tuple and cons-like list patterns to core patterns, lower concrete tuple signature types, and keep the existing boolean-only `ECase` contract unchanged.
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs` and `TypeInference.hs` now keep nested pattern binders visible to arm bodies, register `data` constructors as visible names, typecheck constructor values/applications in expression positions, typecheck declared constructor patterns against ADT scrutinees with payload binders scoped to arm bodies, typecheck bracketed-list and cons-like list patterns against list scrutinees, and typecheck tuple patterns against fixed-arity tuple scrutinees.
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs` infers tuple literals as fixed-arity heterogeneous tuple types, checks concrete tuple signatures against bindings, checks tuple pattern arity/type compatibility, and keeps tuple equality outside the strict runtime equality subset.
- `jazz-next/src/JazzNext/Compiler/Runtime.hs` preserves existing boolean `ECase` execution while evaluating literal / wildcard / variable / constructor / bracketed-list / cons-like list / tuple `EPatternCase` arms and constructor values/applications; constructor over-application now emits deterministic `E3023` diagnostics with the constructor name and expected/received arity. Runtime tuple values evaluate, render, and match in canonical `(value, value)` form.
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs` now covers constructor patterns, bracketed list patterns, cons-like list patterns, tuple patterns, malformed list syntax, and constructor-arm `|` boundary handling in addition to the previously landed simple-pattern cases.
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`, `BindingSignatureCoherenceSpec.hs`, and `RuntimeSemanticsSpec.hs` now cover tuple literal parsing/lowering, concrete tuple signature acceptance/rejection, and runtime tuple rendering.
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs` and `AdtPatternRuntimeSpec.hs` now cover the committed typed/runtime pattern subset, including cons-like list and tuple patterns, and run from the default `bash jazz-next/scripts/test-warning-config.sh` path.
- `docs/spec/adt-pattern-semantics.md` and `docs/spec/pattern-matching-semantics.md` now lock the active constructor/list/tuple/lambda-parameter pattern slice. Future work is staged behind two documented active-path contracts: generic ADT constructor schemes with fresh per-use instantiation, and `name @ pattern` as-patterns.

## Future Contract Seed: Generic ADT Constructor Schemes

This seed is not a queue entry yet. It records the first approved generic ADT
scope without pulling in the whole polymorphism/defaulting solver.

Surface contract:

- Accept named type parameters after the type constructor:

  ```jz
  data Maybe a = Nothing | Just a.
  data Pair a b = Pair a b.
  ```

- Type constructor names and value constructor names remain uppercase.
- Type parameters are lowercase identifiers scoped only to the `data`
  declaration.
- In a generic `data` declaration, constructor payload identifiers must refer
  to declared type parameters. Unknown lower-case payload names reject
  deterministically.
- Existing monomorphic declarations such as `data Box = Box value.` remain
  valid and keep their current monomorphic placeholder behavior.

Type contract:

- Each constructor receives a declaration-owned type scheme:
  - `Nothing : Maybe a`
  - `Just : a -> Maybe a`
  - `Pair : a -> b -> Pair a b`
- Constructor value use, constructor application, and constructor patterns
  instantiate the scheme with fresh type variables per use.
- Fresh constructor instantiation is the only generalized scheme behavior in
  this first batch. Ordinary bindings remain monomorphic.
- No class/defaulting solver, inferred user binding generalization, explicit
  type application syntax, higher-rank polymorphism, or runtime dispatch is in
  scope.

Diagnostics to define before queue promotion:

- duplicate type parameter names,
- generic payload names not declared as type parameters,
- arity mismatches for generic constructors,
- incompatible instantiations across one expression or pattern branch,
- constructor pattern payload binder type mismatches after instantiation.

Likely active-path target files:

- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Likely focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

## Future Contract Seed: As-Patterns

This seed is not a queue entry yet. It records the first approved additional
pattern form.

Surface contract:

- Accept `name @ pattern` in every pattern position that currently accepts the
  active pattern subset.
- `name` must be a lowercase binder.
- The right side is any currently accepted pattern form.

Binder/type/runtime contract:

- Matching first evaluates the inner pattern.
- If the inner pattern matches, `name` binds to the whole scrutinee value for
  the selected arm body.
- Nested binders from the inner pattern remain visible as they are today.
- Duplicate binders in one pattern tree reject deterministically at
  compile-time, including duplicates between the as-pattern binder and inner
  binders.
- Type inference gives the as-pattern binder the scrutinee type and reuses the
  existing inner-pattern type checks.
- Runtime matching delegates to the inner pattern and adds the whole-value
  binding only on success.

Out of scope:

- or-patterns,
- pattern guards,
- pattern synonyms,
- exhaustiveness analysis,
- match-compilation optimizations.

Likely active-path target files:

- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs`

Likely focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

## Scope Guardrails

In scope:

- rebase ADT/pattern planning onto `jazz-next` files, tests, and docs
- `data` declarations, constructor values, and `case` pattern matching as the release-critical core slice
- deterministic compile-time and runtime diagnostics for constructor lookup, branch binding, and no-match behavior
- docs/spec work that explicitly defines the active-path subset and staging

Out of scope for the first executable slices:

- advanced exhaustiveness analysis beyond deterministic first-match/no-match diagnostics
- GADT-like semantics, guards, or-patterns, and pattern synonyms
- JS backend parity or any new work under `jazz-hs/` or `jazz2/`

## Active-Path Owner Map

| stage | current owner files | current behavior | required rebase outcome |
| --- | --- | --- | --- |
| Surface parse | `jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs` | Supports canonical top-level `data` declarations plus surface `case` and lambda parameters with literal, wildcard, variable, uppercase-constructor, bracketed-list, cons-like list, and tuple patterns; tuple literals and concrete tuple signature types parse into structured nodes. | Keep future pattern work focused on new binder/type/runtime contracts for guards, or-patterns, as-patterns, and pattern synonyms. |
| Core AST + lowering | `jazz-next/src/JazzNext/Compiler/AST.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`, `jazz-next/src/JazzNext/Compiler/Desugar.hs` | Carries `EPatternCase`, `PConstructor` / `PList`, tuple expression/signature nodes, and dedicated `SData` declaration metadata with constructor arities; pattern-shaped lambda parameters lower to ordinary unary lambdas with internal single-arm pattern cases; `ECase` remains bool-only for `if`. | Future pattern work should extend the shared pattern representation without regressing `if` or ordinary unary lambda lowering. |
| Binding/type semantics | `jazz-next/src/JazzNext/Compiler/Analyzer.hs`, `jazz-next/src/JazzNext/Compiler/TypeInference.hs` | Supports branch-local binder visibility for nested pattern shapes, constructor expression typing, declared constructor pattern typing, exact-length bracketed-list pattern typing, cons-like list head/tail typing, tuple pattern typing, lambda-parameter pattern typing through the same case engine, and fixed-arity heterogeneous tuple value/signature typing. | Type semantics are complete for the active pattern subset; generic ADT type schemes and additional pattern forms need separate contracts. |
| Runtime execution | `jazz-next/src/JazzNext/Compiler/Runtime.hs`, `jazz-next/src/JazzNext/Compiler/Driver.hs` | Preserves bool-only `ECase` execution while evaluating literal / wildcard / variable / constructor / bracketed-list / cons-like list / tuple `EPatternCase` arms plus constructor values/applications, tuple values, and pattern-shaped lambda parameters lowered through internal pattern cases. | Runtime semantics are complete for the active pattern subset; future matcher forms remain staged. |
| Active verification | `jazz-next/test/JazzNext/Compiler/Parser/*.hs`, `jazz-next/test/JazzNext/Compiler/Semantics/*.hs`, `jazz-next/test/JazzNext/CLI/CLISpec.hs` | Parser coverage includes constructor/list/cons-like-list/tuple forms, tuple literals/signature types, lambda parameter patterns, and case-boundary regressions; semantic coverage now includes constructor values/applications, constructor/list/tuple pattern typing, constructor/list/tuple pattern runtime matching, lambda pattern parameters, concrete tuple signatures, tuple runtime values, and invalid constructor over-application runtime diagnostics. | Keep future ADT/pattern work in focused `jazz-next` suites before broadening the default warning-config run. |

## Dependency Map

| dependency | why it matters | what it unlocks |
| --- | --- | --- |
| `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md` | The runtime plan already names this domain as Milestone 4 work and identifies the active owner files. | Keeps ADT/pattern implementation attached to the active interpreter pipeline. |
| `docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md` | Constructor typing and pattern typing should consume active-path type structures as they evolve. | Avoids wiring ADT semantics into a temporary representation. |
| Current `if` -> `ECase` desugaring in `jazz-next/src/JazzNext/Compiler/Desugar.hs` | Existing control-flow already relies on the current boolean `ECase` form. | Defines the migration constraint when general case forms land. |
| `docs/plans/2026-03-17-jazz-next-lambda-support.md` | Lambda-parameter patterns now lower through internal single-arm pattern cases and share the active `case` binder/type/runtime contract. | Keeps lambda destructuring closed with the same pattern engine while future pattern forms stay blocked separately. |

## Milestone Plan

### Milestone 1: Lock the active-path semantics contract

- [x] Create `docs/spec/adt-pattern-semantics.md` and `docs/spec/pattern-matching-semantics.md`.
- [x] Define the first committed `jazz-next` `case` slice: literal, wildcard, and variable patterns lower to `EPatternCase`, while `data`, constructor application, and richer pattern forms stayed queued follow-up work.
- [x] Explicitly stage tuple patterns, cons-like list patterns, and lambda-pattern parameters behind concrete active-path contracts; all three now have active-path parser/type/runtime coverage.
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

- [x] Update language-state, README, runtime roadmap, and queue metadata to point future work at the active-path plan and executed milestones.
- [x] Replace remaining references that imply legacy `11` is an execution target.
- [x] Run focused parser/type/runtime/CLI checks plus the active-path default verification script as milestones land.

#### Batch 1: Tuple-pattern rejection boundary

This batch landed on `2026-05-22`. It is intentionally a boundary-locking parser batch, not tuple-pattern implementation. It kept tuple values and tuple-pattern semantics deferred until the subsequent tuple value ownership batch.

- [x] Detect tuple-shaped case patterns such as `(left, right)` and reject them with a deterministic parser diagnostic instead of letting them fail through a generic arm parse path.
- [x] Add parser coverage proving tuple-shaped patterns are not accepted in `case` arms.
- [x] Preserve all currently accepted pattern forms: literals, `_`, variable binders, constructor patterns, and exact-length bracketed-list patterns.

Batch 1 files:

- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs`

Batch 1 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 2: Tuple literal values and concrete tuple signatures

This batch landed on `2026-05-23`. It records tuples as an active core runtime
feature for `jazz-next` values and concrete monomorphic signatures without
accepting tuple patterns yet.

- [x] Parse tuple literals such as `(1, True)` as structured surface nodes
  while preserving ordinary parenthesized grouping and operator sections.
- [x] Parse concrete tuple signature types such as `(Int, Bool)` and lower them
  through core signature nodes.
- [x] Infer tuple literals as fixed-arity heterogeneous tuple types and check
  adjacent tuple signatures against their bindings.
- [x] Evaluate runtime tuple values and render them in canonical tuple form.
- [x] Keep tuple-shaped case patterns, cons-like list patterns, and
  lambda-parameter patterns deferred for a separate pattern contract.

Batch 2 files:

- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Batch 2 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 3: Cons-like list pattern rejection boundary

This batch landed on `2026-05-24`. It is intentionally a boundary-locking parser
batch, not cons-list pattern implementation.

- [x] Detect cons-style bracketed list patterns and reject them with a
  deterministic parser diagnostic that names deferred cons-like list pattern
  semantics.
- [x] Add parser coverage proving cons-like list patterns are not accepted in
  `case` arms.
- [x] Preserve all currently accepted pattern forms: literals, `_`, variable
  binders, constructor patterns, and exact-length bracketed-list patterns.
- [x] Preserve existing tuple-value behavior and tuple-shaped case-pattern
  rejection behavior.

Batch 3 files:

- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs`

Batch 3 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 4: Tuple case pattern semantics

This batch landed on `2026-05-25`. It consumes the already landed tuple value
and concrete tuple signature ownership without adding tuple constructor sugar or
lambda-parameter patterns.

- [x] Add tuple pattern nodes to the surface/core AST and lower
  tuple-shaped `case` arms such as `(left, right)` into `EPatternCase`.
- [x] Typecheck tuple patterns against fixed-arity tuple scrutinees, bind
  element variables with their corresponding element types, and reject
  non-tuple or arity-mismatched scrutinees with deterministic `E2011`
  diagnostics.
- [x] Match tuple patterns against runtime tuple values by exact arity and
  recursively match nested element patterns.
- [x] Preserve existing literal, wildcard, variable, constructor, exact-length
  list, and cons-like list rejection behavior.

Batch 4 files:

- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs`

Batch 4 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 5: Cons-like list pattern semantics

This batch landed on `2026-05-25`. It consumed the earlier cons-like list
rejection boundary by narrowing `[head | tail]` to `case`-pattern non-empty
list deconstruction. It does not add lambda-parameter patterns or broader list
destructuring syntax.

- [x] Add cons-like list pattern nodes to the surface/core AST and lower
  bracketed forms such as `[head | tail]` into `EPatternCase`.
- [x] Typecheck cons-like list patterns against list scrutinees, matching the
  head subpattern at the list element type and the tail subpattern at the same
  list type; reject non-list scrutinees with deterministic `E2011` diagnostics.
- [x] Match cons-like list patterns against non-empty runtime lists by
  recursively matching the first element and the remaining tail list.
- [x] Preserve existing literal, wildcard, variable, constructor, exact-length
  list, tuple, malformed-list, and lambda-parameter rejection behavior.

Batch 5 files:

- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- `jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs`

Batch 5 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

Suggested verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

## Definition of Done

- [x] Active ADT/pattern work targets only `jazz-next` files, tests, and docs.
- [x] `jazz-next` parser, AST/lowering, analyzer/type, and runtime agree on one ADT/case/pattern representation for the committed simple slice.
- [x] Docs describe the implemented active-path subset and explicit non-goals.
- [x] Queue, roadmap, and runtime-plan linkage no longer point new ADT work at legacy `jazz-hs` execution files.
