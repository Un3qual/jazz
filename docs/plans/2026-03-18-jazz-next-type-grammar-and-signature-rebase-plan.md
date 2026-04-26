---
id: JN-TYPE-CONSTRAINT-CONCRETE-001
status: done
priority: P1
size: S
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-04-26
plan_section: "Milestone 3 / Batch 4: Concrete constrained-signature normalization"
target_paths:
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Concrete unary non-empty constraints over Int, Bool, and nested concrete lists normalize to the existing monomorphic signature subset while variable-bearing, unknown, duplicate, wrong-arity, type-application, and function-argument constraints stay on deterministic E2009."
supersedes:
  - docs/plans/spec-clarification/2026-03-02/type-system/07-type-grammar-and-arrow-associativity.md
---

# Jazz-Next Type Grammar And Signature Rebase Plan

> Active-path replacement for `docs/plans/spec-clarification/2026-03-02/type-system/07-type-grammar-and-arrow-associativity.md`. New type-grammar planning and execution work belongs in `jazz-next/`; the older `07` plan remains reference-only because its execution targets are `jazz-hs` files.

**Goal:** move type-grammar ownership onto the active `jazz-next` parser/AST/lowering path, define the execution plan for arrow associativity and constrained-signature decisions, and stage the work needed to replace the current raw-signature-text pipeline with parser-owned type structures.

**Architecture:** keep the current safe monomorphic subset working while re-homing type grammar from `TypeInference.hs` into parser-owned structures. The rebase should proceed in vertical slices: preserve current accepted signature surface, introduce parsed type ASTs, then land canonical arrow associativity, constrained-signature rules, normalization, and diagnostics against the same `jazz-next` ownership boundary.

**Tech Stack:** Haskell modules under `jazz-next/src/JazzNext/Compiler`, `runghc` suites under `jazz-next/test/JazzNext/Compiler`, docs/spec updates under `docs/`, repo-root verification via `bash jazz-next/scripts/test-warning-config.sh`.

---

## Plan Progress

- [x] Verified the legacy `07` plan is still execution-bound to `jazz-hs`.
- [x] Verified active `jazz-next` signatures still flow through parser/lowering/analyzer as raw `Text`.
- [x] Verified `jazz-next` currently supports only a narrow monomorphic signature subset and intentionally rejects chained arrows.
- [x] Captured the active-path owner map and replacement-plan scope for `JN-TYPE-AST-IMPL-001`.
- [x] Re-verified on `2026-03-19` that signatures are still stored as raw `Text` in parser/core statements and chained-arrow signature tests still fail with `E2009`.
- [x] On `2026-04-10`, narrowed the active queue target for `JN-TYPE-AST-IMPL-001` to a single autonomous-safe Milestone 1 batch covering structured monomorphic signature nodes plus lowering/type-inference handoff.
- [x] On `2026-04-10`, landed parser-owned structured signature payloads for the supported monomorphic subset, preserved deterministic `E2009` rejection for unsupported forms including nested function signatures, and re-verified the parser/type/CLI suites.
- [x] Milestone 1 complete: parser-owned type AST replaces raw signature `Text` in the active path.
- [x] On `2026-04-13`, canonicalized right-associated chained function arrows, added explicit parenthesized function-type override support, and re-verified the parser/type/CLI suites on the active path.
- [x] Milestone 2 complete: function-arrow associativity and parenthesization rules are canonical in `jazz-next`.
- [x] On `2026-04-13`, narrowed the next executable queue target to a single Milestone 3 batch that preserves the current `@{...}:` surface while moving constrained signatures into explicit parser/core payloads.
- [x] On `2026-04-24`, landed the constrained-signature parser/core payload batch, preserving `@{...}:` syntax while keeping active-path semantics on deterministic `E2009`.
- [x] On `2026-04-26`, landed empty `@{}:` normalization to the existing monomorphic signature subset in `TypeInference.hs`, while keeping non-empty constrained signatures on deterministic `E2009`.
- [x] On `2026-04-26`, landed deterministic duplicate-constraint diagnostics for non-empty constrained signatures, preserving `E2009` while naming the duplicate constraint.
- [x] On `2026-04-26`, locked the first non-empty constrained-signature contract to concrete unary annotation-only constraints and landed the `jazz-next` implementation slice.
- [ ] Type-variable constrained-signature semantics remain blocked on a concrete binding/defaulting and inference-interaction contract.
- [ ] Milestone 3 complete: constrained-signature syntax and semantics are represented in `jazz-next` structures.
- [ ] Milestone 4 complete: canonical grammar docs, normalization rules, and diagnostics align with the active parser/type pipeline.
- [ ] Milestone 5 complete: active-path tests/docs close the rebase and future work no longer depends on legacy `07`.

## Active Baseline (2026-04-26)

- `jazz-next/src/JazzNext/Compiler/Parser.hs` now parses supported monomorphic signature statements into structured parser-owned payloads instead of joined raw text.
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs` and `jazz-next/src/JazzNext/Compiler/AST.hs` now carry explicit signature/type nodes for the supported subset plus tokenized fallback for unsupported surfaces.
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs` forwards structured signature payloads into the core AST.
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs` still enforces signature placement/name coherence only; signature semantics remain owned by `TypeInference.hs`.
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs` now consumes structured signature payloads for `Int`, `Bool`, nested concrete list forms, right-associated chained function arrows, explicit parenthesized function-type overrides, empty `@{}:` constrained signatures over that same monomorphic subset, and concrete unary non-empty constraints over `Int`, `Bool`, and nested concrete lists. Unsupported broader forms continue to report through `E2009`; duplicate non-empty constraints are reported with specific duplicate-constraint text.
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs` explicitly accepts simple list signatures, right-associated chained function signatures, parenthesized list-to-list signatures, parenthesized function-type overrides, empty constrained signatures over monomorphic function types, and concrete unary constrained signatures while keeping unsupported broader surfaces and duplicate non-empty constraints on deterministic `E2009`.
- `docs/plans/2026-03-16-jazz-next-monomorphic-signature-surface.md` already delivered the safe monomorphic subset. This rebase must preserve that subset while moving ownership to the correct compiler layers.

## Scope Guardrails

In scope:

- rebase the type-grammar plan onto `jazz-next` parser, AST, lowering, analyzer, and type-checking files
- define where parsed type and constraint structures should live in the active compiler
- specify execution order for arrow associativity, constrained signatures, normalization, diagnostics, and tests
- preserve current accepted monomorphic signature behavior until broader grammar slices are intentionally implemented

Out of scope:

- implementing full polymorphism/inference beyond the rebase milestones in this document
- ADT/type-constructor semantics outside the type-grammar ownership work
- module-loader or runtime-execution changes unrelated to type-grammar representation

## Active-Path Owner Map

| stage | current owner files | current behavior | required rebase outcome |
| --- | --- | --- | --- |
| Signature parse surface | `jazz-next/src/JazzNext/Compiler/Parser.hs` | Parses the supported monomorphic subset into dedicated parser-owned signature payloads. | Extend beyond the subset only through explicit associativity/constrained-signature decisions. |
| Surface type/signature representation | `jazz-next/src/JazzNext/Compiler/Parser/AST.hs` | `SSSignature` now carries structured parser-facing payloads plus tokenized unsupported fallback. | Add enough structure for associativity and constraints once those rules are explicitly chosen. |
| Lowered/core representation | `jazz-next/src/JazzNext/Compiler/AST.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs` | Carries structured signature payloads into core statements. | Reuse the same representation as associativity and constrained-signature work lands. |
| Signature bookkeeping | `jazz-next/src/JazzNext/Compiler/Analyzer.hs` | Validates adjacency/name coherence only. | Keep bookkeeping here, but continue consuming the structured payload shape. |
| Type semantics | `jazz-next/src/JazzNext/Compiler/TypeInference.hs` | Consumes structured monomorphic signature payloads and preserves `E2009` for unsupported surfaces. | Extend the semantic model only after canonical grammar decisions are locked. |
| Active verification | `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`, `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`, `jazz-next/test/JazzNext/CLI/CLISpec.hs` | Verifies parser-structure and semantic acceptance/rejection behavior for the supported subset. | Expand coverage toward broader grammar decisions without regressing existing supported cases. |

## Dependency Map

| dependency | why it matters | what it unlocks |
| --- | --- | --- |
| `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md` | The runtime plan already identifies raw signature `Text` as a blocker for Milestone 2 type work. | Keeps type-grammar ownership aligned with the active runtime/type pipeline. |
| `docs/plans/2026-03-16-jazz-next-monomorphic-signature-surface.md` | The monomorphic subset is already implemented and verified. | Defines the current supported baseline that the rebase must preserve. |
| `docs/spec/runtime/primitive-semantics.md` and current primitive tests | Some signature cases already rely on current builtin/runtime type expectations. | Avoids parser/type changes that drift from current compile/runtime contracts. |
| Remaining active-path plan rebases (`09`, `11`, stdlib closure) | Future parser/runtime work should consume one type/signature representation, not the current text-plus-mini-parser split. | Prevents later plans from wiring new features into a temporary ownership model. |

## Milestone Plan

### Milestone 1: Move Signature Ownership To Parser/AST/Lowering

- [ ] Introduce parser-facing type/signature nodes in `Parser/AST.hs`.
- [ ] Replace `SSSignature Identifier SourceSpan Text` and `SSignature Identifier SourceSpan Text` with structured payloads.
- [ ] Lower the new structures through `Parser/Lower.hs` into core AST nodes.
- [ ] Keep current supported monomorphic signature surface accepted while preserving explicit rejection of unresolved grammar.

#### Batch 1: Structured monomorphic signature AST and lowering handoff

This batch landed on `2026-04-10`. Later type-grammar work should define a new executable batch explicitly instead of reusing this one.

- [x] Add dedicated parser/core type nodes for the already-supported monomorphic subset: `Int`, `Bool`, nested concrete lists, and exactly one top-level `->`.
- [x] Parse signature statements into those nodes instead of joined raw `Text`, while preserving current deterministic rejection for unsupported forms such as `[a]`, chained arrows, and nested function signatures.
- [x] Lower the structured signature payload through `Parser/Lower.hs` into the core AST without reintroducing text-based signature plumbing.
- [x] Update `TypeInference.hs` to consume the structured signature form while keeping the currently supported compile/type outcomes unchanged for the monomorphic subset.

Batch 1 files:

- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Batch 1 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

Primary files:

- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

### Milestone 2: Canonicalize Function-Arrow Associativity

- [x] Decide and encode canonical associativity for chained arrows in `jazz-next`.
- [x] Support explicit parenthesized override forms and reject ambiguous or unsupported shapes deterministically.
- [x] Migrate current chained-arrow rejection tests into canonical parser/type tests once the decision is implemented.

#### Batch 1: Right-associated function arrows and parenthesized override support

This batch landed on `2026-04-13`.

- [x] Extend parser/core signature type nodes so function types can nest recursively without reintroducing raw `Text`.
- [x] Parse chained arrows right-associatively, so `a -> b -> c` means `a -> (b -> c)` on the active `jazz-next` path.
- [x] Accept parenthesized function-type overrides such as `(Int -> Int) -> Int` while continuing to reject broader unsupported grammar deterministically through `E2009`.
- [x] Update parser/lowering/type tests so the active acceptance surface matches the canonical associativity rule.

Batch 1 files:

- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Batch 1 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

Primary files:

- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

### Milestone 3: Add Constrained-Signature Representation And Semantics

- [x] Decide that constrained signatures remain in the current `@{...}:` surface for the active path.
- [x] Represent constraints explicitly in parser/core AST rather than as implicit text fragments.
- [ ] Define non-empty constraint duplicate-ordering, scope, and inference interaction rules in `TypeInference.hs`.
- [x] Add deterministic invalid-case diagnostics and tests for unsupported and duplicate constrained signatures while allowing empty `@{}:` normalization.

#### Batch 1: Structured constrained-signature payloads with preserved `E2009` rejection

This batch landed on `2026-04-24`. It intentionally preserves the existing `@{...}:` syntax and stops at explicit AST ownership; full constraint semantics, duplicate normalization, and inference interaction remain later Milestone 3/4 work.

- [x] Extend lexer/token plumbing so signature statements can preserve the current `@{ ... }:` constraint prefix without falling back to opaque unsupported-token blobs.
- [x] Add parser/core signature payload nodes for explicit constraint lists and lower them through `Parser/Lower.hs`.
- [x] Update parser and source-pipeline tests so constrained signatures round-trip through structured payloads while `TypeInference.hs` continues rejecting them deterministically with `E2009`.

Batch 1 files:

- `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Batch 1 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

#### Batch 2: Empty constrained-signature monomorphic normalization

This batch landed on `2026-04-26`. It is the smallest executable constrained-signature semantics slice after payload ownership: an empty constraint block has no semantic obligations, so `@{}:` can normalize to the already-supported monomorphic signature subset without accepting non-empty constraints.

- [x] Treat `ConstrainedSignature []` as a transparent wrapper when the body type is `Int`, `Bool`, nested lists, right-associated functions, or explicit parenthesized function types.
- [x] Preserve deterministic `E2009` for non-empty constrained signatures and for named type variables or type applications that still require constraint semantics.
- [x] Add source-pipeline coverage for an empty constrained function signature.

Batch 2 files:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Batch 2 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 3: Duplicate constrained-signature diagnostics

This batch landed on `2026-04-26`. It does not accept non-empty constrained signatures; it only makes one invalid case deterministic and actionable before broader constraint semantics are defined.

- [x] Detect duplicate constraint names in non-empty constrained signatures in source order.
- [x] Keep diagnostic code `E2009` while naming the duplicate constraint in the summary.
- [x] Preserve the generic unsupported-signature path for non-empty constrained signatures without duplicate names.

Batch 3 files:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Batch 3 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Coordination: Non-empty constrained-signature semantics contract

This coordination batch completed on `2026-04-26`. It selected the first accepted non-empty constrained-signature slice: concrete unary annotation-only constraints over `Int`, `Bool`, and nested concrete lists. Variable-bearing constraints remain blocked until binding/defaulting and inference interaction are specified.

- [x] Decide the first accepted non-empty constrained-signature slice: known unary constraint names over concrete `Int`, `Bool`, or nested list arguments normalize as annotations.
- [x] Define duplicate-ordering for the first slice: duplicate constraint names still reject in source order with `E2009`.
- [x] Define inference/defaulting interaction for the first slice: concrete constraints introduce no type variables, no defaulting, and no solver obligations.
- [x] Rewrite `JN-TYPE-CONSTRAINT-NONEMPTY-SEM-001` to the remaining type-variable constraint scope and execute the concrete `jazz-next` implementation batch.

Coordination files:

- `docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md`
- `docs/spec/semantics/bindings-and-signatures.md`
- `docs/jazz-language-state.md`
- `docs/execution/queue.md`

Coordination verification:

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 4: Concrete constrained-signature normalization

This batch landed on `2026-04-26`. It accepts the first non-empty constrained-signature semantics slice without introducing a typeclass solver: known unary constraints over concrete arguments normalize to the same monomorphic signature body already supported by `TypeInference.hs`.

- [x] Accept known unary constraint names (`Default`, `Eq`, `Fractional`, `Integral`, `Num`, `Ord`, `Showable`) when their single argument is `Int`, `Bool`, or a nested concrete list of those types.
- [x] Treat accepted concrete constraints as annotation-only obligations; they do not introduce type variables, defaulting, method resolution, or runtime dispatch.
- [x] Continue rejecting duplicate constraint names, unknown constraint names, wrong arity, type-variable arguments, type applications, and function-type constraint arguments with deterministic `E2009`.
- [x] Add source-pipeline coverage for `@{Eq(Int)}: Int`.

Batch 4 files:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Batch 4 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

Primary files:

- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

### Milestone 4: Canonical Grammar Docs, Normalization, And Diagnostics

- [ ] Publish active-path canonical grammar and normalization rules in docs/spec files.
- [ ] Keep parser/type diagnostics aligned with the new structured representation, including any needed span improvements for inner type nodes.
- [ ] Ensure CLI behavior stays consistent as signature parsing moves earlier in the pipeline.

Primary files:

- `docs/spec/type-system/type-grammar.md`
- `docs/spec/type-system/type-signatures.md`
- `docs/jazz-language-state.md`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/CLI/CLISpec.hs`

### Milestone 5: Verification And Tracker Closure

- [ ] Replace references that still send future execution toward legacy `07`.
- [ ] Run focused parser/type/CLI checks plus the active-path default verification script.
- [ ] Close the queue item and dependent roadmap/runtime links only after the active-path rebase docs are in place.

Suggested verification:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

## Definition of Done

- [ ] Active type-grammar work targets only `jazz-next` files and tests.
- [ ] Parser, AST, lowering, analyzer, and type inference agree on one structured signature representation instead of raw `Text`.
- [ ] Arrow associativity, constrained-signature semantics, and normalization rules are represented in active-path docs and tests.
- [ ] Queue/roadmap/runtime linkage no longer points new type work at legacy `jazz-hs` execution files.
