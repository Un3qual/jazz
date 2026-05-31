---
id: JN-ADT-GENERIC-CONSTRUCTOR-SCHEMES-001
status: completed
priority: P1
size: M
kind: impl
autonomous_ready: no
depends_on:
  - JN-ADT-GENERIC-DATA-PARAMS-001
last_verified: 2026-05-31
plan_section: "Completed implementation batch: generic constructor value/application schemes"
target_paths:
  - jazz-next/src/JazzNext/Compiler/Analyzer.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Derive declaration-owned constructor type schemes from generic data declarations and instantiate them freshly for constructor values and applications, while keeping ordinary bindings monomorphic and pattern/runtime dispatch out of scope."
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
- [x] On `2026-05-22`, added deterministic `E2009` diagnostic text for variable-bearing constrained signatures, naming the missing binding/defaulting contract while leaving full type-variable semantics blocked.
- [x] On `2026-05-22`, landed the monomorphic type-variable constrained-signature contract for known unary constraints, with no polymorphic generalization, defaulting, solver obligations, or runtime dispatch.
- [x] Milestone 3 complete: constrained-signature syntax and the active monomorphic semantics are represented in `jazz-next` structures.
- [x] On `2026-05-22`, locked unsupported constrained-signature `E2009` diagnostics to the attached signature statement span across the invalid constrained-signature families.
- [x] Milestone 4 complete: canonical grammar docs, normalization rules, and diagnostics align with the active parser/type pipeline.
- [x] Milestone 5 complete: active-path tests/docs close the rebase and future work no longer depends on legacy `07`.
- [x] On `2026-05-30`, closed the structured-signature rebase metadata and kept generalized polymorphism/defaulting/type-scheme work blocked as `JN-TYPE-GRAMMAR-CLOSURE-PLAN-001`.
- [x] On `2026-05-31`, split the future generic ADT constructor-scheme slice from broader ordinary binding polymorphism/defaulting solver work.
- [x] On `2026-05-31`, the prerequisite generic ADT declaration-parameter parser/core batch landed, so this constructor value/application scheme batch is now dependency-satisfied.
- [x] On `2026-05-31`, landed generic ADT constructor value/application schemes in `jazz-next`: direct constructor uses instantiate fresh type parameters, repeated payload parameter occurrences are linked within one application, and ordinary constructor aliases remain monomorphic.

## Active Baseline (2026-05-31)

- `jazz-next/src/JazzNext/Compiler/Parser.hs` now parses supported monomorphic signature statements into structured parser-owned payloads instead of joined raw text.
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs` and `jazz-next/src/JazzNext/Compiler/AST.hs` now carry explicit signature/type nodes for the supported subset plus tokenized fallback for unsupported surfaces.
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs` forwards structured signature payloads into the core AST.
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs` still enforces signature placement/name coherence only; signature semantics remain owned by `TypeInference.hs`.
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs` now consumes structured signature payloads for `Int`, `Bool`, nested concrete list forms, concrete tuple forms, right-associated chained function arrows, explicit parenthesized function-type overrides, empty `@{}:` constrained signatures over that same monomorphic subset, concrete unary non-empty constraints over `Int`, `Bool`, nested concrete lists, and concrete tuple compositions, and known unary constraints over lower-case type variables that appear in the signature body. It also derives generic ADT constructor value/application schemes from declaration-owned `data` type parameters while preserving ordinary user binding monomorphism. Unsupported broader forms continue to report through `E2009`; duplicate non-empty constraints are reported with specific duplicate-constraint text.
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs` explicitly accepts simple list signatures, right-associated chained function signatures, parenthesized list-to-list signatures, parenthesized function-type overrides, empty constrained signatures over monomorphic function types, concrete unary constrained signatures, and monomorphic variable constrained signatures while keeping unsupported broader surfaces, duplicate non-empty constraints, and unconstrained signature variables on deterministic `E2009` with primary spans attached to the signature statement.
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

## Future Polymorphism Boundary

The completed generic ADT constructor-scheme slice is intentionally narrower than
the full type-solver work tracked by this plan.

Allowed in the generic ADT slice:

- named type parameters on `data` declarations,
- constructor schemes derived from those declarations,
- fresh per-use constructor instantiation for direct values and applications,
- deterministic diagnostics for malformed generic declarations and
  incompatible constructor instantiations.

Still blocked under this type-grammar plan:

- ordinary user binding generalization,
- generic constructor pattern typing,
- inferred class constraints,
- defaulting beyond the already locked numeric literal defaults,
- typeclass solver-backed constrained signatures,
- explicit type application syntax,
- higher-rank polymorphism,
- runtime dispatch or dictionary passing.

## Completed implementation batch: generic constructor value/application schemes

This executor-safe active-path implementation batch followed the completed
`JN-ADT-GENERIC-DATA-PARAMS-001` parser/core metadata batch. It added
constructor-owned schemes for generic ADT declarations and fresh per-use
instantiation for constructor values and applications only.

Batch scope:

- Derived constructor type schemes from generic `data` declarations now that
  parser and core declarations preserve type parameters and bare constructor
  payload names.
- Instantiated each direct constructor value use with fresh type variables.
- Instantiated constructor applications independently so multiple uses of the
  same generic constructor can refine to different concrete result types.
- Linked repeated type-parameter payload positions inside a single generic
  constructor application.
- Preserved ordinary user bindings as monomorphic; did not add let
  generalization, inferred class constraints, defaulting, explicit type
  application, generic constructor pattern typing, runtime dispatch, or
  dictionary passing.
- Kept malformed generic declaration diagnostics owned by the parser/data
  parameter batch; this batch owns incompatible value/application
  instantiation diagnostics.

Batch target paths:

- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Batch verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

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
- [x] Define non-empty constraint duplicate-ordering, scope, and active monomorphic inference interaction rules in `TypeInference.hs`.
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

- [x] Accept known unary constraint names (`Default`, `Eq`, `Fractional`, `Integral`, `Num`, `Ord`, `Showable`) when their single argument is `Int`, `Bool`, a nested concrete list, or a concrete tuple composition of those types.
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

#### Batch 5: Type-variable constrained-signature contract diagnostic

This batch landed on `2026-05-22`. It does not implement type-variable constrained-signature semantics. Instead, it keeps variable-bearing constrained signatures on deterministic `E2009` and makes the remaining blocker explicit: type-variable constrained signatures require a binding/defaulting contract before inference can accept them.

- [x] Detect lower-case type-variable names inside constrained-signature constraints and bodies.
- [x] Preserve duplicate-constraint diagnostics as the higher-priority invalid constrained-signature message.
- [x] Add source-pipeline coverage for `@{Eq(a)}: a -> a` reporting the missing binding/defaulting contract.

Batch 5 files:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Batch 5 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 6: Monomorphic type-variable constrained signatures

This batch landed on `2026-05-22`. It defines the first safe type-variable constrained-signature contract without adding polymorphic generalization, defaulting, a typeclass solver, or runtime dispatch. Known unary constraints over lower-case type variables normalize as annotation-only obligations when every source type variable in the signature body appears in a supported unary constraint and every constrained variable appears in the body.

- [x] Allocate fresh internal inference variables per accepted constrained signature, reusing the same source variable name within that signature body.
- [x] Keep accepted variable constrained signatures monomorphic: later use sites refine the same binding type instead of instantiating a fresh polymorphic scheme.
- [x] Preserve deterministic `E2009` for unsupported variable contracts, including body variables without a supported unary constraint.

Batch 6 files:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Batch 6 verification:

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

- [x] Publish active-path canonical grammar and normalization rules in docs/spec files.
- [x] Keep parser/type diagnostics aligned with the new structured representation, including any needed span improvements for inner type nodes.
- [x] Ensure CLI behavior stays consistent as signature parsing moves earlier in the pipeline.

#### Batch 1: Unsupported signature diagnostic spans

This batch landed on `2026-05-22`. It is limited to unsupported structured signature diagnostics that already flow through `E2009`; it does not broaden the accepted type grammar, add polymorphic generalization, add defaulting, or introduce solver-backed constraints.

- [x] Add focused source-pipeline coverage proving unsupported constrained-signature cases report `E2009` with the primary span of the attached signature statement.
- [x] Cover the existing invalid constrained-signature families: unknown constraint names, wrong arity, type-application arguments, function-type arguments, duplicate constraints, unconstrained body variables, and unused constrained variables.
- [x] Confirm `TypeInference.hs` already attaches the signature statement span to these diagnostics, so no semantic change is needed.

Batch 1 files:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

Batch 1 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

Primary files:

- `docs/spec/semantics/bindings-and-signatures.md`
- `docs/jazz-language-state.md`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/CLI/CLISpec.hs`

### Milestone 5: Verification And Tracker Closure

- [x] Replace references that still send future execution toward legacy `07`.
- [x] Confirm focused parser/type/CLI checks are recorded in the completed active-path batch evidence; this metadata-only closure reruns the queue/docs gates listed by the execution queue.
- [x] Close the queue item and dependent roadmap/runtime links only after the active-path rebase docs are in place.

Closure note (2026-05-30): the structured signature rebase is complete for the active monomorphic subset. Future generalized type schemes, polymorphic instantiation/generalization, defaulting, solver-backed constraints, runtime dispatch, and associated diagnostics are intentionally blocked as a separate semantics contract under `JN-TYPE-GRAMMAR-CLOSURE-PLAN-001`.

Suggested verification:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

## Definition of Done

- [x] Active type-grammar work targets only `jazz-next` files and tests.
- [x] Parser, AST, lowering, analyzer, and type inference agree on one structured signature representation instead of raw `Text`.
- [x] Arrow associativity, constrained-signature semantics, and normalization rules are represented in active-path docs and tests.
- [x] Queue/roadmap/runtime linkage no longer points new type work at legacy `jazz-hs` execution files.
