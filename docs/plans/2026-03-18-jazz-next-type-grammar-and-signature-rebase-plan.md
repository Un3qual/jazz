---
id: JN-TYPE-AST-IMPL-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: no
depends_on: []
last_verified: 2026-03-19
plan_section: "Milestone 1: Move Signature Ownership To Parser/AST/Lowering"
target_paths:
  - jazz-next/src/JazzNext/Compiler/Parser.hs
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
deliverable: "Monomorphic signatures stop flowing through the compiler as raw `Text`; the parser builds structured type/signature nodes, lowering carries them into the core AST, and the existing accepted/rejected signature surface still passes through compile/type checks."
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
- [ ] Milestone 1 complete: parser-owned type AST replaces raw signature `Text` in the active path.
- [ ] Milestone 2 complete: function-arrow associativity and parenthesization rules are canonical in `jazz-next`.
- [ ] Milestone 3 complete: constrained-signature syntax and semantics are represented in `jazz-next` structures.
- [ ] Milestone 4 complete: canonical grammar docs, normalization rules, and diagnostics align with the active parser/type pipeline.
- [ ] Milestone 5 complete: active-path tests/docs close the rebase and future work no longer depends on legacy `07`.

## Active Baseline (2026-03-18)

- `jazz-next/src/JazzNext/Compiler/Parser.hs` parses signature statements by collecting tokens until `.` and storing joined text, rather than building a type tree.
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs` and `jazz-next/src/JazzNext/Compiler/AST.hs` still represent signatures as `Text`.
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs` forwards signature text unchanged into the core AST.
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs` enforces signature placement/name coherence only; it intentionally leaves signature payload parsing for later phases.
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs` owns the current mini-parser for signatures. It accepts `Int`, `Bool`, nested concrete list forms, parenthesized subtypes, and exactly one top-level `->`.
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs` explicitly accepts simple list and single-arrow signatures, and explicitly rejects chained function signatures with `E2009`.
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
| Signature parse surface | `jazz-next/src/JazzNext/Compiler/Parser.hs` | Collects raw tokens into signature `Text`. | Parse signature/type surface into dedicated parser-owned structures. |
| Surface type/signature representation | `jazz-next/src/JazzNext/Compiler/Parser/AST.hs` | `SSSignature` stores raw `Text`. | Add parser-facing type/signature nodes with enough structure for associativity and constraints. |
| Lowered/core representation | `jazz-next/src/JazzNext/Compiler/AST.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs` | Carries signature text unchanged into core statements. | Lower parsed type/signature structures into core representations consumed by analysis and type inference. |
| Signature bookkeeping | `jazz-next/src/JazzNext/Compiler/Analyzer.hs` | Validates adjacency/name coherence only. | Keep bookkeeping here, but consume structured signature payloads instead of raw text. |
| Type semantics | `jazz-next/src/JazzNext/Compiler/TypeInference.hs` | Owns ad hoc signature parsing plus narrow type checking. | Consume parser-owned type structures and enforce the canonical grammar/semantics defined by this plan. |
| Active verification | `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`, `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`, `jazz-next/test/JazzNext/CLI/CLISpec.hs` | Verifies raw-text-based acceptance/rejection behavior. | Expand toward parser-structure and semantic verification without regressing existing supported cases. |

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

Primary files:

- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

### Milestone 2: Canonicalize Function-Arrow Associativity

- [ ] Decide and encode canonical associativity for chained arrows in `jazz-next`.
- [ ] Support explicit parenthesized override forms and reject ambiguous or unsupported shapes deterministically.
- [ ] Migrate current chained-arrow rejection tests into canonical parser/type tests once the decision is implemented.

Primary files:

- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`

### Milestone 3: Add Constrained-Signature Representation And Semantics

- [ ] Decide whether constrained signatures remain in the current `@{...}:` surface or require a normalized active-path shape.
- [ ] Represent constraints explicitly in parser/core AST rather than as implicit text fragments.
- [ ] Define duplicate-ordering, scope, and inference interaction rules in `TypeInference.hs`.
- [ ] Add deterministic invalid-case diagnostics and tests.

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
