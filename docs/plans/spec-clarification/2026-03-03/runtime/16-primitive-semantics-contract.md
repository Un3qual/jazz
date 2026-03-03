# Primitive Semantics Contract Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Define backend-independent language semantics for primitive operations and values (numeric operations, equality, boolean behavior, list primitives, and basic error behavior).

**Architecture:** Specify primitive semantics in language terms first, then map each backend/runtime path to conformance tests. Prevent backend implementation details (for example JS loose equality) from defining language semantics by accident.

**Tech Stack:** docs/spec files, validation scripts, and future `jazz-next` analyzer/runtime tests.

Execution note:
- `jazz-hs/` references in this plan are legacy evidence only.
- All net-new implementation and tests for this item must land in `jazz-next/`.

---

## Progress

- [x] Primitive semantic drift identified
- [x] Primitive inventory and contract table published
- [x] Equality decision gate finalized
- [x] Remaining decision gates finalized (`numeric behavior`, `primitive errors`)
- [x] Runtime/typechecker conformance tests added
- [x] Docs and trackers aligned

## Decision Lock (Approved 2026-03-03)

- [x] Equality is strict and type-directed.
- [x] No backend coercive equality semantics in the canonical language contract.
- [x] Numeric behavior remains trait-driven with deterministic defaulting rules.
- [x] Numeric model must scale to planned width-specific types (`Int8..Int64`, `UInt8..UInt64`, `Float8..Float64`).
- [x] Primitive runtime failures use fatal diagnostics in v1 (with compile-time prevention preferred where possible).

## Verification Evidence (Current Ambiguity)

- `jazz-hs/src/Types.hs`: builtin traits and builtin function signatures define only a subset of primitive behavior.
- `jazz-hs/src/CodeGen/Javascript.hs`: `==` lowers to JS loose equality; this may not match intended language-level equality semantics.
- `docs/plans/spec-clarification/2026-03-02/runtime/12-backend-target-strategy.md`: backend strategy is being stabilized, increasing need for backend-independent primitive definitions.

## Scope Guardrails

In scope:
- arithmetic primitives (`+`, `-`, `*`, `/`),
- equality (`==`) semantics,
- list primitives (`hd`, `tl`, `map`) observable contracts,
- primitive failure behavior surface (error categories, not full diagnostics system).

Out of scope:
- full standard library API design,
- advanced algebraic law proofs,
- optimizer-specific rewrite rules.

## Decision Gates

- [x] Gate A: Equality contract.
  - [x] Option A1 (selected): strict type-directed equality only.
  - [ ] Option A2: structural equality for compatible value families.
  - [ ] Option A3: retain JS-like coercive behavior (not recommended for interpreter-first direction).
- [x] Gate B: Numeric behavior.
  - [x] Option B1 (selected): integer/float operations remain trait-driven with explicit defaulting rules.
  - [ ] Option B2: explicit literal suffixing or syntax to avoid defaulting ambiguity.
- [x] Gate C: Primitive error model.
  - [ ] Option C1: recoverable runtime errors with explicit error values.
  - [x] Option C2 (selected): fatal runtime diagnostics for invalid primitive calls in v1.

## Phase 0: Primitive Contract Table

- [x] Build a table of all current primitive names and signatures.
- [x] For each primitive, define:
  - argument/return contract,
  - valid and invalid inputs,
  - deterministic semantics independent of backend implementation language.
- [x] Add trait/defaulting extension rules that preserve compatibility with planned numeric widths:
  - signed integer family (`Int8`, `Int16`, `Int32`, `Int64`),
  - unsigned integer family (`UInt8`, `UInt16`, `UInt32`, `UInt64`),
  - floating family (`Float8`, `Float16`, `Float32`, `Float64`).
- [x] Include explicit non-coercion equality examples:
  - valid: `1 == 1`, `True == False`.
  - invalid/type error: `1 == True`, `"1" == 1`.

Create:
- `docs/spec/runtime/primitive-semantics.md`

### Commit Checkpoint (Phase 0)

```bash
git add docs/spec/runtime/primitive-semantics.md \
  docs/plans/spec-clarification/2026-03-03/runtime/16-primitive-semantics-contract.md
git commit -m "docs(spec): define primitive semantics contract table"
```

## Phase 1: Conformance Test Design

- [x] Add tests for primitive behavior contracts in analyzer/runtime path.
- [x] Include explicit mismatch tests (for equality and invalid primitive uses).
- [x] Ensure tests can run regardless of active backend by targeting shared semantic entrypoints.

Modify/Create:
- `jazz-next/test/PrimitiveSemanticsSpec.hs`
- `jazz-next/scripts/test-warning-config.sh`

### Commit Checkpoint (Phase 1)

```bash
git add jazz-next/test/PrimitiveSemanticsSpec.hs jazz-next/scripts/test-warning-config.sh
git commit -m "test(jazz-next): add primitive semantics conformance tests"
```

## Phase 2: Runtime/Backend Alignment

- [ ] Align primitive implementations with the semantic contract.
- [ ] Remove backend-specific behavior that violates contract (for example coercive equality if disallowed).
- [ ] Keep fatal diagnostics consistent across runtime paths for invalid primitive uses that escape compile-time checks.

Modify (active `jazz-next` path):
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/Compiler/Diagnostics.hs`

### Commit Checkpoint (Phase 2)

```bash
git add jazz-next/src/JazzNext/Compiler/AST.hs \
  jazz-next/src/JazzNext/Compiler/Analyzer.hs \
  jazz-next/src/JazzNext/Compiler/TypeInference.hs \
  jazz-next/src/JazzNext/Compiler/Driver.hs \
  jazz-next/src/JazzNext/Compiler/Diagnostics.hs
git commit -m "feat(jazz-next): align primitive implementations with language contract"
```

## Phase 3: Docs and Tracking Closure

- [x] Update language-state doc to reference canonical primitive contract.
- [x] Ensure plan tracker marks primitive semantics no longer implicit.

Modify:
- `docs/jazz-language-state.md`
- `docs/plans/spec-clarification/2026-03-03/README.md`

### Commit Checkpoint (Phase 3)

```bash
git add docs/jazz-language-state.md docs/plans/spec-clarification/2026-03-03/README.md
git commit -m "docs(spec): close primitive semantics clarification"
```

## Verification Commands

```bash
bash jazz-next/scripts/test-warning-config.sh
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PrimitiveSemanticsSpec.hs
```

## Definition of Done

- [x] Primitive semantics are defined in language terms, not backend accident.
- [x] Equality and numeric behavior are explicitly locked.
- [ ] Conformance tests exist for each primitive family.

## Implementation Status Verification (2026-03-03, Batch 3)

- [x] Re-verified unchecked candidate steps before implementation and confirmed phase-1 gaps were still open in `jazz-next`.
- [x] Added `jazz-next/test/PrimitiveSemanticsSpec.hs` conformance coverage for current primitive behavior in the active AST subset (numeric/equality/comparison operators).
- [x] Added explicit strict-equality mismatch assertions (`==`, `!=`) and arithmetic/comparison mismatch assertions.
- [x] Added a dedicated strict-equality mismatch diagnostic code (`E2004`) in `jazz-next/src/JazzNext/Compiler/TypeInference.hs`.
- [x] Added the primitive semantics suite to `jazz-next/scripts/test-warning-config.sh` so it runs in the default verification loop.
- [x] Ran `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PrimitiveSemanticsSpec.hs` and `bash jazz-next/scripts/test-warning-config.sh`.
- [ ] Full primitive-family conformance remains open until list/runtime primitive domains (`map`, `hd`, `tl`, runtime fatal paths) are executable in the active `jazz-next` pipeline.
