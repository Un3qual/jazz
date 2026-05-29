---
id: JN-NUMERIC-WIDTH-SIGNATURE-TYPES-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-05-29
plan_section: "Follow-up: Numeric width and defaulting rollout"
target_paths:
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
  - jazz-next/src/JazzNext/Compiler/AST.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - docs/spec/runtime/primitive-semantics.md
  - docs/spec/semantics/bindings-and-signatures.md
  - docs/plans/spec-clarification/2026-03-03/runtime/16-primitive-semantics-contract.md
  - docs/execution/queue.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Add parser/core/type ownership for width-specific numeric signature type names and cross-platform `Int`/`Float` aliases, preserving Haskell-like same-type operator rules and leaving runtime arithmetic widening out of scope."
---

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
- [x] Batch 2 runtime evaluator + fatal runtime diagnostic path executed in `jazz-next`
- [x] Batch 3 list primitives (`map`/`hd`/`tl`) are executable end-to-end in `jazz-next` (parser/type/runtime/CLI)
- [x] Batch 4 list-primitive fallback diagnostics are deterministic and conformance-tested (`E3011/E3012/E3013/E3015`)
- [x] Docs and trackers aligned
- [x] Specify the first numeric-width/defaulting rollout before user-defined operator work.

## Decision Lock (Approved 2026-03-03)

- [x] Equality is strict and type-directed.
- [x] No backend coercive equality semantics in the canonical language contract.
- [x] Numeric behavior remains trait-driven with deterministic cross-platform defaulting rules.
- [x] Numeric model must scale to planned width-specific types (`Int8..Int64`, `UInt8..UInt64`, `Float16..Float64`); `Float8` is deferred until a concrete bit-layout and determinism contract exists.
- [x] Primitive runtime failures use fatal diagnostics in v1 (with compile-time prevention preferred where possible).

## Follow-up: Numeric width and defaulting rollout

- [x] Preserve the approved numeric family direction: `Int8`, `Int16`, `Int32`, `Int64`, `UInt8`, `UInt16`, `UInt32`, `UInt64`, `Float16`, `Float32`, and `Float64`; defer `Float8` until the language specifies its format and cross-platform behavior.
- [x] Select cross-platform numeric defaults before any user-defined operator implementation batch.
- [x] Define literal defaulting and ambiguous numeric constraint behavior using `Int64` for integer defaults and `Float64` for fractional defaults.
- [x] Decide whether `Int`/`Float` are platform-native aliases (`Int32`/`Float32` on 32-bit targets and `Int64`/`Float64` on 64-bit targets), deterministic cross-platform aliases, or non-canonical aliases outside the width-specific family: selected deterministic cross-platform aliases/defaults.
- [x] Specify primitive operation contracts for width mixing: Haskell-like same concrete type per numeric operator expression, with explicit conversion required for mixed concrete widths.
- [x] Name exact future implementation target paths in `jazz-next/` and focused verification commands, without changing compiler behavior in this docs batch.
- [x] Keep user-defined operators as a follow-up stage after this numeric-width/defaulting contract lands.

First implementation target:

- Add parser/core/type ownership for width-specific signature type names and cross-platform aliases before broadening runtime arithmetic behavior.
- Target paths: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`, `jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`, `jazz-next/src/JazzNext/Compiler/AST.hs`, `jazz-next/src/JazzNext/Compiler/TypeInference.hs`, `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`, and `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`.
- Verification: `bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`; `bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`; `bash jazz-next/scripts/test-warning-config.sh`; `bash scripts/check-execution-queue.sh`; `bash scripts/check-docs.sh`.

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
  - floating family (`Float16`, `Float32`, `Float64`), with `Float8` deferred until its bit layout and deterministic conversion behavior are specified.
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

- [x] Align primitive implementations with the semantic contract.
- [x] Remove backend-specific behavior that violates contract (for example coercive equality if disallowed).
- [x] Keep fatal diagnostics consistent across runtime paths for invalid primitive uses that escape compile-time checks.

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
- [x] Conformance tests exist for each primitive family in the active `jazz-next` subset (`+`, `-`, `*`, `/`, `==`, `!=`, `map`, `hd`, `tl`).

## Implementation Status Verification (2026-03-03, Batch 3)

- [x] Re-verified unchecked candidate steps before implementation and confirmed phase-1 gaps were still open in `jazz-next`.
- [x] Added `jazz-next/test/PrimitiveSemanticsSpec.hs` conformance coverage for current primitive behavior in the active AST subset (numeric/equality/comparison operators).
- [x] Added explicit strict-equality mismatch assertions (`==`, `!=`) and arithmetic/comparison mismatch assertions.
- [x] Added a dedicated strict-equality mismatch diagnostic code (`E2004`) in `jazz-next/src/JazzNext/Compiler/TypeInference.hs`.
- [x] Re-verified strict equality behavior remained non-coercive in active `jazz-next` compile paths and test coverage.
- [x] Added the primitive semantics suite to `jazz-next/scripts/test-warning-config.sh` so it runs in the default verification loop.
- [x] Ran `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PrimitiveSemanticsSpec.hs` and `bash jazz-next/scripts/test-warning-config.sh`.
- [x] Full primitive-family conformance gate is now cleared because list primitive domains (`map`, `hd`, `tl`) are executable in the active `jazz-next` pipeline.

## Implementation Status Verification (2026-03-03, Batch 4)

- [x] Re-verified unchecked candidate steps before implementation and confirmed runtime evaluation and fatal runtime diagnostics were still open in active `jazz-next` paths.
- [x] Added runtime primitive execution semantics for the current AST subset in `jazz-next/src/JazzNext/Compiler/Runtime.hs` (arithmetic, comparison, strict equality/inequality).
- [x] Added fatal runtime division-by-zero diagnostics (`E3001`) and runtime fallback diagnostics for invalid runtime primitive/operator usage (`E3002+`).
- [x] Added driver runtime entrypoints in `jazz-next/src/JazzNext/Compiler/Driver.hs` (`runExpr`, `runSource`) to execute primitives after compile-time checks pass.
- [x] Added runtime primitive conformance coverage in `jazz-next/test/RuntimeSemanticsSpec.hs` and CLI run-mode fatal-path coverage in `jazz-next/test/CLISpec.hs`.
- [x] Ran `bash jazz-next/scripts/test-warning-config.sh` with runtime semantics tests included.

## Implementation Status Verification (2026-03-03, Batch 5)

- [x] Re-verified unchecked candidate steps before implementation and confirmed list literals, function application surface parsing, and `map`/`hd`/`tl` execution were still open in active `jazz-next`.
- [x] Added parser/lowering support for list literals and space-application (`jazz-next/src/JazzNext/Compiler/Parser/{Lexer.hs,AST.hs,Parser.hs,Lower.hs}` and `jazz-next/src/JazzNext/Compiler/AST.hs`).
- [x] Added analyzer/type/runtime support for list/application forms plus builtin list primitives (`map`, `hd`, `tl`) with deterministic diagnostics (`E2006`, `E2007`, `E3009`, `E3010`).
- [x] Added conformance tests across parser/type/runtime/CLI in `jazz-next/test/{PrimitiveSemanticsSpec.hs,RuntimeSemanticsSpec.hs,CLISpec.hs}`.
- [x] Ran `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PrimitiveSemanticsSpec.hs`, `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/RuntimeSemanticsSpec.hs`, `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/CLISpec.hs`, and `bash jazz-next/scripts/test-warning-config.sh`.

## Implementation Status Verification (2026-03-04, Batch 6)

- [x] Re-verified candidate steps before execution and confirmed list-primitive execution was already implemented in `jazz-next` (unchecked items were tracker drift, not missing code).
- [x] Added additional primitive conformance coverage for previously untested list-primitive error paths:
  - compile-time: `tl` non-list argument and `map` non-list collection (`E2006`) in `jazz-next/test/PrimitiveSemanticsSpec.hs`
  - runtime fallback: `hd`/`tl` non-list and `map` mapper/collection validation (`E3011/E3012/E3015/E3013`) in `jazz-next/test/RuntimeSemanticsSpec.hs`
- [x] Fixed runtime primitive contract mismatch in `jazz-next/src/JazzNext/Compiler/Runtime.hs` by validating `map`'s mapper argument before element application, so non-function mappers report `E3015` deterministically instead of generic `E3008`.
- [x] Ran `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PrimitiveSemanticsSpec.hs`, `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/RuntimeSemanticsSpec.hs`, and `bash jazz-next/scripts/test-warning-config.sh`.
