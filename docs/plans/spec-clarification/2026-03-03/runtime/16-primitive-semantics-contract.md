---
id: JN-PRIMITIVE-SURFACE-EXPANSION-PLAN-001
status: blocked
priority: P2
size: L
kind: impl
autonomous_ready: no
depends_on: []
last_verified: 2026-06-01
plan_section: "Follow-up: Primitive deltas after child-plan reseed"
target_paths: []
verification:
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Primitive deltas outside landed Float16/Float32 same-width arithmetic/comparison/equality and the queued structural ADT equality child plan remain blocked until separate concrete contracts define target paths and focused verification."
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
- [x] First numeric-width signature type ownership slice landed in `jazz-next`.

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
- [x] On `2026-05-31`, selected explicit numeric conversions as the next primitive-surface expansion before broader runtime arithmetic widening.
- [x] On `2026-05-31`, landed explicit target-named numeric conversions through the active `jazz-next` prelude/catalog/runtime boundary.
- [x] On `2026-05-31`, landed the default Float64 fractional literal slice in `jazz-next`.
- [x] Same concrete `Float`/`Float64` arithmetic for `+`, `-`, `*`, and `/` landed in `jazz-next`.
- [x] Same concrete `Float`/`Float64` comparison and equality for `==`, `!=`, `<`, `<=`, `>`, and `>=` landed in `jazz-next`.
- [x] Structural tuple/list equality for equality-supported element types landed in `jazz-next`.
- [x] Same concrete `Float16`/`Float32` arithmetic plus comparison/equality landed in `jazz-next`.

First implementation target (landed 2026-05-29):

- Added parser/core/type ownership for width-specific signature type names and cross-platform aliases before broadening runtime arithmetic behavior.
- Target paths: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`, `jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`, `jazz-next/src/JazzNext/Compiler/AST.hs`, `jazz-next/src/JazzNext/Compiler/TypeInference.hs`, `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`, and `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`.
- Verification: `bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`; `bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`; `bash jazz-next/scripts/test-warning-config.sh`; `bash scripts/check-execution-queue.sh`; `bash scripts/check-docs.sh`.

Completed implementation target (landed 2026-05-31 as `JN-NUMERIC-CONVERSIONS-API-001`):
explicit numeric conversions.

- Public conversion names are explicit by target type: `toInt8`, `toInt16`,
  `toInt32`, `toInt64`, `toUInt8`, `toUInt16`, `toUInt32`, `toUInt64`,
  `toFloat16`, `toFloat32`, and `toFloat64`. `toInt` and `toFloat` may exist
  as aliases for `toInt64` and `toFloat64` only if the prelude/catalog boundary
  records them as aliases rather than separate numeric semantics.
- No implicit widening, narrowing, signedness conversion, or int/float mixing
  is introduced.
- Compile-time prevention is preferred. Literal conversions must reject
  statically when the literal is out of target range, when a fractional literal
  targets an integral type without an exact integral value, or when a conversion
  source is non-numeric.
- Dynamic conversion failures use deterministic fatal runtime diagnostics only
  when the value cannot be known statically.
- Integer-to-integer conversions are exact and range-checked.
- Float-to-integer conversions require a finite integral value in range.
- Integer-to-float and float-to-float conversions use deterministic IEEE-style
  target rounding; overflow to a non-finite target is a diagnostic, not silent
  infinity.
- Conversion functions are ordinary prelude-owned public APIs backed by
  catalog/kernel bridge names, not parser magic.

Landed active-path target files:

- `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
- `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/stdlib/Prelude.jz`
- `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

## Completed implementation batch: Float64 fractional literals

This active-path batch landed as the first fractional literal slice after
width-specific signature names and explicit numeric conversions. It implements
the already selected `Float64` default without adding implicit conversions,
runtime floating arithmetic, or a broader numeric solver.

Delivered scope:

- Parse decimal fractional literals such as `1.5` in expression positions.
- Lower fractional literals into the core AST without changing integer literal
  behavior.
- Infer ambiguous fractional literals as `Float`/`Float64`.
- Accept fractional literals under explicit `Float` and `Float64` signatures.
- Evaluate and render Float64 fractional literal values through the active
  runtime path.
- Reject non-integral fractional literals converted directly to integral
  targets at compile time.
- Preserve mixed concrete width rejection and explicit-conversion-only behavior.

Out of scope:

- literal suffix syntax,
- Float16 or Float32 literal targeting,
- implicit integer-to-float promotion,
- mixed-width arithmetic widening,
- runtime dispatch, dictionaries, or a typeclass solver.

Delivered target paths:

- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

## Completed implementation batch: Float64 same-width arithmetic

This active-path batch is the next primitive-surface slice after default
Float64 fractional literals. It widened only same concrete `Float`/`Float64`
arithmetic and kept mixed-width behavior explicit.

Completed on `2026-06-01` as `JN-FLOAT64-SAME-WIDTH-ARITHMETIC-001`.

Executor-safe scope:

- Accepted `+`, `-`, `*`, and `/` when both operands resolve to the same concrete
  `Float`/`Float64` type.
- Returned the same `Float`/`Float64` type for accepted floating arithmetic.
- Evaluated accepted operations through `VFloat` in the active runtime.
- Preserved the existing integer arithmetic and integer division behavior.
- Preserved compile-time rejection for mixed `Int`/`Float`, `Float16`/`Float64`,
  `Float32`/`Float64`, and unrelated non-numeric operands.
- Preserved explicit-conversion-only behavior for all cross-width operations.

Out of scope:

- floating comparison or equality operators,
- literal suffix syntax,
- Float16 or Float32 literal targeting,
- implicit integer-to-float promotion,
- implicit mixed-width arithmetic widening,
- typeclass solver, dictionary passing, or runtime dispatch.

Target paths:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

## Completed implementation batch: Float64 same-width comparison/equality

This active-path batch is the next concrete primitive-surface slice after
Float64 same-width arithmetic. It keeps equality strict and type-directed while
allowing the existing comparison/equality operator family to work for same
concrete floating operands.

Completed on `2026-06-01` as `JN-FLOAT64-SAME-WIDTH-COMPARISON-EQUALITY-001`.

Executor-safe scope:

- Accept `==`, `!=`, `<`, `<=`, `>`, and `>=` when both operands resolve to the
  same concrete `Float`/`Float64` type.
- Return `Bool` for accepted floating comparison and equality operators.
- Evaluate accepted operations through the active `VFloat` runtime path.
- Preserve the existing integer comparison/equality behavior.
- Preserve compile-time rejection for mixed `Int`/`Float`, `Float16`/`Float64`,
  `Float32`/`Float64`, and unrelated non-comparable operands.
- Preserve explicit-conversion-only behavior for all cross-width operations.

Out of scope:

- literal suffix syntax,
- Float16 or Float32 literal targeting,
- implicit integer-to-float promotion,
- implicit mixed-width arithmetic or comparison widening,
- structural tuple/list equality,
- typeclass solver, dictionary passing, or runtime dispatch.

Target paths:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

## Completed implementation batch: structural tuple/list equality

This active-path primitive slice extends strict type-directed equality to
structural list and tuple values when every nested element type is already in
the supported equality subset. It remains non-coercive and does not introduce
general typeclass dispatch.

Completed on `2026-06-01` as `JN-PRIMITIVE-STRUCTURAL-EQUALITY-001`.

Executor-safe scope:

- Accept `==` and `!=` for same-type lists whose element type recursively
  supports runtime equality.
- Accept `==` and `!=` for same-shape tuples whose element types recursively
  support runtime equality.
- Preserve equality operator values and left/right sections for concrete
  equality-supported list and tuple operands.
- Evaluate list and tuple equality through an explicit runtime structural
  helper rather than the broad `RuntimeValue` `Eq` instance.
- Return `False` for unequal list lengths or unequal tuple/list elements when
  both sides are otherwise equality-supported.
- Preserve compile-time rejection for function-valued list/tuple elements,
  unresolved element types such as bare `[] == []`, mismatched operand types,
  and unsupported runtime equality families.

Out of scope:

- structural equality for user-defined ADT constructors,
- structural equality for functions, builtins, operator values, sections, or
  partial constructors,
- implicit numeric conversion or mixed-width equality,
- Float16/Float32 arithmetic, comparison, or equality,
- typeclass solver, dictionary passing, or runtime dispatch.

Target paths:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

## Completed implementation batch: Float16/Float32 same-width arithmetic

This active-path batch extends the same-concrete arithmetic contract beyond
`Float`/`Float64` to values produced by explicit `Float16` and `Float32`
conversions. It keeps literal targeting and width mixing explicit.

Completed on `2026-06-01` as
`JN-FLOAT16-FLOAT32-SAME-WIDTH-ARITHMETIC-001`.

Executor-safe scope:

- Accepted `+`, `-`, `*`, and `/` when both operands resolve to the same
  concrete `Float16` type.
- Accepted `+`, `-`, `*`, and `/` when both operands resolve to the same
  concrete `Float32` type.
- Returned the same concrete floating type for accepted operations.
- Evaluated accepted operations through the existing active runtime
  floating-value path.
- Preserved existing integer arithmetic and same-concrete `Float`/`Float64`
  arithmetic behavior.
- Preserved compile-time rejection for mixed `Int`/floating operands,
  `Float16`/`Float32`, `Float16`/`Float64`, `Float32`/`Float64`, unrelated
  non-numeric operands, and implicit-promotion cases.
- Preserved same-width `Float16`/`Float32` comparison/equality deferral until
  the follow-up batch, which is now landed below.

Out of scope:

- literal suffix syntax,
- Float16 or Float32 literal targeting,
- implicit integer-to-float promotion,
- implicit mixed-width arithmetic widening,
- Float16/Float32 comparison or equality in this arithmetic-only batch,
- typeclass solver, dictionary passing, or runtime dispatch.

Target paths:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

## Completed implementation batch: Float16/Float32 same-width comparison/equality

This active-path batch extends strict same-concrete comparison/equality beyond
`Float`/`Float64` to values produced by explicit `Float16` and `Float32`
conversions. It keeps literal targeting and width mixing explicit.

Completed on `2026-06-01` as
`JN-FLOAT16-FLOAT32-SAME-WIDTH-COMPARISON-EQUALITY-001`.

Executor-safe scope:

- Accepted `==`, `!=`, `<`, `<=`, `>`, and `>=` when both operands resolve to
  the same concrete `Float16` type.
- Accepted `==`, `!=`, `<`, `<=`, `>`, and `>=` when both operands resolve to
  the same concrete `Float32` type.
- Returned `Bool` for accepted comparison and equality operators.
- Evaluated accepted operations through the existing active runtime
  floating-value path.
- Preserved existing integer comparison/equality, same-concrete
  `Float`/`Float64` comparison/equality, and strict non-coercive equality
  behavior.
- Preserved compile-time rejection for mixed `Int`/floating operands,
  `Float16`/`Float32`, `Float16`/`Float64`, `Float32`/`Float64`, unrelated
  non-comparable operands, and implicit-promotion cases.

Out of scope:

- literal suffix syntax,
- Float16 or Float32 literal targeting,
- implicit integer-to-float promotion,
- implicit mixed-width comparison widening,
- structural ADT equality,
- typeclass solver, dictionary passing, or runtime dispatch.

Target paths:

- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

Focused verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

## Follow-up: Primitive deltas after child-plan reseed

On `2026-06-01`, queue curation split three concrete child plans out of this
umbrella follow-up, and the same-width arithmetic plus comparison/equality
child plans landed:

- `docs/plans/2026-06-01-jazz-next-float16-float32-same-width-arithmetic.md`
- `docs/plans/2026-06-01-jazz-next-float16-float32-same-width-comparison-equality.md`
- `docs/plans/2026-06-01-jazz-next-structural-adt-equality.md`

The remaining structural ADT equality child plan is executor-safe because it
inherits strict type-directed equality, active `jazz-next` target paths, and
focused verification from the landed structural tuple/list batch. Remaining
primitive surface work stays blocked until separate contracts define exact
syntax or behavior, target paths, and focused verification for literal
suffixes, Float16/Float32 literal targeting, implicit integer-to-float
promotion, implicit mixed-width behavior, function/operator/section equality,
or broader numeric solver behavior.

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
  - [x] Option A1 (selected): strict type-directed equality only, now including
        list/tuple structures whose nested element types are equality-supported.
  - [ ] Option A2: open-ended structural equality for all compatible value
        families.
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
