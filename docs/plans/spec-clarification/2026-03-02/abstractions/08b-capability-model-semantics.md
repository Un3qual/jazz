# Capability Model Semantics

Status: accepted clarification artifact for first implementation planning
Primary plan: `docs/plans/spec-clarification/2026-03-02/abstractions/08-trait-vocabulary-and-capability-model.md`

## Decisions

1. Classes and impls are user-defined Jazz declarations.
2. Builtin classes and impls are supplied by the Jazz standard library.
3. Builtin impl bodies may call internal, FFI, or kernel bridge functions.
4. Canonical capability names use the Haskell-like `Eq`/`Ord`/`Num` family.
5. Numeric defaults are cross-platform: `Int` and ambiguous integer literals default to `Int64`; `Float` and ambiguous fractional literals default to `Float64`.
6. Numeric operators require one concrete numeric type per operation. Mixed concrete widths require explicit conversion.

## First Parser/AST Batch

The first safe implementation batch is syntax ownership plus explicit inert pipeline handling only.

It should:

- parse canonical `class` declarations into surface/core AST declaration nodes,
- parse canonical `impl` declarations into surface/core AST declaration nodes,
- preserve ordinary binding, signature, and qualified-alias uses of `class`, `impl`, and `trait`,
- keep non-canonical `trait` declarations rejected,
- carry declaration names, type parameters/type arguments, and declaration bodies structurally,
- thread class/impl declaration statements through analyzer, type inference, and runtime as inert declarations with no bindings, constraints, methods, dispatch, defaulting, or runtime values.

Suggested source shapes for the first parser/AST batch:

```jz
class Eq(a) {
  eq :: a -> a -> Bool.
}
```

```jz
impl Eq(Int64) {
  eq = \(left) -> \(right) -> __kernel_eq_int64 left right.
}
```

Symbolic operator methods remain out of scope until user-defined operator work is unblocked. The first class/impl batch should use ordinary identifier method names.

## Later Semantics Batches

Later batches must separately specify:

- class environment construction,
- impl registration and duplicate detection,
- constraint solving,
- method lookup and dispatch,
- defaulting interaction,
- orphan/overlap policy,
- cross-module impl visibility,
- diagnostics and source spans.
