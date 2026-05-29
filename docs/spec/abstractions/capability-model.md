# Capability Model

Status: active semantic contract for staged implementation
Primary plan: `docs/plans/spec-clarification/2026-03-02/abstractions/08-trait-vocabulary-and-capability-model.md`

## Model

Jazz classes and impls are user-defined language declarations. Builtin classes and builtin impls live in the Jazz standard library, not as hardcoded user-facing compiler declarations.

Builtin impl bodies may call internal, FFI, or kernel bridge functions when they need behavior that ordinary Jazz code cannot define directly.

## Canonical Surface

Class declarations use `class`:

```jz
class Eq(a) {
  eq :: a -> a -> Bool.
}
```

Impl declarations use `impl`:

```jz
impl Eq(Int64) {
  eq = \(left) -> \(right) -> __kernel_eq_int64 left right.
}
```

`trait` is not canonical declaration syntax.

## Numeric Defaulting

Numeric defaults are deterministic across targets:

- `Int` defaults to `Int64`.
- Ambiguous integer literals default to `Int64`.
- `Float` defaults to `Float64`.
- Ambiguous fractional literals default to `Float64`.
- Width-specific names such as `Int8`, `Int32`, `UInt64`, and `Float32` are explicit source types.

Numeric operators follow the Haskell-like same-type rule:

- `+`, `-`, `*`, `/`, and numeric comparisons operate on one concrete numeric type per expression.
- A literal may take its type from context.
- Mixed concrete widths, such as `Int32 + Int64`, are type errors unless one side is converted explicitly.

## Staging

The first implementation batch should be parser/AST ownership only. It should parse and lower class/impl declarations as inert declaration nodes and keep analyzer, typechecker, solver, and runtime semantics out of scope.

Later batches must define class environments, impl lookup, constraint solving, method dispatch, overlap/orphan policy, cross-module visibility, and diagnostics before enabling executable class/impl semantics.

