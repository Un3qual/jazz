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

The normative numeric width/defaulting contract lives in `docs/spec/runtime/primitive-semantics.md`. Capability semantics consume that contract rather than redefining it here:

- Standard numeric classes and impls use deterministic cross-platform `Int64` and `Float64` defaults for ambiguous source types and literals.
- Width-specific source names remain explicit.
- Numeric operators follow the Haskell-like same-type rule; mixed concrete widths require explicit conversions.

## Active Implementation

`jazz-next` parses and lowers class/impl declarations as declaration nodes.
Analyzer/type/runtime statement walkers handle those nodes without adding method
lookup, dispatch, defaulting, or runtime values.

The current environment-validation slice rejects duplicate class declarations
and duplicate concrete impl facts, and validates concrete constrained signatures
against visible class declarations plus matching concrete impl facts.

The default bundled prelude now provides the canonical vocabulary class
declarations (`Eq`, `Ord`, `Num`, `Integral`, `Fractional`, `Showable`, and
`Default`) followed by inert concrete impl facts before kernel bridge bindings.
The current fact matrix covers the default aliases `Int`, `Float`, and `Bool`
where scoped, plus width-specific numeric signature names:

- signed and unsigned integer widths (`Int8`, `Int16`, `Int32`, `Int64`,
  `UInt8`, `UInt16`, `UInt32`, and `UInt64`) have `Eq`, `Ord`, `Num`,
  `Integral`, `Default`, and `Showable` facts.
- floating widths (`Float16`, `Float32`, and `Float64`) have `Eq`, `Ord`,
  `Num`, `Fractional`, `Default`, and `Showable` facts.

Later batches must define constraint solving beyond the current concrete fact
checks, method dispatch, overlap/orphan policy, cross-module visibility, and
runtime behavior before enabling executable class/impl semantics.
