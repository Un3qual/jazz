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

Class parameters are explicit lowercase type variables. `Self` is not a
reserved class-body type; if `Self` appears in a future program, it must be an
ordinary explicitly declared parameter name rather than a compiler-invented
receiver placeholder.

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

`jazz-next` parses and lowers class declarations with explicit lowercase
parameter metadata plus signature-only method metadata, and concrete impl
declarations with method binding metadata as declaration nodes. Duplicate class
method signatures and duplicate impl method bindings reject deterministically.
Missing, duplicate, or non-variable class parameters reject at parse time.
Class method body/default syntax, non-binding impl body items, and
method-bearing non-concrete impl bodies reject at parse time.

The current environment-validation slice rejects duplicate class declarations
and duplicate concrete impl facts, and validates concrete constrained signatures
against visible class declarations plus matching concrete impl facts using the
declared class arity.

The first executable method slice supports explicit `Class::method` references
only when a prior visible class method signature and exactly one visible
concrete impl method body exist. Type inference substitutes the class parameter
with the concrete impl target and validates each concrete impl method body
against that substituted method type before the method fact becomes visible.
Missing class method metadata, missing impl method bodies, impl-before-class
method metadata, and ambiguous multiple visible concrete method bodies reject
deterministically. Runtime evaluates the selected concrete impl method body as
an ordinary callable value only when referenced through the explicit
`Class::method` key; class/impl declarations themselves remain non-value
declarations.

The default bundled prelude now provides the canonical vocabulary class
declarations (`Eq(a)`, `Ord(a)`, `Num(a)`, `Integral(a)`, `Fractional(a)`,
`Showable(a)`, and `Default(a)`) followed by inert concrete impl facts before
kernel bridge bindings.
The current fact matrix covers the default aliases `Int`, `Float`, and `Bool`
where scoped, plus width-specific numeric signature names:

- signed and unsigned integer widths (`Int8`, `Int16`, `Int32`, `Int64`,
  `UInt8`, `UInt16`, `UInt32`, and `UInt64`) have `Eq`, `Ord`, `Num`,
  `Integral`, `Default`, and `Showable` facts.
- floating widths (`Float16`, `Float32`, and `Float64`) have `Eq`, `Ord`,
  `Num`, `Fractional`, `Default`, and `Showable` facts.

The default bundled prelude also provides concrete `Eq::equals` method bodies
for `Int`, `Float`, `Bool`, `Float16`, `Float32`, and `Float64`. The bundled
`Eq(Float)` and `Eq(Float64)` method facts may coexist only under this narrow
alias-overlap rule: `Float` is the public alias for `Float64`, so the default
prelude may expose both spellings for the same concrete width. This rule does
not relax duplicate visible impl rejection for explicit duplicate impl facts.

Runtime evidence (landed `2026-07-08`): concrete impl method candidates carry
compiler-owned evidence records at runtime. Evidence records identify the
class, concrete impl target, and method key, and explicit `Class::method`
dispatch consumes them internally. They are not user-visible ordinary values.

Later batches must define user-visible dictionary behavior or dictionary
optimization, overlap/orphan policy beyond the bundled `Float`/`Float64` alias
pair, cross-module method visibility, broader bundled-prelude method-body
families, default methods, and superclasses before enabling broader executable
class/impl semantics.
