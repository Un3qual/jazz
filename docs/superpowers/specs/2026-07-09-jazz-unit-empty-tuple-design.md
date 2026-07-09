# Jazz Unit as Empty Tuple Design

**Date:** 2026-07-09  
**Status:** Approved  
**Scope:** `jazz-next/`

## Context

Jazz lambdas currently require at least one parameter. The parser rejects
`\()` as an empty parameter list, while the surface AST now correctly stores
lambda parameters as `NonEmpty SurfaceLambdaParameter`.

Jazz should support computations with no meaningful input without adding a
second, truly nullary function kind. The selected design follows the
Haskell/ML convention: `()` is the Unit value and Unit pattern, and a
zero-input computation is an ordinary unary function from Unit.

## Goals

- Add `()` as Jazz's Unit value, pattern, and signature type.
- Accept `\() -> expression` as convenient syntax for a lambda with one Unit
  pattern parameter.
- Invoke such a lambda through ordinary application, canonically `function ()`.
- Preserve the existing unary, curried core function model.
- Reuse the existing tuple representation so Unit is the zero-element tuple.
- Cover parsing, lowering, type inference, runtime matching, invocation, and
  diagnostics with focused tests.

## Non-goals

- Add a true arity-zero function type, lambda node, or application node.
- Add a named `Unit` primitive or constructor alongside `()`.
- Change multi-parameter lambda currying or ordinary application syntax.
- Define effects, laziness, memoization, or foreign-function arity behavior.
- Modify `jazz-hs/` or `jazz2/`.

## Surface Syntax and Semantics

### Unit value and type

`()` is the unique value of the zero-element tuple type, also written `()`:

```jazz
unit :: ().
unit = ().
```

Tuple arities remain unambiguous:

- `()` is Unit, the zero-element tuple.
- `(expression)` is a grouped expression, not a one-element tuple.
- `(left, right)` and larger comma-separated forms are ordinary tuples.

### Unit patterns

`()` is an irrefutable pattern for a value of type `()`:

```jazz
result = case () {
  | () -> 42
}.
```

It binds no names. Pattern checking requires the scrutinee to have the
zero-element tuple type.

### Unit lambdas

The canonical zero-input computation form is:

```jazz
thunk :: () -> Int.
thunk = \() -> 42.
result = thunk ().
```

Although the surface spelling resembles an empty parameter list, the lambda
has exactly one parameter: the Unit pattern. It therefore remains compatible
with `NonEmpty SurfaceLambdaParameter` and lowers through the existing
pattern-parameter machinery.

The fully explicit nested spelling `\(()) -> expression` is also accepted:
the outer parentheses delimit Jazz's lambda parameter list and the inner `()`
is the Unit pattern. `\() -> expression` is sugar for that singleton-parameter
form.

Each application evaluates the lambda body normally. The design does not add
implicit sharing, memoization, or nullary invocation semantics.

## Compiler Representation

No new core value or function constructors are required. The existing tuple
representations extend naturally to zero elements:

| Layer | Unit representation |
| --- | --- |
| Surface expression | `SETuple []` |
| Surface pattern | `SPTuple []` |
| Surface signature type | `SurfaceTypeTuple []` |
| Constrained surface type | `SurfaceConstrainedTypeTuple []` |
| Core expression | `ETuple []` |
| Core pattern | `PTuple []` |
| Core signature type | `TypeTuple []` |
| Inferred type | `TTupleType []` |
| Runtime value | `VTuple []` |

For the shorthand lambda, the parser constructs:

```haskell
SELambda (SurfaceLambdaPattern (SPTuple []) :| []) body
```

Lowering then uses its existing pattern-parameter desugaring to produce a
unary `ELambda` whose argument is checked by an `EPatternCase` containing
`PTuple []`.

## Parser Behavior

- An empty parenthesized expression parses as `SETuple []`.
- An empty parenthesized pattern parses as `SPTuple []`.
- An empty parenthesized signature type parses as `SurfaceTypeTuple []`.
- The constrained signature grammar records the same form as
  `SurfaceConstrainedTypeTuple []`.
- Immediately closing the outer parameter delimiters in `\()` constructs one
  Unit-pattern parameter instead of producing the old empty-list diagnostic.
- Existing errors for missing lambda parentheses, trailing commas, malformed
  tuples, and missing lambda bodies remain unchanged.

## Type and Runtime Behavior

The existing tuple inference path produces `TTupleType []` for `ETuple []`.
Tuple unification, signature conversion, and rendering must preserve the empty
element list and render the type as `()`.

The existing runtime tuple path produces `VTuple []`, renders it as `()`, and
matches it against `PTuple []` because both sides have arity zero. Applying a
Unit lambda to any non-Unit value remains a normal pattern mismatch or type
error through the existing lambda-pattern pipeline.

Structural equality treats `() == ()` as true under the existing tuple
equality rule.

## Diagnostics

`\()` is no longer diagnosed as an empty lambda parameter list. Malformed
nearby forms continue to fail at their actual syntax boundary, including:

- `\(,) -> expression`
- `\((),) -> expression`
- an unterminated Unit expression or pattern
- applying a Unit-pattern lambda to a value whose type is not `()`

No new diagnostic code is required unless implementation evidence shows that
an existing generic tuple or pattern diagnostic is misleading.

## Testing Strategy

Implementation will use focused red-green-refactor cycles:

1. Parser tests for Unit expressions, patterns, signature types, `\()`, and
   `\(())`.
2. Lowering tests proving the lambda remains unary and uses `PTuple []`.
3. Type tests for `()`, `() -> a`, correct Unit application, and rejection of
   a non-Unit argument.
4. Runtime tests for rendering Unit, matching Unit patterns, invoking a Unit
   lambda, and evaluating the body on each application.
5. Regression tests for malformed empty-tuple-adjacent syntax.

Before completion, run the focused parser and semantic suites, the full Cabal
suite, the warning-configuration script, documentation checks, package checks,
and a CLI smoke program using a Unit lambda.

## Documentation Impact

- Update authoritative syntax to define `()` as Unit and `\()` as a unary
  Unit-pattern lambda.
- Update tuple and pattern semantics to include arity zero.
- Retain the explicit statement that Jazz has no true nullary functions.
- Keep `NonEmpty SurfaceLambdaParameter` as the surface-AST invariant.

## Delivery

The design, implementation plan, focused test-first compiler changes, and
documentation updates will be committed as coherent checkpoints on
`codex/jazz-next-review-remediation`. Legacy compiler directories remain
untouched.
