# Primitive Semantics

Status: active (phase 1 partial implementation in `jazz-next`; width-specific numeric signature names and `Int`/`Float` aliases are parser/core/type-owned, explicit target-named numeric conversions are implemented through the prelude/catalog/runtime boundary, default Float64 fractional literal values parse/evaluate, and runtime arithmetic widening remains out of scope)
Locked decisions: 2026-03-03
Primary plan: `docs/plans/spec-clarification/2026-03-03/runtime/16-primitive-semantics-contract.md`

## Purpose

Define backend-independent language semantics for primitive operations and values.

## Implementation Target

- New runtime/typechecker implementation work for this contract lands in `jazz-next/`.
- `jazz-hs/` and `jazz2/` are legacy evidence only.

## Boundary Contract Link

- Ownership and migration rules for primitive symbols are defined in:
  - `docs/spec/stdlib-boundary.md`

## Primitive Contract Table

| Primitive | Canonical type contract | Semantics | Invalid input behavior |
| --- | --- | --- | --- |
| `+` | `Num a => a -> a -> a` | Numeric addition in selected numeric domain. | Compile-time type error on mismatched/non-numeric operands. |
| `-` | `Num a => a -> a -> a` | Numeric subtraction in selected numeric domain. | Compile-time type error on mismatched/non-numeric operands. |
| `*` | `Num a => a -> a -> a` | Numeric multiplication in selected numeric domain. | Compile-time type error on mismatched/non-numeric operands. |
| `/` | `Num a => a -> a -> a` | Numeric division in selected numeric domain. | Compile-time type error on mismatched/non-numeric operands. |
| `==` | `Eq a => a -> a -> Bool` | Strict, type-directed equality with no coercion. | Compile-time type error when operand types do not match. |
| `map` | `(a -> b) -> [a] -> [b]` | Applies function to each element in order. | Compile-time type error when function/input list types mismatch. |
| `filter` | `(a -> Bool) -> [a] -> [a]` | Keeps list elements whose predicate evaluates to `True`. | Compile-time type error when predicate/list types mismatch; fatal runtime diagnostic if predicate result is non-`Bool`. |
| `hd` | `[a] -> a` | Returns first element of a non-empty list. | Fatal runtime diagnostic on empty list in v1. |
| `tl` | `[a] -> [a]` | Returns tail of a non-empty list. | Fatal runtime diagnostic on empty list in v1. |
| `print!` | `a -> a` (stub-v1 active behavior) | Evaluates argument in impure context and returns the value unchanged. | No additional runtime failure contract in v1 beyond normal argument evaluation. |

## Equality Contract

1. Equality is strict and type-directed.
2. There is no backend coercive equality in canonical language behavior.
3. Equality only compares operands of the same type family.

Valid examples:

```jz
1 == 1
True == False
```

Invalid examples:

```jz
1 == True
"1" == 1
```

## Numeric Behavior and Defaulting

1. Numeric operations are trait-driven (`Num` family constraints).
2. Defaulting behavior is deterministic for ambiguous numeric literals.
3. The contract must scale to the approved width-specific families:
   - signed: `Int8`, `Int16`, `Int32`, `Int64`
   - unsigned: `UInt8`, `UInt16`, `UInt32`, `UInt64`
   - floating: `Float16`, `Float32`, `Float64`
4. `Float8` is deferred until a separate format contract selects the bit layout, conversion behavior, and cross-platform determinism rules.
5. Backends may differ internally, but observable language results must stay contract-equivalent.

### Width And Defaulting Contract

- `Int` is the cross-platform default integer type and maps to `Int64`.
- Ambiguous integer literals default to `Int64`.
- `Float` is the cross-platform default fractional type and maps to `Float64`.
- Ambiguous fractional literals default to `Float64`.
- Context can choose a narrower explicit type for an integer literal, for example an `Int32` annotation can make `2` an `Int32`.
- Numeric operators require one concrete numeric type per operation, matching the Haskell-like `(+) :: Num a => a -> a -> a` shape.
- Mixed concrete widths, such as `Int32 + Int64`, are type errors unless one side is converted explicitly.
- `jazz-next` now parses, lowers, and type-checks width-specific numeric signature names plus `Int`/`Float` aliases before any runtime arithmetic widening.
- Integer literals can satisfy an explicit integral-width signature annotation; ambiguous integer literals still default through `Int`.
- Decimal fractional literals such as `1.5` parse and lower to the default `Float`/`Float64` literal slice, can satisfy explicit `Float` or `Float64` signatures, and evaluate/render as runtime Float64 values.
- Fractional literals do not target `Float16` or `Float32` directly, integer/fractional operator mixing remains a type error, and runtime floating arithmetic remains out of scope for this slice.

### Explicit Conversion Contract

Explicit numeric conversions are ordinary prelude-owned APIs backed by
catalog/kernel bridge names, not parser magic. The active `jazz-next`
implementation exposes the public aliases from the bundled prelude and keeps
the corresponding `__kernel_*` bridge names available only to no-prelude and
low-level paths.

Public target-named conversions:

- `toInt8`, `toInt16`, `toInt32`, `toInt64`
- `toUInt8`, `toUInt16`, `toUInt32`, `toUInt64`
- `toFloat16`, `toFloat32`, `toFloat64`

Optional aliases (catalog-boundary conditional):

- `toInt` may alias `toInt64` only if the prelude/catalog boundary records it
  as an alias rather than a distinct numeric semantic.
- `toFloat` may alias `toFloat64` under the same condition.

Rules:

1. There are no implicit numeric conversions.
2. Mixed-width operators remain type errors unless the program calls an
   explicit conversion.
3. Non-numeric conversion sources are compile-time type errors.
4. Literal conversions are checked at compile time when possible:
   - direct integer literals must fit the target integral range,
   - direct integer literals for unsigned targets must be non-negative,
   - fractional literals targeting integral types must be exactly integral and
     in range,
   - finite floating targets reject literal overflow instead of producing
     silent infinities.
5. Dynamic integer narrowing, sign changes, float-to-integer conversion, and
   floating narrowing emit deterministic fatal `E3024` runtime diagnostics only
   when the value is not statically known.
6. Integer-to-integer conversions are exact and range-checked.
7. Float-to-integer conversions require finite integral values in range.
8. Integer-to-float and float-to-float conversions use deterministic
   target-format rounding; target overflow is a diagnostic.

## List Primitive Behavior

1. `map` preserves list order.
2. `map` is total for finite lists where function application terminates.
3. `filter` preserves input order of retained elements.
4. `filter` requires predicate applications to evaluate to `Bool`.
5. `hd` and `tl` require non-empty lists.
6. Empty-list `hd`/`tl` failures are fatal runtime diagnostics in v1.

## Runtime Failure Model (v1)

1. Prefer compile-time prevention for invalid primitive usage.
2. If an invalid primitive call escapes to runtime, emit a fatal diagnostic.
3. Fatal diagnostics are backend-independent in category and minimum context:
   - primitive name,
   - observed argument shape/type family,
   - source location when available.

## Backend Independence Rules

1. Backend implementation shortcuts must not change canonical primitive semantics.
2. JavaScript or other host-language coercions are non-authoritative.
3. Conformance tests in `jazz-next` must validate language semantics, not backend quirks.

## Migration Notes

1. Legacy `jazz-hs` behavior such as JS loose equality is historical evidence, not normative behavior.
2. Convergence work should prioritize equality and primitive failure consistency first.
