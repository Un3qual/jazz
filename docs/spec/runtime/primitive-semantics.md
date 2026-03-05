# Primitive Semantics

Status: active (phase 0 contract lock)
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
2. Defaulting behavior must be deterministic for ambiguous numeric literals.
3. The contract must scale to future width-specific families:
   - signed: `Int8`, `Int16`, `Int32`, `Int64`
   - unsigned: `UInt8`, `UInt16`, `UInt32`, `UInt64`
   - floating: `Float8`, `Float16`, `Float32`, `Float64`
4. Backends may differ internally, but observable language results must stay contract-equivalent.

## List Primitive Behavior

1. `map` preserves list order.
2. `map` is total for finite lists where function application terminates.
3. `hd` and `tl` require non-empty lists.
4. Empty-list `hd`/`tl` failures are fatal runtime diagnostics in v1.

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
