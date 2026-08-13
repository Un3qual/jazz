---
title: Prelude
description: Reference the implicit capability vocabulary, conversions, and compatibility helpers.
sidebar_position: 2
---

The Prelude is loaded automatically for ordinary compilation and execution. It
defines the core capability vocabulary and a small compatibility surface; no
import is required.

## Ordering

### `Ordering`

```jazz jazz-signature
data Ordering = LT | EQ | GT.
```

Represents the result of a three-way comparison.

### `LT`

Indicates that the left value sorts before the right value.

### `EQ`

Indicates that the compared values have equal ordering.

### `GT`

Indicates that the left value sorts after the right value.

## Equality and ordering capabilities

### `Eq`

```jazz jazz-signature
class Eq(a) { equals :: a -> a -> Bool. }.
```

Requires an equality operation for `a`. Built-in implementations cover scalar
and numeric types.

### `equals`

```jazz jazz-signature
equals :: a -> a -> Bool.
```

Returns whether two values are equal through the active `Eq(a)` implementation.

### `Ord`

```jazz jazz-signature
class Ord(a) { compare :: a -> a -> Ordering. }.
```

Requires a three-way ordering operation for `a`. Numeric and character values
use their ordinary order. `Text` compares lexicographically by Unicode scalar.

### `compare`

```jazz jazz-signature
compare :: a -> a -> Ordering.
```

Returns `LT`, `EQ`, or `GT` through the active `Ord(a)` implementation.

## Numeric capabilities

### `Num`

```jazz jazz-signature
class Num(a) { }.
```

Marks numeric types accepted by arithmetic and explicit numeric conversions.
Built-in integer and floating types implement it.

### `Integral`

```jazz jazz-signature
class Integral(a) { }.
```

Marks integral numeric types. Built-in signed and unsigned integer types
implement it.

### `Fractional`

```jazz jazz-signature
class Fractional(a) { }.
```

Marks fractional numeric types. Built-in floating types implement it.

## Rendering and defaults

### `Showable`

```jazz jazz-signature
class Showable(a) { show :: a -> Text. }.
```

Requires stable runtime-value rendering for `a`. Built-in scalar and numeric
types implement it.

### `show`

```jazz jazz-signature
show :: a -> Text.
```

Renders a value using its active `Showable(a)` implementation and stable Jazz
value syntax.

### `Default`

```jazz jazz-signature
class Default(a) { defaultValue :: a. }.
```

Provides a type-directed default. Built-in numeric defaults are zero, `Bool`
uses `False`, `Char` uses `'\0'`, and `Text` uses `""`.

### `defaultValue`

```jazz jazz-signature
defaultValue :: a.
```

Returns the value supplied by the active `Default(a)` implementation.

## Compatibility list helpers

### `map`

```jazz jazz-signature
map :: (a -> b) -> [a] -> [b].
```

Applies a function to every item and preserves order. Prefer
[`listMap`](list.md#listmap) in library-oriented code.

### `filter`

```jazz jazz-signature
filter :: (a -> Bool) -> [a] -> [a].
```

Keeps the items whose predicate is `True`, preserving order. Prefer
[`listFilter`](list.md#listfilter) in library-oriented code.

### `hd`

```jazz jazz-signature
hd :: [a] -> a.
```

Returns the first value. This function is partial: `hd []` fails fatally with
`E3009`. Prefer [`listHead`](list.md#listhead) when emptiness is possible.

### `tl`

```jazz jazz-signature
tl :: [a] -> [a].
```

Returns every value after the first. This function is partial: `tl []` fails
fatally with `E3010`. Prefer [`listTail`](list.md#listtail) when emptiness is
possible.

`hd` and `tl` are partial: an empty list fails fatally with `E3009` or `E3010`,
respectively.

## Effectful compatibility value

### `print!`

```jazz jazz-signature
print! :: a -> a.
```

In stub-v1, evaluates and returns its argument without emitting output. Its `!`
suffix still classifies it as impure. Run-mode rendering of the final expression
is separate from `print!`.

## Numeric conversions

Conversion inputs must satisfy `Num(a)`. Integer targets require an in-range
integral value. Floating targets use deterministic target-format rounding;
non-finite or overflowing conversions fail with a runtime diagnostic.

### `toInt8`

```jazz jazz-signature
toInt8 :: @{Num(a)}: a -> Int8.
```

Converts to an 8-bit signed integer and rejects values outside `-128` through
`127`.

### `toInt16`

```jazz jazz-signature
toInt16 :: @{Num(a)}: a -> Int16.
```

Converts to a 16-bit signed integer with range checking.

### `toInt32`

```jazz jazz-signature
toInt32 :: @{Num(a)}: a -> Int32.
```

Converts to a 32-bit signed integer with range checking.

### `toInt64`

```jazz jazz-signature
toInt64 :: @{Num(a)}: a -> Int64.
```

Converts to a 64-bit signed integer with range checking.

### `toUInt8`

```jazz jazz-signature
toUInt8 :: @{Num(a)}: a -> UInt8.
```

Converts to an 8-bit unsigned integer and rejects values outside `0` through
`255`. Example: `toUInt8 255` succeeds; `toUInt8 256` fails.

### `toUInt16`

```jazz jazz-signature
toUInt16 :: @{Num(a)}: a -> UInt16.
```

Converts to a 16-bit unsigned integer with range checking.

### `toUInt32`

```jazz jazz-signature
toUInt32 :: @{Num(a)}: a -> UInt32.
```

Converts to a 32-bit unsigned integer with range checking.

### `toUInt64`

```jazz jazz-signature
toUInt64 :: @{Num(a)}: a -> UInt64.
```

Converts to a 64-bit unsigned integer with range checking.

### `toFloat16`

```jazz jazz-signature
toFloat16 :: @{Num(a)}: a -> Float16.
```

Converts to IEEE binary16 with deterministic rounding and overflow checks.

### `toFloat32`

```jazz jazz-signature
toFloat32 :: @{Num(a)}: a -> Float32.
```

Converts to IEEE binary32 with deterministic rounding and overflow checks.

### `toFloat64`

```jazz jazz-signature
toFloat64 :: @{Num(a)}: a -> Float64.
```

Converts to IEEE binary64 with deterministic rounding and overflow checks.

### `toInt`

```jazz jazz-signature
toInt :: @{Num(a)}: a -> Int64.
```

An alias of `toInt64` with the same range and integral-input requirements.

### `toFloat`

```jazz jazz-signature
toFloat :: @{Num(a)}: a -> Float64.
```

An alias of `toFloat64` with the same rounding and overflow behavior.

Use `--no-prelude` only for compiler or runtime work. Its low-level support
surface is not a user API. See [capabilities](../language/capabilities.md) for
constraint syntax and resolution.
