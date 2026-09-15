---
title: Prelude
description: Reference the implicit capability vocabulary, conversions, and compatibility helpers.
sidebar_position: 2
---

The Prelude is loaded automatically for ordinary compilation and execution. It
defines the core capability vocabulary and a small compatibility surface; no
import is required.

## Booleans

### `not`

```jazz jazz-signature
not :: Bool -> Bool.
```

Negates a Boolean: `not True` is `False`, and `not False` is `True`.
It is an ordinary function and can be passed to other functions, for example
`map not [True, False]`.

## Ordering

### `Ordering`

```jazz jazz-signature
data Ordering = LT | EQ | GT.
```

The result of a three-way comparison.

### `LT`

The left value precedes the right value.

### `EQ`

The values have equal ordering.

### `GT`

The left value follows the right value.

## Equality and ordering capabilities

### `Equatable`

```jazz jazz-signature
class Equatable(a) {
  equals :: a -> a -> Bool.
  differs :: a -> a -> Bool.
  differs = \(left, right) -> not (equals left right).
}.
```

Requires an equality operation for `a`. Implementations cover scalar and numeric
types, Unit, Ordering, lists, pairs, triples, Maybe, Result, NonEmpty, and Queue. Collections
and tuples require `Equatable` for their elements and compare through those
methods. Queue compares FIFO contents, independent of construction history.
`==` calls `equals`; `!=` calls `differs`. ADTs require an explicit implementation.

### `equals`

```jazz jazz-signature
equals :: a -> a -> Bool.
```

### `differs`

```jazz jazz-signature
differs :: a -> a -> Bool.
```

The default negates `equals`; an implementation may override it.

### `Comparable`

```jazz jazz-signature
class @{Equatable(a)}: Comparable(a) { compare :: a -> a -> Ordering. }.
```

Requires `Equatable(a)` evidence and a three-way ordering operation for `a`. Numeric and character values
use their ordinary order. `Text` compares lexicographically by Unicode scalar.

### `compare`

```jazz jazz-signature
compare :: a -> a -> Ordering.
```

### `lessThan`, `lessThanOrEqual`, `greaterThan`, `greaterThanOrEqual`

```jazz jazz-signature
lessThan :: @{Comparable(a)}: a -> a -> Bool.
lessThanOrEqual :: @{Comparable(a)}: a -> a -> Bool.
greaterThan :: @{Comparable(a)}: a -> a -> Bool.
greaterThanOrEqual :: @{Comparable(a)}: a -> a -> Bool.
```

These functions inspect `compare`; they are also written `<`, `<=`, `>`, `>=`.
An implementation of `Comparable` supplies all four comparisons.

### `apply`

```jazz jazz-signature
apply :: (a -> b) -> a -> b.
```

Applies its first argument to its second. `$` is its low-precedence operator
spelling: `not $ True` means `apply not True`.

## Numeric capabilities

### `Num`

```jazz jazz-signature
class Num(a) {
  add :: a -> a -> a.
  subtract :: a -> a -> a.
  multiply :: a -> a -> a.
  divide :: a -> a -> a.
}.
```

Supplies arithmetic for `a`. Primitive numeric types have kernel-backed
implementations; user-defined types may implement the same four methods.
`+`, `-`, `*`, and `/` call these methods. Both operands and the result have type
`a`. Integer division rounds down, and division by zero fails at runtime.

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

## Mapping, folding, and combination

### `Mappable`

```jazz jazz-signature
class Mappable(f) { map :: (a -> b) -> f(a) -> f(b). }.
```

A mapping keeps the collection constructor and can change its element type.
List is available from the Prelude. Importing Queue, Maybe, NonEmpty, Result,
Map, or Dictionary supplies its instance. Result preserves errors;
Map and Dictionary preserve keys and entry order. Text and Set have separate
[module functions](overview.md), with character and ordering restrictions.

### `map`

```jazz jazz-signature
map :: (a -> b) -> f(a) -> f(b).
```

Applies a function to every element while preserving positions, absence, and
errors. The class parameter `f` is the first explicit type argument.

### `Reducible`

```jazz jazz-signature
class Reducible(f) {
  foldLeft :: (b -> a -> b) -> b -> f(a) -> b.
  foldRight :: (a -> b -> b) -> b -> f(a) -> b.
}.
```

Instances cover the Mappable collections plus Set. Sequences use their element
order, Map and Set use key order, and Dictionary uses insertion order. Empty
collections, Nothing, and Err return the initial accumulator. No element
ordering constraint is required to fold a Set. Convert Text with `Text::toChars`
to fold its Unicode scalars.

### `foldLeft`

```jazz jazz-signature
foldLeft :: (b -> a -> b) -> b -> f(a) -> b.
```

Visits elements from left to right, passing the accumulator first.

### `foldRight`

```jazz jazz-signature
foldRight :: (a -> b -> b) -> b -> f(a) -> b.
```

Combines elements from right to left, passing the element first.

### `Combinable`

```jazz jazz-signature
class Combinable(a) { combine :: a -> a -> a. }.
```

Instances concatenate Text, List, Queue, and NonEmpty. Set combines by union
and requires `Comparable` for its elements. Existing module empty values remain
available; NonEmpty keeps its nonempty invariant.

### `combine`

```jazz jazz-signature
combine :: a -> a -> a.
```

Combines the left and right values using their collection implementation.
For seedless folding that returns Nothing on empty input, import
[Reduce](reduce.md).

## Compatibility list helpers

### `filter`

```jazz jazz-signature
filter :: (a -> Bool) -> [a] -> [a].
```

Keeps the items whose predicate is `True`, preserving order. Prefer
[`List::filter`](list.md#filter) in library-oriented code.

### `hd`

```jazz jazz-signature
hd :: [a] -> a.
```

### `tl`

```jazz jazz-signature
tl :: [a] -> [a].
```

`hd` and `tl` are partial: an empty list fails fatally with `E3009` or `E3010`,
respectively. Prefer [`List::head`](list.md#head) and
[`List::tail`](list.md#tail) when emptiness is possible.

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

### `toInt32`

```jazz jazz-signature
toInt32 :: @{Num(a)}: a -> Int32.
```

### `toInt64`

```jazz jazz-signature
toInt64 :: @{Num(a)}: a -> Int64.
```

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

### `toUInt32`

```jazz jazz-signature
toUInt32 :: @{Num(a)}: a -> UInt32.
```

### `toUInt64`

```jazz jazz-signature
toUInt64 :: @{Num(a)}: a -> UInt64.
```

### `toFloat16`

```jazz jazz-signature
toFloat16 :: @{Num(a)}: a -> Float16.
```

### `toFloat32`

```jazz jazz-signature
toFloat32 :: @{Num(a)}: a -> Float32.
```

### `toFloat64`

```jazz jazz-signature
toFloat64 :: @{Num(a)}: a -> Float64.
```

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
