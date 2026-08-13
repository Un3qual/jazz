---
title: Runtime values
description: Reference Jazz value families, rendering, equality, and runtime failure behavior.
sidebar_position: 6
---

## Value families and rendering

The interpreter represents integral and floating values, booleans, characters,
text, lists, tuples, closures, operators and sections, partially applied
constructors, saturated ADT values, and bounded capability methods. Type and
explicit-application metadata do not change user-visible rendering.

Rendered values use stable Jazz-like syntax:

- booleans render as `True` or `False`;
- characters and text use escaped quoted syntax;
- lists render as `[a, b]` and tuples as `(a, b)`;
- saturated constructors render as `Name` or `Name(arguments)`; and
- callable values render as `<function>`.

## Built-in values

### `Bool`

`Bool` has exactly the values `True` and `False`.

### `Int`

`Int` is the default-width signed integer alias for `Int64`.

### `Int8`

`Int8` is a signed 8-bit integer from `-128` through `127`.

### `Int16`

`Int16` is a signed 16-bit integer from `-32,768` through `32,767`.

### `Int32`

`Int32` is a signed 32-bit integer from `-2,147,483,648` through
`2,147,483,647`.

### `Int64`

`Int64` is a signed 64-bit integer from `-9,223,372,036,854,775,808` through
`9,223,372,036,854,775,807`.

### `UInt8`

`UInt8` is an unsigned 8-bit integer from `0` through `255`.

### `UInt16`

`UInt16` is an unsigned 16-bit integer from `0` through `65,535`.

### `UInt32`

`UInt32` is an unsigned 32-bit integer from `0` through `4,294,967,295`.

### `UInt64`

`UInt64` is an unsigned 64-bit integer from `0` through
`18,446,744,073,709,551,615`.

### `Float`

`Float` is the default-width floating alias for IEEE binary64 `Float64`.

### `Float16`

`Float16` uses IEEE binary16 storage and deterministic target rounding.

### `Float32`

`Float32` uses IEEE binary32 storage and deterministic target rounding.

### `Float64`

`Float64` uses IEEE binary64 storage and deterministic target rounding.

### Tuples

Tuples are fixed-size ordered products written `(a, b)`; equality and rendering
proceed element by element.

### Unit

Unit is the zero-element tuple `()`. It carries no information and renders as
the same `()` spelling.

## Numeric values, promotion, and conversion

Integers use arbitrary-size runtime storage until a concrete numeric target
applies. Width-specific integral operations enforce their ranges. Same-width
`Float16`, `Float32`, and `Float64` arithmetic preserves that width, with
deterministic target rounding.

The direct built-in operators `+`, `-`, `*`, `/`, `<`, `<=`, `>`, `>=`, `==`,
and `!=` have one mixed-domain exception: exactly one operand may be a concrete
integral value and the other may be default `Float` or explicit `Float64`.
Arithmetic returns the peer float type; comparison and equality return `Bool`.
An uncommitted integer literal can use the same rule when its range fits the
finite `Float64` integer domain.

Immediately applied built-in operator-value and section spellings use that
same direct rule, and aliases of built-in operator values retain it. A mixed
numeric section stored as a first-class value before application does not;
convert its captured operand explicitly. The exception also does not apply to
`Float16` or `Float32`, mixed concrete float widths, non-literal integral
results awaiting later inference, or user-defined operators.

The bundled Prelude exposes `toInt8`, `toInt16`, `toInt32`, `toInt64`,
`toUInt8`, `toUInt16`, `toUInt32`, `toUInt64`, `toFloat16`, `toFloat32`, and
`toFloat64`; `toInt` and `toFloat` alias the 64-bit targets. These conversions
accept numeric inputs only. Integral targets are exact and range-checked;
float-to-integer conversion additionally requires a finite integral value.
Floating targets round deterministically and diagnose overflow instead of
producing infinity. Statically known invalid conversions are compile-time
errors; invalid dynamic narrowing or float-to-integer conversion fails at
runtime with `E3024`.

Division by integer zero or either signed floating zero is a runtime error.

## Equality

Strict equality requires compatible same-type operands except for the direct
`Float64`-domain integral rule above. It supports booleans, characters, text,
numeric values, and structural lists, tuples, and ADTs whose contents support
equality. Callable equality is rejected.

## Runtime failures

Pattern matching is structural and ordered. Primitive misuse that escapes
compile-time checks produces a stable fatal runtime diagnostic. Host I/O is
routed through a replaceable runtime host so tests remain deterministic; the
public API is described in [IO](../standard-library/io.md) and
[IOError](../standard-library/io-error.md).
