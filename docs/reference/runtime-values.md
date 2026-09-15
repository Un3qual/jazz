---
title: Runtime values
description: Reference Jazz value families, rendering, equality, and runtime failure behavior.
sidebar_position: 6
---

## Value families and rendering

Runtime values include numbers, booleans, characters, text, lists, tuples,
closures, operators and sections, partially applied constructors, saturated
algebraic values, and capability methods. Type annotations and explicit type
application do not change how a value renders.

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

`Float16` follows IEEE binary16 arithmetic precision with deterministic target
rounding.

### `Float32`

`Float32` follows IEEE binary32 arithmetic precision with deterministic target
rounding.

### `Float64`

`Float64` uses IEEE binary64 storage and deterministic target rounding.

### Tuples

Tuples are fixed-size ordered products written `(a, b)`; equality and rendering
proceed element by element.

### Unit

Unit is the zero-element tuple `()`. It carries no information and renders as
the same `()` spelling.

## Numeric values and conversion

Integers use arbitrary-size runtime storage until a concrete numeric target
applies. Width-specific integral operations enforce their ranges. Same-width
`Float16`, `Float32`, and `Float64` arithmetic preserves that width, with
deterministic target rounding. Arithmetic overflow is checked at runtime with
`E3025`, including operations whose operands are both literals. Literal operands
outside the expected type's range are rejected during type checking.

Operators and named functions use the same argument rules. Concrete operands of
different numeric types require explicit conversion. An uncommitted literal can
be inferred at the required type under the ordinary literal rules. Infix syntax,
operator values, sections and aliases have no separate promotion exceptions.

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

Equality notation calls `equals` or `differs` through the selected `Equatable`
implementation. The Prelude supplies primitive scalar implementations and
collection implementations using element evidence. ADTs require an explicit
implementation; callable types have no bundled equality implementation.

## Runtime failures

Pattern matching is structural and ordered. A dynamic failure that cannot be
rejected statically produces a fatal diagnostic with a stable code. Host I/O
failures are values where recovery is possible; the public contracts are in
[IO](../standard-library/io.md) and [IOError](../standard-library/io-error.md).
