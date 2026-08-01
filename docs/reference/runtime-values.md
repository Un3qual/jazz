---
title: Runtime values
description: Reference Jazz value families, rendering, equality, and runtime failure behavior.
sidebar_position: 6
---

# Runtime values

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
public API is described in [IO and errors](../standard-library/io.md).
