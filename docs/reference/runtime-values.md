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

Integers use arbitrary-size runtime storage until a concrete numeric target
applies. Width-specific integral operations enforce their ranges. Floating
values support `Float16`, `Float32`, and `Float64` targets with deterministic
rounding; non-finite literal targets and invalid conversions are diagnosed.
Division by either integer zero or signed floating zero is a runtime error.

## Equality

Strict equality requires compatible same-type operands, with narrow documented
`Float64`-domain integral promotion. It supports booleans, characters, text,
numeric values, and structural lists, tuples, and ADTs whose contents support
equality. Callable equality is rejected.

## Runtime failures

Pattern matching is structural and ordered. Primitive misuse that escapes
compile-time checks produces a stable fatal runtime diagnostic. Host I/O is
routed through a replaceable runtime host so tests remain deterministic; the
public API is described in [IO and errors](../standard-library/io.md).
