---
title: Types and signatures
description: Write primitive, function, tuple, list, generic, and constrained Jazz types.
sidebar_position: 4
---

# Types and signatures

Jazz infers expression types and accepts explicit signatures with `::`.
Function arrows associate to the right; parentheses group types; `[a]` is list
syntax; and `(a, b)` is a tuple.

Implemented primitive type names are `Bool`, `Char`, `Text`, `Int`, `Float`,
`Int8`, `Int16`, `Int32`, `Int64`, `UInt8`, `UInt16`, `UInt32`, `UInt64`,
`Float16`, `Float32`, and `Float64`. `Int` and `Float` are the default-width
aliases. Numeric operations normally require one concrete width. The only
implicit mixed-domain rule is the direct built-in integral-to-`Float` or
`Float64` exception described in [runtime values](../reference/runtime-values.md);
it does not widen to `Float16` or `Float32`, combine concrete float widths, or
apply to user-defined operators. Use the target-named conversion functions for
every other width change.

Named generic types use adjacent lowercase parameters in declarations and
parenthesized arguments in signatures:

Fragment:

<!-- jazz-example: fragment -->

```jazz
data Box a = Box a.
extract :: Box(Int) -> Int.
```

Constraints precede a signature type:

Fragment:

<!-- jazz-example: fragment -->

```jazz
contains :: @{Eq(a)}: [a] -> a -> Bool.
```

Explicit type application is written immediately after a callable expression,
for example `empty @Int`. The capability solver and explicit method dispatch
are intentionally bounded; see [capabilities](capabilities.md).
