---
title: Types and signatures
description: Write primitive, function, tuple, list, generic, and constrained Jazz types.
sidebar_position: 4
---

Jazz infers the type of each expression. An explicit signature documents and
constrains that result; the program is rejected when the implementation cannot
satisfy it. Type variables allow one definition to work uniformly across
several concrete types, while capability constraints require specific
operations from those types.

Numeric operations normally require one concrete width; `Int` and `Float` are
the default-width aliases. The only implicit mixed-domain rule is the direct
built-in integral-to-`Float` or `Float64` exception. It does not widen to
`Float16` or `Float32`, combine concrete float widths, or apply to user-defined
operators. Use the target-named conversion functions for every other width
change. [Runtime values](../reference/runtime-values.md) lists the built-in
types and their exact promotion behavior.

Named types can carry type parameters:

Fragment:

<!-- jazz-example: fragment -->

```jazz
data Box a = Box a.
extract :: Box(Int) -> Int.
```

This signature requires equality for its element type, while `identity` is
unconstrained:

Fragment:

<!-- jazz-example: fragment -->

```jazz
contains :: @{Eq(a)}: [a] -> a -> Bool.

identity :: a -> a.
identity = \(item) -> item.
identity @Int 1.
```

Explicit type application can select a type when inference does not have enough
context, as `identity @Int 1` does above. See [capabilities](capabilities.md)
for the current constraint and dispatch model, and the
[expression grammar](../reference/expression-grammar.md) for exact type syntax.
