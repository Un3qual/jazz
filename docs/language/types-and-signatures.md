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

Numeric operations require matching operand types; `Int` and `Float` are the
default-width aliases. Operators call ordinary functions and have the same
argument rules, including when used as values or sections. Use the target-named
conversion functions for mixed concrete types. [Runtime values](../reference/runtime-values.md)
lists the built-in types and their conversion behavior.

Named types can carry type parameters:

Fragment:

<!-- jazz-example: fragment -->

```jazz
data Box a = Box a.
extract :: Box(Int) -> Int.
```

Parameters can also stand for type constructors. Jazz infers their kinds from
the declaration: complete value types have kind `Type`, and `List` has kind
`Type -> Type`.

Fragment:

<!-- jazz-example: fragment -->

```jazz
data Wrapped f a = Wrapped f(a).
keep :: Wrapped(List, Int) -> Wrapped(List, Int).
keep = \(item) -> item.
```

Named constructors may be partially applied when an argument expects a
constructor. `List(a)` and `[a]` are equivalent. Datatype fields and term
signatures require complete types. Applying `Int` as a constructor,
overapplying a constructor, or forming an infinite kind is an error. An unused
parameter defaults to `Type` when its declaration group is checked; importing
the type preserves those fixed kinds.

This signature requires equality for its element type, while `identity` is
unconstrained:

Fragment:

<!-- jazz-example: fragment -->

```jazz
contains :: @{Equatable(a)}: [a] -> a -> Bool.

identity :: a -> a.
identity = \(item) -> item.
identity @Int 1.
```

Explicit type application can select a type when inference does not have enough
context, as `identity @Int 1` does above. See [capabilities](capabilities.md)
for the current constraint and dispatch model, and the
[expression grammar](../reference/expression-grammar.md) for exact type syntax.
