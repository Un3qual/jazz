---
title: Bindings and functions
description: Define values, write lambdas, use partial application, and express recursion.
sidebar_position: 3
---

A binding gives an expression a name within its lexical scope. A signature
immediately before that binding constrains its inferred type; a disagreement is
a compile-time error.

Fragment:

<!-- jazz-example: fragment -->

```jazz
add :: Int -> Int -> Int.
add = \(left, right) -> left + right.
increment = add 1.
increment 41.
```

Functions are curried. Supplying fewer arguments than a function accepts
returns another function, which is why `add 1` can define `increment` above.
A lambda with several parameters has the same application behavior as nested
single-parameter functions. Pattern lambdas try their bodies in source order.

Declaration order is observable. A non-recursive reference to a later binding
is invalid. Ordinary binding values and closures resolve earlier names against
the environment at their declaration, so a later rebinding changes only
subsequent definitions and expressions; it does not retroactively change a
captured value.

Self-recursive and mutually recursive function groups are the forward-reference
exception. Their members are inferred together and generalized as a group.
Recursive calls remain monomorphic within that group, so this does not provide
polymorphic recursion. A later rebinding cannot retroactively turn an earlier
invalid reference into recursion.

Application binds tighter than infix operators. Explicit type application uses
`@Type`, as described in [types and signatures](types-and-signatures.md).
