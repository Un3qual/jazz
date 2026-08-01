---
title: Bindings and functions
description: Define values, write lambdas, use partial application, and express recursion.
sidebar_position: 3
---

# Bindings and functions

A binding associates a name with an expression. Signatures are separate,
period-terminated statements and must agree with the inferred type.

Fragment:

```jazz
add :: Int -> Int -> Int.
add = \(left, right) -> left + right.
increment = add 1.
increment 41.
```

The compact parameter list lowers to nested unary lambdas, so functions remain
curried and may be partially applied. `\(item) -> body` is an ordinary lambda.
`\|(pattern) -> body |(pattern) -> body` is an ordered multi-body pattern
lambda. Named equation syntax is not accepted.

Top-level recursive bindings and mutually recursive groups are supported when
their types can be established. The analyzer rejects invalid recursive value
cycles and type mismatches rather than relying on runtime initialization order.

Application binds tighter than infix operators. Explicit type application uses
`@Type`, as described in [types and signatures](types-and-signatures.md).
