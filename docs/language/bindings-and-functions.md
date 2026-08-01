---
title: Bindings and functions
description: Define values, write lambdas, use partial application, and express recursion.
sidebar_position: 3
---

# Bindings and functions

A binding associates a name with an expression. Signatures are separate,
period-terminated statements and must agree with the inferred type. A
signature must immediately precede the binding with the same name; an
intervening expression or declaration breaks the attachment.

Fragment:

<!-- jazz-example: fragment -->

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

Declaration order is observable. A non-recursive reference to a later binding
is invalid. Ordinary binding values and closures resolve earlier names against
the environment at their declaration, so a later rebinding changes only
subsequent definitions and expressions; it does not retroactively change a
captured value.

Self-recursive and mutually recursive function groups are the forward-reference
exception. Their members are inferred together with shared monomorphic
placeholders, then generalized as a group; this does not provide polymorphic
recursion. A later rebinding cannot retroactively turn an earlier invalid
forward reference into a recursive group, and recursive type mismatches remain
compile-time errors.

Application binds tighter than infix operators. Explicit type application uses
`@Type`, as described in [types and signatures](types-and-signatures.md).
