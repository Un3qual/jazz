---
title: Control flow
description: Use conditionals, ordered cases, guards, and pattern lambdas.
sidebar_position: 6
---

# Control flow

## Conditionals

Jazz conditionals are expressions:

Fragment:

```jazz
if score >= 60 then "pass" else "retry"
```

The condition must be `Bool`, and both branches must have compatible types.
The compiler lowers this form to its canonical conditional node; it is not
library syntax.

## Cases and guards

Cases try arms from top to bottom:

Fragment:

```jazz
case value {
  | Just item -> item
  | Nothing -> 0
}
```

An arm may have pattern alternatives and guards. Guards are evaluated in
source order and must be boolean. Unreachable arms, non-exhaustive finite
coverage, duplicate binders, and inconsistent alternative binders are
diagnosed. Ordered pattern lambdas use the same pattern semantics.

## Static checks

There are no imperative loops in the current surface. Use recursion and
standard-library folds from the [List guide](../standard-library/list.md).
