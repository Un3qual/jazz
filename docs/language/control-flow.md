---
title: Control flow
description: Use conditionals, ordered cases, guards, and pattern lambdas.
sidebar_position: 6
---

## Conditionals

Jazz conditionals are expressions:

Fragment:

<!-- jazz-example: fragment -->

```jazz
if score >= 60 then "pass" else "retry"
```

The condition must be `Bool`, and both branches must have compatible types.
The compiler lowers this form to its canonical conditional node; it is not
library syntax.

## Cases and guards

Cases try arms from top to bottom:

Fragment:

<!-- jazz-example: fragment -->

```jazz
case value {
  | Just item -> item
  | Nothing -> 0
}
```

An arm may have pattern alternatives and one guard. Patterns are checked
against the scrutinee type; constructor and tuple arity, duplicate binders, and
inconsistent or-pattern binder sets or binder types are diagnosed. Guards can
use pattern binders and must have type `Bool`. Arm bodies must have compatible
result types. Ordered pattern lambdas use the same pattern semantics.

Static exhaustiveness and unreachable-arm analysis are planned, not currently
implemented. At runtime, failed patterns skip their guards, a `False` guard
falls through, and a case or pattern lambda with no selected arm fails with
`E3022`.

## Static checks

There are no imperative loops in the current surface. Use recursion and
standard-library folds from the [List guide](../standard-library/list.md).
