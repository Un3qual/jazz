---
title: Control flow
description: Use conditionals, ordered cases, guards, and pattern lambdas.
sidebar_position: 6
---

## Conditionals

Conditionals produce values:

Fragment:

<!-- jazz-example: fragment -->

```jazz
if score >= 60 then "pass" else "retry"
```

The condition must be `Bool`, and both branches must have compatible types.
Only the selected branch is evaluated.

## Cases and guards

Cases try arms from top to bottom:

Fragment:

<!-- jazz-example: fragment -->

```jazz
case input {
  | Just item -> item
  | Nothing -> 0
}
```

Failed patterns fall through without evaluating their guards. A matching arm's
guard can use names bound by the pattern; `False` continues to the next arm.
Guards must be `Bool`, and every arm body must produce a compatible result
type. Ordered pattern lambdas use the same selection rules.

Static exhaustiveness and unreachable-arm analysis are not implemented. A case
or pattern lambda with no selected arm therefore fails at runtime with `E3022`.

## Static checks

Jazz has no imperative loop construct. Repetition uses recursion or collection
operations such as the folds in the [List module](../standard-library/list.md).
