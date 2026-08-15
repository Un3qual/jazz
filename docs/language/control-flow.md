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

Cases and pattern lambdas must be statically exhaustive. Only unguarded arms
contribute to coverage, so a guarded arm always needs an unguarded covering arm
elsewhere. The compiler reports `E2018` with a missing-pattern example when
coverage is incomplete and `E2019` when earlier unguarded arms make an entire
later arm unreachable.

The runtime keeps `E3022` as a defensive boundary for independently constructed
canonical core. Source programs that pass compilation do not rely on it for
ordinary match selection.

## Static checks

Jazz has no imperative loop construct. Repetition uses recursion or collection
operations such as the folds in the [List module](../standard-library/list.md).
