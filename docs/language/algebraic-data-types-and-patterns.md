---
title: Algebraic data types and patterns
description: Declare generic data types and destructure values with typed, ordered patterns.
sidebar_position: 5
---

# Algebraic data types and patterns

`data` declares one or more constructors. Constructors begin with uppercase
letters and may carry zero or more payloads.

This checked program defines a generic result type and matches both branches:

<!-- jazz-example: executable path=examples/patterns/result.jz -->

```jazz
data Result e a = Err e | Ok a.
unwrapOr :: Int -> Result(Int, Int) -> Int.
unwrapOr = \(fallback, result) -> case result {
  | Err _ -> fallback
  | Ok item -> item
}.
unwrapOr 0 (Ok 41).
```

Patterns cover literals, variables, `_`, constructors, tuples, fixed lists,
cons-like `[head | tail]`, alternatives, and `name @ pattern` as-patterns.
Constructor patterns are structural and bind their payload patterns.

The compiler checks patterns against the scrutinee type. It validates
constructor ownership and arity, list and tuple shape, duplicate binders,
or-pattern binder agreement, guard types, and arm-result agreement.

Case arms are tried in source order. Static exhaustiveness and unreachable-arm
analysis are not implemented yet. If no pattern and guard select an arm at
runtime, evaluation fails with `E3022`. See
[control flow](control-flow.md) for guards and ordering.
