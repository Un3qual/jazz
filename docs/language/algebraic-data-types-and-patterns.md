---
title: Algebraic data types and patterns
description: Declare generic data types and destructure values with exhaustive patterns.
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

Case analysis is checked for reachable, exhaustive coverage across booleans,
ADTs, lists, tuples, and supported literal domains. A runtime no-match
diagnostic remains a defensive boundary if an unchecked path reaches the
interpreter. See [control flow](control-flow.md) for guards and ordering.
