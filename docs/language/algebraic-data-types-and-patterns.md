---
title: Algebraic data types and patterns
description: Declare generic data types and destructure values with typed, ordered patterns.
sidebar_position: 5
---

An algebraic data type describes a fixed set of alternatives. Each constructor
identifies one alternative and may carry values specific to that case. This
makes the possible shapes of a value explicit in its type.

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

Expected output:

<!-- jazz-example-output: case=result -->

```text
41
```

Patterns test a value's shape and bind the parts needed by a branch. They can
match literals, constructors, tuples, lists, alternatives, or a value together
with one of those shapes. `_` ignores a part that the branch does not need.

Pattern alternatives are available at the top level of a case arm or pattern
lambda parameter. Every alternative must bind the same names with the same
types. Nested alternatives and guards on pattern-lambda parameters are not
currently supported; case arms support one guard after the complete pattern.

The compiler checks each pattern against the value being matched. Constructors
must belong to its type and carry the expected number of fields. Tuple and list
shapes, duplicate binders, alternative binders, guard types, and branch result
types are checked before evaluation.

Case arms are tried in source order and must cover the complete resolved input
type. Unguarded constructor arms can exhaust a closed ADT; guarded arms do not
contribute coverage. The compiler reports an example missing pattern for an
incomplete match and rejects an arm whose whole pattern space was covered by
earlier unguarded arms. See [control flow](control-flow.md) for guards,
ordering, and diagnostic codes.
