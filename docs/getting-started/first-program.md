---
title: Your first program
description: Define, type-check, and run a recursive factorial function in Jazz.
sidebar_position: 3
---

This program defines a recursive function and evaluates `factorial 6`:

<!-- jazz-example: executable path=examples/functions/factorial.jz -->

```jazz
factorial :: Int -> Int.
factorial =
  \|(0) -> 1
   |(n) -> n * factorial (n - 1).
factorial 6.
```

Check the program without running it. Success produces no output:

```bash
cabal run jazz -- examples/functions/factorial.jz
```

Then run it:

```bash
cabal run jazz -- --run examples/functions/factorial.jz
```

Result:

<!-- jazz-example-output: case=factorial -->

```text
720
```

`Int -> Int` guarantees that `factorial` accepts and returns an integer. The
pattern clauses are tried from top to bottom: zero stops the recursion, while
the second clause handles every other integer. The final expression supplies
the program's result.

Continue with [bindings and functions](../language/bindings-and-functions.md)
and [control flow](../language/control-flow.md). Exact source forms are listed
in the [expression grammar](../reference/expression-grammar.md).
