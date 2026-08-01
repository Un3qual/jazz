---
title: Your first program
description: Define, type-check, and run a recursive factorial function in Jazz.
sidebar_position: 3
---

# Your first program

This checked example declares a signature, defines a recursive function with
ordered pattern clauses, and evaluates it.

<!-- jazz-example: executable path=examples/functions/factorial.jz -->

```jazz
factorial :: Int -> Int.
factorial =
  \|(0) -> 1
   |(n) -> n * factorial (n - 1).
factorial 6.
```

Compile it. A successful compile is quiet:

```bash
cabal run jazz -- examples/functions/factorial.jz
```

Run it through the interpreter:

```bash
cabal run jazz -- --run examples/functions/factorial.jz
```

Expected output:

```text
720
```

Every statement ends in a period. `Int -> Int` is the function type, and the
two `\|` clauses are tried from top to bottom. Learn more in
[bindings and functions](../language/bindings-and-functions.md) and
[control flow](../language/control-flow.md).
