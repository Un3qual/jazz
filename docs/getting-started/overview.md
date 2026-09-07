---
title: Getting started with Jazz
description: Install the Jazz programming language toolchain, then compile and run your first program.
sidebar_position: 1
---

Jazz can check a source file without running it or evaluate the program and
print its final value. It can also start from a named module and resolve that
module's dependencies.

After [installing Jazz](installation.md), run a program whose value is text:

<!-- jazz-example: executable path=examples/hello.jz -->

```jazz
"Hello, Jazz".
```

```bash
cabal run jazz -- --run examples/hello.jz
```

The interpreter prints the value, including the quotes that identify it as
text:

<!-- jazz-example-output: case=hello -->

```text
"Hello, Jazz"
```

Continue with the [first-program walkthrough](first-program.md) to define and
call a function, then read the [language overview](../language/overview.md).
