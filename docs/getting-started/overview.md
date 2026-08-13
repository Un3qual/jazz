---
title: Getting started
description: Compile and run a first Jazz program.
sidebar_position: 1
---

The Jazz executable can compile a source file, execute it with the interpreter,
or resolve and execute a module graph. Compilation is diagnostic-only: a clean
compile writes no program output. Run mode renders the final value followed by
a newline.

After [installing the development toolchain](installation.md), try the smallest
program:

<!-- jazz-example: executable path=examples/hello.jz -->

```jazz
"Hello, Jazz".
```

```bash
cabal run jazz -- --run examples/hello.jz
```

Expected output:

<!-- jazz-example-output: case=hello -->

```text
"Hello, Jazz"
```

Continue with the [first-program walkthrough](first-program.md), then read the
[language overview](../language/overview.md).
