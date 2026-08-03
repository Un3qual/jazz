---
title: Getting started
description: Build Jazz, run checked examples, and find the next language guide.
sidebar_position: 1
---

The Jazz executable can compile a source file, execute it with the interpreter,
or resolve and execute a module graph. Compilation is diagnostic-only: a clean
compile writes no program output. Run mode renders the final value followed by
a newline.

After [installing the development toolchain](installation.md), try the smallest
checked program:

<!-- jazz-example: executable path=examples/hello.jz -->

```jazz
"Hello, Jazz".
```

```bash
cabal run jazz -- --run examples/hello.jz
```

Expected output:

```text
"Hello, Jazz"
```

Continue with the [first-program walkthrough](first-program.md), then read the
[language overview](../language/overview.md).
