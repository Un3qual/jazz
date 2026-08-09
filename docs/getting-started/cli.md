---
title: Command-line essentials
description: Compile files, run programs, load module graphs, and select warning policies.
sidebar_position: 4
---

Use one positional source file for standalone mode:

```bash
cabal run jazz -- program.jz
cabal run jazz -- --run program.jz
```

With no source path, or with `-`, Jazz reads standard input. Compile success is
quiet; `--run` prints the final value. A source path cannot be combined with
`--entry-module`.

Module mode takes a structured entry name and one or more ordered roots:

```bash
cabal run jazz -- --run --entry-module Example::Main \
  --module-root examples/modules/src
```

The bundled Prelude loads by default. Use `--no-prelude` to disable it or
`--prelude PATH` to supply another source. Runtime statistics and deterministic
profiles are available only with `--run` through `--runtime-stats` and
`--runtime-profile=PATH`.

Warnings are opt-in. For example, `-Wsame-scope-rebinding` enables `W0001`,
and `-Werror=same-scope-rebinding` promotes it. See the complete
[CLI reference](../reference/cli.md) and [diagnostics reference](../reference/diagnostics.md).
