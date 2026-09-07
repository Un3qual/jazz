---
title: Jazz command-line essentials
description: Use the Jazz compiler to compile files, run programs, load module graphs, and select warning policies.
sidebar_position: 4
---

Check a file, or add `--run` to evaluate it:

```bash
cabal run jazz -- program.jz
cabal run jazz -- --run program.jz
```

With no source path, or with `-`, Jazz reads standard input. A successful check
is quiet; run mode prints the final value.

To run a module and its dependencies, select the entry module and the roots to
search:

```bash
cabal run jazz -- --run --entry-module Example::Main \
  --module-root examples/modules/src
```

The bundled Prelude loads by default. `--no-prelude` disables it;
`--prelude PATH` selects another Prelude. Runtime statistics and deterministic
profiles are available in run mode.

Warnings are opt-in. `-Wsame-scope-rebinding` enables `W0001`, while
`-Werror=same-scope-rebinding` also makes it an error. See the exact option and
source-selection rules in the [CLI reference](../reference/cli.md), and warning
identities in the [diagnostics reference](../reference/diagnostics.md).
