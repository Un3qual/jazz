---
title: Contributing
description: Build, test, document, and propose changes to the Jazz language and compiler.
sidebar_position: 4
---

# Contributing

Jazz welcomes focused compiler, runtime, standard-library, tooling, and
documentation contributions. Start by reading the [status](status.md),
[roadmap](roadmap.md), and [governance](governance.md).

## Repository map

- `src/` contains the active Haskell compiler and runtime.
- `jazz/` contains the Jazz-authored standard library and hosted compiler.
- `app/` contains the command-line entry point.
- `test/` contains compiler, runtime, CLI, and repository suites.
- `examples/` contains checked teaching programs.
- `programs/` contains production-shaped correctness and benchmark cases.

## Development loop

Enter the reproducible shell, build, and run ordinary checks:

```bash
nix --extra-experimental-features 'nix-command flakes' develop
cabal build all
cabal test all --test-show-details=direct
bash scripts/check-examples.sh
bash scripts/check-docs.sh
```

Keep compiler behavior and tests in the same change. A complete runnable
program used in public documentation must be a checked file under `examples/`;
inline code is a fragment. Do not change semantics merely to make prose or an
example pass—correct the claim or propose the language change first.

Use focused test components while developing, then run the ordinary matrix.
Exhaustive scale, profiling, and full benchmarks are opt-in gates for relevant
changes rather than routine documentation checks.
