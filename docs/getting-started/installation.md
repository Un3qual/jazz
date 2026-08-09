---
title: Installation
description: Enter the reproducible Jazz development environment and build the command-line compiler.
sidebar_position: 2
---

Jazz is experimental and does not yet publish stable binary releases. The
supported contributor workflow builds from the repository with Nix and Cabal.

## Prerequisites

Install Git and Nix with flakes enabled. From the repository root, enter the
development shell and build all ordinary components:

```bash
nix --extra-experimental-features 'nix-command flakes' develop
cabal build all
```

Locate the executable with:

```bash
cabal list-bin jazz
```

Run the ordinary test matrix with:

```bash
cabal test all --test-show-details=direct
```

The exhaustive parser-scale suites, compiler profiling builds, and full
benchmarks are intentionally separate from ordinary development checks. See
the [project status](../project/status.md) for the current maturity boundary.
