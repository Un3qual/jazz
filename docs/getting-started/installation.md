---
title: Installation
description: Enter the reproducible Jazz development environment and build the command-line compiler.
sidebar_position: 2
---

Jazz does not yet publish stable binary releases. Build it from the repository
with Nix and Cabal.

## Prerequisites

Install Git and Nix with flakes enabled. From the repository root:

```bash
nix --extra-experimental-features 'nix-command flakes' develop
cabal build all
```

Find the built executable:

```bash
cabal list-bin jazz
```

Run the test suite:

```bash
cabal test all --test-show-details=direct
```

Parser-scale suites, profiling builds, and full benchmarks are separate from
the ordinary test command. See [Project status](../project/status.md) for the
current maturity boundary.
