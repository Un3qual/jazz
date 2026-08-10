---
title: Contributing
description: Build, test, document, and propose changes to the Jazz language and compiler.
sidebar_position: 4
---

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

## Before changing Jazz

Semantic language changes are RFC-first. Maintenance and documentation fixes
that preserve observable behavior may proceed implementation-first. In either
case, keep implementation, focused tests, and affected public documentation in
the same change.

Enter the reproducible Nix development shell and use focused tests while
working. Before review, run the verification tier appropriate to the change.
Long parser-scale, corpus, profiling, and benchmark workloads run weekly,
manually, or for release candidates rather than on every pull request.

The
[canonical contributor guide](https://github.com/un3qual/jazz/blob/main/CONTRIBUTING.md)
contains setup, command, repository-ownership, RFC, CI-tier, issue, and
pull-request details.

## Public examples

A complete runnable program used in public documentation must be a checked file
under `examples/`; inline code is a fragment. Do not change semantics merely to
make prose or an example pass—correct the claim or propose the language change
first.
