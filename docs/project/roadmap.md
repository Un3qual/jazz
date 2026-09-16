---
title: Roadmap
description: Review the undated Jazz horizons for language completion, self-hosting, native execution, and ecosystem work.
sidebar_position: 2
---

The roadmap names direction, not release promises. Semantic work moves into the
public language only after design review, implementation, tests, and matching
documentation.

## Language completion

- design effect typing beyond the name-based purity contract;
- close remaining numeric, pattern, and cross-module semantic edges; and
- keep diagnostics, reference pages, and standard-library contracts aligned.

## Self-hosting

Self-hosting is deferred. Resume only with an explicit execution goal and a
fresh accepted design for a compiler that can compile its own sources. The
former hosted frontend has been removed; current development focuses on the
Haskell compiler and the Jazz standard library.

## Native backend

- choose and prove a backend architecture for a concrete execution target;
- define the versioned runtime and host ABI;
- add native code emission, object generation, linking, and conformance; and
- preserve deterministic diagnostics and runtime semantics across targets.

## Ecosystem

- publish reproducible releases and installation artifacts;
- build package and dependency conventions after module semantics stabilize;
- add formatter, language-server, and semantic editor support; and
- grow teaching material, libraries, and production-shaped examples.

For shipped behavior, use the current [status](status.md), not this roadmap.
