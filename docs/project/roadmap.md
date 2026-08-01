---
title: Roadmap
description: Review the undated Jazz horizons for language completion, self-hosting, native execution, and ecosystem work.
sidebar_position: 2
---

# Roadmap

The roadmap names direction, not release promises. Semantic work moves into the
public language only after design review, implementation, tests, and matching
documentation.

## Language completion

- extend capability dispatch beyond the current concrete profile;
- design effect typing beyond the name-based purity contract;
- close remaining numeric, pattern, and cross-module semantic edges; and
- keep diagnostics, reference pages, and standard-library contracts aligned.

## Self-hosting

- add Jazz-authored name resolution, type inference, and semantic validation;
- promote hosted components only after complete differential conformance;
- lower closures, recursion, control flow, and module programs through typed
  core; and
- compile the compiler's own sources through the canonical hosted pipeline.

## Native backend

- complete backend-neutral lowered IR for the full language;
- define the versioned runtime and host ABI;
- add native code emission, object generation, linking, and conformance; and
- preserve deterministic diagnostics and runtime semantics across targets.

## Ecosystem

- publish reproducible releases and installation artifacts;
- build package and dependency conventions after module semantics stabilize;
- add formatter, language-server, and semantic editor support; and
- grow teaching material, libraries, and production-shaped examples.

For shipped behavior, use the current [status](status.md), not this roadmap.
