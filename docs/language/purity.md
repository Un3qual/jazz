---
title: Purity
description: Follow the current name-based purity contract for effectful Jazz functions.
sidebar_position: 10
---

Jazz separates known effectful calls from ordinary functions. It currently
tracks that boundary through names: a name ending in `!` is impure, while every
other binding is pure by default.

The compiler enforces three rules:

- a pure binding body cannot directly call a known impure callee;
- an impure binding may call pure or impure callees; and
- top-level expression statements may call impure names so programs can
  perform entry-point effects.

The host-I/O functions in the [IO module](../standard-library/io.md) follow this
naming contract.

**Partial:** this is direct-call analysis, not an effect type system. It does
not prove the purity of unknown higher-order callables, propagate effects
polymorphically, analyze a cross-module call graph, or enforce purity at
runtime. Those areas remain [planned](../project/roadmap.md).
