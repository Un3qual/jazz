---
title: Purity
description: Follow the current name-based purity contract for effectful Jazz functions.
sidebar_position: 10
---

The current purity model is intentionally small. A name ending in `!` is
impure; every other binding name is pure by default.

Rules enforced today:

- a pure binding body cannot directly call a known impure callee;
- an impure binding may call pure or impure callees; and
- top-level expression statements may call impure names so programs can
  perform entry-point effects.

The host-I/O functions in the [IO module](../standard-library/io.md) follow this
naming contract.

**Partial:** this is direct-call analysis, not an effect type system. There is
no effect polymorphism, higher-order proof for unknown callables, cross-module
purity graph, or runtime purity enforcement. Those areas remain
[planned](../project/roadmap.md).
