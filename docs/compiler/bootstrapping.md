---
title: Bootstrapping
description: Understand the current Haskell compiler and deferred self-hosting work.
sidebar_position: 3
---

Jazz compilation and execution use the Haskell compiler and analyzed-core
interpreter. The Jazz-authored standard library runs through this compiler.
Jazz does not currently have a self-hosted compiler.

## Current boundary

The former Jazz-authored lexer, parser, and canonical-core lowerer have been
removed along with their dedicated comparison and scale harnesses. They were
separately tested programs under the Haskell interpreter and did not participate
in ordinary compilation. Language features now have one active compiler
implementation and its behavioral tests.

The experimental Typed Core and Lowered IR backend was also removed. Neither
that backend nor the former hosted frontend is a required intermediate step
for future compiler work.

## Deferred self-hosting

Self-hosting means a Jazz compiler implementation can compile its own sources
through a behaviorally equivalent pipeline. Resuming this effort requires an
explicit execution goal and a fresh accepted design for implementation,
integration, and verification. No alternate frontend is kept in the active tree.

Native compilation is a separate future effort and likewise requires a concrete
execution target and fresh design. See the [roadmap](../project/roadmap.md) and
current [status](../project/status.md).
