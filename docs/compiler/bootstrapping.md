---
title: Bootstrapping
description: Understand Jazz self-hosting stages and their promotion requirements.
sidebar_position: 3
---

Jazz uses the current Haskell compiler and interpreter as stage 0. Hosted
compiler components are Jazz modules executed by that stage. A self-hosted
compiler is a promoted Jazz implementation that can compile its own source
through a behaviorally equivalent pipeline; the trusted stage-0 seed can remain
available for reproducible bootstrap builds.

## Hosted front end

The hosted front end covers lexing, parsing, and canonical-core lowering.
These stages are compared against stage 0 across the accepted and rejected
parser corpus. Repeated runs must match complete values or structured failures,
not only success counts.

## Current boundary

Ordinary compilation uses the Haskell parsing and semantic pipeline, and run
mode interprets analyzed core. The Jazz-authored frontend remains separately
tested; it is not yet a complete semantic compiler.

The experimental Typed Core and Lowered IR backend has been removed. It had no
native emitter or execution consumer. Its schemas and validators are no longer
part of the hosted compiler or required steps toward self-hosting.

## Promotion

A hosted stage is promoted only when it covers its accepted input domain,
matches the canonical stage deterministically, preserves structured failures,
and integrates with the next pipeline stage. A complete hosted compiler still
needs Jazz-authored name resolution, type inference, semantic validation, and
full module integration before it can compile its own sources.

Native compilation remains a separate future goal. A concrete execution target
and a fresh design must justify its intermediate representations, emission,
linking, and runtime requirements. See the [roadmap](../project/roadmap.md) and
current [status](../project/status.md).
