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

The hosted work currently includes canonical token definitions, a Jazz lexer,
a complete parser, canonical-core schemas and lowering, and typed-core and
lowered-IR schemas with total validators.

The lexer, parser, and core lowering are compared against stage 0 across the
accepted and rejected parser corpus. Repeated runs must match complete values or
structured failures, not only success counts.

## Current boundary

Hosted front-end parity is implemented, but ordinary compilation still uses
the stage-0 parsing and semantic pipeline. Typed core and lowered IR cover a
bounded opt-in profile rather than the full language, and ordinary run mode
continues to evaluate canonical core with the interpreter.

The bounded backend profile supports concrete scalar bindings, direct calls,
closed named functions as values, unary closure parameters and results,
explicit empty environments, and unary higher-order closure calls. It does not
yet support lexical capture, anonymous or nested closures, currying,
oversaturation, recursion, full control flow, or complete module integration.

## Promotion

A hosted stage is promoted only when it covers its accepted input domain,
matches the canonical stage deterministically, preserves structured failures,
and integrates with the next pipeline stage. Self-hosting additionally requires
a complete Jazz-authored semantic compiler, full module integration, native
emission and linking, a runtime, and end-to-end conformance.

Until those gates pass, hosted components are tested compiler stages rather
than the canonical shipping pipeline. See the [roadmap](../project/roadmap.md)
and current [status](../project/status.md).
