---
title: Bootstrapping
description: Understand Jazz self-hosting progress and the evidence required to promote hosted components.
sidebar_position: 3
---

Jazz uses the current Haskell compiler and interpreter as stage 0. Hosted
compiler components are ordinary Jazz modules executed by that stage. A
self-hosted compiler means the canonical compiler implementation is written in
Jazz and can compile its own source through a promoted, behaviorally equivalent
pipeline; it does not require discarding a trusted seed compiler.

## Implemented hosted components

- canonical tokens and a Jazz-authored lexer;
- a complete Jazz-authored parser split into focused grammar modules;
- canonical-core schemas, full surface lowering, and module lowering;
- typed-core and lowered-IR schemas with total validators; and
- repeated differential suites against the Haskell stage-0 implementation.

The fixed parser corpus assigns every accepted and rejected fixture to one
family. Hosted parsing and core lowering run repeatedly and must match complete
stage-0 values or structured failures, not merely success counts.

## Promotion boundary

**Partial:** hosted front-end parity is implemented, but production compilation
still uses Haskell-owned parsing and semantic phases. Typed core and lowered IR
currently cover a bounded opt-in profile. That profile supports closed named
functions as values, unary closure parameters and results, explicit empty
environments, unary higher-order closure calls, anonymous and nested unary
closures, and binder-resolved scalar or closure-valued lexical capture.
Capture environments are immutable, ordered by first binder occurrence, and
use deterministic lifted function and layout identities. Concrete scalar
bindings are evaluated once in source order and reused by later entry
expressions in entry modules with explicitly empty export lists. Scalar value
interfaces are not produced yet. The profile also preserves unary curried
staging for named functions, callable parameters, and inline lambdas; partial
application returns an ordinary closure, and oversaturation proceeds only while
each intermediate result remains callable. The profile now transports ordered
recursive-group binder identities for capture-free, non-escaping direct self
and mutual recursion and lowers their recursive references through the existing
direct-call representation. Closure-shaped recursive groups remain excluded,
and the profile does not replace the canonical-core and reference-interpreter
compile/run path.

Promotion requires the remaining typed-core and closure boundaries, a complete
Jazz-authored semantic compiler, full module integration, a native backend and
runtime, and end-to-end deterministic conformance. Until those gates pass,
“hosted” means tested compiler components, not the shipping canonical compiler.
The horizons are tracked on the [roadmap](../project/roadmap.md).
