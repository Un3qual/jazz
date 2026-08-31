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

The hosted front end currently covers lexing, parsing, canonical-core lowering,
and validation schemas for typed core and lowered IR.

The lexer, parser, and core lowering are compared against stage 0 across the
accepted and rejected parser corpus. Repeated runs must match complete values or
structured failures, not only success counts.

## Current boundary

Hosted front-end parity is implemented, but ordinary compilation still uses
the stage-0 parsing and semantic pipeline. Typed core and lowered IR cover a
bounded opt-in profile rather than the full language. It supports scalar
bindings, direct calls, function values, unary closures, lexical capture,
higher-order calls, curried application, capture-free, non-escaping direct self
and mutual recursion, and closure-shaped self and mutual recursion when every
external capture is available before the first group member. Closure-shaped
groups share one immutable environment containing ordered external captures;
members reuse it and reconstruct self or peer closures without cyclic
initialization. Value-producing conditionals and the admitted scalar, tuple,
and local-constructor pattern cases may nest anywhere in this profile. In value
positions, lowering keeps their deterministic control flow and explicit value
transport. A case evaluates its scrutinee once and tries arms in source order.
Nested pattern failures and false guards continue to the next arm. Tuple fields
are matched in source order; a variant tag is tested before any field, and only
the selected tag's fields are projected. Pattern binders become visible only
after a complete match and remain local to the selected guard and body.

For a complete named or lifted function result, the opt-in lowerer records
direct or closure tail intent instead. That result position propagates through
selected conditional branches and selected bounded scalar-case bodies, which
terminate directly without a result join. Conditions, scrutinees, guards,
operands, and nested value contexts remain ordinary value positions. Partial
applications still return closure values, and an oversaturated application can
tail-terminate only at its final exact stage. The synthetic module entry remains
ordinary call/join/return lowering. These terminators record intent only: they
do not change the Lowered IR contract or validator, runtime ABI, public
language behavior, hosted compiler, or promise native stack optimization.

The backend independently checks totality for its admitted pattern subset.
Guarded rows do not cover. Complete closed local-constructor sets and the single
tuple shape need no synthetic wildcard, while open scalar literal domains still
need an unguarded catch-all. This backend boundary remains separate from the
source-level exhaustiveness and unreachable-arm analysis shipped under RFC 0012.

Managed `Text` is the first non-closure managed value in this profile. Text
literals, bindings, parameters, results, captures, calls, control-flow joins,
returns, and tail-call operands use one semantic Text layout. Strict equality,
length, append, and append-char lower to exact pure runtime-service
dependencies; inequality reuses equality followed by Boolean-not. Text-only
transport declares no service, and referenced services are deduplicated in a
fixed catalog order.

Non-unit tuples and exactly saturated local algebraic-data constructors are the
second managed-data family. The producer retains concrete product and nominal
variant recipes, including concrete generic, recursive, and mutually recursive
layouts whose fields stay inside the admitted profile. Lowering assigns stable
semantic layout identities, emits runtime layouts first, product and variant
layouts in first semantic discovery order next, and closure environments last,
then constructs every field exactly once from left to right. The resulting
managed references cross the same bindings, direct and closure call boundaries,
captures, control-flow joins, returns, and tail-call operands as managed Text.

The same opt-in backend stage now supports source-ordered tuple and local-
constructor matching, including nested tuple and constructor patterns,
as-patterns, and top-level alternatives. This is backend support for existing
Jazz cases, not a change to public language semantics or the IR version.

Lists and list fields, list patterns, other managed scrutinees, product or
variant equality, first-class non-nullary constructors, and Text literal
patterns remain deferred. Pattern lambdas remain deferred because a parameter
mismatch happens at invocation time and therefore needs a match-failure
contract integrated with closures, currying, recursion, and callable parameter
identity. Imported data and complete multi-module integration, later or
interleaved external captures, scalar exports, runtime-host and runtime ABI
changes, and native execution also remain outside the profile. Ordinary compile
and run modes continue to use canonical core and the interpreter.

Backend preparation also covers only a subset of the language. The current
supported forms and exclusions are maintained in
[Project status](../project/status.md).

## Promotion

A hosted stage is promoted only when it covers its accepted input domain,
matches the canonical stage deterministically, preserves structured failures,
and integrates with the next pipeline stage. Self-hosting additionally requires
a complete Jazz-authored semantic compiler, full module integration, native
emission and linking, a runtime, and end-to-end conformance.

Until those gates pass, hosted components are tested compiler stages rather
than the canonical shipping pipeline. See the [roadmap](../project/roadmap.md)
and current [status](../project/status.md).
