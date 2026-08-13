---
title: Compiler pipeline
description: Follow source through parsing, analysis, module interfaces, and interpreted execution.
sidebar_position: 2
---

## Standalone source

1. Select bundled, explicit, or absent Prelude source.
2. Lex and parse the user source, then lower it to canonical core.
3. Prepare the Prelude and merge its ambient declarations.
4. Resolve names, analyze scopes and patterns, infer and check types, validate
   capabilities and purity, and collect structured diagnostics.
5. Apply warning enablement and promotion.
6. In run mode, evaluate canonical core when no error-severity compile
   diagnostic exists.

## Module graphs

1. Resolve the entry path against ordered module roots.
2. Parse and lower each selected source once while building a deterministic,
   dependency-first graph.
3. Compile the Prelude once, then compile each module against explicit imported
   interfaces and its private local inventory.
4. Publish only the module's selected typed export inventory.
5. In run mode, establish dependency exports without running dependency
   expression statements, then execute entry-module expressions.

Compile and run results each retain one ordered diagnostic stream. Warning and
error views are severity filters rather than duplicate records. Optional
runtime statistics and profiles observe semantic events without changing
program results.

## Opt-in backend preparation

A bounded typed-core producer can retain a validated single-module scalar and
direct-call profile during inference. The opt-in path also transports closed
named functions as values, recursively represented unary closure parameters
and results, explicit empty environments, unary higher-order closure calls, and
anonymous or nested unary closures into validated backend-neutral control-flow
IR. Lexical capture resolves exact binder identities, orders scalar and
closure-valued environment fields by first occurrence, and emits immutable
environments with deterministic lifted function and layout identities.
Concrete scalar bindings are also retained in entry modules with explicitly
empty export lists, evaluated once in source order, and reused by exact binder
identity in later entry expressions. Scalar value interfaces are not produced
yet. Curried applications retain one unary closure stage per source
application. Partial application therefore produces the remaining closure
directly, while additional arguments are evaluated and applied in order only
when the preceding result is callable. Proven complete direct declaration
calls keep their coalesced direct-call lowering. Capture-free, non-escaping
direct self and mutual recursion transports ordered recursive groups by exact
binder identity, validates that metadata in Haskell and hosted Jazz, and reuses
the same direct-call representation during lowering. If any recursive member
escapes or captures a supported prior binder, the whole group is closure-shaped
and shares one immutable environment containing the ordered union of external
captures. The environment is constructed once; member closures reuse it, and
self/peer references reconstruct the corresponding closure from the current
environment. Captures introduced or rebound at or after the first member fail
the bounded producer profile.

This path does not participate in ordinary compile or run, which remain on
canonical core and the reference interpreter. Full control flow, multi-module
integration, tail-call guarantees, native emission, linking, and a native
runtime remain promotion gates. See the current [status](../project/status.md).
