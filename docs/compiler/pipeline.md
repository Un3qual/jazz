---
title: Compiler pipeline
description: Compare standalone and module-graph compilation through the shared compiler stages.
sidebar_position: 2
---

Jazz uses the same parsing, semantic analysis, diagnostic, and interpretation
model for standalone source and module graphs. Module compilation adds graph
discovery, dependency interfaces, and explicit runtime exports.

## Standalone source

1. **Source and modules:** select the user source and bundled, explicit, or
   absent Prelude.
2. **Parse:** lex and parse each selected source, then construct canonical core.
3. **Resolve:** merge the Prelude's ambient declarations and resolve visible
   names.
4. **Analyze:** check scopes and patterns, infer and check types, and validate
   capability and purity rules.
5. **Diagnose:** apply warning enablement and promotion to the ordered
   diagnostic stream.
6. **Interpret:** in run mode, evaluate canonical core only when compilation
   produced no errors.

## Module graphs

1. **Source and modules:** resolve the entry path against ordered module roots,
   follow imports, and build a dependency-first graph.
2. **Parse:** parse and lower every selected source once.
3. **Resolve:** compile the Prelude once, resolve each module against imported
   interfaces and private local definitions, and publish only selected typed
   exports.
4. **Analyze:** apply the same semantic checks used for standalone source to
   each module in dependency order.
5. **Diagnose:** retain one ordered stream across graph and compile failures.
6. **Interpret:** establish dependency runtime exports without running
   dependency expression statements, then execute the entry module.

## Backend preparation

When a program fits the supported typed-core subset, analysis can retain its
types for backend preparation. The compiler validates typed core, lowers it to
backend-neutral IR, and validates the lowered program. A program outside that
subset still follows the ordinary canonical-core pipeline.

Ordinary compile and run modes do not enter this path. Its current coverage and
remaining promotion gates are listed in [Project status](../project/status.md);
[Compiler architecture](architecture.md) explains the responsibility of each
representation.

The supported subset includes scalar bindings, direct calls, function values,
unary closures, lexical capture, higher-order calls, curried application, and
capture-free, non-escaping direct self and mutual recursion. Closure-shaped
self and mutual recursion is also supported when every external capture is
available before the first group member. These groups share one immutable
environment containing ordered external captures, and reconstruct self or peer
closures from it without cyclic initialization. Bounded value-producing
conditionals and the admitted scalar, tuple, and local-constructor pattern cases
may nest within all of these expressions. In value positions, lowering evaluates
each condition or scrutinee once, preserves source-ordered arms with nested
pattern-failure and false-guard fallthrough, and transports the selected result
through explicit edges. Tuple fields are matched in source order. A variant tag
is tested before any field, and only the selected tag's fields are projected.
Pattern binders become visible only after a complete match and remain local to
the selected guard and body.

The backend checks totality independently from source pattern coverage. Guarded
rows do not cover. Complete closed local-constructor sets and the single tuple
shape need no synthetic wildcard, while open scalar literal domains require an
unguarded catch-all.

For a complete named or lifted function result, lowering records direct or
closure tail intent instead. The result position recurses into selected
conditional branches and bounded scalar-case bodies, so they terminate directly
without a result join. Conditions, scrutinees, guards, operands, and nested
value contexts remain value positions. Partial applications still return
closure values, and oversaturated calls tail-terminate only at their final exact
stage. Module entry remains ordinary call/join/return lowering. This records
intent in the existing Lowered IR only; it changes neither its schema, format,
or validator nor the runtime ABI, public language semantics, hosted compiler,
or native-stack behavior.

Managed `Text` values can cross the same bindings, call boundaries, captures,
control-flow edges, returns, and tail-call operands. One stable Text layout
supports literal construction and transport. Strict equality, length, append,
and append-char use exact pure runtime-service dependencies; inequality calls
equality and then Boolean-not. The lowerer emits only referenced services in a
fixed order and does not expose them through `RuntimeHost` or a native ABI.

The same opt-in producer now admits non-unit tuples and exactly saturated local
algebraic-data constructors. Concrete product layouts have structural semantic
identity; concrete generic, recursive, and mutually recursive variant layouts
have nominal semantic identity and declaration-ordered zero-based constructor
tags. Layouts are deduplicated and emitted after catalog-owned runtime layouts
and before closure environments. Tuple elements and constructor fields are
evaluated exactly once from left to right, and their managed references cross
the complete established binding, callable, capture, control-flow, return, and
tail-operand profile.

That opt-in backend stage now supports source-ordered tuple and local-
constructor matching, including nested tuple and constructor patterns,
as-patterns, and top-level alternatives. It reuses existing public case
semantics and the existing IR version; it does not change the public language
contract.

Lists and list fields, list patterns, other managed scrutinees, product or
variant equality, first-class non-nullary constructors, and Text literal
patterns remain outside the path. Pattern lambdas remain outside it because
invocation-time mismatch must be defined across closure construction, currying,
recursion, and callable identity. Text uncons, from-chars, concat, and I/O also
remain separate contracts. Source-level exhaustiveness and unreachable-arm
diagnostics remain independently implemented under RFC 0012. Imported data,
complete multi-module integration, later or interleaved external captures,
scalar exports, native emission, linking, runtime ABI changes, and a native
runtime also remain outside this path. Ordinary compile and run modes continue
to use canonical core and the interpreter.
