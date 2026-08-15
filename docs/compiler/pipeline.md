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
conditionals and scalar pattern cases may nest within all of these expressions.
In value positions, lowering evaluates each condition or scrutinee once,
preserves source-ordered scalar literal, wildcard, and variable arms with
false-guard fallthrough, and transports the selected result through explicit
branch and join edges. A final unguarded wildcard or variable is required by
this opt-in backend profile.

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

Managed constructor, list, and tuple patterns, managed scrutinees, and Text
literal patterns remain outside the path. Pattern lambdas remain outside it
because invocation-time mismatch must be defined across closure construction,
currying, recursion, and callable identity. Text uncons, from-chars, concat,
I/O, and collection transport also remain separate contracts. Source-level
exhaustiveness and unreachable-arm diagnostics are implemented under RFC 0012;
the backend profile's final-catch-all requirement is a separate lowering
boundary. Later or interleaved external captures, scalar exports, complete
multi-module integration, native emission, linking, and a native runtime also
remain outside this path.
