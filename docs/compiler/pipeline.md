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
conditionals may nest within all of these expressions. Lowering evaluates each
condition once, selects one branch, and resumes through a result join while
transporting block-local ambient and in-flight values as explicit edge
arguments. Bounded scalar pattern cases may nest there too. Lowering evaluates
their scrutinee once, tries scalar literal, wildcard, and variable arms in
source order, continues after false guards, scopes variable binders to one arm,
and transports the selected result through one join. A final unguarded
wildcard or variable is required by this opt-in backend profile; that syntactic
gate is not a public exhaustiveness result.

Managed patterns remain outside the path until managed values have stable
layouts, tags, projections, and ownership during production and lowering.
Pattern lambdas remain outside it because invocation-time mismatch must be
defined across closure construction, currying, recursion, and callable
identity. Exhaustiveness and unreachable-arm diagnostics remain separate
because they need coverage reasoning and a language-level diagnostic policy.
Later or interleaved external captures, scalar exports, complete multi-module
integration, native emission, linking, and a native runtime also remain outside
this path.
