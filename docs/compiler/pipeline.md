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

The optional backend path begins during analysis when a program fits the
supported typed-core profile. It finalizes typed core, validates it, lowers it
to backend-neutral IR, and validates the lowered program. Failure to fit this
profile does not alter ordinary canonical-core compilation or interpretation.

A qualifying program can carry concrete scalar bindings, direct calls, named
function values, anonymous and nested unary closures, and captured scalar or
closure values into the lowered control-flow representation. Capture fields
are ordered by first use and stored in immutable environments. Curried source
applications retain one unary closure stage per application, so partial
application returns the remaining closure and additional arguments are applied
in order only while each intermediate result remains callable.

This path does not participate in ordinary compile or run, which remain on
canonical core and the reference interpreter. Recursion, full control flow,
scalar exports, multi-module integration, native emission, linking, and a native
runtime remain promotion gates. See [Compiler architecture](architecture.md)
and the current [project status](../project/status.md).
