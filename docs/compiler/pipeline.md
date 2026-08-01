---
title: Compiler pipeline
description: Follow source through parsing, analysis, module interfaces, and interpreted execution.
sidebar_position: 2
---

# Compiler pipeline

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
direct-call profile during inference, then lower it to validated
backend-neutral control-flow IR. This path does not participate in ordinary
compile or run. Closure/recursion lowering, full control flow, multi-module
integration, native emission, linking, and a native runtime remain promotion
gates. See the current [status](../project/status.md).
