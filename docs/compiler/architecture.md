---
title: Compiler architecture
description: Follow the stages that turn Jazz source into diagnostics or an evaluated value.
sidebar_position: 1
---

The Jazz toolchain is a compiler front end and interpreter. Ordinary compile
and run mode use canonical core as their executable representation. A separate,
bounded path produces typed core and backend-neutral lowered IR for future
native compilation.

## Source and modules

Compilation begins by selecting the user source and the bundled, explicit, or
absent Prelude. Module mode locates an entry module, follows imports, rejects
cycles, and orders dependencies before their consumers. Each source unit is
loaded once for the resolved graph.

The output of this stage is either one source unit or a deterministic module
graph with explicit dependency relationships.

## Parse

Lexing divides source into located tokens. Parsing determines their structure
and lowers the surface program into canonical core. Failures retain source
spans and become structured diagnostics.

Canonical core removes surface-only notation while preserving the bindings,
expressions, patterns, declarations, and module information needed by later
stages.

## Resolve

Name resolution connects references to visible local, Prelude, or imported
definitions. In module mode, each module is resolved against dependency
interfaces and its private local inventory. Explicit export lists determine the
typed interface published to consumers.

The result is canonical core with unambiguous names plus the module interfaces
needed for dependency checking and runtime publication.

## Analyze

Semantic analysis checks scopes, binding relationships, patterns, signatures,
types, capability requirements, and the current purity rules. Type inference
adds types where no signature is written and validates explicit signatures
where they are present.

Analysis retains one semantic program and accumulates structured diagnostics;
it does not print messages or execute user expressions.

## Diagnose

Errors and warnings share one source-ordered diagnostic stream. Warning
configuration controls which warning categories are enabled and which are
promoted to errors. Rendering into terminal text happens only at the reporting
boundary, so diagnostic identity and ordering do not depend on presentation.

Any error-severity compile diagnostic prevents evaluation.

## Interpret

Run mode evaluates canonical core after successful analysis. Module
dependencies publish their selected runtime exports without executing their
top-level expression statements; the entry module then evaluates its own
expressions. Host operations for files, streams, arguments, and exit pass
through the runtime host boundary.

The interpreter produces a value or a stable runtime diagnostic. Optional
statistics and profiles observe evaluation without changing the result.

## Prepare a backend

An opt-in path retains typed information from analysis, validates it, lowers it
to backend-neutral control-flow IR, and validates the lowered result. Programs
outside the supported subset continue through ordinary compilation unchanged.

This stage does not participate in ordinary compile or run mode. See
[Project status](../project/status.md) for its current language coverage and
[Bootstrapping](bootstrapping.md) for how a compiler stage becomes canonical.
