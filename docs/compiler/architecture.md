---
title: Compiler architecture
description: Follow the stages that turn Jazz source into diagnostics or an evaluated value.
sidebar_position: 1
---

The Jazz toolchain is a compiler front end and interpreter. Its canonical core
is indexed by compiler phase: lowering constructs syntax, resolution attaches
stable binding identities, and analysis constructs checked trees containing the
semantic decisions used by execution.

## Source and modules

Compilation begins by selecting the user source and the bundled, explicit, or
absent Prelude. Module mode locates an entry module, follows imports, rejects
cycles, and orders dependencies before their consumers. Each source unit is
loaded once for the resolved graph.

Both standalone and module inputs become one dependency-ordered program graph.
The Prelude retains its own source identity and analyzed artifact.

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

Resolution publishes lexical groups, captures, and validated import visibility
once. Operator values become callable references; declared binary operators and
sections become applications and capturing lambdas. Primitive operations retain
their distinct evaluation and promotion rules. Public interfaces carry the
exported names, closed schemes, and capability identities needed by consumers.

## Analyze

Semantic analysis checks scopes, binding relationships, patterns, signatures,
types, capability requirements, and the current purity rules. Type inference
adds types where no signature is written and validates explicit signatures
where they are present.

Checking returns each subtree with its type, binding schemes, patterns, explicit
instantiation arguments, and selected method evidence. Finalization applies solved
substitutions and checks completeness. It does not reconstruct lexical scope or
repeat inference. The analyzed artifact retains structured diagnostics.

## Diagnose

Errors and warnings share one source-ordered diagnostic stream. Warning
configuration controls which warning categories are enabled and which are
promoted to errors. Rendering into terminal text happens only at the reporting
boundary, so diagnostic identity and ordering do not depend on presentation.

Any error-severity compile diagnostic prevents evaluation.

## Interpret

Run mode consumes checked types, ordered instantiations, result representations,
and selected method identities directly. Calls whose implementation remains open
use dynamic dispatch.

One program traversal evaluates the Prelude and modules in dependency order.
Dependencies publish exports without forcing top-level bindings or executing
expression statements. One scope traversal owns sequential execution and
definition-site environments for both pure and host evaluation. Pure cells use
lazy values; host cells use an evaluation-instance cache so repeated forcing does
not repeat effects or share state across closure calls. Host operations for files,
streams, arguments, and exit pass through the runtime host boundary.

The interpreter produces a value or a stable runtime diagnostic. Optional
statistics and profiles observe evaluation without changing the result.
