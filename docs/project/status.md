---
title: Project status
description: See the implemented, partial, and planned Jazz language and compiler capabilities.
sidebar_position: 1
---

Updated: 2026-08-13

Jazz is experimental and pre-1.0. This matrix separates implemented behavior
from partial areas and planned work.

| Area                                                                  | Status      | Evidence                                                              |
| --------------------------------------------------------------------- | ----------- | --------------------------------------------------------------------- |
| Source, literals, bindings, lambdas, blocks, and operators            | Implemented | [Language overview](../language/overview.md)                          |
| ADTs, typed patterns, ordered cases, and guards                       | Implemented | [ADTs and patterns](../language/algebraic-data-types-and-patterns.md) |
| Static exhaustiveness and unreachable-arm analysis                    | Planned     | [Control flow](../language/control-flow.md)                           |
| Type inference, signatures, generic named types, and numeric widths   | Implemented | [Types and signatures](../language/types-and-signatures.md)           |
| Modules, import visibility, explicit exports, and cycle diagnostics   | Implemented | [Module resolution](../reference/module-resolution.md)                |
| Interpreter, stable rendering, runtime hosts, and observations        | Implemented | [Runtime values](../reference/runtime-values.md)                      |
| Bundled Prelude and explicit-import collection, text, and I/O modules | Implemented | [Standard library](../standard-library/overview.md)                   |
| Structured errors and opt-in warning policy                           | Implemented | [Diagnostics](../reference/diagnostics.md)                            |
| Capability declarations and concrete method dispatch                  | Partial     | [Capabilities](../language/capabilities.md)                           |
| Name-based purity analysis                                            | Partial     | [Purity](../language/purity.md)                                       |
| Jazz-authored lexer, parser, and canonical-core lowering              | Partial     | [Bootstrapping](../compiler/bootstrapping.md)                         |
| Typed-core production and backend-neutral IR lowering                 | Partial     | [Compiler pipeline](../compiler/pipeline.md)                          |
| Canonical Jazz-authored semantic compiler                             | Planned     | [Roadmap](roadmap.md)                                                 |
| Native code generation, linking, and runtime                          | Planned     | [Roadmap](roadmap.md)                                                 |
| Stable releases, package ecosystem, and language server               | Planned     | [Roadmap](roadmap.md)                                                 |

`Partial` means that working, tested behavior has an explicit boundary. The
typed-core and backend-neutral lowering path currently covers scalar bindings,
direct calls, function values, unary closures, lexical capture, higher-order
calls, partial application, ordered application of additional arguments, and
capture-free, non-escaping direct self and mutual recursion. It also covers
closure-shaped self and mutual recursion when every external capture precedes
the first group member. These groups reuse one immutable shared environment
containing ordered external captures and reconstruct self or peer closures
without cyclic initialization. Bounded value-producing conditionals can nest
throughout that profile and lower to deterministic multi-block control flow
with explicit branch and join transport. It excludes later or interleaved
external captures, pattern cases and guards, scalar exports, and complete
multi-module integration. Ordinary compile and run modes remain on canonical
core and the interpreter.
