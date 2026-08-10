---
title: Project status
description: See the implemented, partial, and planned Jazz language and compiler capabilities.
sidebar_position: 1
---

Updated: 2026-07-31

Implementation snapshot: `b0ff07799029c27728799b817488d5bead85ee72`

Jazz is experimental and pre-1.0. This matrix describes the unchanged compiler
implementation snapshot above; documentation-only commits do not alter that
semantic baseline.

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

Only `Implemented`, `Partial`, and `Planned` are used as status labels. A
partial area has working, tested behavior but retains a stated boundary.

The opt-in typed-core and backend-neutral lowering profile includes closed
named functions as values, recursively represented unary closure parameters
and results, explicit empty environments, and unary higher-order closure calls.
Ordinary compile and run remain on canonical core and the reference interpreter.
The opt-in profile still excludes scalar bindings, anonymous or nested
closures, lexical capture, currying and partial application, oversaturation,
and recursion.
