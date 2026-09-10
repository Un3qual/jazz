---
title: Project status
description: See the implemented, partial, and planned Jazz language and compiler capabilities.
sidebar_position: 1
---

Updated: 2026-09-10

Jazz is experimental and pre-1.0. This matrix separates implemented behavior
from partial areas and planned work.

| Area                                                                  | Status      | Evidence                                                              |
| --------------------------------------------------------------------- | ----------- | --------------------------------------------------------------------- |
| Source, literals, bindings, lambdas, blocks, and operators            | Implemented | [Language overview](../language/overview.md)                          |
| ADTs, typed patterns, ordered cases, and guards                       | Implemented | [ADTs and patterns](../language/algebraic-data-types-and-patterns.md) |
| Static exhaustiveness and unreachable-arm analysis                    | Implemented | [Control flow](../language/control-flow.md)                           |
| Type inference, signatures, generic named types, and numeric widths   | Implemented | [Types and signatures](../language/types-and-signatures.md)           |
| Modules, qualified classes, explicit exports/re-exports, and cycles   | Implemented | [Module resolution](../reference/module-resolution.md)                |
| Interpreter, stable rendering, runtime hosts, and observations        | Implemented | [Runtime values](../reference/runtime-values.md)                      |
| Bundled Prelude and explicit-import collection, text, and I/O modules | Implemented | [Standard library](../standard-library/overview.md)                   |
| Structured errors and opt-in warning policy                           | Implemented | [Diagnostics](../reference/diagnostics.md)                            |
| Capability declarations and concrete method dispatch                  | Partial     | [Capabilities](../language/capabilities.md)                           |
| Name-based purity analysis                                            | Partial     | [Purity](../language/purity.md)                                       |
| Jazz-authored lexer, parser, and canonical-core lowering              | Partial     | [Bootstrapping](../compiler/bootstrapping.md)                         |
| Canonical Jazz-authored semantic compiler                             | Planned     | [Roadmap](roadmap.md)                                                 |
| Native code generation, linking, and runtime                          | Planned     | [Roadmap](roadmap.md)                                                 |
| Stable releases, package ecosystem, and language server               | Planned     | [Roadmap](roadmap.md)                                                 |

`Partial` means that working, tested behavior has an explicit boundary.
Capability dispatch supports the current concrete profile, and purity uses the
current name-based rules. The Jazz-authored frontend has differential coverage
but is not yet the canonical semantic compiler. Ordinary execution uses the
Haskell compiler and analyzed-core interpreter; native compilation remains
planned.

The Haskell compiler supports alias-qualified class methods, constraints, and
impl heads. Hosted frontend support is deferred, including existing parser
parity comparisons affected by the new qualification grammar and diagnostics.
