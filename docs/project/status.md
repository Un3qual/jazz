---
title: Project status
description: See the implemented, partial, and planned Jazz language and compiler capabilities.
sidebar_position: 1
---

Updated: 2026-08-27

Jazz is experimental and pre-1.0. This matrix separates implemented behavior
from partial areas and planned work.

| Area                                                                  | Status      | Evidence                                                              |
| --------------------------------------------------------------------- | ----------- | --------------------------------------------------------------------- |
| Source, literals, bindings, lambdas, blocks, and operators            | Implemented | [Language overview](../language/overview.md)                          |
| ADTs, typed patterns, ordered cases, and guards                       | Implemented | [ADTs and patterns](../language/algebraic-data-types-and-patterns.md) |
| Static exhaustiveness and unreachable-arm analysis                    | Implemented | [Control flow](../language/control-flow.md)                           |
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
without cyclic initialization. Bounded value-producing conditionals and scalar
pattern cases can nest throughout that profile. In value positions, their
deterministic multi-block control flow preserves explicit branch and join
transport: a case evaluates its scrutinee once, retains source-ordered literal,
wildcard, and variable arms, falls through false guards, and keeps variable
binders arm-local.

For complete named or lifted function results, the profile records direct and
closure tail intent. It propagates that result position through selected
conditional branches and bounded scalar-case bodies, which terminate directly
without result joins. Conditions, scrutinees, guards, operands, and nested value
contexts remain ordinary value positions. Partial applications still return
closure values; oversaturated calls tail-terminate only at the final exact
stage; and module entry remains ordinary call/join/return lowering. This uses
the existing Lowered IR schema, format, and validator and changes neither the
runtime ABI, public language semantics, hosted compiler, nor native-stack
behavior. The opt-in profile still requires a final unguarded wildcard or
variable as its independent lowering-totality boundary; source-level static
exhaustiveness and unreachable-arm analysis are implemented under RFC 0012.

Managed `Text` construction and transport now spans bindings, parameters,
results, captures, calls, conditional and scalar-case results, returns, and
tail-call operands. The Lowered IR path uses one semantic Text layout and exact
pure services for equality, length, append, and append-char; inequality reuses
equality followed by Boolean-not. Text-only transport declares no service, and
referenced services are deduplicated in fixed catalog order.

Non-unit tuple and exactly saturated local algebraic-data construction and
transport now span the same complete profile. Concrete generic, recursive, and
mutually recursive data layouts may contain admitted scalars, Text, closures,
products, or variants. Lowering gives products structural semantic identities,
variants nominal semantic identities, and constructors declaration-ordered
zero-based tags; it emits deduplicated layouts deterministically and evaluates
every field exactly once from left to right.

Constructor and tuple destructuring patterns and other managed scrutinees remain
deferred pending the separately ordered RFC 0015 pattern child. Lists and list
fields, product or variant equality, first-class non-nullary constructors,
pattern-lambda backend lowering, Text uncons/from-chars/concat/I/O, imported
data, complete multi-module integration, later or interleaved external
captures, and scalar exports remain excluded. No managed product or variant
work adds a `RuntimeHost` operation, runtime ABI, or native execution path.
Ordinary compile and run modes remain on canonical core and the interpreter.
