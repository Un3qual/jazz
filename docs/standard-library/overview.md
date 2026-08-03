---
title: Standard library
description: Survey the bundled Prelude and explicit-import Jazz library modules.
sidebar_position: 1
---

The bundled [Prelude](prelude.md) loads automatically unless the CLI uses
`--no-prelude`. Other modules require an explicit import.

| Module            | Public abstraction          | Purpose                                                                  |
| ----------------- | --------------------------- | ------------------------------------------------------------------------ |
| `Prelude`         | `Ordering` and capabilities | Scalar capabilities, conversions, and basic helpers                      |
| `List`            | built-in `[a]`              | Total list queries, transformations, folds, grouping, and stable sorting |
| `Maybe`, `Result` | public ADTs                 | Optional and recoverable values                                          |
| `NonEmpty`        | public ADT                  | Lists with a statically present first value                              |
| `Dictionary`      | abstract                    | Insertion-ordered lookup using `Eq` keys                                 |
| `Queue`           | abstract                    | Persistent first-in, first-out traversal                                 |
| `Map`, `Set`      | abstract                    | Persistent ordered collections using `Ord` values                        |
| `Char`, `Text`    | built-in values             | Unicode-scalar conversion and text processing                            |
| `IOError`, `IO`   | public errors and effects   | Strict UTF-8 host files, streams, arguments, and exit                    |

`Maybe`, `Result`, and `NonEmpty` expose constructors. `Dictionary`, `Queue`,
`Map`, and `Set` keep their representations private. All collection updates
return new values; older values remain usable.

Complexity statements describe logical Jazz operations. `n` is a collection
or input length, `m` is a second input length, and `k` is output length.
Callback cost is excluded unless stated otherwise.
