---
title: Standard library
description: Browse the bundled Prelude and explicit-import library modules.
sidebar_position: 1
---

The bundled [Prelude](prelude.md) loads automatically unless the CLI uses
`--no-prelude`. Other library modules require an explicit import. Collection
updates return new values, so older values remain usable.

Complexity statements describe logical Jazz operations. Callback cost is
excluded unless a page states otherwise.

## Data

- [Maybe](maybe.md) — optional values.
- [Result](result.md) — successful values and recoverable errors.
- [NonEmpty](nonempty.md) — lists with a statically present first value.

## Collections

- [List](list.md) — total access, transformation, folds, grouping, and sorting.
- [Dictionary](dictionary.md) — insertion-ordered lookup with `Eq` keys.
- [Queue](queue.md) — persistent first-in, first-out traversal.
- [Map](map.md) — ordered key-value storage with `Ord` keys.
- [Set](set.md) — persistent ordered unique values.

## Text

- [Char](char.md) — Unicode scalar conversion and classification.
- [Text](text.md) — immutable Unicode-scalar text processing.

## System

- [IO](io.md) — files, streams, arguments, and process exit.
- [IOError](io-error.md) — stable error categories and values.
