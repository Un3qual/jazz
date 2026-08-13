---
title: NonEmpty
description: Represent a list with a statically present first value.
---

Import `NonEmpty` to use `NonEmpty(a)`, represented publicly as
`NonEmpty a [a]`. The first field is the head and the list field is the tail.

## Construction and conversion

- `nonEmptySingleton` constructs a one-value sequence.
- `nonEmptyFromList` returns `Nothing` only for `[]`.
- `nonEmptyToList` returns the head followed by the tail.
- `nonEmptyPrepend` adds a new head.
- `nonEmptyAppendList` appends an ordinary list.

## Querying and traversal

`nonEmptyHead` and `nonEmptyTail` are total. The module also exports
`nonEmptyLast`, `nonEmptyLength`, `nonEmptyMap`, `nonEmptyFoldLeft`, and
`nonEmptyFoldRight`.

Head, tail, singleton construction, prepend, and conversion to or from a list
are `O(1)`. Traversal, mapping, length, last, append, and folds are `O(n)`.
The constructor is public and can be used in
[patterns](../language/algebraic-data-types-and-patterns.md).
