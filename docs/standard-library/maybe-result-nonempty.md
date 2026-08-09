---
title: Maybe, Result, and NonEmpty
description: Model optional values, recoverable errors, and statically non-empty lists.
sidebar_position: 4
---

## Maybe

`Maybe(a) = Nothing | Just a`. Public helpers are `maybeMap`,
`maybeAndThen`, `maybeWithDefault`, `maybeOrElse`, `maybeFilter`,
`maybeIsJust`, `maybeIsNothing`, `maybeToList`, and `maybeFromList`.
`maybeFromList` returns the first value and ignores the tail.

## Result

`Result(e, a) = Err e | Ok a`. Public helpers are `resultMap`,
`resultMapError`, `resultAndThen`, `resultRecover`, `resultWithDefault`,
`resultIsOk`, `resultIsErr`, `resultToMaybe`, `resultErrorToMaybe`, and
`resultFromMaybe`.

Maybe and Result transformations are `O(1)` apart from invoked callbacks. They
preserve the branch they do not target and do not convert the absent or error
branch merely to select a supplied default.

## NonEmpty

`NonEmpty(a)` is publicly represented as `NonEmpty a [a]`. Construct it with
`nonEmptySingleton` or use `nonEmptyFromList`, which returns `Nothing` only for
`[]`. `nonEmptyHead` and `nonEmptyTail` are total.

The module also exports `nonEmptyToList`, `nonEmptyLast`, `nonEmptyPrepend`,
`nonEmptyAppendList`, `nonEmptyMap`, `nonEmptyLength`, `nonEmptyFoldLeft`, and
`nonEmptyFoldRight`. Head, tail, singleton construction, prepend, and conversion
to or from a list are `O(1)`; traversal, mapping, length, last, append, and folds
are `O(n)`.

These modules expose their constructors for ordinary
[pattern matching](../language/algebraic-data-types-and-patterns.md).
