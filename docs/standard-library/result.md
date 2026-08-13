---
title: Result
description: Represent a successful value or a recoverable error.
---

Import `Result` to use `Result(e, a)`. `Err e` carries a recoverable error and
`Ok a` carries a successful value.

## Transforming values

- `resultMap` transforms the value inside `Ok`.
- `resultMapError` transforms the error inside `Err`.
- `resultAndThen` sequences an operation that can return another `Result`.
- `resultRecover` maps an error into a replacement result.

## Defaults and inspection

`resultWithDefault` selects an `Ok` value or a supplied default.
`resultIsOk` and `resultIsErr` inspect the active constructor.

## Conversions

`resultToMaybe` keeps an `Ok` value and discards an error.
`resultErrorToMaybe` keeps an `Err` value and discards a success.
`resultFromMaybe` supplies an error for `Nothing` and converts `Just` to `Ok`.

These operations are `O(1)` apart from invoked callbacks. A transformation
preserves the branch it does not target. The constructors are public and can be
used in [patterns](../language/algebraic-data-types-and-patterns.md). See
[Maybe](maybe.md) when absence does not need an error value.
