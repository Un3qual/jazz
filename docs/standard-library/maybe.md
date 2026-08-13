---
title: Maybe
description: Represent a value that may be absent.
---

Import `Maybe` to use the `Maybe(a)` type and its helpers. The type has two
constructors: `Nothing` for absence and `Just a` for a present value.

## Transforming values

- `maybeMap` transforms the value inside `Just` and leaves `Nothing` unchanged.
- `maybeAndThen` sequences an operation that may also return `Nothing`.
- `maybeFilter` keeps a present value only when its predicate returns `True`.

## Defaults and alternatives

- `maybeWithDefault` returns the present value or a supplied default.
- `maybeOrElse` selects an alternative `Maybe` when the first is `Nothing`.
- `maybeIsJust` and `maybeIsNothing` inspect the active constructor.

## Conversions

`maybeToList` converts `Nothing` to `[]` and `Just value` to `[value]`.
`maybeFromList` returns the first list value and ignores the tail, or returns
`Nothing` for an empty list.

These operations are `O(1)` apart from invoked callbacks. The constructors are
public and can be used in [patterns](../language/algebraic-data-types-and-patterns.md).
See [Result](result.md) when an absent branch needs an error value.
