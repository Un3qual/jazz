---
title: Maybe
description: Represent a value that may be absent.
---

Import `Maybe` when an operation may return no value. The module exposes the
type, both constructors, and helpers for transforming or eliminating the
optional branch.

## Type and constructors

### `Maybe`

<!-- jazz-signature -->

```jazz
data Maybe a = Nothing | Just a.
```

`Maybe(a)` contains either no value or one value of type `a`.

### `Nothing`

Constructs an absent `Maybe(a)`. The expected type determines `a`.

### `Just`

Constructs a present `Maybe(a)` from one value. Both constructors are public
and may be used in [patterns](../language/algebraic-data-types-and-patterns.md).

## Transforming

### `maybeMap`

<!-- jazz-signature -->

```jazz
maybeMap :: (a -> b) -> Maybe(a) -> Maybe(b).
```

Applies the function to the value inside `Just`. `Nothing` passes through
unchanged. This is `O(1)` apart from the function call.

Example: `maybeMap (\(value) -> value * 2) (Just 3)` produces `Just 6`.

### `maybeAndThen`

<!-- jazz-signature -->

```jazz
maybeAndThen :: (a -> Maybe(b)) -> Maybe(a) -> Maybe(b).
```

Calls the function for `Just` and returns its `Maybe` result without adding
another layer. `Nothing` skips the function. This is `O(1)` apart from the
function call.

### `maybeFilter`

<!-- jazz-signature -->

```jazz
maybeFilter :: (a -> Bool) -> Maybe(a) -> Maybe(a).
```

Keeps a present value when the predicate returns `True`; otherwise returns
`Nothing`. An absent value never calls the predicate.

Example: `maybeFilter (\(value) -> value > 0) (Just -2)` produces `Nothing`.

## Defaults and alternatives

### `maybeWithDefault`

<!-- jazz-signature -->

```jazz
maybeWithDefault :: a -> Maybe(a) -> a.
```

Returns the value inside `Just`, or the first argument when the value is
`Nothing`. Example: `maybeWithDefault 0 Nothing` produces `0`.

### `maybeOrElse`

<!-- jazz-signature -->

```jazz
maybeOrElse :: Maybe(a) -> Maybe(a) -> Maybe(a).
```

Returns the second argument when it is `Just`; otherwise returns the fallback
passed first. Example: `maybeOrElse (Just 4) Nothing` produces `Just 4`.

## Inspection

### `maybeIsJust`

<!-- jazz-signature -->

```jazz
maybeIsJust :: Maybe(a) -> Bool.
```

Returns `True` for `Just` and `False` for `Nothing`. This is `O(1)`.

### `maybeIsNothing`

<!-- jazz-signature -->

```jazz
maybeIsNothing :: Maybe(a) -> Bool.
```

Returns `True` for `Nothing` and `False` for `Just`. This is `O(1)`.

## Conversion

### `maybeToList`

<!-- jazz-signature -->

```jazz
maybeToList :: Maybe(a) -> [a].
```

Converts `Nothing` to `[]` and `Just value` to `[value]`. This is `O(1)`.

### `maybeFromList`

<!-- jazz-signature -->

```jazz
maybeFromList :: [a] -> Maybe(a).
```

Returns `Just` containing the first list value, or `Nothing` for an empty list.
The remainder of a non-empty list is ignored. This is `O(1)`.

Use [Result](result.md) instead when the absent branch should carry an error.
