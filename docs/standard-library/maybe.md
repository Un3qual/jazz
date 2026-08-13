---
title: Maybe
description: Represent a value that may be absent.
---

Use `Maybe` when a value may be absent and the absent case needs no error
information.

## Type and constructors

### `Maybe`

```jazz jazz-signature
data Maybe a = Nothing | Just a.
```

`Maybe(a)` contains either no value or one value of type `a`.

### `Nothing`

The expected type determines the parameter of `Nothing`.

### `Just`

Both constructors are available to
[patterns](../language/algebraic-data-types-and-patterns.md).

## Transforming

### `maybeMap`

```jazz jazz-signature
maybeMap :: (a -> b) -> Maybe(a) -> Maybe(b).
```

Calls the function for `Just`; `Nothing` passes through unchanged.

### `maybeAndThen`

```jazz jazz-signature
maybeAndThen :: (a -> Maybe(b)) -> Maybe(a) -> Maybe(b).
```

Calls the function for `Just` without nesting its `Maybe` result. `Nothing`
skips the function.

### `maybeFilter`

```jazz jazz-signature
maybeFilter :: (a -> Bool) -> Maybe(a) -> Maybe(a).
```

Keeps a present value when the predicate returns `True`; otherwise returns
`Nothing`. An absent value never calls the predicate.

These transformations are `O(1)` apart from callback work.

## Defaults and alternatives

### `maybeWithDefault`

```jazz jazz-signature
maybeWithDefault :: a -> Maybe(a) -> a.
```

Returns the first argument only for `Nothing`.

### `maybeOrElse`

```jazz jazz-signature
maybeOrElse :: Maybe(a) -> Maybe(a) -> Maybe(a).
```

Returns the second argument when it is `Just`; otherwise returns the fallback
passed first.

## Inspection

### `maybeIsJust`

```jazz jazz-signature
maybeIsJust :: Maybe(a) -> Bool.
```

### `maybeIsNothing`

```jazz jazz-signature
maybeIsNothing :: Maybe(a) -> Bool.
```

## Conversion

### `maybeToList`

```jazz jazz-signature
maybeToList :: Maybe(a) -> [a].
```

Maps `Nothing` to `[]` and `Just value` to `[value]`.

### `maybeFromList`

```jazz jazz-signature
maybeFromList :: [a] -> Maybe(a).
```

Uses the first list value, or returns `Nothing` for an empty list. The remainder
is ignored.

Use [Result](result.md) instead when the absent branch should carry an error.
