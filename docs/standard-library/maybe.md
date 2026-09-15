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

### `map`

```jazz jazz-signature
map :: (a -> b) -> Maybe(a) -> Maybe(b).
```

Calls the function for `Just`; `Nothing` passes through unchanged.

### `andThen`

```jazz jazz-signature
andThen :: (a -> Maybe(b)) -> Maybe(a) -> Maybe(b).
```

Calls the function for `Just` without nesting its `Maybe` result. `Nothing`
skips the function.

### `filter`

```jazz jazz-signature
filter :: (a -> Bool) -> Maybe(a) -> Maybe(a).
```

Keeps a present value when the predicate returns `True`; otherwise returns
`Nothing`. An absent value never calls the predicate.

These transformations are `O(1)` apart from callback work.

## Defaults and alternatives

### `withDefault`

```jazz jazz-signature
withDefault :: a -> Maybe(a) -> a.
```

Returns the first argument only for `Nothing`.

### `orElse`

```jazz jazz-signature
orElse :: Maybe(a) -> Maybe(a) -> Maybe(a).
```

Returns the second argument when it is `Just`; otherwise returns the fallback
passed first.

## Inspection

### `isJust`

```jazz jazz-signature
isJust :: Maybe(a) -> Bool.
```

### `isNothing`

```jazz jazz-signature
isNothing :: Maybe(a) -> Bool.
```

## Conversion

### `toList`

```jazz jazz-signature
toList :: Maybe(a) -> [a].
```

Maps `Nothing` to `[]` and `Just value` to `[value]`.

### `fromList`

```jazz jazz-signature
fromList :: [a] -> Maybe(a).
```

Uses the first list value, or returns `Nothing` for an empty list. The remainder
is ignored.

Use [Result](result.md) instead when the absent branch should carry an error.

## Generic methods

Importing Maybe supplies element-based Equatable, Mappable, and Reducible instances. Mapping preserves Nothing; either fold returns its initial value for Nothing.
