---
title: Result
description: Represent a successful value or a recoverable error.
---

Import `Result` for operations with an explicit success or error branch.

## Type and constructors

### `Result`

```jazz jazz-signature
data Result e a = Err e | Ok a.
```

`Result(e, a)` contains an error of type `e` or a successful value of type `a`.

### `Err`

The error branch.

### `Ok`

The success branch. Both constructors are available to
[patterns](../language/algebraic-data-types-and-patterns.md).

## Transforming

### `resultMap`

```jazz jazz-signature
resultMap :: (a -> b) -> Result(e, a) -> Result(e, b).
```

Transforms the value inside `Ok` and preserves `Err` unchanged.

### `resultMapError`

```jazz jazz-signature
resultMapError :: (e -> f) -> Result(e, a) -> Result(f, a).
```

Transforms the value inside `Err` and preserves `Ok` unchanged.

### `resultAndThen`

```jazz jazz-signature
resultAndThen :: (a -> Result(e, b)) -> Result(e, a) -> Result(e, b).
```

Calls the function for `Ok` and returns its result without nesting. `Err` skips
the function and passes through.

### `resultRecover`

```jazz jazz-signature
resultRecover :: (e -> Result(f, a)) -> Result(e, a) -> Result(f, a).
```

Calls the recovery function for `Err`. `Ok` skips recovery and keeps its value.
The recovery may change the error type.

All transformation operations are `O(1)` apart from the callback.

## Defaults and inspection

### `resultWithDefault`

```jazz jazz-signature
resultWithDefault :: a -> Result(e, a) -> a.
```

Uses the first argument only for `Err`.

### `resultIsOk`

```jazz jazz-signature
resultIsOk :: Result(e, a) -> Bool.
```

### `resultIsErr`

```jazz jazz-signature
resultIsErr :: Result(e, a) -> Bool.
```

## Conversion

### `resultToMaybe`

```jazz jazz-signature
resultToMaybe :: Result(e, a) -> Maybe(a).
```

Converts `Ok value` to `Just value` and discards an error as `Nothing`.

### `resultErrorToMaybe`

```jazz jazz-signature
resultErrorToMaybe :: Result(e, a) -> Maybe(e).
```

Converts `Err error` to `Just error` and discards a success as `Nothing`.

### `resultFromMaybe`

```jazz jazz-signature
resultFromMaybe :: e -> Maybe(a) -> Result(e, a).
```

Converts `Just value` to `Ok value`. `Nothing` becomes `Err` containing the
error supplied first.

Conversions are `O(1)`. Use [Maybe](maybe.md) when absence needs no error value.
