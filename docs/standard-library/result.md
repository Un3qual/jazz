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

### `map`

```jazz jazz-signature
map :: (a -> b) -> Result(e, a) -> Result(e, b).
```

Transforms the value inside `Ok` and preserves `Err` unchanged.

### `mapError`

```jazz jazz-signature
mapError :: (e -> f) -> Result(e, a) -> Result(f, a).
```

Transforms the value inside `Err` and preserves `Ok` unchanged.

### `andThen`

```jazz jazz-signature
andThen :: (a -> Result(e, b)) -> Result(e, a) -> Result(e, b).
```

Calls the function for `Ok` and returns its result without nesting. `Err` skips
the function and passes through.

### `recover`

```jazz jazz-signature
recover :: (e -> Result(f, a)) -> Result(e, a) -> Result(f, a).
```

Calls the recovery function for `Err`. `Ok` skips recovery and keeps its value.
The recovery may change the error type.

All transformation operations are `O(1)` apart from the callback.

## Defaults and inspection

### `withDefault`

```jazz jazz-signature
withDefault :: a -> Result(e, a) -> a.
```

Uses the first argument only for `Err`.

### `isOk`

```jazz jazz-signature
isOk :: Result(e, a) -> Bool.
```

### `isErr`

```jazz jazz-signature
isErr :: Result(e, a) -> Bool.
```

## Conversion

### `toMaybe`

```jazz jazz-signature
toMaybe :: Result(e, a) -> Maybe::Maybe(a).
```

Converts `Ok value` to `Just value` and discards an error as `Nothing`.

### `errorToMaybe`

```jazz jazz-signature
errorToMaybe :: Result(e, a) -> Maybe::Maybe(e).
```

Converts `Err error` to `Just error` and discards a success as `Nothing`.

### `fromMaybe`

```jazz jazz-signature
fromMaybe :: e -> Maybe::Maybe(a) -> Result(e, a).
```

Converts `Just value` to `Ok value`. `Nothing` becomes `Err` containing the
error supplied first.

Conversions are `O(1)`. Use [Maybe](maybe.md) when absence needs no error value.

## Generic methods

Importing Result supplies Equatable for both alternatives and Mappable/Reducible for Result(error). Mapping preserves Err; folds skip Err and visit the single Ok value.
