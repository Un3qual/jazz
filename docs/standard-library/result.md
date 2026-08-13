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

Constructs an error result from a value of type `e`.

### `Ok`

Constructs a successful result from a value of type `a`. Both constructors are
public and may be used in
[patterns](../language/algebraic-data-types-and-patterns.md).

## Transforming

### `resultMap`

```jazz jazz-signature
resultMap :: (a -> b) -> Result(e, a) -> Result(e, b).
```

Transforms the value inside `Ok` and preserves `Err` unchanged. Example:
`resultMap (\(value) -> value + 1) (Ok 2)` produces `Ok 3`.

### `resultMapError`

```jazz jazz-signature
resultMapError :: (e -> f) -> Result(e, a) -> Result(f, a).
```

Transforms the value inside `Err` and preserves `Ok` unchanged. Example:
`resultMapError show (Err 4)` produces `Err "4"`.

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
This can also change the error type. Example: `resultRecover (\(_) -> Ok 0)
(Err "missing")` produces `Ok 0`.

All transformation operations are `O(1)` apart from the callback.

## Defaults and inspection

### `resultWithDefault`

```jazz jazz-signature
resultWithDefault :: a -> Result(e, a) -> a.
```

Returns the value inside `Ok`, or the first argument for `Err`.

### `resultIsOk`

```jazz jazz-signature
resultIsOk :: Result(e, a) -> Bool.
```

Returns `True` for `Ok` and `False` for `Err`. This is `O(1)`.

### `resultIsErr`

```jazz jazz-signature
resultIsErr :: Result(e, a) -> Bool.
```

Returns `True` for `Err` and `False` for `Ok`. This is `O(1)`.

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
