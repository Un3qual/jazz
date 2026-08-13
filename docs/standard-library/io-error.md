---
title: IOError
description: Handle stable errors returned by Jazz host I/O operations.
---

Import `IOError` to inspect recoverable failures from the [IO](io.md) module.
The public values hide platform exception types and operating-system error
numbers.

## Error categories

### `IOErrorCategory`

<!-- jazz-signature -->

```jazz
data IOErrorCategory = NotFound | PermissionDenied | AlreadyExists | InvalidData | ResourceExhausted | Interrupted | Unsupported | Other.
```

A stable category for recoverable host I/O failures.

### `NotFound`

The requested file or resource does not exist.

### `PermissionDenied`

The host denied the requested operation.

### `AlreadyExists`

Creation failed because the target already exists.

### `InvalidData`

The host data cannot be decoded or accepted, including invalid UTF-8 input.

### `ResourceExhausted`

The host lacks a required finite resource such as space or handles.

### `Interrupted`

The operation was interrupted before completion.

### `Unsupported`

The host does not support the requested operation.

### `Other`

A recoverable host failure that does not fit another stable category.

## Error value

### `IOError`

`IOError` is the recoverable error type returned by IO operations.

### `IOError`

<!-- jazz-signature -->

```jazz
data IOError = IOError IOErrorCategory Maybe(Text) Text.
```

The constructor stores the category, an optional path, and a human-readable
message. File errors attach `Just path` when one is available; stream errors use
`Nothing`.

IO functions return `Result(IOError, value)`, so errors may be handled with the
[Result API](result.md) or matched directly with these public constructors.
