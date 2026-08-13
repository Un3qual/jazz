---
title: IOError
description: Handle stable errors returned by Jazz host I/O operations.
---

Import `IOError` to inspect recoverable failures from the [IO](io.md) module.
The public `IOErrorCategory` constructors are `NotFound`, `PermissionDenied`,
`AlreadyExists`, `InvalidData`, `ResourceExhausted`, `Interrupted`,
`Unsupported`, and `Other`.

`IOError category maybePath message` contains the category, an optional path,
and a human-readable message. File errors attach a path when one is available;
stream errors do not. These values hide platform exception types and operating
system error numbers.

IO operations return `Result(IOError, value)`, so errors can be handled with
the [Result API](result.md) or matched directly with public constructors.
