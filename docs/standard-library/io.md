---
title: IO
description: Perform strict UTF-8 file, stream, argument, and process operations.
---

Import `IO` for effectful host operations. Recoverable operations return
[Result](result.md); costs depend on the installed host and payload size.

## Files

### `readText!`

```jazz jazz-signature
readText! :: Text -> Result(IOError, Text).
```

Reads the file at the supplied path as strict UTF-8. Success returns `Ok text`.
Failure returns `Err` with an [IOError](io-error.md) that includes the path when
available.

### `writeText!`

```jazz jazz-signature
writeText! :: Text -> Text -> Result(IOError, ()).
```

Writes the second argument to the path supplied first using strict UTF-8.
Success returns `Ok ()`; failure returns an error with the path when available.

## Standard streams

### `readStdin!`

```jazz jazz-signature
readStdin! :: () -> Result(IOError, Text).
```

Reads standard input as strict UTF-8. Stream errors do not attach a path. Pass
`()` to perform the read.

### `writeStdout!`

```jazz jazz-signature
writeStdout! :: Text -> Result(IOError, ()).
```

Writes text to standard output. Success returns `Ok ()`; stream errors do not
attach a path.

### `writeStderr!`

```jazz jazz-signature
writeStderr! :: Text -> Result(IOError, ()).
```

Writes text to standard error. Success returns `Ok ()`; stream errors do not
attach a path.

## Process

### `arguments!`

```jazz jazz-signature
arguments! :: () -> List(Text).
```

Returns process arguments in host-provided order. Pass `()` to retrieve them.

### `exit!`

```jazz jazz-signature
exit! :: Int -> ().
```

Terminates through the installed runtime host with a status from `0` through
`255`. An out-of-range status fails with fatal diagnostic `E3030` before the
host is called.

The `!` suffix participates in Jazz's current
[purity contract](../language/purity.md).
