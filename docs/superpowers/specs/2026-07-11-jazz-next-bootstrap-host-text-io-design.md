# Jazz-Next Bootstrap Host Text I/O Design

## Status

Approved in discussion on `2026-07-11`. This design is the curation contract
for `JN-BOOTSTRAP-HOST-TEXT-IO-001`; implementation remains pending until a
child plan is written and promoted.

## Goal

Add the backend-neutral host boundary needed by Jazz-authored bootstrap tools
to read UTF-8 source text, write canonical text output, inspect process
arguments, and terminate with an explicit status. Recoverable failures remain
ordinary Jazz values, and the same Jazz-visible contract must be implementable
by both the stage-0 Haskell interpreter and the future native runtime.

## Architectural Boundary

The public boundary consists of two ordinary explicit-import Jazz modules:

- `IOError` owns stable error categories and the recoverable error value; and
- `IO` owns effectful text, stream, argument, and exit operations.

Both modules live under `jazz-next/stdlib/`. Neither module is added to the
bundled prelude. Programs opt into the API through normal imports.

The Haskell evaluator receives a typed `RuntimeHost` capability record. The
evaluator is monadic over that record so effects work at any expression depth,
including closure bodies, `if` branches, selected pattern arms, and block
terminal expressions. Production CLI execution installs the real process host.
Focused tests install deterministic hosts.

Existing pure evaluator entry points remain available as compatibility wrappers
over an effect-disabled host. The disabled host reports `Unsupported` through
the ordinary raw outcome protocol for recoverable operations and through a
runtime diagnostic for `exit!`; programs that do not use host effects preserve
their current behavior.

This design rejects two alternatives:

1. Converting the evaluator directly to concrete Haskell `IO` would couple all
   runtime tests and embeddings to the stage-0 host implementation.
2. Returning deferred effect values for the driver to execute would not resume
   effects correctly when they occur inside closures or control flow without a
   second continuation/evaluation architecture.

No `unsafePerformIO` or equivalent hidden-effect escape hatch is permitted.

## Public Jazz Modules

### `IOError`

`jazz-next/stdlib/IOError.jz` exports exactly:

- type `IOErrorCategory`;
- constructors `NotFound`, `PermissionDenied`, `AlreadyExists`, `InvalidData`,
  `ResourceExhausted`, `Interrupted`, `Unsupported`, and `Other`;
- type `IOError`; and
- constructor `IOError`.

The source declarations are equivalent to:

```jazz
data IOErrorCategory
  = NotFound
  | PermissionDenied
  | AlreadyExists
  | InvalidData
  | ResourceExhausted
  | Interrupted
  | Unsupported
  | Other.

data IOError = IOError IOErrorCategory (Maybe(Text)) Text.
```

The optional path is `Just(path)` for file operations and `Nothing` for
standard-stream operations. The final `Text` is a normalized message. Host
exception class names, call stacks, platform-specific error numbers, and
locale-specific messages do not enter Jazz semantics.

The stable normalized messages are:

| Category | Message |
| --- | --- |
| `NotFound` | `resource not found` |
| `PermissionDenied` | `permission denied` |
| `AlreadyExists` | `resource already exists` |
| `InvalidData` | `input is not valid UTF-8` |
| `ResourceExhausted` | `resource exhausted` |
| `Interrupted` | `operation interrupted` |
| `Unsupported` | `operation unsupported` |
| `Other` | `host I/O failed` |

The stage-0 host maps only errors it can classify reliably. An unclassified
Haskell or platform error becomes `Other`; it is never guessed into a narrower
category. Deterministic test hosts cover every public category independently of
the current platform's available error predicates.

### `IO`

`jazz-next/stdlib/IO.jz` exports exactly:

```jazz
readText! :: Text -> Result(IOError, Text).
writeText! :: Text -> Text -> Result(IOError, ()).
readStdin! :: () -> Result(IOError, Text).
writeStdout! :: Text -> Result(IOError, ()).
writeStderr! :: Text -> Result(IOError, ()).
arguments! :: () -> List(Text).
exit! :: Int -> ().
```

The module imports `Maybe`, `Result`, and `IOError`. It does not re-export their
types or constructors. Every public name ends in `!`, so the current stub-v1
purity rule rejects calls from pure bindings and permits calls from impure
bindings and top-level expressions.

`readText!` and `readStdin!` decode input strictly as UTF-8. Invalid byte
sequences return `Err(IOError InvalidData path message)` rather than replacing
bytes or raising a fatal runtime diagnostic. `writeText!`, `writeStdout!`, and
`writeStderr!` encode Jazz `Text` as UTF-8 without locale-sensitive conversion.

`arguments! ()` returns process arguments excluding the executable name.
`exit! status` requests immediate process termination in the production host.
A deterministic test host may record the status and return `Right ()` so
evaluator tests can inspect control flow.

## Private Kernel Protocol

The compiler/runtime catalog adds these private `KernelIntrinsic` symbols:

```jazz
__kernel_readTextRaw! :: Text -> (Bool, Text, Text, Text).
__kernel_writeTextRaw! :: Text -> Text -> (Bool, Text, Text, Text).
__kernel_readStdinRaw! :: () -> (Bool, Text, Text, Text).
__kernel_writeStdoutRaw! :: Text -> (Bool, Text, Text, Text).
__kernel_writeStderrRaw! :: Text -> (Bool, Text, Text, Text).
__kernel_arguments! :: () -> List(Text).
__kernel_exit! :: Int -> ().
```

The four-field raw outcome is:

```text
(succeeded, payload, category-token, normalized-message)
```

On success, `succeeded` is `True`, `payload` contains read text when
applicable, and both error fields are empty text. Successful write operations
also use an empty payload. On recoverable failure, `succeeded` is `False`, the
payload is empty, the category token is one of the stable kebab-case tokens
below, and the message is the normalized message from the public table.

| Category | Private token |
| --- | --- |
| `NotFound` | `not-found` |
| `PermissionDenied` | `permission-denied` |
| `AlreadyExists` | `already-exists` |
| `InvalidData` | `invalid-data` |
| `ResourceExhausted` | `resource-exhausted` |
| `Interrupted` | `interrupted` |
| `Unsupported` | `unsupported` |
| `Other` | `other` |

`IO.jz` is the sole owner of decoding this private tuple and token protocol
into `Result` and `IOError` constructors. Unknown tokens map to `Other`; they do
not crash pattern matching. This keeps the runtime independent from Jazz ADT
allocation and follows the existing private-raw/public-Jazz-wrapper pattern
used by `Text.textUncons`.

All seven symbols remain available through kernel self-bridges but receive no
public prelude aliases. The future native runtime implements the same semantic
operations behind its versioned ABI without exposing these private names as a
user-facing language surface.

## Runtime Host Interface

The Haskell runtime host capability carries operations equivalent to:

```haskell
data HostIOFailure = HostIOFailure HostIOCategory Text

data RuntimeHost m = RuntimeHost
  { runtimeHostReadText :: Text -> m (Either HostIOFailure Text)
  , runtimeHostWriteText :: Text -> Text -> m (Either HostIOFailure ())
  , runtimeHostReadStdin :: m (Either HostIOFailure Text)
  , runtimeHostWriteStdout :: Text -> m (Either HostIOFailure ())
  , runtimeHostWriteStderr :: Text -> m (Either HostIOFailure ())
  , runtimeHostArguments :: m [Text]
  , runtimeHostExit :: Integer -> m (Either HostIOFailure ())
  }
```

`HostIOCategory` mirrors the private tokens, not the Jazz constructors. Runtime
application converts recoverable I/O results into the structural tuple before
returning to Jazz code. Because a successful exit never returns in production,
an exit failure is instead promoted to a deterministic runtime diagnostic; this
lets the disabled host reject exit requests rather than silently succeeding.

The evaluator exposes host-parameterized entry points whose outer shape is
`m (Either Diagnostic result)`. Internally, `ExceptT Diagnostic m` or an
equivalent single error channel carries fatal interpreter diagnostics while
recoverable host failures remain ordinary tuple values. Fatal evaluator errors
must not be converted into `IOError`.

Library-facing pure wrappers run with an `Identity` host. Driver functions add
host-parameterized source and module-graph variants. Existing deterministic
helpers continue to use the disabled host. The CLI installs the production
`RuntimeHost IO` only for run mode; compile-only paths perform no host effects.

## Production Host Behavior

The production stage-0 host:

- reads file and standard-input bytes before strict UTF-8 decoding;
- writes UTF-8 bytes to files and standard streams;
- preserves the exact caller-supplied path in public errors;
- returns process arguments without the executable path;
- flushes through normal handle semantics; and
- delegates `exit!` to process termination using the requested integer status
  after validating it against the platform-supported exit range.

Out-of-range exit statuses are fatal runtime diagnostics because `exit!` does
not return `Result`. The accepted portable range is `0..255` for this bootstrap
contract.

The production host catches only synchronous I/O failures at the operation
boundary. Asynchronous exceptions are not translated into Jazz `IOError`
values.

## Data Flow

File input follows this path:

```text
Jazz readText! path
  -> IO.jz wrapper
  -> __kernel_readTextRaw! path
  -> RuntimeHost read operation
  -> strict UTF-8 decode
  -> raw success/failure tuple
  -> Ok Text or Err IOError in Jazz
```

Output follows the same path in reverse through UTF-8 encoding. A bootstrap
executable handles the returned `Result`, writes any canonical diagnostic, and
calls `exit!`. Existing CLI terminal-expression rendering remains unchanged;
the bootstrap executable's production `exit!` prevents an additional rendered
terminal value after canonical output is written.

## Verification Contract

Implementation uses test-driven development and adds focused coverage for:

1. exact builtin inventory, names, ownership, arities, and private visibility;
2. exact intrinsic types and deterministic invalid-argument diagnostics;
3. pure-binding rejection and impure-binding/top-level acceptance for every
   new public and private effect name;
4. checked-in `IOError.jz` exports, constructors, payloads, and generic module
   transport;
5. checked-in `IO.jz` exports and conversion of every raw category token into
   the corresponding `Result(IOError, value)`;
6. successful reads, writes, stream operations, argument access, and recorded
   exit through a deterministic host;
7. categorized failure for every recoverable operation, including unknown-token
   fallback to `Other`;
8. effects executed inside closures, `if` branches, selected pattern arms, and
   block terminal expressions;
9. production-host temporary-file UTF-8 round trips, missing paths, and invalid
   UTF-8 input;
10. no-prelude behavior proving only private kernel bridge names remain
    available;
11. bundled-prelude reproducibility and stdlib two-space indentation; and
12. the full warning-config matrix, execution-queue checks, docs checks, and
    `git diff --check`.

Tests must not depend on ambient process arguments, terminal contents, locale,
or platform-specific exception wording. Production exit behavior is tested
through an injected host rather than terminating the test process.

## Scope and Non-Goals

This child includes the public modules, private kernel catalog/type/purity
contracts, monadic runtime-host seam, production stage-0 host operations,
driver/CLI injection, and the focused tests required above.

It does not add:

- stack-safe tail-call evaluation;
- the Jazz-authored lexer or parser;
- text indexing, slicing, concatenation, builders, search, ordering, or scalar
  classification;
- immutable bytes as a Jazz-visible type;
- directory walking, environment access, subprocesses, networking, clocks,
  randomness, or concurrency;
- general effect inference, effect polymorphism, or effect types;
- package semantics, re-exports, or wildcard imports;
- backend-neutral lowered IR, LLVM lowering, object generation, linking, or the
  native runtime; or
- changes to `jazz-hs/` or `jazz2/`.

The Haskell runtime-host interface is a stage-0 implementation seam. The Jazz
module API, error categories, raw semantic operations, UTF-8 rules, and
verification fixtures are the durable contract carried forward to the native
runtime.
