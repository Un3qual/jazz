# RFC 0007: Runtime host boundary

Status: Accepted
Date: 2026-07-31
Supersedes: Runtime-host and stack-safe evaluation decisions dated 2026-07-10 and 2026-07-11.

## Decision

All effectful stage-0 execution passes through a typed `RuntimeHost m`
capability record owned by `src/Jazz/Compiler/RuntimeHost.hs`. The host provides
only:

- strict UTF-8 file text reads and writes;
- strict UTF-8 standard-input, standard-output, and standard-error operations;
- process arguments; and
- an explicit process-exit request.

The evaluator is parameterized over that capability, so an effect can occur at
any expression depth, including closures, selected conditional branches, case
guards and arms, and terminal block expressions. Effects are not deferred to
the driver and no hidden `unsafePerformIO`-style escape hatch is permitted.

Public Jazz I/O is owned by ordinary explicit-import modules `IO` and
`IOError`. `IO` exposes `readText!`, `writeText!`, `readStdin!`,
`writeStdout!`, `writeStderr!`, `arguments!`, and `exit!`. Recoverable file and
stream kernel operations return a small structural outcome that `IO` converts
into ordinary `Result(IOError, a)` values. Stable error categories are
`NotFound`, `PermissionDenied`, `AlreadyExists`, `InvalidData`,
`ResourceExhausted`, `Interrupted`, `Unsupported`, and `Other`.

Recoverable host failures are ordinary Jazz values. Fatal interpreter errors
and a failed exit request remain structured runtime diagnostics; they are not
converted into `IOError`. Host exception names, call stacks, platform error
numbers, locale-specific messages, and Haskell values do not enter the Jazz
contract.

Pure evaluator entry points use the same implementation with a disabled host.
The disabled host deterministically reports unsupported operations and
preserves pure-program behavior. Focused tests inject deterministic hosts that
record operation order and synthesize every public failure category without
depending on ambient arguments, terminals, locale, or platform error wording.
Only CLI run mode installs the production host; compile-only paths perform no
host effects.

The runtime owns one private explicit evaluation machine shared by disabled-
host and host-backed execution. It preserves call-by-value, left-to-right
evaluation, executes each reached effect exactly once, never executes effects
from an unselected branch, and transfers tail positions without consuming one
Haskell call frame per Jazz call. The machine is reference-interpreter
machinery, not bytecode and not input to lowered IR or LLVM.

A future native runtime must implement the same Jazz-visible text, error,
argument, stream, file, and exit semantics behind a versioned ABI. Jazz source
continues to target its standard-library APIs rather than Haskell operations or
LLVM intrinsics.

## Context

Running the evaluator directly in concrete Haskell `IO` would couple every
test and embedding to stage 0. Returning deferred effects for the driver to
execute would require a second continuation system and would not correctly
resume effects nested inside control flow. The earlier recursive pure and host
evaluators also duplicated semantics and consumed host stack for tail-recursive
Jazz programs.

A typed capability seam plus one shared evaluation machine keeps effects
injectable, deterministic under test, and portable to the native runtime while
preserving the interpreter as a trustworthy semantic reference.

## Consequences

- New host capabilities require an accepted semantic contract and a stable
  Jazz API; arbitrary filesystem, environment, process, network, clock,
  randomness, and concurrency access are not implied.
- Runtime ordering and exact-once behavior are testable without performing
  real external effects.
- Strict UTF-8 decoding failures are recoverable Jazz values rather than
  replacement text or leaked host exceptions.
- The CLI owns applying a successful exit request after requested observation
  artifacts are finalized.
- Non-tail recursion and total memory exhaustion remain ordinary resource
  limits; this RFC guarantees host-stack-safe tail transfer, not unlimited
  computation.
