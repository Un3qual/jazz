# RFC 0006: Lowered IR contract

Status: Accepted
Date: 2026-07-31
Supersedes: Lowered-IR decisions dated 2026-07-21 and representation correction dated 2026-07-30.

## Decision

Jazz uses a permanent backend-neutral lowered IR between validated typed core
and backend-specific generation. The IR owns explicit control flow, closure-
converted function shape, calls, concrete runtime value representations, and
target-independent data-layout requests. It does not expose LLVM instruction
objects, target offsets or alignments, object-file details, or native-runtime
implementation choices.

A version-1 lowered program contains ordered layout requests, runtime-service
declarations, function definitions, and one entry-function identifier. Stable
identifiers for functions, blocks, temporaries, and layouts are distinct and
deterministic; they cannot depend on absolute paths, pointers, hashes, or map
iteration order.

Functions declare an optional closure environment, ordered parameters, a
result representation, ordered blocks, and an entry block. Capturing functions
receive an explicit managed closure environment and are called through closure
values. Non-capturing functions may use direct calls. The IR contains no
lexical lookup or nested function definitions.

Blocks use explicit typed parameters for values crossing control-flow edges.
Temporaries are single-assignment and block-local. Every block has exactly one
terminator: return, jump, conditional branch, tagged-variant switch, or direct
or closure tail call. Tail calls are terminators rather than an optimization
inferred later from a call followed by return.

Representations include unit, boolean, fixed-width signed and unsigned
integers, `Float16`/`Float32`/`Float64`, Unicode scalar `Char`, managed layout
references, and callable closure signatures. Semantic `Int` and `Float`, type
variables, overloaded literals, unresolved names, constraints, and source-
level function types must be resolved before this boundary.

Layout requests describe products, tagged variants, closure environments,
managed text, and homogeneous managed lists without fixing target pointer
width, padding, allocation headers, or garbage-collector metadata. Runtime
calls use declared semantic service identifiers and signatures; a later native
ABI maps those services to platform symbols.

The complete validator returns ordered structured failures and checks version,
identifier uniqueness and resolution, operand scope and use order,
representations, immediate ranges, layout references, block-edge arguments,
switch coverage, call signatures, closure environments, entry constraints, and
terminators. Human prose is not the canonical validation contract.

Haskell and Jazz modules mirror the IR schema and validator. Their checked
adapter translates already-built IR and rejects malformed values instead of
supplying defaults. Jazz represents unsigned immediate payloads as canonical
decimal `Text`, allowing the validator to own the full `UInt64` domain
`0..18446744073709551615`; the Haskell mirror retains an arbitrary-precision
integer payload. Version-1 layout and switch tags remain limited to
`0..Int64::max` by their shared tag carrier.

The IR contract and validators are implemented. The opt-in typed-core profile
from RFC 0005 lowers closed scalar expressions and non-capturing direct calls
into this contract. General core-to-IR lowering, closures, recursion, control
flow, managed layouts, runtime services, LLVM lowering, object generation,
linking, and the native runtime are not implemented by this RFC.

## Context

Canonical core is a semantic interpreter input, not a backend representation,
and the original inference result did not carry concrete information for each
node. Introducing LLVM structures directly into the frontend would couple
semantic phases to one backend before a complete typed boundary existed.

Mirrored, executable Haskell and Jazz contracts give stage 0 an independent
validator and give later Jazz-authored stages a stable output target. Defining
the contract before broad lowering makes invalid states and backend ownership
reviewable on their own.

## Consequences

- Typed-core specialization and lowering must produce concrete representations
  before constructing IR.
- LLVM and native-runtime layers consume this contract but cannot add backend-
  specific constructors to it.
- Values crossing blocks are explicit arguments, simplifying deterministic
  validation and later SSA translation.
- Closure conversion and tail-call intent are frontend-lowering
  responsibilities, not reverse-engineered backend optimizations.
- A semantic IR change requires aligned schemas, validators, adapters, fixtures,
  and a version decision.
