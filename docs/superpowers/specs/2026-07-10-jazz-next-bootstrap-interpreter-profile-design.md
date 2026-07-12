# Jazz-Next Bootstrap Interpreter Profile Design

## Status

Approved in discussion on `2026-07-10`. The full bootstrap interpreter profile
is not yet implemented. Its first child, backend-independent `Char`/`Text`
literal semantics, is complete under plan
`JN-BOOTSTRAP-CHAR-TEXT-LITERALS-001` with `status: done`; the remaining profile
capabilities below are still staged follow-up work.

## Goal

Make the active `jazz-next` interpreter capable of hosting the first real
compiler component written in Jazz: a multi-module lexer that reads Jazz source,
produces structured tokens with source spans, reports deterministic lexical
diagnostics, and matches the Haskell lexer over the existing parser corpus.

This design defines the minimum bootstrap language and host-runtime profile. It
does not claim that the first tranche is sufficient to port the complete
compiler. Later compiler slices add efficient collections, named product data,
binary output, and artifact generation behind the same boundaries.

## Bootstrap Definition

The Haskell implementation under `jazz-next/` is stage 0. Stage 0 remains the
authoritative compiler and interpreter while Jazz-authored compiler modules are
introduced incrementally.

The first bootstrap milestone is reached when a Jazz-authored lexer can:

1. receive a source path and source text through the bootstrap host boundary;
2. traverse the source as Unicode scalar values through ordinary in-process
   runtime primitives without rereading or re-encoding the source;
3. produce a typed list of tokens containing exact source spans;
4. return lexical success or failure as an ordinary Jazz ADT;
5. serialize the result into a canonical textual comparison format; and
6. match the Haskell lexer over the accepted and rejected parser fixtures.

This is hosted bootstrapping, not the final self-hosting fixed point. The later
fixed point requires stage 0 to run the Jazz-authored compiler and emit LLVM IR
for a native stage-1 compiler, then requires stage 1 to emit semantically
equivalent stage-2 LLVM IR and a conforming native binary.

## Long-Term Native Target

LLVM-generated native binaries are the committed destination after hosted
bootstrapping. The interpreter remains stage 0, the reference execution engine,
and a useful development surface; it is not the permanent artifact format for
compiled Jazz programs.

The retained compiler pipeline is:

```text
Jazz source
  -> surface AST
  -> canonical typed core
  -> backend-neutral lowered IR
  -> LLVM IR
  -> LLVM object generation and native link
  -> native Jazz binary plus native runtime
```

The backend-neutral lowered IR is a permanent compiler boundary. It owns
closure conversion, explicit control flow, concrete runtime representation
choices, calls, and data-layout requests without encoding LLVM instruction
objects throughout the frontend. LLVM lowering consumes this IR; the
interpreter continues to consume canonical core.

There is no planned bytecode format or bytecode VM between canonical core and
LLVM. A temporary executable bytecode layer would duplicate control-flow,
calling-convention, runtime, and artifact work that the LLVM backend must later
replace.

The first native runtime exposes the same semantic services as the bootstrap
kernel through a small versioned ABI: allocation and garbage-collector hooks,
`Char`/`Text` representation, text I/O, process arguments and exit, fatal
diagnostics, and later deterministic collections. Jazz stdlib and compiler code
target Jazz APIs rather than Haskell values or LLVM intrinsics, so the stage-0
Haskell implementation and native runtime implementation can coexist without
forking source code.

LLVM tool invocation, object generation, platform linking, garbage collection,
and native ABI implementation require a separate accepted backend design before
implementation. This bootstrap profile fixes their architectural boundary but
does not promote them ahead of the hosted lexer milestone.

## Chosen Profile

Use a balanced bootstrap profile:

- keep the trusted host boundary small and effectful;
- make text storage, file access, and stack-safe execution interpreter-owned;
- make `Maybe`, `Result`, token types, spans, traversal helpers, and lexer logic
  Jazz-owned;
- allow efficient collection implementations to remain host-backed behind
  Jazz stdlib APIs when the semantic compiler needs them; and
- defer language features that do not shorten the path to the first
  Jazz-authored compiler component.

Two alternatives were rejected:

1. A minimal text-and-file-only profile would start the lexer sooner but would
   leave generic signatures and stack safety unresolved, forcing the first
   substantial Jazz compiler modules to be rewritten.
2. A language-completion profile would add records, packages, full effects,
   default methods, superclasses, and native code generation before
   bootstrapping. Those features are useful independently but are not on the
   critical path to a hosted lexer or parser.

## Current Baseline

The active interpreter already provides the structural language foundation:

- immutable recursive bindings and lexical closures;
- lists and tuples;
- generic data-declaration metadata and direct generic constructor schemes;
- constructor, list, tuple, as-, or-, and guarded pattern matching;
- `if` expressions and user-defined functions;
- ordinary binding generalization and per-use instantiation;
- constrained signatures, explicit type application, and compiler-owned
  capability evidence for the supported subset;
- parse-once module graphs with explicit typed export inventories; and
- interpreter-backed CLI execution.

The missing bootstrap capabilities are source text as a Jazz value, host I/O as
a Jazz effect, broader generic signature notation, stack-safe deep traversal,
and canonical compiler-output serialization.

## Bootstrap Value Model

### `Char`

`Char` represents one Unicode scalar value, excluding surrogate code points.
Character literals use single quotes and support the same mandatory escapes as
text literals. The active `Char`/`Text` child provides scalar-value equality;
total ordering remains a later bootstrap-profile requirement.

Later bootstrap-profile work must provide the constant-time scalar
classification and conversion needed by lexing:

- scalar value to and from `UInt32` with checked failure;
- ASCII letter, digit, alphanumeric, whitespace, and newline predicates; and
- equality and ordering through the ordinary primitive comparison surface.

Locale-sensitive classification is outside the bootstrap profile.

### `Text`

`Text` is immutable Unicode text. Text literals use double quotes. The first
surface supports escaped quote, backslash, newline, carriage return, tab, null,
and scalar escapes. Invalid escapes and invalid scalar values are lexical
diagnostics, not replacement characters.

The active `Char`/`Text` child provides literals, patterns, signatures, exact
scalar-sequence equality, deterministic rendering, and module transport. Exact
`Text` equality is therefore part of the active bootstrap contract; ordering,
traversal, indexing, slicing, concatenation, builders, and search remain later
profile work.

The later Jazz-visible text API must add:

- `textEmpty`, `textLength`, `textIsEmpty`;
- `textUncons`, returning the first `Char` and remaining `Text` safely;
- checked indexing and slicing;
- concatenation and a multi-fragment builder path;
- lexicographic ordering;
- prefix, suffix, and substring checks.

`textLength`, checked indexing, and slicing count Unicode scalar values rather
than UTF-8 code units or bytes.

The lexer must be able to traverse a `Text` value without invoking file, CLI, or
external FFI operations once per character. The host-backed immutable
representation and operations such as `textUncons` are ordinary in-process
stage-0 runtime primitives exposed through Jazz APIs.

## Generic Type Surface

The current compiler already owns generalized ordinary binding schemes and
generic constructor schemes internally. The bootstrap profile extends the
source signature grammar so compiler APIs can state their types directly.

The accepted surface must include:

- lower-case type variables;
- parameterized named types using `TypeName(Arg1, Arg2)`;
- nested applications such as `Result(IOError, List(Token))`;
- applications inside lists, tuples, and function types; and
- recursive generic ADTs whose constructor payloads use declared parameters.

The profile remains rank-1. It does not add higher-rank types, type lambdas,
associated types, or user-visible dictionary values.

Generic values and constructor/type metadata must survive module interfaces and
explicit export inventories. A Jazz bootstrap module must be able to export a
type, its constructors, and functions whose signatures mention that type.

## Host Effect Boundary

Host operations are kernel bridges exposed through Jazz stdlib aliases. Their
source names end in `!` and participate in the existing stub-v1 purity checks.

The bootstrap host API consists of:

- `readText! :: Text -> Result(IOError, Text)`;
- `writeText! :: Text -> Text -> Result(IOError, ())`;
- `readStdin! :: () -> Result(IOError, Text)`;
- `writeStdout! :: Text -> Result(IOError, ())`;
- `writeStderr! :: Text -> Result(IOError, ())`;
- `arguments! :: () -> List(Text)`; and
- `exit! :: Int -> ()`.

`IOError` is a Jazz-visible ADT with a stable error category, path when
applicable, and message text. Host exception class names and platform-specific
numeric error codes do not enter Jazz semantics.

Path interpretation follows the existing CLI process environment. Directory
walking, environment-variable access, subprocesses, sockets, clocks, and random
numbers are outside this profile.

## Stack-Safe Evaluation

The detailed contract is accepted in
[`2026-07-11-jazz-next-stack-safe-evaluation-design.md`](2026-07-11-jazz-next-stack-safe-evaluation-design.md).
`Runtime.hs` owns one interpreter-private explicit machine shared by pure and
host-backed evaluation. `ModuleRuntime.hs` retains dependency/export ownership,
and `Driver.hs` remains a public façade. These controls are permanent reference
interpreter machinery, not bytecode or input to future LLVM lowering.

Compiler traversals must not consume one Haskell call frame per Jazz tail call.
The runtime introduces an explicit evaluation loop or trampoline for tail
positions, including:

- a closure body returned directly from another closure;
- either branch of `if`;
- a selected pattern-case arm;
- a block's terminal expression; and
- tail-recursive calls reached through an ordinary binding.

Non-tail recursion remains limited by available resources, but runtime failure
must be a structured Jazz diagnostic rather than a leaked Haskell stack
exception. The lexer readiness suite includes large synthetic text and token
inputs to prove tail-recursive traversal does not overflow the host stack.

Tail-call support must preserve current lexical environments, runtime type
hints, module paths, capability evidence, and diagnostic behavior.

## Jazz-Owned Bootstrap Library

The following facilities are implemented as `.jz` modules using the bootstrap
kernel rather than as public builtins:

- `Maybe(a)` and `Result(e, a)`;
- empty-tuple unit helpers using the existing `()` representation;
- list folds, traversals, zips, partitioning, reversal, append, and builders;
- source position and source span ADTs;
- token and lexical diagnostic ADTs;
- parser-state helpers;
- canonical escaping and rendering; and
- association-list maps and sets sufficient for lexer/parser development.

The bundled prelude remains the public ownership layer. New host functions use
`__kernel_*` bridge bindings internally and ordinary public aliases from Jazz
source, preserving the existing stdlib boundary.

## Later Compiler-Ready Facilities

The following do not block the first lexer, but must land before the complete
resolver and typechecker move into Jazz:

1. Deterministic efficient `Array`, `Map`, and `Set` APIs. Host-backed persistent
   implementations are permitted initially, but iteration order is part of the
   Jazz contract.
2. Named record or named-constructor-field syntax for compiler state and module
   interfaces. Positional ADTs remain sufficient for the first lexer.
3. Immutable `Bytes`, UTF-8 encode/decode, and efficient text/byte builders for
   diagnostics and artifacts.
4. A backend-neutral lowered IR with explicit control flow, closure conversion,
   calls, and runtime data-layout requests.
5. LLVM IR emission from the lowered IR plus native runtime ABI conformance.
6. A versioned canonical representation for core AST, inferred types, module
   interfaces, lowered IR, diagnostics, and normalized LLVM parity evidence.

These facilities must be introduced behind Jazz-level APIs so replacing a host
implementation does not require rewriting compiler logic.

## Data Flow

The first hosted compiler path is:

```text
jazz-next CLI
  -> stage-0 module loader
  -> Jazz bootstrap main module
  -> readText! source path
  -> Jazz Text traversal
  -> Jazz lexer state machine
  -> Result(LexError, List(Token))
  -> canonical Jazz renderer
  -> stdout or comparison file
```

The Haskell reference lexer continues to consume the same source independently.
The parity harness normalizes both results into one comparison schema and
compares exact token kind, lexeme, line, column, and diagnostic data.

No Jazz bootstrap module imports Haskell compiler internals. Its only privileged
dependencies are the documented kernel bridges.

## Diagnostics and Failure Model

Bootstrap APIs use ordinary result values for recoverable failures. File access,
UTF-8 decoding, checked indexing, checked slicing, scalar conversion, and lexical
failure must not abort the host process or expose Haskell exceptions.

Fatal interpreter failures use structured diagnostics with:

- a stable code;
- a concise summary;
- source or file context when available; and
- deterministic notes.

Canonical comparison output must escape text deterministically and must not
include host paths unless the test explicitly supplies a normalized path.

## Verification Contract

Each implementation child adds focused unit tests plus the existing full
package and repository checks. The bootstrap readiness gate requires:

1. literal, escape, Unicode scalar, invalid-input, and boundary tests for
   `Char` and `Text`;
2. success and categorized-failure tests for every host I/O operation;
3. generic signature and cross-module generic export/import tests;
4. tail-recursive text and token traversals over large synthetic inputs without
   host stack overflow;
5. exact Jazz-versus-Haskell lexer parity for every current accepted and rejected
   parser fixture;
6. deterministic repeated output from the Jazz lexer;
7. no-prelude tests proving only kernel bridge names remain visible;
8. bundled-prelude drift tests covering every new bridge and public alias; and
9. `cabal test --project-dir=jazz-next all`,
   `bash jazz-next/scripts/test-warning-config.sh`, queue/docs validation, and
   `git diff --check` passing after each completed child.

## Implementation Slices

The design decomposes into independently reviewable children:

1. bootstrap scalar/text contract and runtime values;
2. text literals, escapes, typing, rendering, and diagnostics;
3. generic named-type applications in source signatures and module interfaces;
4. Jazz `Maybe`/`Result` plus collection traversal modules;
5. host text-I/O kernel bridges and prelude aliases;
6. stack-safe tail-position runtime evaluation;
7. canonical token/diagnostic comparison format; and
8. Jazz-authored lexer with differential parity coverage.

Only one child should be promoted to `Ready Now` at a time, with exact target
paths and verification recorded in the queue and child plan. The current empty
queue is not changed by this design document.

## Non-Goals

The bootstrap interpreter profile does not add:

- full effect inference or effect polymorphism;
- packages, package metadata, re-exports, wildcard imports, or hiding imports;
- default class methods, superclasses, overlap/orphan policy, or user-visible
  dictionaries;
- pattern synonyms;
- cross-module user-defined operators;
- mutable variables or general shared mutable state;
- concurrency, clocks, randomness, networking, subprocesses, or environment
  access;
- implementation of LLVM lowering, object generation, native linking, or the
  native runtime in the lexer-readiness tranche;
- Wasm or JavaScript generation; or
- a stage-1/stage-2 fixed-point claim.

Those features require separate accepted contracts when they become the
smallest verified dependency of a later bootstrap milestone.
