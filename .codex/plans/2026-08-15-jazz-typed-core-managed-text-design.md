# Jazz typed-core managed Text design

**Date:** 2026-08-15

**Status:** Approved for implementation planning

## Purpose

Extend the opt-in Haskell Typed Core and backend-neutral Lowered IR path with
one usable managed-value vertical slice: `Text` construction and transport,
strict equality, length, append, and append-char.

Ordinary `compile` and `run` remain on canonical core and the reference
interpreter. Public `Text` behavior, the Jazz-authored standard library, the
Typed Core and Lowered IR schemas, their mirrored validators, and Lowered IR
version 1 remain unchanged.

## Accepted boundary

The Typed Core producer will retain `Text` values through the existing scalar,
closure, recursion, conditional, scalar-pattern-case, and tail-position
profile. The supported value positions are:

- literals;
- module-scope bindings;
- direct and closure parameters and results;
- lexical and recursive-group captures that satisfy the existing capture
  profile;
- call arguments and returns;
- conditional branch results and value-position joins; and
- scalar-pattern-case arm results and value-position joins.

The pattern-case scrutinee remains inside the existing immediate-scalar
profile. This decision does not authorize `Text` literal patterns, managed
wildcard or variable scrutinees, or any other managed pattern lowering.

The producer will also retain:

- `==` and `!=` when both operands are resolved `Text` values; and
- exactly saturated applications of the existing kernel builtins for Text
  length, append, and append-char.

The approved kernel builtins are compiler-owned semantic identities resolved
through `BuiltinCatalog`. Production must not string-match raw source spelling
after builtin resolution.

## Typed Core ownership

Inference remains the sole semantic owner. Finalization reuses the existing
nodes and contracts:

```text
TypedTextType
TypedManagedTextRecipe
TypedTextLiteral
TypedBuiltinName
TypedLiteralExpr
TypedVariableExpr
TypedApplyExpr
TypedBinaryExpr
```

`TTextType` finalizes to `TypedTextType` with `TypedManagedTextRecipe`.
`LText` finalizes to `TypedTextLiteral`. Text variables retain their exact
binder references, and function schemes may contain managed Text recipes in
arguments, results, or closure signatures.

An approved kernel callee finalizes to `TypedVariableExpr` with a canonical
`TypedBuiltinName` and no binder reference. Its ordered application spine uses
the existing `TypedApplyExpr` staging and final result information. Production
accepts the spine only when it supplies the builtin's exact arity.

A bare approved kernel builtin is a `TypedCoreCallableValueUnsupported`
profile failure. A partial approved kernel application is a
`TypedCoreCallArityUnsupported` failure with exact expected and actual arity.
Oversaturation retains the ordinary source diagnostic and blocks production
before profile failures are considered. Other builtins and non-local calls
retain their current profile failures.

Finalization must not add a second inference pass, allocate solver variables,
select evidence again, or change inference-only results.

## Backend-neutral Text representation

The runtime-support catalog defines one canonical Text layout:

```text
LoweredLayoutId "jazz.layout.text.v1"
LoweredLayout "jazz.layout.text.v1" LoweredTextLayout
LoweredManagedReferenceRepresentation "jazz.layout.text.v1"
```

Every `TypedManagedTextRecipe`, including one nested inside a closure recipe,
maps to that representation. `LoweredConstructText` constructs a literal.
Existing parameters, temporaries, closure environments, calls, block edges,
returns, and tail calls transport the managed reference without adding a new
IR constructor.

The layout describes semantic identity only. Pointer width, allocation
headers, ownership, reference counting, tracing, destruction, and target
layout remain future runtime and ABI decisions.

## Pure Text runtime services

A new
`src/Jazz/Compiler/LoweredIR/RuntimeServiceCatalog.hs` module owns the exact
semantic IDs and call signatures:

| Service ID                         | Lowered IR signature   | Semantics                        |
| ---------------------------------- | ---------------------- | -------------------------------- |
| `jazz.runtime.text.equal.v1`       | `(Text, Text) -> Bool` | Unicode-scalar sequence equality |
| `jazz.runtime.text.length.v1`      | `(Text) -> Int64`      | Unicode-scalar count             |
| `jazz.runtime.text.append.v1`      | `(Text, Text) -> Text` | Ordered concatenation            |
| `jazz.runtime.text.append-char.v1` | `(Text, Char) -> Text` | Append one Unicode scalar        |

Here `Text` means the canonical managed Text reference and `Int64` is the
current concrete representation of semantic `Int` in this profile.

These services are pure backend dependencies. They are not `RuntimeHost`
effects, Haskell callbacks, public builtin names, or native symbols. A future
backend maps each semantic service to its versioned runtime ABI while
preserving the existing reference-interpreter behavior.

Text `==` emits `jazz.runtime.text.equal.v1`. Text `!=` emits the same service
followed by the existing Boolean-not primitive. No inequality service is
declared. A function whose final expression is a Text runtime call emits the
call as an instruction followed by `LoweredReturn`; runtime calls are not tail
terminators in Lowered IR version 1.

## Runtime-support requirement collection

The lowerer performs one structural dependency collection over validated Typed
Core before instruction emission. It records:

- whether any node, scheme, parameter, result, or nested closure recipe uses
  `TypedManagedTextRecipe`; and
- which approved Text equality or kernel operations occur.

This pass does not infer types, resolve names, select evidence, classify call
shape, or reconstruct source intent. It reads only validated semantic
identities and recipes.

The emitted program contains the Text layout exactly once when Text is
required. It contains only referenced Text runtime services, deduplicated and
ordered by the fixed catalog order: equality, length, append, append-char. A
program that only constructs or transports Text declares no runtime service.

## Expression lowering

Text literal lowering allocates one new block-local temporary with
`LoweredConstructText`, preserving the source literal payload exactly.

Text variables reuse the existing binder, parameter, capture, and carried-
operand paths after `loweredRepresentation` learns the canonical Text mapping.
Closure environment layouts may contain the Text representation without a new
closure rule.

Text equality lowers both operands once, left to right, preserving the current
carried-operand discipline across nested control flow. It then emits the
equality runtime call and, for `!=`, Boolean negation.

Approved kernel applications are recognized from the canonical
`TypedBuiltinName` at the root of an exactly saturated `TypedApplyExpr` spine.
Arguments lower once in source order. The complete operand list is passed to
the catalog-owned runtime service, whose result representation must agree with
the final Typed Core node.

Conditionals and scalar-pattern cases reuse their current deterministic CFG,
ambient transport, joins, and function-result lowering. This batch changes
only the representations carried across those edges and the expressions that
can produce them.

## Failure behavior

Failure precedence remains:

1. source diagnostics;
2. producer-profile failures;
3. Typed Core invariant failures;
4. lowerer-profile failures; and
5. Lowered IR invariant failures.

Malformed Text equality keeps the existing source type diagnostic. Unsupported
managed collections and patterns retain their existing structured profile
failures. Unknown or unsupported builtins remain non-local or unsupported
call failures. An approved builtin with the wrong arity fails during Typed Core
production. A runtime service signature or Text representation mismatch is
owned by the existing Lowered IR validator. Failed production or lowering
returns no partial artifact.

## Verification design

Every production change begins with a focused failing expectation. Exact
source-to-Typed-Core and source-to-Lowered-IR fixtures will cover:

- Text literals in module entry and function results;
- Text bindings evaluated once and reused;
- Text parameters, returns, captures, direct calls, closure calls, and tail
  calls;
- Text transported through value-position joins and function-result
  conditionals and scalar cases;
- equality as one runtime-service call;
- inequality as equality followed by Boolean negation;
- saturated length, append, and append-char calls with left-to-right argument
  evaluation;
- one Text layout and only referenced services in fixed catalog order; and
- Text construction or transport without unused service declarations.

Negative fixtures will prove deterministic rejection of:

- bare and partial approved Text kernel builtins at the producer boundary;
- oversaturated approved Text kernel calls through ordinary source
  diagnostics;
- Text literal patterns and managed scrutinees;
- lists, tuples, ADTs, and their layouts or projections;
- Text uncons, from-chars, and concat;
- imported-module execution and public `Text` module cutover; and
- Text I/O and `RuntimeHost` operations.

The existing Typed Core and Lowered IR contract suites continue to prove
Haskell and hosted-Jazz schema and validator parity. No hosted-Jazz source
changes are expected because the schemas and validation rules already support
the selected nodes, managed representations, layouts, services, and calls.

Focused verification runs the typed-core producer/lowerer, Typed Core
contract, and Lowered IR contract suites serially. Closeout runs the full Cabal
suite inside the checked-in Nix shell with `--jobs=1`, documentation and queue
checks, repository audit, formatting checks, and `git diff --check`.

## Documentation and execution state

Implementation closeout updates compiler pipeline, bootstrapping, project
status, RFC index, queue, and blocker contracts. It must distinguish the
opt-in backend profile from ordinary compile/run behavior and from the public
Text contract, which is already implemented by the reference interpreter.

The implementation plan may promote one child under the bootstrap interpreter
umbrella after this written design is reviewed. Completion closes that child
without automatically promoting managed collections or another Text child.

## Approaches rejected

### Transport only

Transport alone would establish representation plumbing but leave the
backend-neutral path unable to inspect or transform Text. Adding the scalar-
result kernel keeps the batch coherent and materially advances future hosted
compiler and native-backend coverage.

### Full Text standard library

Uncons, from-chars, concat, and the higher-level public module require managed
lists, products, variants such as `Maybe`, and multi-module integration. Text
I/O additionally requires the runtime-host ABI. Combining those contracts
would make one batch own several independent managed-data and execution
boundaries.

### New Text-specific IR instructions

Lowered IR version 1 already defines managed Text layout construction and
semantic runtime-service calls. New instructions or a version bump would
duplicate the accepted abstraction without adding capability.

### Call the Haskell interpreter from the lowerer

Executing reference-interpreter primitives during lowering would fold runtime
behavior into compilation, fail for non-literal values, and couple future
backends to stage 0. The lowerer emits semantic dependencies only.

## Non-goals

- Lists, tuples, ADTs, managed patterns, projections, switches, or layouts.
- Text uncons, from-chars, concat, traversal, search, splitting, or formatting.
- Partial or first-class runtime-service values.
- Imported `Text` module execution or multi-module Typed Core lowering.
- Text I/O, runtime-host effects, native ABI implementation, or memory
  management.
- A Lowered IR executor, LLVM, object generation, linking, or native runtime.
- Ordinary compile/run cutover or any public language semantic change.
- Typed Core or Lowered IR schema, validator, or version changes.

## Acceptance criteria

The batch is complete when the opt-in producer and lowerer construct and
transport Text throughout the established callable and CFG profile, lower
strict equality plus the three approved scalar-result kernel operations to the
exact catalog-owned services, preserve deterministic dependency and failure
ordering, reject every excluded managed form without partial artifacts, and
pass focused plus full serialized verification. Ordinary compile/run and
public Text behavior must remain unchanged.
