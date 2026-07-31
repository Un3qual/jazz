# Jazz-Next Backend-Neutral Lowered IR Design

## Status

Approved in discussion and reviewed in written form on `2026-07-21`.

Implementation status (`2026-07-21`): complete.
`JN-BOOTSTRAP-LOWERED-IR-CONTRACT-FOUNDATION-001` established the matching
Haskell/Jazz schemas, complete ordered validators, checked comparison adapter,
and exact repeated parity over 10 valid and 31 invalid fixtures. Typed-core
elaboration, core-to-IR lowering, LLVM, object/link, and native-runtime work
remain unpromoted.

Hosted lexing, parsing, and canonical-core lowering already match stage 0 over
the complete fixed parser corpus. The next compiler milestone establishes the
permanent boundary between typed frontend semantics and backend-specific code
generation. The first implementation child defined here establishes the IR
contract and its executable validation evidence; it does not yet lower
canonical core or implement a backend.

## Goal

Define and implement the first permanent, backend-neutral lowered-IR boundary
for `jazz-next`. The boundary must be suitable for both:

- Jazz-authored compiler stages that eventually produce native stage-1 and
  stage-2 compilers; and
- the Haskell stage-0 compiler that will validate and later consume the same IR
  before LLVM lowering exists.

The IR owns explicit control flow, closure-converted function shape, calls,
runtime value representations, and target-independent data-layout requests.
It does not expose LLVM instruction objects, target offsets, object-file
details, or native-runtime implementation choices.

## Current Boundary

`JazzNext.Compiler.AST.Expr` is the canonical core consumed by analysis, type
inference, and the reference interpreter. It is structurally canonical but is
not a fully annotated typed-core tree. `InferenceResult` retains the canonical
expression, its root type, runtime type hints, diagnostics, and module
interface facts; it does not preserve a concrete representation for every
expression node.

The hosted `CoreTypes.jz` and `CoreLower.jz` modules intentionally match that
same pre-inference stage-0 boundary. They must remain the semantic input to the
reference interpreter and must not be mutated into a backend representation.

Consequently, the first lowered-IR child establishes the representation,
validator, and comparison contract before any core-to-IR lowerer is promoted.
A later typed-core elaboration contract must supply the per-node types,
resolved names, evidence, and representation choices needed for honest
lowering.

## Considered Approaches

### Selected: Mirrored Jazz and Haskell Contract

Define the semantic schema in this design, implement its ordinary Jazz ADTs for
the hosted/self-hosted producer, and implement a matching Haskell representation
for stage-0 validation and eventual backend consumption. A fixed canonical
fixture family and checked conversion boundary prevent the mirrors from
drifting.

This has more initial surface than a one-language model, but it establishes the
actual handoff the bootstrap pipeline needs instead of postponing it until LLVM
work begins.

### Rejected: Jazz-Only Contract

A Jazz-only schema would reach hosted construction sooner. It would leave stage
0 without a typed consumer or independent validation oracle, however, and would
force the production handoff to be redesigned alongside the first backend.

### Rejected: Haskell-Only Contract

A Haskell-only schema would be convenient for a future LLVM implementation but
would not advance the Jazz-authored compiler. Porting it later would create a
second semantic transition and make stage-0/stage-1 drift harder to identify.

## Ownership

Production Haskell ownership belongs under `jazz-next/src/JazzNext/Compiler/`:

- `LoweredIR.hs` owns the stage-0 IR data model and stable identifiers;
- `LoweredIR/Validate.hs` owns structural and semantic invariant validation;
  and
- later backend consumers may depend on these modules, but they may not add
  LLVM-specific constructors to them.

Jazz-authored ownership belongs under `jazz-next/jazz/compiler/`:

- `LoweredIRTypes.jz` owns the ordinary Jazz representation; and
- `LoweredIRValidate.jz` owns the hosted invariant validator.

Test support belongs under
`jazz-next/test/JazzNext/Compiler/Bootstrap/`:

- `CanonicalLoweredIRComparison.hs` performs checked structural conversion
  between the Haskell representation and ordinary Jazz runtime values; and
- `JazzLoweredIRContractSpec.hs` owns the fixed valid and invalid fixture
  families plus exact repeated comparison.

The comparison adapter is not a lowerer. It must only translate already-built
IR values and validation results. It must reject unknown constructors, invalid
field shapes, and values outside the contract rather than guessing defaults.

## Program Structure

One lowered program contains:

- a version identifying the semantic IR contract;
- ordered data-layout requests;
- ordered runtime-service declarations with explicit call signatures;
- ordered function definitions; and
- one entry-function symbol.

Version `1` is the only supported semantic contract in this child. Validators
reject any other value before reporting the rest of the program failures, and
the expected and actual versions remain structured validation data.

The entry-function symbol names a non-capturing, zero-parameter function.
`LoweredProgram` carries neither entry arguments nor an entry environment, so a
parameterized or capturing entry function would otherwise introduce values no
caller can supply.

Ordering is observable in canonical comparison output. Lookup semantics use
stable identifiers rather than list position, but renderers preserve source
order so diagnostics and parity evidence remain deterministic.

### Stable Identifiers

Functions, blocks, temporaries, and layouts use distinct identifier types.
Identifiers contain deterministic textual or numeric payloads produced by the
lowerer. They are not raw LLVM names, addresses, source-file absolute paths, or
host-generated hashes.

The validator treats duplicate identifiers in the same namespace as failures.
References must resolve in the namespace and scope appropriate to their kind.

### Functions and Closures

A function declares:

- its symbol;
- an optional closure-environment parameter;
- ordered ordinary parameters and their value representations;
- one result representation;
- ordered blocks; and
- its entry-block identifier.

Closure conversion is explicit at this boundary. A capturing function receives
one environment value whose layout request lists captured fields in stable
order. Closure construction names a function symbol and supplies an environment
operand. The environment parameter must be a managed reference to a
`LoweredClosureEnvironmentLayout`; an arbitrary scalar or another managed
layout kind is invalid. Non-capturing functions may be called directly without
allocating an empty environment. Capturing functions are callable only through
constructed closures, so direct calls and direct tail calls to them are
invalid.

The IR does not retain lexical scope, free-variable lookup, or nested function
definitions. A later lowerer must lift nested lambdas and choose captured-field
order before it constructs this representation.

### Blocks and Temporaries

A block declares a stable block identifier, ordered block parameters, ordered
instructions, and exactly one terminator. Block parameters carry value
representations and act as explicit join values. This provides an SSA-shaped
control-flow boundary without introducing LLVM phi nodes or requiring an
optimization pipeline in the first milestone.

The entry block has no block parameters and no jump, branch, or switch edge may
target it. Function parameters are the only values supplied on function entry.
Parameter identifiers must be unique within the combined environment/ordinary
function parameter namespace and within each block-parameter namespace.

Each instruction may define at most one temporary. Every temporary identifier
is defined once within its block and may be reused independently in another
block. A temporary may be used only by a later instruction or the terminator in
its defining block. Values that cross a block edge must be passed as block
arguments. Cross-block and cross-function temporary references are invalid.

The first contract does not define mutable local slots. A later optimization
pass may rewrite the graph while preserving the same validated semantics.

## Value Representations

The IR distinguishes semantic values from backend storage details. Every
parameter, block parameter, temporary result, call result, and return value has
one explicit representation.

The permanent representation vocabulary includes:

- unit;
- boolean;
- signed and unsigned integers with the existing `8`, `16`, `32`, and `64`
  widths;
- `Float16`, `Float32`, and `Float64`;
- Unicode scalar `Char`;
- managed references identified by a data-layout request; and
- callable closure references identified by an explicit call signature.

`Int` and `Float` aliases must already be resolved to `Int64` and `Float64`
before entering lowered IR. Type variables, unresolved named types, overloaded
numeric literals, capability constraints, and source-level function types are
not lowered representations.

Managed references express tracing and field shape through layout requests.
They do not specify pointer width, byte offsets, alignment, address spaces, or
LLVM types. Those target-specific choices belong to the native-runtime and LLVM
contracts.

## Data-Layout Requests

A layout request has a stable layout identifier and one backend-neutral shape:

- ordered product fields;
- tagged variants with stable numeric tags and ordered payload fields;
- a closure environment with ordered captured fields;
- runtime-managed text;
- or a runtime-managed homogeneous list.

Field representations determine which values require runtime tracing. The
validator rejects unknown layout references, duplicate variant tags, and
recursive layout references that do not pass through a managed-reference
boundary.

Variant tags are observed as `UInt64`, but the Haskell/Jazz contract carries
them through the shared signed `Int` interchange. Version 1 therefore limits
layout and switch-case tags to `0..Int64::max`; later widening requires a new
versioned tag encoding.

Layout requests describe semantic payload shape. They intentionally leave
allocation strategy, headers, collector metadata, padding, and target ABI
alignment to later contracts.

## Operands, Instructions, and Calls

Operands are typed references to function parameters, block parameters,
temporaries, function symbols, or immediate scalar values. Managed aggregate
values are constructed through instructions rather than embedded as immediate
host objects.

Signed and unsigned integer immediate payloads must fit their declared width.
The Haskell model stores both as arbitrary-precision `Integer`. Jazz's signed
payload uses `Int`, while the unsigned payload uses canonical signed-decimal
`Text` because Jazz has no arbitrary-precision source numeric type and an
`Int` constructor field coerces through signed 64-bit range before validation.
The comparison adapter bridges Haskell unsigned `Integer` values to canonical
decimal text. This preserves validator ownership of negative, malformed,
overflowing, and full-domain `UInt64` payloads. `UInt64` immediates therefore
use the exact inclusive domain `0..18446744073709551615`.

Character immediates must be Unicode scalar values. In particular, a
Haskell-side producer cannot use surrogate code points that Jazz source and
the checked character constructors cannot produce.

The permanent instruction vocabulary covers:

- scalar primitive operations;
- product, variant, list, text, and closure construction;
- checked field, tag, and payload projection;
- direct calls to known function symbols;
- calls through closure operands; and
- calls to named runtime services.

Primitive operations have fixed structural signatures before backend lowering:
arithmetic is binary over one concrete numeric representation, ordering is
binary over one concrete numeric representation, equality/inequality is binary
over one concrete representation, boolean negation is unary `Bool`, and boolean
conjunction/disjunction are binary `Bool`. Result representations are validated
independently even when an unresolved operation prevents result inference.

Direct and closure calls carry an explicit call signature. Argument count and
representations must match that signature. Runtime-service calls refer to an
ordered program declaration that pairs a stable semantic service identifier
with its signature. The declarations do not contain Haskell function names,
foreign symbols, or LLVM intrinsics; a later native-runtime ABI contract maps
semantic identifiers to platform symbols.

The contract distinguishes ordinary calls from tail calls. Tail calls are
terminators so a backend can preserve stack-safe behavior without reverse
engineering a call followed by a return.

## Explicit Control Flow

Every block ends in exactly one of:

- return;
- jump with arguments for the target block parameters;
- conditional branch with arguments for both targets;
- tagged-variant switch with explicit cases and an optional default; or
- direct or closure tail call.

A switch scrutinee must be a managed reference whose declared layout is a
tagged variant, and every explicit case tag must occur in that layout. A
default does not make impossible explicit tags or non-variant scrutinees valid.
When no default is present, the explicit cases must cover every distinct valid
tag declared by the layout.

Jump, branch, and switch targets must exist in the same function. Target
arguments must match block-parameter count and representations exactly. Return
and tail-call results must match the enclosing function signature.

Pattern semantics remain owned by canonical typed core and its lowerer. The IR
contains only explicit tag tests, scalar operations, projections, and control
flow; it does not contain source patterns or implicit match failure behavior.

## Validation Results

Validation returns ordinary structured data. A failure records:

- a stable failure kind;
- the function, block, and instruction position when applicable;
- the referenced identifier or expected representation when applicable; and
- structured details for versions, identifiers, representations, immediate
  ranges, arities, indices, or tags when applicable.

The first validator must detect at least:

- duplicate or unresolved function, block, layout, parameter, and temporary
  identifiers;
- unsupported semantic IR versions;
- missing or foreign entry functions and entry blocks;
- parameterized or capturing entry functions;
- parameterized entry blocks;
- jump, branch, or switch edges targeting an entry block;
- missing terminators;
- invalid use order or cross-function operands;
- instruction result/operand representation mismatches;
- integer immediates outside their supported representation range and
  non-scalar character immediates;
- layout or switch-case tags outside the shared unsigned tag carrier;
- invalid layout references, variant tags, and field projections, with absent
  variant tags reported separately from invalid payload-field indices;
- no-default switches that do not cover every valid layout tag;
- jump, branch, and switch arity or representation mismatches;
- direct, closure, runtime, and tail-call signature mismatches;
- closure construction whose environment representation disagrees with the
  target function or whose environment parameter is not backed by a closure
  environment layout; and
- direct calls or direct tail calls to capturing functions.

Validation failure structures are the comparison contract. Human-readable
diagnostics may render them later, but prose strings and diagnostic formatting
are not canonical evidence in this child.

Both validators must traverse the complete input and return failures in stable
program, function, block, and instruction order. They must not depend on map
iteration order.

## Canonical Comparison

The canonical renderer converts the complete program or validation result into
ordinary constructor-shaped values using stable identifier and list ordering.
It must not use Haskell `Show`, source-string inspection, absolute paths,
pointer identities, or backend-specific names.

The fixed fixture family materializes complete ordinary Jazz values through
hosted source and validates the same complete programs in Haskell, then
compares:

1. complete canonical program values;
2. complete ordered validation results; and
3. repeated output from both implementations.

Repetition proves determinism. It is not a performance or scale test.

## First Implementation Child

The first child is
`JN-BOOTSTRAP-LOWERED-IR-CONTRACT-FOUNDATION-001`.

It implements:

- the Haskell and Jazz data schemas described above;
- checked canonical conversion;
- invariant validators in both implementations;
- a fixed valid fixture family covering scalar values, join blocks, variant
  switching, product and variant layouts, direct calls, closure calls, runtime
  calls, and direct/closure tail calls; and
- a fixed invalid fixture family covering every required validation category.

It does not expose a placeholder core-to-IR lowering function. A later child
will be promoted only after a typed-core elaboration design names how resolved
types, evidence, and per-node representation data reach the lowerer.

The first child is independently shippable because it establishes an executable
and backend-consumable contract with exact cross-language evidence. It does not
claim source-program compilation or native execution.

## Later Ordered Work

After this child closes, later design and implementation gates proceed in this
order:

1. define the fully annotated typed-core/elaboration boundary;
2. lower expression foundations and direct calls;
3. lower closure environments and recursive bindings;
4. lower conditionals, pattern matches, blocks, and tail calls;
5. close source-corpus lowered-IR parity;
6. define LLVM lowering and the versioned native-runtime ABI;
7. add object generation and platform linking; and
8. prove native stage-1/stage-2 equivalence.

Only one independently verifiable child is promoted at a time. This ordering
does not pre-approve LLVM, object/link, garbage collector, or native-runtime
implementation.

## Verification Strategy

The first child requires:

- focused exact Haskell/Jazz fixture parity for all valid and invalid forms;
- repeated deterministic canonical rendering and validation;
- regression execution of the hosted canonical-core suites;
- warning-clean `jazz-next` development compilation;
- the routine Cabal test matrix;
- `cabal check`;
- queue and documentation validators; and
- `git diff --check`.

Tests assert data and behavior, not implementation text or source layout. The
default test matrix must not run exhaustive parser scale components. This child
adds no exhaustive scale suite; large synthetic IR graphs remain manual,
opt-in evidence for rare performance investigations.

## Queue Transition

After this written design is approved, its implementation plan must name the
exact files, fixture inventory, focused commands, and queue/frontmatter values
for `JN-BOOTSTRAP-LOWERED-IR-CONTRACT-FOUNDATION-001`. The plan describes
responsibilities and observable behavior without reproducing all implementation
code.

Only after the plan is reviewed may the child move into `Ready Now`. Closure of
the child must archive its evidence, leave no stale queue entry, and update the
bootstrap profile without implying that typed-core lowering or LLVM exists.

## Non-Goals

This milestone does not:

- change canonical core or the reference interpreter input;
- invent per-node types or representations absent from current inference;
- lower source, surface AST, or canonical core into the new IR;
- implement optimization passes, mutable local slots, or target-specific SSA;
- introduce LLVM values, instructions, modules, data layouts, or tool calls;
- generate objects, link binaries, or define a platform ABI;
- implement allocation, garbage collection, native text/list storage, or host
  services;
- introduce bytecode or a VM;
- add exhaustive default performance tests; or
- modify `jazz-hs/` or `jazz2/`.
