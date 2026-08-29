# Jazz typed-core managed products and variants design

**Date:** 2026-08-27

**Status:** Approved for implementation planning

## Purpose

Extend the opt-in Haskell Typed Core and backend-neutral Lowered IR path with
the next complete managed-data family: non-unit tuples and local algebraic data
types. The approved direction covers construction, transport, deterministic
layout requests, and ordered `case` selection over tuple and constructor
patterns.

Ordinary `compile` and `run` remain on canonical core and the reference
interpreter. Public tuple, ADT, and pattern behavior remains unchanged. Lists,
pattern lambdas, imported-module execution, runtime services, native ABI work,
and normal backend cutover remain separate decisions.

## Why this is the next contract

Typed Core already represents tuple values, constructor values, data
declarations, concrete product and variant recipes, and the complete active
pattern vocabulary. Lowered IR version 1 already represents product and
variant layouts, managed references, product and variant construction, field
and tag projection, and tagged switches. Its validators already check those
operations in both Haskell and hosted Jazz.

The current producer and lowerer nevertheless reject every non-unit tuple and
ADT value. Closing that gap advances the data-heavy Jazz-authored compiler
without introducing a new IR node, version, runtime service, or ABI. Managed
lists remain independent because Lowered IR has list construction but no
equivalent list projection operation; list inspection therefore needs a
separate runtime-service or representation decision.

## Proposed child order

The durable contract authorizes two implementation children in this order:

1. **Managed product and variant construction.** Retain local data
   declarations, non-unit tuple expressions, and exactly saturated local
   constructor applications; derive exact product and variant layouts; and
   transport those values through the already-supported binding, callable,
   closure, capture, control-flow, return, and tail-operand profile.
2. **Managed product and variant pattern cases.** Extend ordered `case`
   lowering with tuple and constructor decomposition, nested matching,
   arm-local projected binders, guards, top-level alternatives, and an
   independently total backend decision tree.

Only the first child may be promoted after this contract is accepted and an
aligned implementation plan exists. Closing the first child does not
automatically promote pattern cases or any list work.

## Typed Core production boundary

Inference remains the sole semantic owner. The shared traversal will retain
provisional data declarations, tuple elements, and constructor application
spines only when the opt-in profile is selected. Finalization applies the
existing solved state and reuses these existing contracts:

```text
TypedDataStatement
TypedDataDeclaration
TypedConstructorDeclaration
TypedTupleExpr
TypedVariableExpr
TypedApplyExpr
TypedPatternCaseExpr
TypedManagedProductRecipe
TypedManagedVariantRecipe
```

Finalization must not reparse signatures, allocate solver variables, repeat
inference, select evidence again, or reconstruct constructor ownership from
source spelling. Constructor identity comes from the resolved constructor
namespace and its exact declaration binder. Generic constructor use carries
the existing ordered `TypedInstantiation` chosen by inference.

The producer retains each local source data declaration in source order. Type
parameters and constructor field types use the already-resolved inference
contracts, and constructor binder paths remain stable. The local typed module
interface and export inventory retain corresponding type and constructor
facts even though the one-module lowered artifact has no export table.

Imported data declarations, imported constructors, ambient Prelude data, and
multiple modules remain rejected at the existing input or module-profile
boundary.

## Construction profile

The first child supports:

- tuple expressions with two or more elements; unit remains the existing
  immediate representation;
- nullary local constructors as exactly saturated constructor values;
- local constructor applications with their exact declared arity;
- concrete generic constructor specializations whose representation recipes
  contain only already-supported scalars, `Text`, closures, products, and
  variants;
- direct and mutually recursive local ADT layouts; and
- products and variants in every value position already authorized for
  managed `Text`.

Tuple elements and constructor fields evaluate exactly once from left to
right. A complete tuple emits one `LoweredConstructProduct`; a complete
constructor emits one `LoweredConstructVariant`. The result is one managed
reference to the exact layout.

A non-nullary constructor is not a first-class backend callable in this
profile. Bare and partial constructor applications fail with existing
callable/arity profile failures. Oversaturation retains ordinary source
diagnostic precedence. Constructor applications may nest, but no constructor
call is converted into a synthetic lowered function.

List fields, unresolved representation parameters, unsupported imported
types, and equality or other operations over products and variants remain
closed. A declaration may mention an excluded field type when no concrete
layout requiring that field is emitted; attempting to construct or transport
that specialization fails without a partial artifact.

## Canonical managed layout identity

A new managed-layout catalog owns conversion from a concrete Typed Core recipe
to one semantic Lowered IR representation and layout request. It has no target
size, alignment, pointer-width, allocation, or garbage-collection policy.

Every generated identity uses a versioned, length-prefixed textual encoding.
It may contain only semantic module paths, resolved local type names, concrete
type arguments, and representation recipes. It may not depend on absolute
paths, spans, hashes, map iteration order, pointers, or compiler-process state.

Product identity is structural: equal ordered concrete field recipes share one
product layout within a program. Product identity is distinct from closure
environment identity even when their fields have the same representations.

Variant identity is nominal: it contains the current module path, resolved
type name, and ordered concrete type arguments. Two named data types never
share a variant layout merely because their constructor fields have the same
shape. Every constructor receives its zero-based tag from declaration order,
and its ordered specialized fields define the corresponding
`LoweredVariantLayout`.

The canonical encoder covers every recipe allowed by the child. Nested
products and variants are encoded by their semantic layout identities rather
than by target details. Recursive and mutually recursive variants reserve an
identity on first discovery, so fields may refer to layouts still being
collected without expanding forever.

Layout emission remains deterministic and preserves all existing artifacts:

1. catalog-owned runtime layouts such as managed `Text` retain their current
   order;
2. newly required product and variant layouts follow first semantic discovery
   order, deduplicated by layout identity; and
3. closure environment layouts retain their current function-shape order.

The first semantic discovery walk covers the typed module interface, ordered
statements, schemes, expressions, and patterns. It visits expression children
in source evaluation order and marks a layout before following recursive
dependencies.

## Pattern-case profile

The second child extends the existing ordered case CFG rather than replacing
source order with a global tag dispatch. It supports:

- wildcard and variable patterns over products and variants;
- immediate scalar literal patterns nested inside managed patterns;
- constructor patterns with exact declared field counts;
- fixed-arity tuple patterns;
- nested constructor and tuple patterns;
- as-patterns; and
- top-level or-patterns whose alternatives already satisfy the Typed Core
  binder-agreement contract.

List patterns, cons patterns, `Text` literal patterns, nested or-patterns, and
pattern-lambda parameters remain excluded.

The lowerer compiles the source-ordered rows into a deterministic decision
tree. A constructor test projects the tag, then uses `LoweredSwitch` with a
default continuation to later source rows. Constructor fields are projected
only after the tag succeeds. Tuple fields are projected from their product
layout. Nested failure continues to the next eligible row without evaluating
that row's guard or body.

Pattern binders are block parameters carrying the exact projected operands.
An as-pattern additionally carries the complete current operand. All binders
are available to the selected guard and body and cannot escape the arm. Every
projected field, ambient local, shared recursive environment, capture, and
in-flight operand crossing a block edge remains explicit and deterministic.

Guards execute only after the complete pattern succeeds. A false guard resumes
selection at the next source arm. A selected body either jumps to the existing
result join or, in true function-result position, uses the existing tail-result
lowering discipline.

## Backend totality

The lowerer must remain correct for independently constructed, structurally
valid Typed Core. It therefore cannot trust the source analyzer's `E2018`
result as an implicit proof and cannot add an unreachable value, trap
terminator, or match-failure runtime service.

The managed-pattern decision-tree builder also performs its own pure totality
check over the admitted product/variant pattern subset. Unguarded rows
contribute coverage; guarded rows do not. Top-level or-patterns contribute the
union of their alternatives. Closed variant spaces come from the retained
local data declarations, while a tuple has one product shape and scalar
literal domains remain open.

An incomplete arbitrary Typed Core case fails with the existing
`LoweredIRIncompletePatternCase` result before emission. Complete constructor
sets do not require a synthetic wildcard arm. This backend check owns no public
diagnostic and does not replace the canonical source-level coverage analyzer.

## Failure ordering and fail-closed behavior

Failure precedence remains:

1. source diagnostics;
2. producer-profile failures;
3. Typed Core invariant failures;
4. lowerer-profile failures; and
5. Lowered IR invariant failures.

Failed production or lowering returns no partial Typed Core or Lowered IR
artifact. Existing typed validators continue to own constructor membership,
arity, instantiation, pattern shape, binder agreement, recipe agreement, and
visibility. The lowerer independently owns admitted concrete representations,
layout identity, exact constructor saturation, pattern-totality, projection
order, and emitted CFG shape.

Unknown layouts, wrong tags, wrong field counts, projection mismatches, and
incomplete switches remain Lowered IR validator concerns for externally built
artifacts. No implementation layer repairs malformed values or supplies a
default representation.

## Verification design

Every implementation change begins with a focused failing expectation. The
first child requires exact source-to-Typed-Core and source-to-Lowered-IR
fixtures for:

- local nullary and field-carrying constructors;
- generic and recursively nested ADTs;
- nested non-unit tuples;
- deterministic structural product identity and nominal variant identity;
- constructor tag order and field order;
- duplicate layout elimination and stable layout emission order;
- left-to-right field evaluation through nested control flow;
- bindings, parameters, results, captures, direct and closure calls, joins,
  returns, and tail operands containing products or variants; and
- Text, closure, product, and variant fields inside one managed value.

Negative fixtures lock rejection of lists, imported constructors, bare and
partial non-nullary constructors, unsupported equality, unresolved recipes,
and malformed or colliding generated identities.

The second child adds exact fixtures for constructor and tuple selection,
nested patterns, as-patterns, top-level alternatives, repeated guarded
constructors, guard fallthrough, complete constructor sets without a wildcard,
arm-local projected binders, tail-position bodies, and independently rejected
incomplete Typed Core cases. Exact Lowered IR fixtures prove that tags are
tested before fields are projected and that source ordering is preserved.

The existing Typed Core and Lowered IR contract suites continue to prove
Haskell and hosted-Jazz schema and validator parity. No hosted Jazz schema or
validator change is expected because this design makes existing valid nodes
reachable rather than changing their contracts.

Focused verification runs the typed-core producer/lowerer, Typed Core
contract, and Lowered IR contract suites serially inside the checked-in Nix
shell. Each child closes with the full serialized suite, documentation and
queue checks, repository audit, touched-file formatting, and
`git diff --check`.

## Alternatives rejected

### Combine lists with products and variants

Lists have a homogeneous opaque managed layout and construction operation but
no list projection instruction. Inspection needs a separate semantic service
or representation contract. Combining it here would also pull in broader Text
operations and make one batch own two independent runtime boundaries.

### Construction without deterministic layout identity

Anonymous encounter-index layouts would make artifacts depend on traversal
accidents and would not give recursive or generic specializations a stable
semantic name. Target offsets and object layouts are equally premature.

### Require a final wildcard for every managed case

That would avoid a backend coverage check but reject ordinary exhaustive ADT
programs such as complete `Maybe` or `Result` cases. The retained declarations
and typed patterns are sufficient for the lowerer to prove its admitted closed
spaces without a runtime failure service.

### Trust source coverage diagnostics

Lowering accepts independently constructed validated Typed Core, not only
producer output. Treating an earlier source diagnostic pass as an unrecorded
proof would weaken the validation boundary.

### Add new product- or variant-specific Typed Core nodes

The existing tuple, variable, application, data declaration, and pattern nodes
already carry the required semantic identity and recipes. New nodes would
duplicate the accepted schema without closing a missing invariant.

## Non-goals

- Managed lists, list patterns, list services, or broader Text operations.
- Text literal patterns or structural equality over products or variants.
- First-class or partially applied constructors.
- Pattern lambdas or invocation-time match failure.
- Nested or-patterns or pattern synonyms.
- Imported constructors, imported data layouts, multiple modules, or scalar
  exports in Lowered IR.
- RuntimeHost changes, I/O, memory management, runtime ABI, LLVM, object files,
  linking, or native execution.
- Ordinary compile/run cutover or any public language semantic change.
- Typed Core or Lowered IR schema, validator, or version changes.

## Acceptance criteria

The design is ready for implementation planning when RFC 0015 is accepted and
the first child can be described with exact target paths and verification
without depending on the second child. The durable contract must preserve
single-pass semantic ownership, deterministic concrete layout identity,
left-to-right construction, source-ordered guarded matching, independent
backend totality, fail-closed artifacts, and all explicit non-goals above.
