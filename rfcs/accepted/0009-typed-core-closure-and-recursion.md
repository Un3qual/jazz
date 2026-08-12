# RFC 0009: Typed-core closures and recursive callable groups

Status: Accepted
Date: 2026-08-09
Supersedes: None.

## Decision

Jazz will extend the opt-in typed-core producer and backend-neutral lowerer from
the closed scalar/direct-call profile to first-class closures and recursive
callable groups. Ordinary compile and run continue to use canonical core and
the reference interpreter. Only an explicit producer request may return the
expanded validated typed core and validated lowered IR.

The accepted delivery order remains two umbrella milestones:

1. non-recursive closures; then
2. self- and mutually recursive callable groups.

Those milestones are promoted as six executor-sized children, one at a time:

1. `JN-BOOTSTRAP-TYPED-CORE-CLOSURE-CALL-FOUNDATION-001`: closed named
   functions used as values, transported callable shape and binder references,
   staged closure parameters and results, empty environments, and higher-order
   closure calls;
2. `JN-BOOTSTRAP-TYPED-CORE-SCALAR-BINDING-001`: concrete scalar `let`
   production and lowering as a capture-source prerequisite;
3. `JN-BOOTSTRAP-TYPED-CORE-LEXICAL-CAPTURE-001`: binder-resolved inline and
   nested captures, deterministic environment layouts, and lambda lifting;
4. `JN-BOOTSTRAP-TYPED-CORE-CURRIED-APPLICATION-001`: multi-stage callable
   results, currying, partial application, and ordered oversaturation;
5. `JN-BOOTSTRAP-TYPED-CORE-DIRECT-RECURSION-001`: transported recursive-group
   membership plus capture-free, non-escaping self and mutual direct recursion;
   and
6. `JN-BOOTSTRAP-TYPED-CORE-CLOSURE-RECURSION-001`: escaping or capturing
   recursive groups with one immutable shared external-capture environment.

Each child has its own complete accepted and rejected fixture manifests. A
later child may start only after the preceding child preserves the complete
scalar/direct-call manifest and passes the Haskell and Jazz typed-core and
lowered-IR validators.

### Callable shapes and staged application

Jazz source functions are curried. One source application evaluates the
callee, evaluates one argument, applies that argument, and obtains either a
final value or another callable value before the next source argument is
evaluated. General closure lowering must preserve that staging.

Typed-core finalization classifies every executable function once for the
complete bounded program and records that decision in its scheme:

```text
data TypedCallableShape
  = TypedDirectCallableShape
  | TypedClosureCallableShape

TypedScheme ... TypedType TypedRepresentationRecipe (Maybe TypedCallableShape)
```

`Nothing` is required for a non-callable scheme. A named callable declaration
carries exactly one shape, and every node/reference for that binder must agree
with its type, recipe, and shape. Anonymous and nested lambdas are always
closure-shaped by contract; their `TypedNodeInfo` carries the required unary
nested recipe, so they need no second shape field. The lowerer consumes and
verifies named scheme shapes and the mandatory lambda recipe shape; it must not
reclassify functions or regroup callable recipes.

The two callable shapes are:

- a **direct shape** is capture-free, never used as a value, and called only
  through a statically known complete leading-lambda application; or
- a **closure shape** is used as a value, called through a callable value,
  captures a lexical value, participates in a closure-shaped recursive group,
  or is applied at fewer than all of its source lambda stages.

A lowered function definition has only one shape. The lowerer must not call an
environment-bearing function directly or maintain separate direct and closure
ABIs for one function identity. A direct shape may retain the current
multi-parameter direct-call lowering only when static knowledge proves that
each coalesced intermediate source application performs closure construction
and cannot run arbitrary computation. Every general closure call consumes
exactly one source application operand.

Callable types remain concrete and monomorphic in this profile. Representation
recipes preserve call boundaries:

```text
A -> B -> C

direct-only complete chain: TypedClosureRecipe [ARep, BRep] CRep
general closure value:      TypedClosureRecipe [ARep]
                              (TypedClosureRecipe [BRep] CRep)
```

Flattening either recipe tree must equal the source function-arrow sequence.
At each typed application node, consuming one source argument removes the first
argument representation from that tree; the application node carries the
remaining callable recipe or final result recipe. Haskell and Jazz typed-core
validators accept and validate both groupings. Existing direct-call fixtures
retain their current flattened recipes.

Closure parameters and results use recursively nested
`LoweredClosureRepresentation` values. Lowered IR version 1 already validates
those representations and remains version 1. Polymorphic representations,
evidence-bearing executable nodes, and unresolved representation parameters
remain structured profile failures.

A capture-free function used as a value has an explicit empty
`LoweredClosureEnvironmentLayout`. The create site constructs the empty
environment and then the closure. There is no null, implicit, target-specific,
or interpreter-owned environment representation.

Partial application is the ordinary callable result of applying one unary
closure stage; it does not introduce a synthetic adapter ABI. Oversaturation
lowers as ordered successive unary closure calls, and the next argument is not
evaluated until the previous call has returned its callable result. Applying an
additional argument after a non-callable result is an ordinary source type
diagnostic. Independently constructed malformed typed-core fixtures continue
to exercise the typed-core and lowerer invariant boundaries for that case.

### Typed binder and recursive-group identity

Capture and recursion use resolved binder identity, not textual-name matching.
Typed-core variable nodes therefore retain both the diagnostic name and an
optional binder reference:

```text
TypedVariableExpr TypedNodeInfo TypedCoreName (Maybe TypedBinderId)
```

`Just binderId` is required whenever the referenced declaration has a
`TypedBinderId` in the typed program, regardless of its `TypedCoreName`
constructor. `Nothing` is permitted only for an entity without a declaration
in that artifact. Both validators check that a referenced binder is visible at
that expression path and that its type and representation agree with the
defining contract.

`Jazz.Compiler.RecursiveBindings` remains the sole owner of recursive-group
membership and declaration/rebinding semantics. During typed-core production,
`inferRecursiveGroupsOrdered` runs once on the resolved canonical scope and its
statement members are mapped to typed binder identities. `TypedModule` carries
an ordered list of:

```text
TypedRecursiveGroup [TypedBinderId]
```

Groups are ordered by their earliest source member; members are ordered by
source statement position. Both validators require every member to be a local
typed `let` binder, reject empty groups, duplicate members, membership in more
than one group, unknown binders, and group membership that disagrees with
binder-reference reachability. The lowerer consumes this validated list and
must not inspect inference internals or rebuild a name-based call graph.

The binder-reference and recursive-group schema changes land in the Haskell and
Jazz types, validators, checked comparison adapters, and parity fixtures in the
same children that first require them. They are typed-core contract extensions,
not sidecar maps.

### Capture, lifting, and generated identity

A nested lambda is lifted to a module-local lowered function. Its closure
environment contains exactly the free lexical binders used by the lifted body.
Capture resolution chooses the nearest visible binder identity. Fields are
ordered by the first occurrence of each resolved binder during canonical
left-to-right typed-expression traversal. Host map/set iteration cannot change
the layout. Each lifted body projects its captures from its environment;
lowered functions contain no lexical lookup.

The bounded profile permits scalar and closure-valued captures with concrete
representations. Non-closure managed data layouts for text, lists, products,
and variants, runtime services, type parameters, and capability-evidence values
remain outside these children.

Every source-bound named function retains its existing `module::name` identity
regardless of direct or closure shape. New lifted function and environment
identities use the injective internal grammar:

```text
$jz1$<domain>$m<count>$<len>:<segment>...$p<count>$<decimal>,...$n<len>:<name>
```

The permitted domains are `lambda-fn`, `closure-env`, and `recursive-env`.
The module count and length prefixes make module text unambiguous; path items
are comma-separated; the path count distinguishes an empty path; and domain
tags separate generated namespaces. A recursive environment uses the first
member binder path and the literal binder name `group`. Duplicate generated
identity is a lowerer-profile failure reported at the owning statement before
lowered IR construction. The lowered-IR validators continue to treat
identifiers as opaque and validate uniqueness.

### Recursive callable groups

A capture-free recursive group that never escapes as a value retains direct
shapes and direct recursive calls. If any member requires a closure shape, the
whole group is closure-shaped and uses one shared environment layout. The
layout contains the ordered union of external captures only; group members are
not stored in it. Union ordering follows member source order and then canonical
capture order within each member.

The initial closure-recursion profile constructs the shared environment once at
the first group member. Every external capture binder must therefore be visible
before that first member. If a member would capture a binder introduced or
rebound between group members, production rejects the group at that member with
an ordered recursive-environment profile failure. This preserves each accepted
member's declaration-time capture without mutation or placeholders.

Each member closure is constructed at its source binding statement using the
already-created shared environment. Inside a member body, a reference to self
or a peer reconstructs that member's closure from the current shared
environment. This supports accepted self and mutual recursion without cyclic
heap initialization, mutable fields, allocation placeholders, or new runtime
services. Callable identity is neither renderable nor comparable in Jazz, so
reconstructing an equivalent peer closure is not observable.

The first recursion children accept only concrete monomorphic root function
members whose bodies otherwise fit the preceding closure profile. Recursive
aliases, patterns, non-closure managed data, capability evidence, imports,
cross-module groups, and groups with later external captures remain structured
profile failures.

This RFC does not select tail-call terminators. A recursive call is lowered as
an ordinary direct or unary closure call according to its shape. Tail-call
classification and native stack guarantees require a separate child after
control-flow ownership is defined.

### Contract and failure ordering

The lowered-IR constructors already represent environment layouts, environment
parameters, closure construction, projections, recursively callable
representations, and closure calls. No lowered-IR constructor or version change
is authorized by this RFC. The typed-core recipe, variable-reference, module,
and validator changes described above update Haskell and Jazz schemas,
validators, checked adapters, and exact fixtures together.

Failure precedence remains:

1. ordinary source diagnostics;
2. typed-core producer-profile failures;
3. typed-core invariant failures;
4. lowerer-profile failures; and
5. lowered-IR invariant failures.

Within a profile phase, the total path rank is input/program, then module, then
statements in ascending source order. Within one statement, classification,
generated-identity, and recursive-group failures precede expression
descendants; descendants then follow canonical expression pre-order. Failures
at the same path use the contract's declared failure-kind order. A later
statement's classification failure never precedes an earlier statement's
descendant failure. Removing a callable, capture, arity, or recursion rejection
exposes newly reachable descendant failures without changing the relative order
of unaffected siblings. Failed production or lowering returns no partial
artifact.

Required combined-failure cases include:

- underapplication with an unsupported supplied operand;
- capture plus an unsupported representation;
- recursion plus rebinding or visibility failure;
- several closure-shape reasons affecting one function;
- unaffected sibling failures retaining their previous relative order; and
- malformed typed-core oversaturation after a non-callable result.

The last case is a typed-core invariant fixture and must not reach lowering; it
is not a producer-profile failure for well-typed source.

### Implementation ownership and verification

The following matrix is the source for each queue row's `target_paths` and
narrow focused gate. A child may touch a conditional owner only when its
contract or harness actually changes.

| Child                   | Required target paths                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | Conditional target paths                                                                                                                                                                                   | Required fixture boundary                                                                                                             | Focused gate |
| ----------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------- | ------------ |
| Closure-call foundation | `src/Jazz/Compiler/TypeInference/Elaboration.hs`; `src/Jazz/Compiler/TypedCore.hs`; `src/Jazz/Compiler/TypedCore/Validate.hs`; `jazz/compiler/TypedCoreTypes.jz`; `jazz/compiler/TypedCoreValidate.jz`; `src/Jazz/Compiler/LoweredIR/Lower.hs`; `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`; `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`; `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`; `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`; `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`; `test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs` | `jazz.cabal` only for shared-harness registration                                                                                                                                                          | Named function value, higher-order parameter/result, nested recipe validation, empty environment; reject non-concrete callable shapes | G1           |
| Scalar binding          | `src/Jazz/Compiler/TypeInference/Scope.hs`; `src/Jazz/Compiler/TypeInference/Elaboration.hs`; `src/Jazz/Compiler/LoweredIR/Lower.hs`; `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`; `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`                                                                                                                                                                                                                                                                                                                                                                     | typed-core mirrors/contract specs only if the scalar binder invariant changes; `jazz.cabal` only for registration                                                                                          | Concrete scalar `let` production, ordered reuse, entry lowering; retain unsupported managed bindings                                  | G2           |
| Lexical capture         | `src/Jazz/Compiler/RecursiveBindings.hs`; `test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs`; `src/Jazz/Compiler/TypeInference/Elaboration.hs`; `src/Jazz/Compiler/LoweredIR/Lower.hs`; `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`; `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`; `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`; `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`; `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`; `test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs`                                                    | typed-core mirrors only if the binder-reference contract changes after child 1; `jazz.cabal` only for registration                                                                                         | Inline/nested lambda, scalar and closure capture, shadowing, field order, lifted identity; reject unsupported captures                | G3           |
| Curried application     | `src/Jazz/Compiler/TypeInference/Elaboration.hs`; `src/Jazz/Compiler/TypedCore/Validate.hs`; `jazz/compiler/TypedCoreValidate.jz`; `src/Jazz/Compiler/LoweredIR/Lower.hs`; `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`; `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`; `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`; `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`; `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`; `test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs`                                                                      | typed-core type mirrors only if constructors change; `jazz.cabal` only for registration                                                                                                                    | Multi-stage result, partial application, ordered callable oversaturation; source-diagnostic non-callable oversaturation               | G1           |
| Direct recursion        | `src/Jazz/Compiler/RecursiveBindings.hs`; `test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs`; `src/Jazz/Compiler/TypeInference/Elaboration.hs`; `src/Jazz/Compiler/TypedCore.hs`; `src/Jazz/Compiler/TypedCore/Validate.hs`; `jazz/compiler/TypedCoreTypes.jz`; `jazz/compiler/TypedCoreValidate.jz`; `src/Jazz/Compiler/LoweredIR/Lower.hs`; `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`; `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`; `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`; `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`                           | `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs` and `test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs` only if emitted direct artifacts change; `jazz.cabal` only for registration | Transported self/mutual group, direct calls, reachability validation; reject mismatched group metadata                                | G4           |
| Closure recursion       | `src/Jazz/Compiler/RecursiveBindings.hs`; `test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs`; `src/Jazz/Compiler/TypeInference/Elaboration.hs`; `src/Jazz/Compiler/LoweredIR/Lower.hs`; `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`; `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`; `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`; `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`; `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`; `test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs`                                                    | typed-core mirrors only if group invariants change; `jazz.cabal` only for registration                                                                                                                     | Escaping self recursion, shared prior captures, mutual peer reconstruction; reject later/interleaved captures                         | G5           |

The gate aliases expand to these exact commands:

- **G1:** `cabal test jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1`
- **G2:** `cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1`
- **G3:** `cabal test recursive-bindings-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1`
- **G4:** `cabal test recursive-bindings-spec jazz-typed-core-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1`
- **G5:** `cabal test recursive-bindings-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1`

Each child owns two ordered manifests:

1. source-to-typed-core fixtures; and
2. independently constructed valid typed-core lowerer fixtures.

Both manifests contain exact accepted artifacts and exact rejected failures,
run twice, and retain all unaffected direct-call boundary fixtures. Produced
artifacts pass the Haskell validators and the existing Jazz contract runners.
The contract runners are extended in place or share one extracted harness;
encoders, renderers, and Jazz validation orchestration must not be duplicated.

The combined manifests cover at least:

- a closed named function passed to and called by a higher-order function;
- closure parameters, unary closure results, and empty environments;
- an inline anonymous lambda;
- a concrete scalar binding captured by a nested lambda;
- nested capture, shadowing, and deterministic multi-capture order;
- partial application and callable oversaturation with preserved evaluation
  order;
- source-diagnostic non-callable oversaturation;
- capture of an unsupported managed or unresolved value;
- direct self and mutual recursion;
- escaping self recursion through a closure;
- a closure-shaped mutual group with prior external captures;
- rejection of a later/interleaved external capture;
- recursive visibility and rebinding rejection; and
- repeatable combined-failure ordering.

Malformed typed-core oversaturation belongs only to
`JazzTypedCoreContractSpec`; it must fail typed-core validation and must not be
placed in the valid typed-core lowerer manifest.

Focused verification uses the existing registered suites:

```text
cabal test jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec \
  jazz-typed-core-expression-direct-call-spec recursive-bindings-spec \
  --test-show-details=failures --jobs=1
```

Promotion also requires the full serialized compiler suite in the checked-in
Nix environment, repository audit, documentation checks, execution-queue
checks, and `git diff --check`.

### Acceptance and queue lifecycle

Accepting this RFC moves it to `rfcs/accepted/`, changes its status, adds RFC
0009 to `rfcs/README.md`, creates the first child's implementation plan under
`.codex/plans/`, and replaces the design candidate with only that executor-ready
queue row plus matching blocker metadata.

Completing a child removes its row and stale blocker wording, updates
`docs/compiler/pipeline.md`, `docs/compiler/bootstrapping.md`, and
`docs/project/status.md` to describe the expanded opt-in profile, and promotes
only the next accepted child. Queue and docs validators plus `git diff --check`
run after every promotion and closeout.

## Context

RFC 0005 established a single-pass typed-core producer whose implemented
profile accepts one resolved module of closed scalar expressions and concrete
non-capturing direct calls. Its current recipe derivation flattens all
right-associated function arrows. RFC 0006 established lowered IR version 1
with recursively nestable closure representations and the immutable closure
environment operations needed here. The current producer and lowerer reject
callable values, captures, and recursion.

Closures and recursion must be designed together even though they are delivered
incrementally. A recursive function may escape as a value, a capturing
recursive function must retain its external lexical environment, and mutually
recursive peers must agree on environment ownership. Treating recursion as only
a lowerer name graph would work for a narrow direct case but would violate the
shared source semantics and leave no sound path to first-class recursion.

The selected model preserves unary source staging, transports binder/group
identity through validated typed core, and uses the immutable IR already
accepted by RFC 0006. First-member materialization plus peer reconstruction
avoids adding mutation solely to tie a recursive knot.

## Alternatives considered

**Flatten all closure calls into multi-operand calls.** This changes Jazz's
left-to-right curried staging when an intermediate application computes or
selects a closure. Flattening remains valid only for a statically known complete
direct leading-lambda chain.

**Implement partial application with synthetic adapters.** Staged unary
closures already make partial application the ordinary result of one source
call. Adapters would add generated functions and environments without a
semantic need.

**Recompute recursive groups from typed names in the lowerer.** This duplicates
the canonical recursive owner and can disagree under rebinding, aliases, or
lexical visibility. Validated typed core transports the shared result instead.

**Store recursive peer closures inside a cyclic environment.** This requires
mutable initialization, allocation placeholders, or a new runtime service that
lowered IR version 1 does not provide. Peer reconstruction from a shared
external-capture environment is sufficient for the accepted bounded profile.

**Combine every closure and recursion change in two implementation children.**
That hides distinct schema, binding, capture, application, and recursion seams.
Six ordered children retain the two approved milestones while keeping each
queue row independently verifiable.

**Lower a native backend for the scalar/direct-call profile first.** That would
exercise a temporary subset while closures, recursion, control flow, modules,
and managed data still determine the lasting ABI. Completing the
backend-neutral callable boundary first reduces backend rework.

## Consequences

- General closure calls preserve Jazz's unary curried evaluation order.
- Direct-call coalescing remains available only for statically known complete
  leading-lambda chains.
- Typed core gains explicit lexical binder references and transported recursive
  groups; both mirrors validate those facts before lowering.
- Every closure has a concrete immutable environment, including capture-free
  closures.
- Partial application needs no special callable representation beyond a
  closure-valued result.
- Accepted recursive closure groups need no cyclic mutation or runtime service,
  but groups with external captures introduced after the first member remain
  out of profile.
- Peer closures may be reconstructed repeatedly; allocation optimization is a
  later backend concern.
- Control flow, patterns, non-closure managed data layouts, runtime services,
  imports, multi-module lowering, polymorphism, capability evidence, tail-call
  selection, LLVM emission, object generation, linking, and the native runtime
  remain unpromoted.
- Accepting this RFC authorizes implementation planning only. It does not make
  closure or recursion lowering part of shipped Jazz behavior until each child
  and its verification gate land.
