# Jazz typed-core direct recursion design

**Date:** 2026-08-12

**Status:** Approved for implementation

## Purpose

Extend the opt-in typed-core producer and backend-neutral lowerer with
capture-free, non-escaping self and mutual direct recursion. Recursive calls
remain ordinary direct calls, and the existing canonical-core interpreter
continues to own normal `compile` and `run` behavior.

This is the fifth ordered child of accepted RFC 0009. It completes only
`JN-BOOTSTRAP-TYPED-CORE-DIRECT-RECURSION-001`; escaping or capturing recursive
groups remain the separate closure-recursion child.

## Accepted boundary

An accepted recursive group consists only of concrete monomorphic root
function bindings. Every member:

- is a leading-lambda function with a direct callable shape;
- is capture-free;
- does not escape as a value or participate in partial application;
- refers to itself or peers through exact `TypedBinderId` values; and
- otherwise satisfies the existing scalar, closure-call, lexical-capture, and
  curried-application contracts.

Both singleton self cycles and multi-member mutual cycles are accepted. Group
and member order follow canonical source statement order. Recursive aliases,
patterns, non-closure managed values, capability evidence, imports,
cross-module groups, closure-shaped members, and external captures remain
structured profile failures.

## Architecture

`Jazz.Compiler.RecursiveBindings` remains the sole owner of recursive-group
discovery, declaration positions, rebinding behavior, outer-binding behavior,
and lexical shadowing. Scope analysis already transports canonical recursive
member statement indexes on provisional callable declarations. Finalization
will consume that evidence once; it will not run a second name-based recursion
analysis.

Typed Core gains the contract extension fixed by RFC 0009:

```text
TypedRecursiveGroup [TypedBinderId]

TypedModule
  modulePath sourcePath imports exports interface recursiveGroups statements moduleInfo
```

The Haskell and hosted-Jazz type definitions change together. Groups are
ordered by their earliest member, and members are ordered by source statement
position. The producer maps the provisional statement indexes to the exact
local `TypedLetStatement` binder identities and attaches the resulting groups
to the module.

Typed Core describes recursive membership independently of backend support.
The validators therefore accept structurally valid direct- or closure-shaped
groups, while this producer and lowerer profile admits only all-direct groups.
That separation keeps closure-recursion artifacts representable without
silently enabling their execution in this child.

## Producer behavior

The producer first retains the existing callable-shape analysis. A fully
applied capture-free self or peer call does not turn its owner into a closure;
using a member as a value, capturing a lexical value, or underapplying it does.

For an all-direct canonical group, finalization:

1. exposes every member scheme while finalizing each member body;
2. resolves self and peer variables to their declaration binders;
3. preserves the existing direct representation recipes and arities;
4. emits one ordered `TypedRecursiveGroup`; and
5. keeps declarations and function bodies in source order.

Forward visibility is group-scoped. A mutual peer is visible inside another
member body because both belong to the same canonical group; this does not make
unrelated later declarations visible elsewhere.

If any member is closure-shaped, non-monomorphic, rebound, not a root lambda,
or otherwise outside the accepted profile, production returns ordered
`TypedCoreRecursiveFunctionUnsupported` and existing companion failures with no
partial typed artifact. The next closure-recursion child owns removal of that
profile rejection.

## Typed-core validation

Both validators use binder identities, never textual names, to validate the
new field. They enforce:

- no empty group;
- every member is a local callable `TypedLetStatement` binder;
- no duplicate member within a group;
- no binder belongs to more than one group;
- groups and members are in canonical statement order;
- every declared group agrees with cyclic binder-reference reachability; and
- every local cyclic callable component is declared exactly once.

While validating a member body, only that member's recursive peers are added to
the active scheme context. All other statement-order visibility rules remain
unchanged. Existing variable validation continues to check binder identity,
type, representation recipe, callable shape, and complete direct-call arity.

The validators reuse the existing failure vocabulary for unknown and duplicate
binders and add exactly one new kind, `TypedRecursiveGroupMismatch`, for empty,
ordering, membership, or reachability disagreement. Existing detail
constructors identify the owning group index or binder. Haskell and hosted Jazz
must emit the same failure kind, detail, path, and order.

## Lowering

The lowerer receives an already validated `TypedModule`. It indexes the module's
recursive groups by binder and no longer reconstructs strongly connected
components from function bodies.

An all-direct group is accepted when every member resolves to an existing
direct `FunctionShape`. Because function shapes are indexed before bodies are
emitted, self and forward peer references lower through the existing direct
callee path. Recursive calls emit the same ordinary direct-call instruction as
non-recursive direct calls. No Lowered IR constructor, version, environment,
placeholder, mutable cell, adapter ABI, tail-call marker, or runtime service is
added.

A valid typed-core group containing a closure-shaped member remains a lowerer
profile failure at the owning statement. This preserves the independently
constructed closure-recursion boundary for the following child.

## Data flow

```text
resolved canonical scope
  -> RecursiveBindings ordered statement groups
  -> provisional callable declarations
  -> ordered TypedBinderId groups on TypedModule
  -> Haskell and hosted-Jazz invariant validation
  -> direct-recursion profile check
  -> existing direct-call Lowered IR emission
```

No stage consumes a sidecar name map. The producer is the only bridge from
canonical statement membership to typed binder membership, and the lowerer
trusts only validated typed-core metadata.

## Failure behavior

Failure precedence remains ordinary source diagnostics, producer-profile
failures, typed-core invariant failures, lowerer-profile failures, and then
Lowered IR invariant failures.

Within production or lowering, a recursive-group classification failure is
reported at its owning statement before expression descendants. Statements
remain ordered by source position, and descendants remain in canonical
expression pre-order. Removing the direct-recursion rejection may expose an
existing descendant failure, but it must not reorder unaffected failures.
Failed production, validation, or lowering returns no partial artifact.

## Verification design

The source-to-typed-core manifest adds exact accepted programs for direct self
recursion and direct mutual recursion. Expected programs include literal binder
identities, ordered group metadata, direct callable shapes, and exact
self/peer variable references. Each result is produced and compared twice.

The independently constructed typed-core manifest adds valid direct recursive
programs and proves their exact Lowered IR direct calls. Malformed fixtures
cover empty groups, unknown members, duplicate members, membership in multiple
groups, non-canonical order, a missing declared cycle, a spurious declared
group, and disagreement between group membership and binder-reference
reachability.

Existing fixtures continue to prove:

- closure-shaped self and mutual recursion remain rejected by the lowerer;
- producer rejection of escaping or capturing recursive groups;
- rebinding, outer-binding, parameter-shadowing, and nested-scope ownership;
- recursion plus sibling or descendant failure ordering;
- exact Haskell and hosted-Jazz validator parity; and
- preservation of every earlier typed-core and lowerer fixture.

The focused gate is RFC 0009's G4 command:

```bash
cabal test recursive-bindings-spec jazz-typed-core-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
```

Closeout also runs the full serialized compiler suite in the checked-in Nix
environment, documentation and execution-queue checks, repository audit, and
`git diff --check`. Implementation milestones are committed as they become
green.

## Approaches rejected

Removing the producer and lowerer recursion failures while retaining the
lowerer's SCC reconstruction would be a smaller patch, but it would duplicate
recursive ownership, omit the accepted typed-core contract, and allow the
producer and lowerer to disagree.

Combining direct and closure recursion would avoid one queue transition but
would also introduce shared external-capture environments, peer-closure
reconstruction, and escaping callable behavior before their ordered child. It
is deliberately excluded.

## Non-goals

- Closure-shaped, escaping, or capturing recursion.
- Tail-call classification or stack guarantees.
- Control flow or pattern expansion.
- Multi-module or imported recursive groups.
- Non-closure managed recursive values.
- Normal compile/run cutover.
- LLVM emission, object generation, linking, native ABI work, bytecode, or a
  virtual machine.
- New caches, sidecar dependency maps, compatibility adapters, or duplicate
  validator harnesses.

## Acceptance criteria

The batch is complete when direct self and mutual recursion produce validated
typed-core groups, lower through the existing direct-call representation, and
pass exact repeated fixtures plus G4 and the full serialized compiler suite.
Closure recursion and every unrelated boundary above must still fail at their
documented phase, public compiler-boundary pages must reflect the new opt-in
profile, and queue state must advance only to the closure-recursion curation
candidate.
