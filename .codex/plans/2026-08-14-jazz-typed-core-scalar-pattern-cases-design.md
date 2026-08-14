# Jazz typed-core scalar pattern cases design

**Date:** 2026-08-14

**Status:** Approved for implementation

## Purpose

Extend the opt-in typed-core producer and backend-neutral lowerer with ordered
scalar pattern cases and guards. The batch establishes pattern testing,
arm-local scalar binding, guard fallthrough, and result joining on top of RFC
0010's deterministic conditional CFG without introducing managed values,
pattern-callable semantics, exhaustiveness analysis, or a runtime failure ABI.

Ordinary `compile` and `run` behavior remains on canonical core and the
reference interpreter. Accepted RFC 0011 owns the implementation boundary.

## Approved boundary

A supported case:

- appears at any expression path admitted by the current scalar, closure,
  capture, currying, recursion, and conditional profile;
- evaluates one scrutinee exactly once;
- gives the scrutinee one concrete representation already accepted by
  `scalarRepresentation`: unit, `Bool`, `Char`, a supported signed or unsigned
  integer width, or a supported floating width;
- contains only scalar literal, wildcard, or variable patterns;
- tries arms in source order;
- evaluates an arm guard only after that arm's pattern matches;
- continues to the next arm when a literal does not match or a guard is false;
- gives every guard the final type and representation `Bool`;
- gives every arm body one concrete unified result type and representation;
- permits any scalar or closure result already admitted by the current
  profile; and
- ends in an unguarded wildcard or variable arm.

Every non-final wildcard or variable arm must have a guard. That rule prevents
an earlier irrefutable arm from making the required final fallback unreachable
without asking this batch to own unreachable-arm analysis. A final variable
pattern binds the scrutinee in its body; a final wildcard does not.

Text literals and every list, non-unit tuple, and ADT value remain managed or
structured values outside this profile. Their patterns remain outside even
when the ordinary interpreter supports them.

## Durable contract

Accepted RFC 0011 owns the target boundary. Existing Typed Core already
represents the required semantics:

```text
TypedPatternCaseExpr info scrutinee arms
TypedCaseArm pattern maybeGuard body
TypedLiteralPattern info literal
TypedWildcardPattern info
TypedVariablePattern info binder name
```

Existing Lowered IR version 1 already provides the control-flow and comparison
vocabulary:

```text
LoweredPrimitiveOperation comparison operands
LoweredBranch condition matchBlock matchArgs nextBlock nextArgs
LoweredJump target operands
LoweredBlock target parameters instructions terminator
```

No Typed Core schema, hosted-Jazz schema, Lowered IR constructor, or version
change is required. The Haskell and hosted-Jazz validators retain their
existing complete pattern schema; production and lowering extend into the
already-validated subset.

The final catch-all rule is deliberately enforced as a producer and lowerer
profile boundary. It does not amend public case semantics, prove
exhaustiveness, or suppress the interpreter's existing `E3022` diagnostic when
ordinary execution reaches a case with no selected arm.

## Producer behavior

The shared inference traversal will retain a provisional pattern case instead
of the current `TypedCorePatternCaseUnsupported` result. The provisional case
contains:

- the resolved case result type;
- the provisional scrutinee;
- source-ordered provisional arms;
- each arm's original pattern plus exact inferred pattern bindings;
- the optional provisional guard; and
- the provisional body.

The existing ordinary pattern inference remains the semantic owner. Typed-core
production consumes the pattern typing and guard/body results produced during
that same traversal; it does not infer patterns or expressions again.

Finalization first confirms the complete profile shape. It then emits one
`TypedPatternCaseExpr`, exact `TypedCaseArm` values, and exact scalar pattern
nodes. A variable pattern receives a `TypedBinderId` derived from the module,
owning statement path, case expression path, arm index, and pattern position.
That binder is visible only while finalizing its guard and body.

Literal patterns carry the scrutinee's exact final type and representation.
Wildcard patterns carry the same contract without a binder. Variable patterns
carry that contract plus their exact binder and resolved current-module value
name. Finalization specializes guards against `Bool` and bodies against the
unified case result type so numeric defaulting, callable specialization, and
closure result recipes remain exact.

Every provisional-tree consumer that owns callable shape, free-name discovery,
capture specialization, scalar references, recursive reachability, or
application profiles traverses the scrutinee and then each arm's guard and body
in source order. Pattern-bound names are removed from the free-name environment
for that arm only.

Removing the parent pattern-case failure may expose unsupported descendants.
Those descendants retain their existing kinds and canonical order. Failed
production returns no partial Typed Core.

## Lowerer profile validation

The lowerer accepts only a validated `TypedPatternCaseExpr` satisfying the
approved boundary. It independently rejects:

- an empty arm list;
- a scrutinee or literal without a supported scalar representation;
- a constructor, list, cons-list, tuple, as-pattern, or or-pattern;
- a non-final unguarded wildcard or variable arm;
- a final literal pattern;
- a guarded final wildcard or variable arm;
- a guard whose final representation is not `Bool`; or
- an arm body whose result representation differs from the case result.

This check is separate from Typed Core structural validation. The full schema
can describe later pattern contracts, while this lowerer stays fail-closed on
the bounded RFC 0011 subset.

## Ordered CFG lowering

The lowerer evaluates the scrutinee once in the current block and records it as
an explicit carried operand. Arms then lower in source order.

For a literal arm:

1. emit an exact scalar equality comparison against the transported scrutinee;
2. terminate the current test block with `LoweredBranch` to the arm's matched
   block or its next-arm block;
3. if the arm has a guard, lower it only in the matched block and branch to the
   body or the same next-arm block; and
4. lower the body and jump to the shared result join.

For a wildcard arm, enter its guard or body without a comparison. For a
variable arm, first add an arm-local binding from the exact pattern binder to
the transported scrutinee, then lower its guard or body. Guard-false edges do
not carry that binder into the next arm. The final catch-all lowers directly to
its body and has no unmatched continuation.

Every successful body passes its result as the final join argument. The join
reconstructs the ambient operand environment and exposes one result block
parameter for continued expression lowering. A nested case or conditional may
therefore finish in its own join before the containing arm continues.

## Ambient transport and scope

Arm CFG edges use RFC 0010's explicit ambient transport model. The edge
contract contains, in deterministic order:

1. block-local local bindings ordered by `TypedBinderId`;
2. block-local shared closure environments ordered by `LoweredLayoutId`; and
3. carried in-flight operands ordered by their internal stable carrier id.

The scrutinee is a carried operand whenever its representation is block-local.
Immediate scrutinees and function parameters may be reused directly. Each
successor remaps transported values to its own block parameters.

Pattern-variable binders are deliberately arm-local. They are added only to
the matched arm state, are available to that arm's guard and body, and are
removed before a guard-false edge enters the next arm. They never enter the
shared result join as named locals, although an arm body may return their value
as the ordinary case result.

## Identity and ordering

Generated block identifiers derive only from the typed statement path, case
expression path, zero-based arm index, and a literal role such as `match`,
`guard`, `body`, `next`, or `join`. The grammar uses counted path segments, as
RFC 0010 does, so empty and nested paths remain injective without hashes or
host paths.

Block emission follows structured source traversal. The scrutinee's existing
blocks come first, followed by each arm's test, guard, and body subtrees in
source order, followed by the case join. Temporary numbering restarts within
each block because temporaries remain block-local.

## Failure behavior

Failure precedence remains:

1. ordinary source diagnostics;
2. producer-profile failures;
3. Typed Core invariant failures;
4. lowerer-profile failures; and
5. Lowered IR invariant failures.

Within a source-valid case, production and lowering visit the scrutinee first,
then arms in source order, and within each arm the pattern, guard, and body.
The implementation does not reorder failures to favor the final catch-all
check. No failed stage returns a partial typed or lowered artifact.

Source-valid cases outside the profile still follow ordinary compile and run.
A missing catch-all is a typed-core production profile failure, not a new
source diagnostic. Ordinary interpreter execution retains its existing
runtime `E3022` behavior if no arm is selected.

## Why adjacent pattern work is deferred

### Managed patterns

Constructor, list, cons-list, tuple, and managed text patterns need concrete
managed values before they can be lowered. That work owns layout identity,
allocation representation, variant tags, product and variant field projection,
list shape, and the lifetime or runtime contract for managed references.
Combining it with ordered case CFG would make failures ambiguous between value
representation and selection semantics. A later managed-value contract should
land construction first and destructuring second.

### Pattern lambdas

Pattern lambdas are not merely another case-expression location in the backend
profile. Their match occurs at invocation time, and failure behavior becomes a
callable contract. They interact with closure environments, curried argument
staging, partial application, recursive callable groups, and exact parameter
binder identity. A later contract can reuse the proven scalar case matcher
inside function entry blocks without making this batch revise callable ABI
rules.

### Exhaustiveness and unreachable arms

Static coverage requires a separate analysis over constructor spaces, literal
domains, or-patterns, guards, and module-visible ADT definitions. Guards are
generally not proof of coverage, and unreachable-arm policy changes public
diagnostics. The required final catch-all is therefore only a syntactic
backend-profile gate. It neither proves general exhaustiveness nor introduces
warnings or errors into ordinary compilation.

## Approaches rejected

### Group literal arms into `LoweredSwitch`

Switch grouping is attractive for unguarded integral tags but does not preserve
the general scalar contract cleanly. Repeated literals with different guards
must remain ordered, scalar representations do not all share one switch key,
and guard-false edges must resume at the next source arm rather than at a
coalesced default. An optimizer may recognize eligible arm chains later.

### Normalize cases into `TypedIfExpr`

Desugaring after inference would hide explicit pattern and binder identity,
weaken Typed Core parity, complicate source paths, and make arm-local scope an
implicit lowering convention. Typed Core already owns the durable pattern
schema, so the lowerer should consume it directly.

### Add a backend `E3022` failure path now

Lowering non-total cases would require a trap terminator or runtime service and
a stable diagnostic/runtime ABI. That is independently useful but not required
to prove ordered scalar pattern selection. The final catch-all keeps this batch
total and independently reviewable.

## Verification design

Exact repeated source-to-Typed-Core fixtures will cover:

- root literal selection with a wildcard fallback;
- variable fallback binding in an arm body;
- a guarded variable arm using its binder;
- literal guard success and guard-false fallthrough;
- repeated literals with different guards;
- earlier scalar bindings used in the scrutinee, guards, and bodies;
- scalar captures projected before arm tests;
- scalar and closure-valued arm results;
- nested cases in scrutinee, guard, and body positions;
- cases inside conditionals, closure applications, curried calls, and supported
  recursive function bodies; and
- later sibling expressions that reuse ambient values after the case join.

Exact lowered fixtures will prove:

- one scrutinee evaluation;
- deterministic block identifiers and structured emission order;
- exact scalar comparison operations;
- pattern-success and guard-fallthrough edges;
- arm-local binder remapping and non-leakage;
- ambient and in-flight operand transport;
- one shared result join with the exact representation;
- block-local temporary reuse; and
- nested case/conditional CFG composition.

Rejected fixtures will prove that production and lowering remain closed for:

- a missing final catch-all;
- a guarded final catch-all;
- a final literal arm;
- an earlier unguarded wildcard or variable arm;
- text, constructor, list, cons-list, tuple, as-, and or-patterns;
- pattern lambdas;
- unsupported scrutinee, guard, or body descendants;
- imports and multi-module programs; and
- arbitrary valid Typed Core that violates the lowerer profile.

Focused verification runs the typed-core producer, typed-core contract, and
Lowered IR contract suites serially in the checked-in Nix environment.
Closeout also runs the full serialized suite, documentation and queue checks,
the repository audit, and `git diff --check`.

## Non-goals

- Managed text, list, non-unit tuple, ADT, or other structured value production.
- Constructor, list, cons-list, tuple, as-, or or-pattern lowering.
- Pattern lambdas or pattern-shaped callable parameters.
- Exhaustiveness, redundancy, or unreachable-arm analysis and diagnostics.
- A backend no-match trap, `E3022` runtime service, or runtime ABI change.
- Pattern-test optimization, switch selection, decision trees, or a general CFG
  optimizer.
- Local statement blocks beyond the current typed-core profile.
- Tail-call classification or tail-call terminators.
- Imported inputs, scalar export expansion, or multi-module typed-core programs.
- Runtime services, effects, native emission, linking, bytecode, or a VM.
- Normal compile/run cutover or a public compiler embedding API.

## Acceptance criteria

After RFC 0011 is accepted and an implementation child is promoted, the batch
is complete when every supported scalar case produces exact validated Typed
Core, lowers to deterministic validated multi-block Lowered IR, evaluates the
scrutinee once, preserves source-ordered literal and guard fallthrough, scopes
variable binders to their arms, transports every block-local value explicitly,
and joins all body results under one representation.

Every non-goal must still fail at its documented opt-in profile boundary.
Ordinary compile and run behavior must remain unchanged. Focused and full
serialized verification, documentation checks, queue checks, repository audit,
and `git diff --check` must pass before the implementation child closes.
