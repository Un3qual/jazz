# Jazz typed-core tail-position lowering design

**Date:** 2026-08-14

**Status:** Approved for implementation

## Purpose

Extend the opt-in typed-core lowerer so calls in true function-result position
emit the tail-call terminators already defined by Lowered IR version 1. The
change applies to direct and closure-shaped named or lifted functions across
the current scalar, closure, recursion, conditional, and scalar-pattern-case
profile.

Ordinary `compile` and `run` remain on canonical core and the reference
interpreter. Typed Core production, module-entry evaluation, schemas, mirrored
validators, and public language behavior remain unchanged.

## Accepted boundary

The lowerer treats the body of every named or lifted `LoweredFunction` as a
function-result position. Within that body:

- an exact local direct call whose result is the function result becomes
  `LoweredDirectTailCall`;
- an exact unary closure-call stage whose result is the function result becomes
  `LoweredClosureTailCall`;
- the selected branch of a tail-position conditional inherits function-result
  position;
- the selected body of a tail-position scalar pattern case inherits
  function-result position;
- conditions, scrutinees, guards, call operands, and binary operands remain
  ordinary value positions; and
- every other supported tail expression ends with `LoweredReturn`.

A partial application still returns its produced closure normally. An
oversaturated application evaluates earlier call stages in source order and
may tail-transfer only its final exact closure-call stage. Tail classification
is not restricted to recursive callees: any supported local direct or closure
call in true function-result position uses the corresponding terminator.

The synthetic module-entry function stays on the existing value-producing
path. Earlier module statements and the terminal entry expression continue to
produce an operand followed by `LoweredReturn`; a later contract may decide
whether module initialization permits entry tail transfer.

## Durable contract

Accepted RFC 0013 owns this delta. Lowered IR version 1 already defines:

```text
LoweredDirectTailCall LoweredFunctionId [LoweredOperand]
LoweredClosureTailCall LoweredOperand [LoweredOperand]
```

The existing Haskell and hosted-Jazz validators already require the target
call result to equal the enclosing function result. This batch changes only
the Haskell lowerer that constructs validated Lowered IR. It adds no Typed Core
or Lowered IR constructor, version, runtime service, validation failure, or
hosted-Jazz schema change.

## Function-result lowering

`emitFunction` will lower a body through a dedicated function-result boundary
instead of requesting an operand and appending `LoweredReturn` afterward. The
boundary receives the enclosing function result representation and returns a
state whose current path is already terminated.

For an ordinary tail expression, the boundary first uses existing value
lowering. It then checks the exact operand that the expression produced:

1. if the operand is the temporary produced by the most recent
   `LoweredDirectCall`, remove that one call instruction and terminate the
   current block with the corresponding `LoweredDirectTailCall`;
2. if it is the temporary produced by the most recent `LoweredClosureCall`,
   remove that one call instruction and terminate with
   `LoweredClosureTailCall`; or
3. otherwise terminate with `LoweredReturn`.

This is not a global peephole optimizer. The rewrite runs only at a
source-derived function-result boundary, requires exact temporary identity and
representation agreement, and can consume only the call instruction that
immediately produced the returned operand. Earlier evaluation instructions,
capture projections, closure construction, and argument evaluation remain in
their original order.

## Conditional tail propagation

A conditional in function-result position keeps its existing condition
lowering and explicit ambient edge transport. Its condition block branches to
deterministic then and else blocks. Each branch is lowered recursively through
the same function-result boundary, so it ends in either `LoweredReturn`,
`LoweredDirectTailCall`, or `LoweredClosureTailCall`.

The tail form emits no result join block because neither branch continues.
Nested conditionals repeat the same rule. A conditional used as a condition,
argument, scrutinee, guard, binary operand, scalar initializer, or module-entry
value continues to use the existing branch-and-join form.

## Scalar-pattern-case tail propagation

A scalar pattern case in function-result position retains its existing
single-evaluation scrutinee, source-ordered literal tests, guard fallthrough,
arm-local binders, deterministic block identities, and explicit control-edge
transport.

Each selected arm body is lowered recursively through the function-result
boundary. After one body terminates, lowering resumes only to construct the
later alternative blocks reachable from earlier pattern or guard failure. The
tail form emits no result join block. Scrutinees and guards remain value
positions, and the existing required final unguarded catch-all remains
unchanged.

## State and ordering

The internal CFG state remains the sole owner of block ordering and block-local
temporary numbering. Tail completion preserves all already-emitted
instructions, appends exactly one terminator, and leaves no unterminated
current block.

Conditional and pattern alternatives reuse the existing continuation-template
discipline: completed blocks from an earlier alternative are carried forward,
while lexical operands for a later alternative restart from the common
pre-branch state and are remapped through explicit block parameters.

Block identifiers, ambient-slot ordering, temporary identifiers, closure
environment identities, and recursive-group identities do not change.

## Failure behavior

Failure precedence remains:

1. ordinary source diagnostics;
2. producer-profile failures;
3. Typed Core invariant failures;
4. lowerer-profile failures; and
5. Lowered IR invariant failures.

Tail classification creates no new rejection. If ordinary value lowering
fails, the existing ordered lowerer failures are returned. If a produced
operand or call result disagrees with the enclosing function result, the
existing unsupported-expression or Lowered IR validation boundary remains
authoritative; the lowerer does not repair the mismatch. Failed lowering
returns no partial artifact.

## Verification design

Exact source-to-Lowered-IR expectations will prove:

- direct self and mutual recursion use `LoweredDirectTailCall`;
- capturing and closure-shaped recursion use `LoweredClosureTailCall` after
  their required environment and closure instructions;
- non-recursive local direct calls in function-result position also use direct
  tail terminators;
- conditional branches return or tail-transfer directly without a result join;
- guarded and unguarded scalar-case bodies return or tail-transfer directly
  without a result join;
- conditions, scrutinees, guards, arguments, nested calls used as operands, and
  the synthetic module entry remain ordinary value lowering; and
- partial application returns a closure, while only the final exact stage of
  an oversaturated call may tail-transfer.

Every new expectation is run against real source or independently constructed
valid Typed Core and compared as exact Lowered IR. Existing Lowered IR contract
fixtures continue to prove Haskell and hosted-Jazz validator parity for both
tail terminators.

Focused verification runs the typed-core producer/lowerer, Typed Core contract,
and Lowered IR contract suites serially. Closeout runs the full serialized
suite inside the checked-in Nix shell, documentation and queue checks,
repository audit, formatting checks, and `git diff --check`.

## Approaches rejected

### Global call-plus-return peephole pass

A whole-program rewrite that scans arbitrary `call; return` instruction pairs
would infer source intent backward from emitted IR and complicate block and
failure ownership. Tail intent belongs to structured frontend lowering, so the
local rewrite is entered only from an explicit function-result context.

### Recursive calls only

Limiting tail terminators to self or peer calls would make the same tail
position lower differently based on call-graph membership. Lowered IR defines
tail transfer by control position and signature, not by recursion detection.

### Join first, tail-call after the join

Keeping conditional and case result joins would hide the call inside a
predecessor and leave the join returning a block parameter. Recognizing that
shape afterward would require CFG optimization. Propagating function-result
context into selected branches and arms expresses the contract directly and
eliminates unnecessary joins.

### Tail-transfer the module entry now

The synthetic entry function also sequences top-level initialization and
statement evaluation. Keeping it unchanged avoids silently deciding a future
module-initialization and native-entry ABI contract in this lowering batch.

## Non-goals

- Typed Core production, specialization, schemas, or validators.
- New Lowered IR constructors, versions, validators, or hosted-Jazz changes.
- Tail-position analysis for unsupported blocks, managed patterns, pattern
  lambdas, imports, or multi-module Typed Core.
- Tail transfer from the synthetic module-entry function.
- A source diagnostic for non-tail recursion or an annotation requesting tail
  calls.
- A native stack-safety guarantee, LLVM lowering, runtime ABI, object
  generation, linking, bytecode, or a virtual machine.
- General CFG optimization, dead-block elimination, inlining, liveness, or SSA
  rewriting.
- Changes to ordinary canonical-core interpretation or public runtime
  semantics.

## Acceptance criteria

The batch is complete when every supported named or lifted function body uses
tail-call terminators for exact direct and closure calls in true result
position, function-result conditionals and scalar pattern cases propagate that
position into selected bodies without result joins, non-tail evaluation remains
unchanged, and exact focused plus full serialized verification passes. The
queue must close the child without inventing another successor.
