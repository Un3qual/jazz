# Jazz typed-core conditional control-flow design

**Date:** 2026-08-13

**Status:** Approved for implementation

## Purpose

Extend the opt-in typed-core producer and backend-neutral lowerer with value-
producing `if` expressions. Conditions and branches may use every expression
form already supported by the current scalar, closure, capture, currying, and
recursion profile. Ordinary `compile` and `run` behavior remains on canonical
core and the reference interpreter.

This is the first semantic-compiler child after accepted RFC 0009. It owns only
conditional control flow. Pattern cases, guards, pattern lambdas, managed data,
tail calls, module integration, and normal-pipeline cutover remain separate
contracts.

## Accepted boundary

An accepted conditional:

- has a source-valid `Bool` condition;
- has source-valid branches with one unified concrete result type and
  representation;
- appears at any expression path already admitted by the opt-in profile,
  including entry expressions, scalar initializers, named function bodies,
  nested lambdas, closure bodies, call operands, and either side of another
  conditional;
- evaluates its condition exactly once and executes exactly one branch; and
- returns the selected branch value through one explicit CFG join parameter.

Branch results may be any currently supported scalar or closure
representation. A conditional does not authorize text, lists, non-unit tuples,
ADTs, capability evidence, unresolved or polymorphic representations, local
blocks, pattern cases, user-defined operators, imports, or new runtime
services.

## Durable contract

Accepted RFC 0010 owns this delta. Existing Typed Core already represents the
source semantics:

```text
TypedIfExpr info condition thenExpression elseExpression
```

Existing Lowered IR version 1 already represents the required control flow:

```text
LoweredBranch condition thenBlock thenOperands elseBlock elseOperands
LoweredJump target operands
LoweredBlock target parameters instructions terminator
```

No Typed Core schema, hosted-Jazz schema, Lowered IR constructor, or version
change is required. Haskell and hosted-Jazz validators retain their current
conditional and edge invariants. The implementation extends production and
lowering into those already-validated contracts.

## Producer behavior

The shared inference traversal will retain a provisional conditional instead
of the current `TypedCoreControlFlowUnsupported` marker. It carries the final
resolved result type plus the provisional condition and branches. The
condition is specialized against `Bool`; both branches are specialized against
the unified conditional result type so numeric literal defaulting and callable
specialization remain exact.

Every provisional-tree consumer that currently owns callable shape, free-name
discovery, capture specialization, scalar references, recursive reachability,
or application profiles will traverse condition, then branch, and else branch
in canonical order. Finalization emits `TypedIfExpr` with child paths `0`, `1`,
and `2`, preserving existing source-diagnostic and profile-failure precedence.

Removing the conditional profile rejection may expose unsupported descendants.
Those descendants retain their current failure kinds and paths; the removed
parent failure does not reorder unaffected siblings. Failed production returns
no partial typed artifact.

## CFG lowering

The lowerer will replace its single-block instruction accumulator with a small
internal CFG builder. It owns:

- completed blocks in deterministic emission order;
- the current block identifier, parameters, reversed instructions, and next
  block-local temporary number;
- the existing local-binding and shared-environment operand maps; and
- helpers that terminate the current block and begin a successor block.

For one `TypedIfExpr`, lowering:

1. lowers the condition in the current block;
2. terminates that block with `LoweredBranch` to deterministic then and else
   block identifiers;
3. lowers each branch independently from the same ambient lexical state;
4. terminates each successful branch with `LoweredJump` to one join block;
5. passes the selected result as the final join argument; and
6. resumes expression lowering in the join block with a
   `LoweredBlockParameterOperand` for that result.

Nested conditionals use the same algorithm recursively. A condition that is
itself conditional therefore branches from its inner join block, and a branch
that is conditional jumps to the outer join from its inner join block.

## Ambient value transport

Lowered IR temporaries and block parameters are block-local. A branch cannot
refer directly to a temporary defined before the split, and code after the
join cannot refer directly to a value available only in a predecessor block.

Every conditional edge therefore transports the complete current ambient
operand environment required by the existing profile:

1. local bindings whose operands are block-local, ordered by
   `TypedBinderId`; then
2. shared closure environments whose operands are block-local, ordered by
   `LoweredLayoutId`.

Immediate operands and function parameters need no edge transport because
they are valid in every block. Each successor remaps transported operands to
its own block parameters before lowering expressions. Then and else edges
receive the same ambient parameter contract. The join contract repeats that
ambient contract and appends one `result` parameter with the conditional's
exact representation. Each branch passes its remapped ambient operands plus
its result. Branch-local temporaries and branch-local construction details do
not leak through the join.

Passing the complete current ambient environment is deliberate. It keeps
later sibling expressions valid after a conditional without introducing a
second liveness analysis. The environment is bounded by already-materialized
scalar bindings, capture projections, and shared closure environments, and its
ordering is deterministic.

## Identity and ordering

Generated block identifiers are injective within a lowered function and derive
only from the typed statement path, typed expression path, and the literal
role `then`, `else`, or `join`:

```text
if$s<count>$<comma-separated-statement-indexes>
  $e<count>$<comma-separated-expression-indexes>$<role>
```

Counts distinguish empty paths, and decimal indexes preserve the canonical
typed path without hashes or host paths. Blocks emit in structured traversal
order: the condition's existing blocks, then subtree, else subtree, and join.
Temporary numbering restarts at `t1` in each block because temporaries are
block-local. Block parameter identifiers use deterministic ordinal `live<N>`
names followed by `result` for the join value.

## Failure behavior

Failure precedence remains:

1. ordinary source diagnostics;
2. producer-profile failures;
3. typed-core invariant failures;
4. lowerer-profile failures; and
5. Lowered IR invariant failures.

Production visits condition, then branch, and else branch in that order.
Lowering also reports failures in that order. A conditional with an invalid
condition or branch produces no partial Typed Core or Lowered IR. Exact
representation disagreement remains owned by source inference or Typed Core
validation rather than repaired by the lowerer.

## Verification design

The source-to-typed-core manifest gains exact repeated accepted fixtures for:

- root scalar selection;
- conditionals in scalar bindings and named function bodies;
- earlier scalar values used in both branches;
- captured scalar values projected before a branch;
- closure-valued branch results and later closure application;
- nested conditionals in the condition and in each branch; and
- conditional operands inside existing binary and call expressions.

Exact lowered programs prove deterministic block identifiers, one evaluation
of the condition, then/else branch terminators, ambient edge arguments,
block-parameter remapping, result joins, block-local temporary reuse, nested
CFG ordering, and preservation of closure/capture lowering.

Rejected fixtures prove that pattern cases, managed branch values, local
blocks, unsupported operators, imports, and unresolved representations remain
closed. Existing conditional rejection fixtures are updated so only the newly
reachable descendant failures remain. Haskell and hosted-Jazz typed-core and
Lowered IR contract suites continue to compare exact values twice.

Focused verification runs the typed-core producer, typed-core contract, and
Lowered IR contract suites serially. Closeout also runs the full serialized
suite in the checked-in Nix environment, documentation and queue checks,
repository audit, and `git diff --check`.

## Approaches rejected

### Lower both branches into one instruction stream

Selecting between eagerly computed branch values would avoid CFG state, but it
would execute both branches and violate source semantics, effects, failures,
and allocation behavior.

### Add branches without ambient edge transport

Emitting then, else, and join blocks alone works only for literals and function
parameters. Earlier scalar temporaries and projected captures would become
out-of-scope operands in successor blocks, so the Lowered IR validator would
correctly reject common programs.

### Implement all cases and patterns now

Combining `if`, ordered cases, guards, pattern binding, managed ADT layouts,
and switch lowering would mix several independent semantic boundaries and be
too large for one reviewable batch. Conditional CFG construction is the shared
foundation and lands first.

## Non-goals

- Pattern cases, guards, pattern lambdas, exhaustiveness, or unreachable-arm
  analysis.
- Text, lists, non-unit tuples, ADTs, or other new managed representations.
- Local statement blocks inside the typed-core profile.
- Tail-call classification, stack guarantees, or tail-call terminators.
- Imported values, scalar exports beyond the existing bounded contract, or
  multi-module typed-core programs.
- Runtime services, effects, native emission, object generation, linking, a
  native ABI, bytecode, or a virtual machine.
- Normal compile/run cutover or a public compiler embedding API.
- A general optimizer, liveness analysis, SSA construction pass, or phi-node
  syntax outside existing block parameters.

## Acceptance criteria

The batch is complete when source-valid bounded `if` expressions produce exact
validated Typed Core, lower to deterministic validated multi-block Lowered IR,
transport every block-local ambient value explicitly, preserve nested closure
and capture behavior, and pass focused plus full serialized verification.
Every non-goal above must still fail at its documented boundary, normal
compile/run must remain unchanged, and the queue must close the child without
inventing a successor.
