# RFC 0013: Typed-core tail-position lowering

Status: Accepted
Date: 2026-08-14
Supersedes: None.

## Decision

Jazz will lower exact local direct and closure calls in true named or lifted
function-result position to the tail-call terminators already defined by
Lowered IR version 1. Tail position propagates into the selected branches of a
value-producing conditional and the selected bodies of a bounded scalar
pattern case. Conditions, scrutinees, guards, operands, and nested calls whose
results are consumed by another operation remain ordinary value positions.

Tail classification is based on control position and exact call signature, not
recursive-group membership. A partial application returns its closure
normally. An oversaturated application preserves ordered earlier stages and
may tail-transfer only its final exact closure-call stage.

The lowerer enters this behavior only while emitting a named or lifted
function body. The synthetic module-entry function retains ordinary call
instructions followed by `LoweredReturn`. No Typed Core constructor, Lowered
IR constructor or version, runtime service, validation failure, hosted-Jazz
schema, or public compile/run behavior changes.

For a non-control-flow tail expression, structured function-result lowering
may replace only the most recent call instruction when its exact temporary is
the returned operand and its result representation equals the enclosing
function result. It preserves every preceding instruction and emits the
matching direct or closure tail terminator. Other values emit
`LoweredReturn`.

A tail-position conditional emits its condition branch and recursively
terminates both selected branches without a result join. A tail-position
scalar pattern case preserves ordered tests, guard fallthrough, binders, and
edge transport, but recursively terminates every selected arm body without a
result join. Nested tail conditionals and cases follow the same rule.

Ordinary compile and run remain on canonical core and the reference
interpreter. This decision authorizes one tail-position-lowering child only.

## Context

RFC 0009 deliberately deferred tail-call selection until control-flow
ownership was established. RFC 0010 added deterministic conditional CFG
lowering, and RFC 0011 added ordered scalar pattern-case CFG lowering. Lowered
IR version 1 and both Haskell and hosted-Jazz validators already define and
validate `LoweredDirectTailCall` and `LoweredClosureTailCall`, but the active
Typed Core lowerer emits calls as instructions followed by returns or result
joins.

A global call-plus-return or CFG peephole pass would reconstruct source tail
intent after structured lowering. Propagating a function-result destination
while the lowerer still owns the typed expression path makes the decision
explicit, preserves deterministic construction and failure order, and avoids
unnecessary result joins.

## Consequences

- Direct, closure-shaped, self-recursive, mutually recursive, and
  non-recursive local calls share one control-position rule.
- Capture projection, closure construction, and argument evaluation remain
  ordinary ordered instructions before a tail terminator.
- Tail-position conditional branches and scalar-case bodies terminate
  directly; value-position uses retain their existing result joins.
- Typed Core production and mirrored validators do not change because the
  existing contracts already contain every required node and terminator.
- Tail terminators record frontend intent for future native lowering but do
  not by themselves claim native stack safety, a runtime ABI, or executable
  native output.
- Module-entry tail transfer, managed patterns, pattern lambdas, multi-module
  lowering, LLVM, object generation, linking, and the native runtime require
  separate contracts.

## Implementation status

Implemented on 2026-08-14 by
`JN-BOOTSTRAP-TYPED-CORE-TAIL-POSITION-LOWERING-001`.

The opt-in lowerer now records direct and closure tail intent only for complete
named or lifted function results. It recursively applies that result boundary
to selected conditional branches and bounded scalar-pattern-case bodies, while
conditions, scrutinees, guards, operands, and nested value contexts continue to
lower as values. Partial applications return closure values and oversaturated
applications tail-terminate only at their final exact stage. The synthetic
module entry remains ordinary call/join/return lowering.

The implementation reuses the existing Lowered IR schema, format, validator,
and runtime ABI. It changes no public language semantics or hosted-Jazz
compiler behavior and makes no native-stack optimization guarantee.
