# RFC 0010: Typed-core conditional control flow

Status: Accepted
Date: 2026-08-13
Supersedes: None.

## Decision

Jazz will extend the opt-in typed-core producer and backend-neutral lowerer
with value-producing `if` expressions. A supported conditional may appear at
any expression path admitted by the existing scalar, closure, capture,
currying, and recursion profile. Its condition is `Bool`, both branches have
one concrete unified result representation, the condition executes once, and
only the selected branch executes.

Typed Core continues to represent a conditional as `TypedIfExpr`. Lowered IR
version 1 continues to represent it with `LoweredBranch`, then and else blocks,
`LoweredJump`, and a join block whose final parameter carries the selected
result. No schema constructor or IR version changes.

Every value that crosses a block edge is explicit. Local bindings and shared
closure environments backed by a temporary or block parameter are transported
to both branches and the join as deterministic block parameters. Immediate
operands and function parameters remain directly usable. The join appends the
conditional result after the ordered ambient parameters. The lowerer does not
infer hidden cross-block scope or permit a predecessor's block-local operand
to leak into a successor.

Generated block identifiers derive injectively from the typed statement path,
typed expression path, and `then`, `else`, or `join` role. Block emission follows
structured condition, then, else, join order. Temporary identifiers remain
block-local and restart in each block.

Ordinary compile and run modes remain on canonical core and the reference
interpreter. This decision authorizes one conditional-control-flow child only.

## Context

RFC 0009 completed the accepted closure and recursive-callable sequence. The
remaining typed-core profile excludes full control flow even though Typed Core
already has conditional nodes and Lowered IR already has validated branch,
jump, block-parameter, and edge-argument contracts.

Removing only the producer rejection would still leave the lowerer single-
block. Emitting branches without transporting ambient temporaries would violate
the existing Lowered IR rule that temporaries and block parameters are local to
their defining blocks. A correct vertical slice therefore needs both retained
conditional production and a deterministic internal CFG builder.

Conditional control flow is separable from pattern control flow. Ordered cases
also require pattern binding, managed value layouts, failure fallthrough, and
eventually exhaustiveness policy. Keeping those concerns out makes this delta
large enough to be meaningful while remaining independently reviewable.

## Consequences

- The shared inference traversal retains provisional conditionals and every
  callable, capture, scalar-reference, and specialization traversal descends
  through their three children in canonical order.
- Typed-core finalization emits exact `TypedIfExpr` nodes and removes the
  parent conditional-profile failure while preserving descendant failures.
- The lowerer becomes multi-block internally and explicitly remaps ambient
  block-local operands at every branch and join edge.
- Nested conditionals and closure-valued branches must work without a new IR
  constructor, version, runtime service, or eager branch evaluation.
- Haskell and hosted-Jazz validators remain aligned on their existing schemas
  and invariant vocabulary; checked exact fixtures prove the newly reachable
  valid artifacts.
- Pattern cases, guards, pattern lambdas, managed data, tail calls, modules,
  native work, and normal compile/run cutover require separate contracts.

## Implementation closure

`JN-BOOTSTRAP-TYPED-CORE-CONDITIONAL-CONTROL-FLOW-001` completed on
2026-08-13. Exact source and arbitrary-valid-Typed-Core fixtures cover scalar
results, parameter use, captured scalars, closure-valued branch results,
conditionals inside closure applications, and conditionals nested in the
condition, then, and else positions. The resulting Lowered IR validates with
deterministic counted block identities and explicit transport of named ambient
values plus in-flight evaluation operands. Ordinary compile and run remain on
canonical core and the reference interpreter.
