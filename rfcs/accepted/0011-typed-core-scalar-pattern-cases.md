# RFC 0011: Typed-core scalar pattern cases

Status: Accepted
Date: 2026-08-14
Supersedes: None.

## Decision

Jazz will extend the opt-in typed-core producer and backend-neutral lowerer
with ordered scalar pattern cases. A supported case may appear at any
expression path admitted by the existing scalar, closure, capture, currying,
recursion, and conditional profile.

The scrutinee is evaluated exactly once and has one concrete immediate scalar
representation already supported by the profile. Arms are tried in source
order. Supported patterns are scalar literals, wildcards, and variables.
Literal patterns must have the scrutinee's exact final type and representation.
A variable pattern binds the scrutinee under one exact `TypedBinderId` for its
guard and body. Wildcards introduce no binding.

Every non-final wildcard or variable arm must have a guard. The final arm must
be an unguarded wildcard or variable. Guards have type `Bool`, execute only
after their pattern matches, and continue to the next arm when false. Arm
bodies share one concrete unified result representation, which may be any
scalar or closure representation already supported by the current profile.

Typed Core continues to represent these cases with `TypedPatternCaseExpr`,
`TypedCaseArm`, `TypedLiteralPattern`, `TypedWildcardPattern`, and
`TypedVariablePattern`. Lowered IR version 1 continues to represent their
control flow with primitive comparisons, `LoweredBranch`, `LoweredJump`, and
block parameters. Lowering uses an ordered arm chain rather than normalizing
the case into typed conditionals or grouping arms into a switch.

Every value crossing an arm-test, guard, body, continuation, or join edge is
explicit. The scrutinee, existing block-local ambient values, and in-flight
operands are transported in deterministic order and remapped to successor
block parameters. Arm-local pattern binders do not escape their guard or body.
Each successful body jumps to one result join. The final catch-all has no
unmatched successor.

No new Typed Core constructor, Lowered IR constructor or version, runtime
service, trap terminator, or backend `E3022` path is introduced. A source-valid
case outside this bounded profile returns a structured producer-profile
failure while ordinary compile and run continue to use canonical core and the
reference interpreter.

## Context

RFC 0010 established deterministic multi-block lowering for value-producing
conditionals, including explicit transport of block-local ambient and in-flight
values. The remaining typed-core profile still rejects every pattern case even
though Typed Core already has a complete pattern schema and validator and
Lowered IR already has the branch, jump, block-parameter, primitive-comparison,
and switch vocabulary required by later pattern work.

Full pattern control flow is not one coherent next child. Managed constructor,
list, tuple, and text patterns depend on managed value production, layout
identity, tags, field projections, and ownership rules that the producer and
lowerer do not yet support. Pattern lambdas add invocation-time match failure
and interact with closure construction, currying, recursion, and callable
parameter identity. Static exhaustiveness and unreachable-arm analysis require
coverage reasoning and a diagnostic policy independent of runtime selection.

A scalar-only arm chain establishes ordered testing, arm-local binding, guard
fallthrough, and result joining without taking ownership of those later
contracts. Requiring a final syntactic catch-all makes the bounded lowered
profile total without claiming that Jazz performs exhaustiveness analysis and
without adding a runtime-failure ABI.

## Consequences

- The shared inference traversal retains a provisional case containing the
  scrutinee and source-ordered arms. Each arm retains its pattern typing,
  optional guard result, and body result from that same traversal.
- Typed-core finalization emits exact pattern and binder identities and removes
  the parent pattern-case profile failure only when the complete bounded shape
  is supported. Unsupported descendants retain their existing ordered paths
  and failure kinds.
- The lowerer independently enforces the supported pattern forms and final
  catch-all rule for arbitrary validated Typed Core. It does not trust producer
  provenance or repair type and representation disagreement.
- Literal tests, pattern success, guard success, guard fallthrough, body
  evaluation, and result joining are represented as a deterministic ordered
  CFG. The scrutinee is never reevaluated.
- Repeated literal arms remain meaningful when guards differ because lowering
  preserves source order instead of coalescing literal keys.
- The syntactic catch-all requirement is a profile boundary, not a public
  exhaustiveness rule. Ordinary interpreter execution retains its existing
  `E3022` behavior for a source-valid case with no selected arm.
- Haskell and hosted-Jazz Typed Core and Lowered IR validators remain aligned
  on their existing schemas and invariant vocabulary. Exact fixtures prove the
  newly reachable valid artifacts and the still-closed boundary.
- Managed patterns, as-patterns, or-patterns, pattern lambdas, exhaustiveness,
  unreachable-arm diagnostics, runtime failure services, tail calls, modules,
  native work, and normal compile/run cutover require separate contracts.
