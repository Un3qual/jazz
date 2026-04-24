# Pattern Matching Semantics

Status: active (literal, wildcard, variable, constructor, and exact-length bracketed-list `case` patterns parse/lower, typecheck, and execute end-to-end in `jazz-next`)
Locked decisions: 2026-03-18
Primary plan: `docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md`

## Purpose

Define the active-path contract for `case` expressions and the first executable
pattern subset in `jazz-next`.

## Implementation Target

- All new parser/analyzer/type/runtime work for this contract lands in
  `jazz-next/`.
- `jazz-hs/` and `jazz2/` are read-only legacy evidence only.

## Current Active Slice

Canonical surface form:

```jz
case <scrutinee> {
  | <pattern> -> <expr>
  | <pattern> -> <expr>
}
```

Current parser/core invariants:

1. A `case` expression has one scrutinee expression and one or more arms.
2. Every arm begins with `|` and uses `->` between pattern and body.
3. The currently landed surface/core pattern set includes:
   - integer literals
   - boolean literals
   - wildcard `_`
   - variable binders such as `item`
   - uppercase constructor patterns such as `Just item` or `Nothing`
   - bracketed list patterns such as `[head, _]` or `[]`
4. Constructor/list patterns are preserved structurally in `EPatternCase`.
   Declared constructor patterns typecheck against ADT scrutinees and bind
   payload variables in arm bodies; bracketed-list patterns typecheck against
   list scrutinees and bind element variables in arm bodies. Runtime matching
   supports declared constructors and exact-length bracketed lists.
5. Arm bodies are full expressions; nested `case`, `if`, lambdas, block-valued
   scrutinees, and infix/operator expressions remain valid inside arm bodies.
6. Lowering preserves direct `case` expressions as `EPatternCase Expr [CaseArm]`.
7. The older `ECase Expr Expr Expr` form remains the internal boolean-branch
   representation used after `if` desugaring.

## Matching Contract For The Committed Runtime Subset

1. Arms are tested from top to bottom.
2. The first matching arm wins.
3. Literal patterns match when the scrutinee value equals the literal and the
   literal belongs to the supported simple subset (`Int` or `Bool`).
4. `_` matches any value and binds no name.
5. A variable pattern matches any value and binds the scrutinee to that name
   only within the selected arm body.
6. A constructor pattern matches a saturated constructor value with the same
   constructor name and payload count, then recursively matches payload
   subpatterns.
7. A bracketed-list pattern matches a runtime list with exactly the same
   element count, then recursively matches element subpatterns.
8. Non-selected arm bodies are not evaluated.
9. A binder introduced by one arm is not visible in sibling arms or outside the
   `case` expression.

Examples:

```jz
flag = case n { | 0 -> True | _ -> False }.
copy = case value { | item -> item }.
maybeValue = case value { | Just item -> item | Nothing -> 0 }.
firstOrZero = case values { | [head, _] -> head | [] -> 0 }.
```

## Current Active Execution State

1. Parser, surface AST, and core AST now represent constructor and bracketed
   list patterns in `jazz-next`.
2. Analyzer/type/runtime execution is end-to-end for literal / wildcard /
   variable / constructor / exact-length bracketed-list patterns.
3. Declared constructor patterns typecheck against the scrutinee ADT type,
   bind payload variables in arm bodies, reject unknown constructor names or
   arity mismatches with deterministic `E2011` diagnostics, and participate
   in ordinary arm-result agreement checks.
4. Literal patterns must agree with the scrutinee type; incompatible literal
   patterns produce compile-time `E2011` diagnostics.
5. All arm bodies must agree on one result type; mismatched arm result types
   produce compile-time `E2012` diagnostics.
6. Bracketed-list patterns typecheck against list scrutinees, bind element
   variables in arm bodies, reject incompatible scrutinees with deterministic
   `E2011` diagnostics, and participate in ordinary arm-result agreement
   checks.
7. Runtime constructor/list pattern matching is first-match and recursive over
   nested subpatterns.
8. If no arm matches at runtime, evaluation emits deterministic `E3022`
   diagnostics rather than falling through silently.

## Deferred Pattern Forms

The following remain explicitly out of scope for the end-to-end committed
subset:

1. Cons-like list patterns.
2. Tuple patterns.
3. Lambda-parameter patterns.

## Non-Goals (Milestone 1)

1. Pattern guards, as-patterns, or-patterns, and pattern synonyms.
2. Exhaustiveness analysis beyond deterministic first-match semantics.
3. Match-compilation optimizations or decision-tree lowering.
4. Any new parser/type/runtime behavior under `jazz-hs/` or `jazz2/`.
