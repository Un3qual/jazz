# Pattern Matching Semantics

Status: active (literal, wildcard, variable, constructor, exact-length bracketed-list, cons-like list, and fixed-arity tuple patterns parse/lower, typecheck, and execute end-to-end in `jazz-next` `case` arms and lambda parameters)
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
   - cons-like list patterns such as `[head | tail]`
   - tuple patterns such as `(left, right)` or `(1, flag)`
4. Constructor/list/tuple patterns are preserved structurally in `EPatternCase`.
   Declared constructor patterns typecheck against ADT scrutinees and bind
   payload variables in arm bodies; bracketed-list patterns typecheck against
   list scrutinees and bind element variables in arm bodies; cons-like list
   patterns typecheck against non-empty list deconstruction with the head
   subpattern at the element type and the tail subpattern at the same list type;
   tuple patterns typecheck against fixed-arity tuple scrutinees and bind
   element variables in arm bodies. Runtime matching supports declared
   constructors, exact-length bracketed lists, cons-like lists, and fixed-arity
   tuples.
5. Arm bodies are full expressions; nested `case`, `if`, lambdas, block-valued
   scrutinees, and infix/operator expressions remain valid inside arm bodies.
6. Lowering preserves direct `case` expressions as `EPatternCase Expr [CaseArm]`.
7. The older `ECase Expr Expr Expr` form remains the internal boolean-branch
   representation used after `if` desugaring.
8. Tuple values and fixed-arity tuple case patterns are active core runtime
   features.
9. Lambda parameter patterns lower to ordinary unary lambdas whose bodies
   perform an internal single-arm `EPatternCase`, so parameter destructuring
   reuses the same binder, type, and runtime matching contract.

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
8. A cons-like list pattern matches a non-empty runtime list, recursively
   matching its head subpattern against the first element and its tail
   subpattern against the remaining list.
9. A tuple pattern matches a runtime tuple with exactly the same element count,
   then recursively matches element subpatterns.
10. Non-selected arm bodies are not evaluated.
11. A binder introduced by one arm is not visible in sibling arms or outside the
   `case` expression.

Examples:

```jz
flag = case n { | 0 -> True | _ -> False }.
copy = case value { | item -> item }.
maybeValue = case value { | Just item -> item | Nothing -> 0 }.
firstOrZero = case values { | [head, _] -> head | [] -> 0 }.
headPlusNext = case values { | [head | tail] -> head + hd tail | [] -> 0 }.
sumPair = case pair { | (left, right) -> left + right }.
sumPairFn = \((left, right)) -> left + right.
```

## Current Active Execution State

1. Parser, surface AST, and core AST now represent constructor, bracketed-list,
   and tuple patterns in `jazz-next`.
2. Analyzer/type/runtime execution is end-to-end for literal / wildcard /
   variable / constructor / exact-length bracketed-list / cons-like list /
   fixed-arity tuple patterns.
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
7. Cons-like list patterns typecheck against list scrutinees, bind the head
   subpattern at the list element type and the tail subpattern at the same list
   type, reject incompatible scrutinees with deterministic `E2011` diagnostics,
   and participate in ordinary arm-result agreement checks.
8. Tuple patterns typecheck against fixed-arity tuple scrutinees, bind element
   variables in arm bodies, reject incompatible scrutinees or arity mismatches
   with deterministic `E2011` diagnostics, and participate in ordinary
   arm-result agreement checks.
9. Runtime constructor/list/tuple pattern matching is first-match and recursive
   over nested subpatterns, including cons-like list head/tail matching.
10. If no arm matches at runtime, evaluation emits deterministic `E3022`
   diagnostics rather than falling through silently.

## Deferred Pattern Forms

No additional pattern forms are part of the end-to-end committed subset.

## Non-Goals (Milestone 1)

1. Pattern guards, as-patterns, or-patterns, and pattern synonyms.
2. Exhaustiveness analysis beyond deterministic first-match semantics.
3. Match-compilation optimizations or decision-tree lowering.
4. Any new parser/type/runtime behavior under `jazz-hs/` or `jazz2/`.
