# Pattern Matching Semantics

Status: active (Milestone 1 contract lock; parser/core `case` subset is landed in `jazz-next`, type/runtime execution remains staged)
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
3. The currently landed surface/core pattern set is limited to:
   - integer literals
   - boolean literals
   - wildcard `_`
   - variable binders such as `item`
4. Arm bodies are full expressions; nested `case`, `if`, lambdas, block-valued
   scrutinees, and infix/operator expressions remain valid inside arm bodies.
5. Lowering preserves direct `case` expressions as `EPatternCase Expr [CaseArm]`.
6. The older `ECase Expr Expr Expr` form remains the internal boolean-branch
   representation used after `if` desugaring.

## Matching Contract For The Committed Simple Subset

1. Arms are tested from top to bottom.
2. The first matching arm wins.
3. Literal patterns match when the scrutinee value equals the literal and the
   literal belongs to the supported simple subset (`Int` or `Bool`).
4. `_` matches any value and binds no name.
5. A variable pattern matches any value and binds the scrutinee to that name
   only within the selected arm body.
6. Non-selected arm bodies are not evaluated.
7. A binder introduced by one arm is not visible in sibling arms or outside the
   `case` expression.

Examples:

```jz
flag = case n { | 0 -> True | _ -> False }.
copy = case value { | item -> item }.
```

## Current Implementation Gap

1. Parser, surface AST, core AST, and lowering already implement this simple
   subset in `jazz-next`.
2. Until `JN-ADT-SIMPLE-SEM-001` lands, analyzer/type flows still emit the
   deterministic `E2011` placeholder for direct `EPatternCase` typing.
3. Until `JN-ADT-SIMPLE-SEM-001` lands, runtime evaluation still emits the
   deterministic `E3022` placeholder if execution reaches direct pattern
   matching.
4. Those placeholders are a temporary implementation gap, not the long-term
   contract for the committed simple subset.

## Deferred Pattern Forms

The following remain explicitly out of scope for the committed simple subset:

1. Constructor patterns.
2. List patterns, including cons-like forms.
3. Tuple patterns.
4. Lambda-parameter patterns.

## Non-Goals (Milestone 1)

1. Pattern guards, as-patterns, or-patterns, and pattern synonyms.
2. Exhaustiveness analysis beyond deterministic first-match semantics.
3. Match-compilation optimizations or decision-tree lowering.
4. Any new parser/type/runtime behavior under `jazz-hs/` or `jazz2/`.
