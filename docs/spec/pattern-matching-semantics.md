# Pattern Matching Semantics

Status: active (literal, wildcard, variable, constructor, exact-length bracketed-list, cons-like list, fixed-arity tuple, as-patterns, top-level case-arm or-patterns, top-level lambda-parameter or-patterns, ordered multi-body pattern-lambda clauses, and single `if` case-arm guards parse/lower, typecheck, and execute end-to-end in `jazz-next`; Haskell-style function equations, lambda-parameter guards, and nested/grouped or-patterns are out of scope)
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
  | <pattern> [if <guard-expr>] -> <expr>
  | <pattern> -> <expr>
}
```

Pattern-shaped function heads use ordinary bindings to lambdas:

```jz
first = \([item | _]) -> item.
choose = \(Just item | Also item, fallback) -> item.
```

Alternatives with different bodies use ordered pattern-lambda clauses:

```jz
length =
  \|([]) -> 0
   |([_ | rest]) -> 1 + length rest.
```

Every clause has the same source arity. The clauses are attempted in source
order, and each clause's binders are visible only in that clause body.
Explicit `case` remains canonical for computed scrutinees, matching after
setup work, and nested value inspection.

Current parser/core invariants:

1. A `case` expression has one scrutinee expression and one or more arms.
2. Every arm begins with `|`, may use at most one `if <guard-expr>` between
   pattern and body arrow, and uses `->` before the body.
3. The currently landed surface/core pattern set includes:
   - integer literals
   - boolean literals
   - wildcard `_`
   - variable binders such as `item`
   - uppercase constructor patterns such as `Just item` or `Nothing`
   - bracketed list patterns such as `[head, _]` or `[]`
   - cons-like list patterns such as `[head | tail]`
   - the zero-element tuple / Unit pattern `()`
   - tuple patterns such as `(left, right)` or `(1, flag)`
   - as-patterns such as `whole @ Just item`
   - top-level case-arm and lambda-parameter or-patterns such as
     `Just item | Also item`
4. Constructor/list/tuple/as/or-patterns are preserved structurally in `EPatternCase`.
   Declared constructor patterns typecheck against ADT scrutinees and bind
   payload variables in arm bodies; bracketed-list patterns typecheck against
   list scrutinees and bind element variables in arm bodies; cons-like list
   patterns typecheck against non-empty list deconstruction with the head
   subpattern at the element type and the tail subpattern at the same list type;
   tuple patterns typecheck against fixed-arity tuple scrutinees and bind
   element variables in arm bodies; the zero-element tuple pattern `()`
   matches only the Unit value `()` and binds no names; as-patterns typecheck
   their inner pattern against the scrutinee and bind the whole scrutinee value
   at the scrutinee type after the inner pattern succeeds; or-patterns
   typecheck every alternative against the same scrutinee, require the same binder names in
   every alternative, and expose only compatible common binders to the arm
   guard and body. Runtime matching supports declared constructors,
   exact-length bracketed lists, cons-like lists, fixed-arity tuples,
   as-patterns, and left-to-right or-pattern alternatives.
5. Arm guards and bodies are full expressions; nested `case`, `if`, lambdas,
   block-valued scrutinees, and infix/operator expressions remain valid inside
   arm bodies. Guards run under the pattern binder scope but introduce no
   binders.
6. Lowering preserves direct `case` expressions as `EPatternCase Expr [CaseArm]`.
7. The older `ECase Expr Expr Expr` form remains the internal boolean-branch
   representation used after `if` desugaring.
8. Tuple values and fixed-arity tuple case patterns are active core runtime
   features, including `()` as the zero-element Unit value and pattern.
9. Ordinary lambda parameter patterns lower to unary lambdas whose bodies
   perform an internal single-arm `EPatternCase`, so parameter destructuring
   reuses the same binder, type, runtime matching, and no-match diagnostic
   contract as `case` patterns. Top-level lambda-parameter or-patterns lower
   to `POr` inside that internal single-arm case; lambda guards remain
   rejected.
   The shorthand `\()` lowers through this rule as one `PTuple []` parameter,
   equivalent to `\(())`; it is not a nullary lambda.
   Ordered `\|` clauses lower to the same core machinery: generated unary
   lambda arguments wrap one ordered `EPatternCase`. A one-parameter clause
   uses its pattern directly; a multi-parameter clause uses a tuple scrutinee
   and tuple pattern. Currying and partial application are preserved.
10. Pattern guards are optional case-arm expressions introduced by `if`.
    They are stored on `CaseArm`, typecheck as `Bool` under pattern binders,
    and do not participate in arm-result agreement.
11. Functions are ordinary value bindings whose values are lambdas.
    Pattern-shaped lambda parameters, multiple parameters, and top-level
    lambda-parameter or-patterns are active. Ordered alternatives with
    distinct bodies use `\|(patterns) -> body |(patterns) -> body`.
    Haskell-style `name pattern... = body.` equations are rejected.

## Matching Contract For The Committed Runtime Subset

1. Arms are tested from top to bottom.
2. The first arm whose pattern matches and whose guard is absent or evaluates
   to `True` wins.
3. Literal patterns match when the scrutinee value equals the literal and the
   literal belongs to the supported simple subset (`Int` or `Bool`). Fractional
   literal patterns remain rejected until a dedicated pattern-matching batch
   wires the implemented floating equality semantics into this subsystem.
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
   The zero-element case therefore matches only `()` and binds no names.
10. An as-pattern `name @ pattern` delegates to the inner pattern first, then
    binds `name` to the whole scrutinee value only when the inner pattern
    succeeds.
11. An or-pattern inside one case arm or pattern-shaped lambda parameter tries
    alternatives from left to right. The first successful alternative supplies
    the pattern bindings. If no alternative matches, the whole pattern fails
    and matching proceeds to the next arm or, for the internal lambda
    single-arm case, emits the existing no-match diagnostic.
12. When a pattern matches, an absent guard selects the arm; a `True` guard
   selects the arm; a `False` guard falls through to later arms.
13. Guards for failed patterns and non-selected arm bodies are not evaluated.
14. A binder introduced by one arm is not visible in sibling arms or outside the
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
choose = \(Just item | Also item) -> item.
chooseBody =
  \|(Nothing, fallback) -> fallback
   |(Just item, _) -> item.
sameValue = case value { | whole @ Just item -> whole | _ -> value }.
positive = case value {
  | Just item if item > 0 -> item
  | _ -> 0
}.
positiveAlt = case value {
  | Just item | Also item if item > 0 -> item
  | Nothing -> 0
}.
```

## Current Active Execution State

1. Parser, surface AST, and core AST now represent constructor, bracketed-list,
   tuple, and top-level or-patterns in `jazz-next` case arms and
   pattern-shaped lambda parameters.
2. Analyzer/type/runtime execution is end-to-end for literal / wildcard /
   variable / constructor / exact-length bracketed-list / cons-like list /
   fixed-arity tuple / as-patterns / top-level case-arm and lambda-parameter
   or-patterns / single
   guarded case arms.
3. Pattern-shaped lambda parameters lower to internal single-arm pattern cases
   and reuse the same binder, type, runtime matching, and no-match diagnostic
   behavior, including top-level `POr` alternatives. Ordered pattern-lambda
   clauses lower to curried generated arguments around one ordered pattern
   case and reuse the same `E3022` no-match behavior.
4. Declared constructor patterns typecheck against the scrutinee ADT type,
   bind payload variables in arm bodies, reject unknown constructor names or
   arity mismatches with deterministic `E2011` diagnostics, and participate
   in ordinary arm-result agreement checks.
5. Literal patterns must agree with the scrutinee type; incompatible literal
   patterns produce compile-time `E2011` diagnostics.
6. All arm bodies must agree on one result type; mismatched arm result types
   produce compile-time `E2012` diagnostics.
7. Bracketed-list patterns typecheck against list scrutinees, bind element
   variables in arm bodies, reject incompatible scrutinees with deterministic
   `E2011` diagnostics, and participate in ordinary arm-result agreement
   checks.
8. Cons-like list patterns typecheck against list scrutinees, bind the head
   subpattern at the list element type and the tail subpattern at the same list
   type, reject incompatible scrutinees with deterministic `E2011` diagnostics,
   and participate in ordinary arm-result agreement checks.
9. Tuple patterns typecheck against fixed-arity tuple scrutinees, bind element
   variables in arm bodies, reject incompatible scrutinees or arity mismatches
   with deterministic `E2011` diagnostics, and participate in ordinary
   arm-result agreement checks.
10. Runtime constructor/list/tuple pattern matching is first-match and recursive
   over nested subpatterns, including cons-like list head/tail matching and
   as-pattern whole-value binding after inner-pattern success.
11. As-pattern binders receive the scrutinee type, inner binders keep the
   existing nested-pattern type rules, and duplicate binders in one pattern
   tree reject with deterministic `E2011` diagnostics.
12. Guard expressions typecheck as `Bool` in the pattern-extended arm
   environment, pattern binders are visible to guards, and guard types do not
   affect arm body result agreement.
13. Runtime evaluates guards only after pattern success; `False` falls through
   to later arms; failed patterns skip guard evaluation.
14. If no arm is selected at runtime because all patterns fail or matching
   guards are `False`, evaluation emits deterministic `E3022` diagnostics
   rather than falling through silently.

## Active Or-Pattern Contract

Or-patterns are a single pattern form, not multiple arms or parameters. In
case arms they stay inside one arm:

```jz
case value {
  | Just item | Also item if item > 0 -> item
  | Nothing -> 0
}
```

Pattern-shaped lambda parameters may use the same top-level alternative shape:

```jz
choose = \(Just item | Also item) -> item.
```

Surface rules:

- The first `|` starts the case arm.
- Later top-level `|` tokens before the optional guard or `->` separate
  alternatives in the same arm.
- In a lambda parameter list, top-level `|` tokens before the parameter
  delimiter separate alternatives for that one pattern parameter.
- Each alternative is one currently accepted non-or case pattern: literal,
  wildcard, variable, constructor, exact-length list, cons-like list, tuple, or
  as-pattern.
- `[head | tail]` remains the cons-like list pattern form, and pipe operators
  in arm bodies remain expression operators.
- Or-patterns are not accepted inside constructor/list/tuple/as-pattern
  subpatterns. Lambda parameters accept only the top-level alternative form,
  with no lambda guards.

Binder and type rules:

- Every alternative typechecks against the same scrutinee type.
- Every alternative must bind exactly the same set of names. Binder-set
  mismatches reject with deterministic `E2011`.
- Duplicate binders inside one alternative keep the existing duplicate case
  pattern binder `E2011`; the same binder name may appear in separate
  alternatives.
- For each common binder, the inferred binder types from all alternatives must
  unify. Incompatible common binder types reject with deterministic `E2011`
  text naming the binder.
- Guards and bodies see only the compatible common binders.
- Arm result agreement stays body-owned through the existing `E2012` path.

Runtime rules:

- Alternatives are tried left-to-right.
- The first matching alternative supplies bindings for the arm guard and body.
- If no alternative matches, the pattern fails and runtime continues with the
  next case arm.
- A matching alternative with a `False` guard falls through to the next arm.
- If no arm is selected after pattern and guard checks, runtime emits the
  existing no-match diagnostic `E3022`.
- In a lambda parameter, the internal single-arm case reuses the same
  left-to-right matching and emits `E3022` when no alternative matches.

## Active Pattern Guard Contract

Pattern guards are not a new pattern node; they are optional boolean
expressions attached to a case arm after the pattern and before `->`:

```jz
case value {
  | Just item if item > 0 -> item
  | _ -> 0
}
```

Surface rules:

- A guarded arm has the shape `| <pattern> if <guard-expr> -> <body-expr>`.
- The `if` token is the guard introducer and reuses the existing keyword.
- Unguarded arms keep the existing `| <pattern> -> <body-expr>` form.
- At most one guard expression is accepted per arm.

Binder and type rules:

- Pattern matching and pattern typechecking run before the guard expression.
- Binders introduced by the pattern are visible to the guard expression and the
  selected arm body.
- Guard expressions introduce no binders and do not change duplicate-binder
  checks.
- The guard expression must typecheck as `Bool` in the pattern-extended arm
  environment.
- Guard expressions do not participate in arm result agreement; `E2012` remains
  body-result ownership.

Runtime rules:

- Arms are tested from top to bottom.
- Runtime matches the pattern first.
- If the pattern fails, the guard and body are not evaluated.
- If the pattern matches, runtime evaluates the guard in the environment
  containing pattern binders.
- `True` selects the arm and evaluates the body.
- `False` falls through to the next arm.
- If no arm is selected after pattern and guard checks, runtime emits the
  existing no-match diagnostic `E3022`.
- Runtime errors from guard evaluation are fatal only for guards that are
  actually evaluated.

Diagnostics:

- Malformed guard syntax is parser-owned and points at the guard introducer or
  malformed guard expression.
- A non-`Bool` guard is a compile-time type diagnostic at the guard expression
  span, using guard-specific text in the existing boolean-condition diagnostic
  family.
- Pattern mismatch, unknown constructor, arity mismatch, duplicate binder, and
  arm-result mismatch diagnostics keep their existing ownership and codes.

## Deferred Pattern Forms

Pattern synonyms, nested/grouped or-patterns, lambda-parameter guards, and
multiple guard clauses remain blocked until separate active-path contracts
define parser shape, binder scope, type rules, runtime matching behavior,
diagnostics, target paths, and focused verification.

## Non-Goals

1. Pattern synonyms, nested/grouped or-patterns, lambda-parameter guards, and
   multiple guard clauses per arm.
2. Exhaustiveness analysis beyond deterministic first-match semantics.
3. Match-compilation optimizations or decision-tree lowering.
4. Any new parser/type/runtime behavior under `jazz-hs/` or `jazz2/`.
