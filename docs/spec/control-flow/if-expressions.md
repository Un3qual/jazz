# If Expressions

Status: active
Locked decisions: 2026-03-03; required `then` boundary: 2026-07-12
Primary plan: `docs/plans/spec-clarification/2026-03-03/control-flow/14-if-expression-surface-and-semantics.md`

## Purpose

Define `if` as a canonical surface construct with explicit parsing, typing, evaluation, and desugaring semantics.

## Implementation Target

- New implementation work for this contract lands in `jazz-next/`.
- `jazz-hs/` and `jazz2/` remain read-only legacy evidence.

## Canonical Surface Form

Canonical expression form:

```jz
if <condition> then <thenExpr> else <elseExpr>
```

Key parsing rules:

1. `if`, `then`, and `else` are keywords and are not valid as identifiers.
2. `condition`, `thenExpr`, and `elseExpr` are full expressions.
3. `then` and `else` are mandatory.
4. Nested `if` expressions associate with the nearest `else`.
5. `if` parses into `EIf` before desugaring.

## Precedence and Grouping

1. `if` is expression-level control flow, not a statement form.
2. The condition is parsed as a full expression up to the `then` keyword.
3. Branch expressions each parse as full expressions.
4. Parentheses are required when disambiguation is needed with surrounding infix/application structure.

## Canonical Desugaring

`if` is lowered in a dedicated control-flow desugar pass:

```jz
if cond then thenExpr else elseExpr
```

becomes

```jz
case cond {
  | True -> thenExpr
  | False -> elseExpr
}
```

Desugaring invariants:

1. The desugar pass preserves source spans for diagnostics.
2. Analyzer/typechecker behavior is defined on desugared forms.
3. Backend/runtime implementations must behave as if this desugaring happened.

## Typing Rules

1. `condition` must typecheck to `Bool`.
2. `thenExpr` and `elseExpr` must unify to one result type.
3. If condition type is not `Bool`, emit a compile-time type error.
4. If branch types do not unify, emit a compile-time type error that reports both branch types.

## Evaluation Semantics

1. Evaluate `condition` first.
2. If condition evaluates to `True`, evaluate only `thenExpr`.
3. If condition evaluates to `False`, evaluate only `elseExpr`.
4. The non-selected branch is not evaluated.

`if` is therefore syntax rather than an ordinary Jazz function. In Jazz's
strict application model, a normal three-argument function would receive
already-evaluated branch arguments and could not preserve rule 4. The surface
construct lowers to existing boolean pattern control flow, so it does not
require a second runtime conditional primitive.

## Valid Examples

```jz
if isReady then value else fallback
```

```jz
if (x > 0) then x else (0 - x)
```

```jz
if cond then (if inner then a else b) else c
```

## Invalid Examples

```jz
if cond then x
```

Reason: missing `else` branch.

```jz
if cond then x else y else z
```

Reason: extra `else` branch.

```jz
if 1 then x else y
```

Reason: condition is not `Bool`.

## Legacy Drift Notes

Legacy `jazz-hs` has an `EIf` AST constructor and codegen branch but no parser/analyzer path. This document is the canonical source for converging implementation and tests in `jazz-next`.
