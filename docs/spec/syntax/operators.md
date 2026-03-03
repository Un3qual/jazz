# Operators and Sections

Status: active (phase 0 contract lock)
Locked decisions: 2026-03-03
Primary plan: `docs/plans/spec-clarification/2026-03-03/syntax/15-operator-fixity-and-sections.md`

## Purpose

Define one canonical, testable operator model for fixity, precedence, associativity, and section behavior.

## Implementation Target

- New parser/analyzer/desugar work for this contract lands in `jazz-next/`.
- `jazz-hs/` and `jazz2/` stay read-only legacy evidence.

## Built-in Operator Table (v1 Frozen Set)

The built-in operator set is frozen for v1 and is parsed with the following precedence tiers (highest to lowest):

| Tier | Operators | Associativity |
| --- | --- | --- |
| 1 | `*`, `/` | Left |
| 2 | `+`, `-` | Left |
| 3 | `\|` | Left |
| 4 | `==`, `!=`, `<`, `<=`, `>=`, `>` | Left |
| 5 | `$` | Right |

Notes:

1. `$` is low-precedence right-associative application.
2. `.` is a program-level terminator and is not part of the infix expression table.
3. The parser must consume operator behavior from a centralized operator metadata table (not ad hoc branches).

## Canonical Section Semantics

Canonical section forms:

1. Left section: `(expr <op>)`
2. Right section: `(<op> expr)`

Examples:

```jz
(10 +)
(+ 10)
(* 2)
```

## Section AST Contract

Sections are represented explicitly in the AST contract (for example `ESectionLeft` and `ESectionRight`), not by leaking synthetic lambda parameter names.

AST-level invariants:

1. Parser output for sections is deterministic and synthetic-name free.
2. Synthetic internal names are allowed only in lowering artifacts, never in parser-visible AST contracts.
3. External tests assert section-node shape, not generated variable names.

## Section Lowering Rules

Lowering occurs in a desugar phase:

1. `(expr <op>)` lowers to `\(x) -> (<op> expr x)`.
2. `(<op> expr)` lowers to `\(x) -> (<op> x expr)`.
3. Lowering must preserve source spans for diagnostics.

## Invalid and Restricted Forms

1. Empty or malformed section syntax is invalid.
2. Ambiguous tokenizations that do not match canonical section forms are invalid.
3. Operator forms outside the v1 frozen set are invalid unless explicitly enabled by a staged extension phase.

## Staged Extensibility Model

Stage 1 (current):

1. Frozen built-in operators.
2. Canonical section AST + lowering.

Stage 2:

1. Controlled user-defined operator declarations.
2. Restricted character set and fixed precedence tiers.

Stage 3 (optional, only if needed):

1. Custom precedence declarations.
2. Additional validation to prevent ambiguity regressions.

## Compatibility and Drift Prevention

1. Any operator-table change requires same-change updates to this spec and parser tests.
2. Any section semantic change requires same-change updates to this spec and section-contract tests.
3. Governance maintenance checklist rules in `docs/spec/governance/spec-authority-policy.md` apply.

## Legacy Drift Notes

Legacy `jazz-hs` currently leaks a synthetic lambda parameter (`__partialInfixLambdaParam0`) in section parsing. This document locks the target behavior for `jazz-next` convergence.
