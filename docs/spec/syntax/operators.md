# Operators and Sections

Status: active (phase 0 contract lock; Stage 2 fixed-tier contract lock)
Locked decisions: 2026-03-03; Stage 2 contract locked 2026-06-04
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

Bare operator values are first-class:

```jz
(+)
(/)
(==)
```

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

1. Parser output for sections and bare operator values is deterministic and synthetic-name free.
2. Synthetic internal names are allowed only in lowering artifacts, never in parser-visible AST contracts.
3. External tests assert operator/section node shape, not generated variable names.

## Section Evaluation Contract

Section forms denote unary function values with deterministic argument order:

1. `(expr <op>) arg` is semantically equivalent to `<op> expr arg`.
2. `(<op> expr) arg` is semantically equivalent to `<op> arg expr`.
3. Bare operator values behave like ordinary curried functions:
   - `(+) a b` is semantically equivalent to `a + b`.
   - `((+) a) b` is semantically equivalent to `a + b`.
4. Right sections remain distinct from ordinary partial application for non-commutative operators:
   - `(/ 2)` is semantically equivalent to `\x -> x / 2`.
   - `((/) 2)` is semantically equivalent to `\x -> 2 / x`.
5. Implementations may realize these forms either by explicit lambda lowering or by runtime-native callable values, but externally observable behavior must match these equations.
6. Type and runtime diagnostics must remain deterministic for invalid operand combinations.

## Invalid and Restricted Forms

1. Empty or malformed section syntax is invalid.
2. Ambiguous tokenizations that do not match canonical section forms are invalid.
3. Operator forms outside the v1 frozen set are invalid unless explicitly enabled by a staged extension phase.

## Staged Extensibility Model

Stage 1 (current):

1. Frozen built-in operators.
2. Canonical section AST + executable section semantics.

Stage 2:

1. Controlled user-defined operator declarations.
2. Restricted character set and fixed precedence tiers.
3. No custom precedence, no custom associativity, no runtime overload dispatch,
   and no new built-in operators.

Stage 3 (optional, only if needed):

1. Custom precedence declarations.
2. Additional validation to prevent ambiguity regressions.

## Stage 2 Fixed-Tier User Operators

Stage 2 introduces declaration syntax only for parser-visible user-defined
operator symbols:

```jz
operator <symbol> tier <1-5>.
```

Grammar:

```ebnf
operator-declaration ::= "operator" operator-symbol "tier" operator-tier "."
operator-tier        ::= "1" | "2" | "3" | "4" | "5"
```

Examples:

```jz
operator %% tier 2.
operator <? tier 4.
operator >> tier 5.
```

The declaration is a top-level source declaration. It must appear before any
use of the symbol as an infix operator, bare operator value, or operator
section.

### Tier Mapping

User-defined operators choose one of the existing fixed tiers and inherit that
tier's associativity:

| Declared tier | Built-in tier anchor | Inherited associativity |
| --- | --- | --- |
| `1` | `*`, `/` | Left |
| `2` | `+`, `-` | Left |
| `3` | `\|` | Left |
| `4` | `==`, `!=`, `<`, `<=`, `>=`, `>` | Left |
| `5` | `$` | Right |

Stage 2 does not provide syntax for custom numeric precedence or custom
associativity. For example, `operator %% precedence 20.` and
`operator %% tier 2 right.` are invalid.

### Allowed Symbols

A Stage 2 user-defined operator symbol is a non-empty ASCII token containing
only these characters:

```text
! % & * + - / < > ? ^ | ~
```

The symbol must not be any built-in operator or reserved grammar token. The
following exact tokens are reserved and invalid as user-defined operator
symbols even if some of their characters appear in the allowed set:

- Built-in operators: `*`, `/`, `+`, `-`, `|`, `==`, `!=`, `<`, `<=`, `>=`,
  `>`, `$`.
- Grammar tokens: `.`, `$`, `::`, `->`, `=>`, `=`, `{`, `}`, `(`, `)`, `[`,
  `]`, `,`, `;`, `\`, single quote, and double quote.
- Comment-form tokens reserved by the lexer: `//`, `/*`, `*/`, and `--`.

Characters outside the allowed set are invalid, including `:`, `.`, `$`, `_`,
letters, digits, whitespace, braces, parentheses, brackets, comma, semicolon,
backslash, and quote characters.

### Scope and Visibility

Stage 2 declarations are source-unit local:

1. A declaration is visible only from its declaration point through the rest of
   the same source unit.
2. A declaration is not imported, exported, re-exported, or made visible to
   dependency modules.
3. Imported modules may use their own declared operators internally, but callers
   must declare any user operator symbols they use in their own source unit.
4. Duplicate declarations of the same symbol in one source unit are invalid,
   even when the duplicate repeats the same tier.
5. Operator declarations are not allowed inside expressions, blocks, classes,
   impls, lambdas, pattern arms, or any other nested scope.

### Invalid Stage 2 Cases

These forms are invalid:

```jz
operator == tier 4.       // built-in operator
operator -> tier 5.       // reserved grammar token
operator .. tier 1.       // reserved character
operator abc tier 2.      // letters are not operator characters
operator %% tier 6.       // tier outside 1-5
operator %% precedence 2. // custom precedence is out of scope
operator %% tier 2 left.  // custom associativity is out of scope
```

Using a user operator before its declaration is invalid:

```jz
value = a %% b.
operator %% tier 2.
```

### Out of Scope for Stage 2

The Stage 2 declaration is parser/fixity metadata. It does not define a runtime
function, add a kernel primitive, introduce runtime overload dispatch, or add
new built-in operators. Any executable semantics for declared user operators
must be specified and implemented by a separate later child.

## Compatibility and Drift Prevention

1. Any operator-table change requires same-change updates to this spec and parser tests.
2. Any section semantic change requires same-change updates to this spec and section-contract tests.
3. Governance maintenance checklist rules in `docs/spec/governance/spec-authority-policy.md` apply.

## Legacy Drift Notes

Legacy `jazz-hs` currently leaks a synthetic lambda parameter (`__partialInfixLambdaParam0`) in section parsing. This document locks the target behavior for `jazz-next` convergence.
