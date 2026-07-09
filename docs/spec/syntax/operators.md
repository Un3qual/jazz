# Operators and Sections

Status: active (phase 0 contract lock; Stage 2 fixed-tier/executable binding contract lock; custom precedence/associativity contract lock)
Locked decisions: 2026-03-03; Stage 2 fixed-tier contract locked 2026-06-04; Stage 2 executable binding contract locked 2026-06-27; custom precedence locked 2026-07-08; custom associativity locked 2026-07-08
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
3. Same-source executable bindings through ordinary function values.
4. Adjacent operator-specific signatures for executable user operators.

Stage 3:

1. Custom precedence declarations.
2. Optional `left`, `right`, and `nonassoc` associativity declarations.
3. No runtime overload dispatch, cross-module operator APIs, or new built-in
   operators.

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

Fixed-tier declarations may optionally override the inherited associativity
with the custom associativity syntax described below.

### Custom Numeric Precedence User Operators

Custom numeric precedence declarations use the same source-unit-local operator
symbol rules as fixed-tier declarations:

```jz
operator <symbol> precedence <1-99>.
```

Grammar:

```ebnf
operator-declaration ::= "operator" operator-symbol "precedence" operator-precedence "."
operator-precedence  ::= integer in the inclusive range 1..99
```

Examples:

```jz
operator %% precedence 25.
operator <+> precedence 99.
```

Rules:

1. Higher precedence numbers bind tighter.
2. Built-in tier anchors keep their current relative ordering.
3. A custom-precedence operator without explicit associativity defaults to left
   associativity.
4. Existing `operator <symbol> tier <1-5>.` declarations remain valid.
5. Duplicate declarations, built-in symbols, reserved symbols, and invalid
   operator characters are rejected under the same rules as fixed-tier
   declarations.

### Custom Associativity

Both fixed-tier and custom-precedence declarations may include one optional
associativity keyword:

```jz
operator <symbol> tier <1-5> <associativity>.
operator <symbol> precedence <1-99> <associativity>.
```

Grammar:

```ebnf
operator-associativity ::= "left" | "right" | "nonassoc"
```

Examples:

```jz
operator %% tier 2 left.
operator <| precedence 10 right.
operator ?> tier 4 nonassoc.
```

Rules:

1. `left` groups same-precedence chains to the left.
2. `right` groups same-precedence chains to the right.
3. `nonassoc` rejects unparenthesized chains at the same precedence because
   grouping must be explicit.
4. Omitted associativity keeps the inherited tier associativity for `tier`
   declarations and defaults to left associativity for `precedence`
   declarations.
5. Unknown associativity keywords are invalid.

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
   even when the duplicate repeats the same tier or precedence.
5. Operator declarations are not allowed inside expressions, blocks, classes,
   impls, lambdas, pattern arms, or any other nested scope.

### Executable Operator Bindings

Operator declarations publish fixity metadata only. A declared user operator
becomes executable only when the same source unit provides an ordinary binding
for the parenthesized operator name:

```jz
operator %% tier 2.
(%%) = \(left) -> \(right) -> left + right.
result = 1 %% 2.
```

The binding form is:

```ebnf
operator-binding ::= "(" operator-symbol ")" "=" expression "."
```

An operator binding may have an immediately adjacent ordinary type signature
for the same parenthesized operator name:

```jz
operator %% tier 2.
(%%) :: Int -> Int -> Int.
(%%) = \(left) -> \(right) -> left + right.
result = 1 %% 2.
```

The signature form is:

```ebnf
operator-signature ::= "(" operator-symbol ")" "::" signature-type "."
```

Rules:

1. The operator symbol must be a user operator already declared earlier in the
   same source unit.
2. Built-in operators cannot be rebound with operator-binding syntax.
3. Operator bindings are allowed only at file scope or directly in module
   bodies, matching operator declarations. They are invalid inside expression
   blocks, classes, impls, lambdas, pattern arms, or other nested scopes.
4. Operator bindings are not imported, exported, re-exported, or made visible
   to dependency modules.
5. The right-hand side is an ordinary Jazz expression. It is type-checked as an
   ordinary value and must be callable at each operator use site.
6. A declared user operator used without an executable binding is a compile
   error: `operator '<symbol>' has no executable binding`.
7. An operator signature must name a user operator already declared earlier in
   the same source unit.
8. Built-in operators cannot receive operator signatures.
9. Operator signatures are allowed only at file scope or directly in module
   bodies, matching operator declarations and bindings. They are invalid inside
   expression blocks, classes, impls, lambdas, pattern arms, or other nested
   scopes.
10. An operator signature applies only when it immediately precedes the matching
   operator binding. It constrains the hidden ordinary binding type.

Executable equivalences:

1. `left %% right` is equivalent to `((%%) left) right`.
2. `(left %%)` is equivalent to `((%%) left)`.
3. `(%% right)` is equivalent to `\left -> ((%%) left) right`, so right
   sections preserve argument order for non-commutative functions.
4. `(%%)` is the ordinary callable value bound by `(%%) = <expression>.`

Executable operator bindings do not introduce implicit overload resolution,
dictionaries, typeclass solver behavior, new built-ins, operator imports or
exports, or runtime overload dispatch.

### Invalid Stage 2 Cases

These forms are invalid:

```jz
operator == tier 4.       // built-in operator
operator -> tier 5.       // reserved grammar token
operator .. tier 1.       // reserved character
operator abc tier 2.      // letters are not operator characters
operator %% tier 6.       // tier outside 1-5
operator %% precedence 0. // precedence outside 1-99
operator %% precedence 100. // precedence outside 1-99
operator %% tier 2 sideways. // invalid associativity keyword
operator ?> precedence 10 nonassoc.
value = 1 ?> 2 ?> 3.      // non-associative chain needs parentheses
```

Using a user operator before its declaration is invalid:

```jz
value = a %% b.
operator %% tier 2.
```

### Out of Scope for Stage 2

Stage 2 executable bindings remain ordinary source-local bindings. They do not
add kernel primitives, operator imports or exports, runtime overload dispatch,
cross-module operator APIs, non-adjacent operator signatures, or new built-in
operators.

## Compatibility and Drift Prevention

1. Any operator-table change requires same-change updates to this spec and parser tests.
2. Any section semantic change requires same-change updates to this spec and section-contract tests.
3. Governance maintenance checklist rules in `docs/spec/governance/spec-authority-policy.md` apply.

## Legacy Drift Notes

Legacy `jazz-hs` currently leaks a synthetic lambda parameter (`__partialInfixLambdaParam0`) in section parsing. This document locks the target behavior for `jazz-next` convergence.
