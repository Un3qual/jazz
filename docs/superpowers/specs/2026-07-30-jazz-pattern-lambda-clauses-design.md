# Jazz Pattern-Lambda Clauses Design

## Status

Approved on `2026-07-30`.

This design corrects the multi-body decision in
[`2026-07-30-jazz-remove-function-equations-design.md`](2026-07-30-jazz-remove-function-equations-design.md).
Haskell-style named equations remain removed. Ordered function-head pattern
matching remains available through Jazz's historical `\|` case-lambda spelling.

## Goal

Let an ordinary Jazz binding assign a lambda with multiple ordered pattern
heads and distinct bodies, without repeating the binding name or reintroducing
Haskell-style function declarations.

## Canonical Syntax

An ordinary lambda keeps its current spelling:

```jazz
unwrap = \(Just item) -> item.
```

A lambda with ordered clauses uses `\|` for the first head and `|` for each
following head:

```jazz
choose =
  \|(Nothing, fallback) -> fallback
   |(Just item, _) -> item.
```

The form is an expression and remains first class:

```jazz
chooser =
  \|(Nothing, fallback) -> fallback
   |(Just item, _) -> item.

applyChooser = \(value) -> chooser value 0.
```

One or more clauses are accepted after `\|`. The ordinary `\(patterns) -> body`
form remains preferred when only one head is needed.

Named function equations remain invalid:

```jazz
choose Nothing fallback = fallback.
choose (Just item) _ = item.
```

## Relationship to Existing Pattern Syntax

Each clause head is a comma-separated parameter list using the same pattern
forms as ordinary pattern-lambda parameters. All clauses in one lambda must
have the same arity.

An or-pattern still means that several patterns share one body:

```jazz
payload = \(Just item | Also item) -> item.
```

A clause lambda means that heads have independently scoped bodies:

```jazz
payloadOr =
  \|(Just item) -> item
   |(Nothing) -> 0.
```

Clause patterns are attempted from top to bottom. The first matching clause is
selected. Binders belong only to that clause's body, so different clauses do
not need to bind the same names. If no clause matches, execution reports the
existing non-exhaustive pattern diagnostic `E3022`.

Clause guards, `where` clauses, nested/grouped or-patterns, and exhaustiveness
analysis are not added by this change. Existing `if`, blocks, and nested `case`
expressions remain available inside a clause body.

## Expression Boundaries

Whitespace and indentation are not semantic. Clause bodies may be written
inline or across lines:

```jazz
sign = \|(0) -> 0 |(value) -> if value < 0 then -1 else 1.
```

At the top level of a clause body, a `|` begins the next clause only when the
following tokens form a parenthesized parameter head followed by `->`.
Parenthesized, bracketed, and braced `|` tokens remain owned by their nested
expression or pattern. Ordinary pipe operators in clause bodies remain
expressions unless they form the complete next-head boundary.

The final clause body ends at the same enclosing expression boundary as an
ordinary lambda body, such as the binding's terminating dot, a closing
delimiter, or a caller-provided expression stop.

## Surface Representation

The surface AST gains an expression-level clause representation:

```text
SurfacePatternLambdaClause
  source span
  non-empty parameter-pattern list
  body expression

SEPatternLambda
  non-empty ordered clause list
```

The existing `SELambda` representation remains unchanged for ordinary
single-body lambdas. A multi-body lambda is not a statement and does not carry
or repeat a function name. The removed `SSFunction`, `SurfaceFunctionClause`,
and equation-specific generated-name paths remain removed.

The Jazz-authored parser schema mirrors this expression shape so hosted and
Jazz-authored parsing continue to compare the same canonical surface values.

## Lowering

Lowering checks the common clause arity and creates one generated lambda
argument for each parameter position using the existing
`LambdaPatternArgument` generated-name family.

For one parameter, the generated argument is the pattern-case scrutinee. For
multiple parameters, the generated arguments are assembled into a tuple
scrutinee. Each source clause becomes one ordered case arm: a one-parameter
head lowers directly to its pattern, while a multi-parameter head lowers to a
tuple pattern.

The resulting pattern case is wrapped in nested unary core lambdas in source
parameter order. No new core callable form is introduced.

Conceptually:

```jazz
choose =
  \|(Nothing, fallback) -> fallback
   |(Just item, _) -> item.
```

lowers to the existing core equivalent of:

```jazz
choose =
  \(first, second) ->
    case (first, second) {
      | (Nothing, fallback) -> fallback
      | (Just item, _) -> item
    }.
```

This preserves currying, partial application, recursive ordinary bindings,
pattern typing, ordered fallthrough, and `E3022` runtime behavior.

## Diagnostics

The parser reports deterministic errors for:

- `\|` without a following parenthesized head;
- a head without `->`;
- a missing body;
- mixed clause arity, pointing at the first mismatching head; and
- malformed patterns through the existing pattern diagnostic path.

No compatibility warning accepts or rewrites `name pattern = body.` Named
equations remain syntax errors.

## Authored-Source Policy

Authored `.jz` code uses clause lambdas when a function's entire definition is
ordered dispatch over its parameter patterns. In particular, the benchmark
functions migrated from named equations to immediate `case` dispatch are
migrated again to `\|` clauses.

Explicit `case` remains canonical when:

- the scrutinee is computed rather than the complete function-head parameter
  set;
- matching occurs after setup expressions;
- matching is nested inside another branch; or
- the code is genuinely inspecting a value rather than defining the function
  by ordered heads.

The repository feature inventory and editor fixture include a representative
multi-body pattern lambda. Syntax highlighting recognizes the `\|` introducer
as lambda syntax.

## Verification

Test-first implementation must prove:

- `\|` clause syntax fails before the parser implementation exists;
- single- and multi-parameter clause lambdas parse with exact ordered surface
  structure;
- clauses lower to curried lambdas around one ordered pattern case;
- constructor, list, tuple, literal, wildcard, and variable heads execute;
- clause order, recursion, partial application, and `E3022` behavior are
  preserved;
- mismatched arity and malformed heads receive deterministic diagnostics;
- ordinary lambdas and same-body lambda or-patterns remain unchanged;
- named Haskell-style equations remain rejected;
- hosted and Jazz-authored parser/core comparisons remain exact; and
- the complete authored corpus, docs, and repository audits pass.

## Out of Scope

- Restoring named function equations or any `name pattern = body.` form.
- Changing the core callable representation.
- Adding clause guards, `where` clauses, pattern synonyms, or exhaustiveness
  analysis.
- Making indentation semantic.
- Modifying the read-only `jazz-hs/` or `jazz2/` reference implementations.
- Advancing typed-core, backend, or bootstrap milestones.
