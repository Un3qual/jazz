# Jazz Function-Equation Removal Design

## Status

Implemented on `2026-07-30`, then superseded in part on `2026-07-30`.

The equation-removal boundary remains active, but this document's requirement
that lambdas have only one body is superseded by
[`2026-07-30-jazz-pattern-lambda-clauses-design.md`](2026-07-30-jazz-pattern-lambda-clauses-design.md).
Jazz retains ordered multi-body function-head matching through the historical
`\|` pattern-lambda clause syntax.

The language boundary was approved by the user's clarification:

> Get rid of the Haskell-style equations, but retain multiple function-head
> pattern matching with the old syntax.

## Goal

Remove Haskell-style function-equation declarations from Jazz while preserving
the pattern-shaped lambda parameters that Jazz supported before equations were
added.

## Language Boundary

This syntax is removed:

```jazz
length [] = 0.
length [_ | rest] = 1 + length rest.
```

The existing ordinary-binding and pattern-lambda syntax remains:

```jazz
head = \([item | _]) -> item.

samePayload =
  \(Just item | Also item, fallback) ->
    item.

combine =
  \(left, right) ->
    left + right.
```

Pattern-lambda parameters may contain multiple function parameters. Existing
top-level or-pattern alternatives also remain, subject to their existing rule
that every alternative binds the same names. Ordered heads with different
bodies use the `\|` clause-lambda syntax defined by the superseding design:

```jazz
length =
  \|([]) -> 0
   |([_ | rest]) -> 1 + length rest.
```

Multiple arguments can be matched independently in every clause head:

```jazz
zip =
  \|([], _) -> []
   |(_, []) -> []
   |([leftHead | leftTail], [rightHead | rightTail]) ->
      [(leftHead, rightHead) | zip leftTail rightTail].
```

Ordinary `case` expressions and all existing pattern forms remain unchanged.

## Compiler Design

Remove the equation-only representation and processing from both frontend
implementations:

- `SurfaceFunctionClause` and `SSFunction`;
- equation lookahead, grouping, clause parsing, and head-pattern parsing;
- function-equation lowering and generated equation-argument names;
- equation-specific forcing, resolution, scope, canonical-comparison, and
  failure paths; and
- the equivalent Jazz-authored parser schema and canonical-core lowering.

Do not leave unreachable equation AST nodes as compatibility scaffolding. Jazz
is pre-bootstrap and has no released source-compatibility requirement for this
syntax.

The existing `SurfaceLambdaPattern` path remains active for ordinary
single-body lambdas. The superseding clause-lambda design adds a distinct
expression-level surface node and lowers it to ordinary lambdas around one
ordered pattern case, so no analyzer, inference, or runtime redesign is needed.

## Source Migration

Every authored `.jz` source, including compiler modules, standard library
modules, benchmark programs, examples, and fixtures, must use the retained
language:

1. A single equation becomes an ordinary binding with a pattern lambda.
2. Alternatives that share one body may use the existing lambda or-pattern.
3. Alternatives with distinct bodies become one `\|` clause lambda.
4. Multiple equation arguments become comma-separated parameters in every
   clause head.
5. Guards in equation clauses become existing `if` or `case` expressions.

No source is migrated to identifier-only lambdas when a retained pattern-lambda
head expresses the same contract directly.

## Documentation and Inventory

Current language documentation, execution gates, editor examples, and feature
inventory must stop advertising function equations. Historical design and plan
documents remain as records, but are marked superseded by this decision.

The authored-source feature audit replaces equation coverage with explicit
coverage for:

- pattern-shaped lambda parameters;
- multiple lambda parameters;
- top-level lambda or-pattern alternatives; and
- ordered multi-body pattern-lambda clauses.

## Verification

Tests must prove both sides of the boundary:

- `name pattern = body.` is rejected as declaration syntax;
- single and multiple pattern-lambda parameters still parse and run;
- top-level lambda or-pattern alternatives still parse and run;
- different-body dispatch works through ordered `\|` clauses;
- hosted and Jazz-authored parser/core comparisons remain exact; and
- the complete authored `.jz` corpus parses, compiles, and passes its existing
  semantic and benchmark expectations.

Implementation follows red-green TDD: add the rejection and preservation tests
first, observe equation acceptance fail the new expectation, then remove the
production paths and migrate the corpus.

## Out of Scope

- Changing `if`, `case`, lambda, or pattern semantics.
- Adding guards, `where` clauses, pattern synonyms, or a replacement named
  multi-clause declaration form.
- Modifying the read-only `jazz-hs/` or `jazz2/` implementations.
- Advancing typed-core, backend, or bootstrap milestones.
