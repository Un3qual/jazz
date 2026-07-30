# Jazz Function-Equation Removal Design

## Status

Approved by the user's 2026-07-30 clarification:

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
that every alternative binds the same names. Since a lambda has one body,
alternatives with different bodies use an explicit `case`:

```jazz
length =
  \(items) ->
    case items {
      | [] -> 0
      | [_ | rest] -> 1 + length rest
    }.
```

Multiple arguments can be matched independently in the lambda head when one
body suffices, or as a tuple in `case` when alternatives need different bodies:

```jazz
zip =
  \(left, right) ->
    case (left, right) {
      | ([], _) -> []
      | (_, []) -> []
      | ([leftHead | leftTail], [rightHead | rightTail]) ->
          [(leftHead, rightHead) | zip leftTail rightTail]
    }.
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

The existing `SurfaceLambdaPattern` path remains the sole function-head pattern
surface. It continues lowering to ordinary lambdas and one-arm pattern cases,
so no analyzer, inference, or runtime redesign is needed.

## Source Migration

Every authored `.jz` source, including compiler modules, standard library
modules, benchmark programs, examples, and fixtures, must use the retained
language:

1. A single equation becomes an ordinary binding with a pattern lambda.
2. Alternatives that share one body may use the existing lambda or-pattern.
3. Alternatives with distinct bodies become an ordinary lambda plus `case`.
4. Multiple equation arguments become lambda parameters; tuple matching is
   used inside `case` when clause dispatch depends on several arguments.
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
- explicit multi-arm `case` dispatch.

## Verification

Tests must prove both sides of the boundary:

- `name pattern = body.` is rejected as declaration syntax;
- single and multiple pattern-lambda parameters still parse and run;
- top-level lambda or-pattern alternatives still parse and run;
- different-body dispatch works through explicit `case`;
- hosted and Jazz-authored parser/core comparisons remain exact; and
- the complete authored `.jz` corpus parses, compiles, and passes its existing
  semantic and benchmark expectations.

Implementation follows red-green TDD: add the rejection and preservation tests
first, observe equation acceptance fail the new expectation, then remove the
production paths and migrate the corpus.

## Out of Scope

- Changing `if`, `case`, lambda, or pattern semantics.
- Adding guards, `where` clauses, pattern synonyms, or a replacement
  multi-clause declaration form.
- Modifying the read-only `jazz-hs/` or `jazz2/` implementations.
- Advancing typed-core, backend, or bootstrap milestones.
