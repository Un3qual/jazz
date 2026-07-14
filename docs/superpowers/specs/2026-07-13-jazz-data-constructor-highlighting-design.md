# Jazz Data-Constructor Highlighting Design

## Status

Approved on `2026-07-13`; implementation pending.

## Goal

Give constructor names in Jazz `data` declarations a TextMate scope distinct
from the scopes used for their payload types. For example:

```jazz
data Point = XYPoint Int Int.
```

`XYPoint` is a constructor declaration, while both `Int` occurrences remain
built-in type references. The active VS Code theme continues to choose the
rendered colors; the extension supplies independently themeable scopes rather
than hard-coded colors.

## Scope

This change applies only to constructor declaration positions inside `data`
declarations:

- the first constructor after `=`;
- each later constructor after `|`.

It does not classify constructor uses in expressions or patterns, add semantic
highlighting, or introduce a language server. Type names, built-in types, type
variables, punctuation, and keywords retain their existing scopes.

## TextMate Structure

Add a `data-declarations` repository rule and include it before the general
keyword and uppercase-type rules. The rule begins at a `data` keyword followed
by the declared uppercase type name and ends at that declaration's terminating
dot.

Within that region, a constructor matcher captures an `=` or `|` delimiter and
the immediately following uppercase identifier. Only the identifier receives
the constructor-specific scope:

```text
entity.name.function.constructor.jazz
```

The enclosing rule then delegates payload references to the existing built-in
and general type rules. This contextual region is preferred to a global
delimiter matcher because `=` and `|` also occur in bindings, cases, patterns,
and operators.

## Validation

Extend `fixtures/representative.jz` with the exact ambiguity this change must
handle:

```jazz
data Point = XYPoint Int Int.
```

The repository audit will continue parsing the entire fixture with the active
Jazz parser. Its structural JSON checks will additionally require the manifest
grammar to contain the data-declaration rule and the constructor-specific
scope. Focused validation will decode all extension JSON, parse the fixture,
compile the TextMate regexes, and run `repository-audit-spec` plus
`parser-foundation-spec`; the complete Jazz test suite remains the final gate
before pushing the PR branch.

## Non-Goals

- Hard-coded token colors or theme overrides.
- Constructor highlighting outside `data` declarations.
- Semantic tokens, type-aware classification, or an editor language service.
- Changes to Jazz syntax, parsing, lowering, or runtime behavior.
