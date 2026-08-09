---
title: Expression grammar
description: Reference implemented Jazz declarations, expressions, patterns, types, and precedence.
sidebar_position: 2
---

This is a compact description of the accepted surface, not a parser generator
grammar.

```text
source      := statement*
statement   := signature | binding | data | class | impl
             | operator-declaration | operator-signature | operator-binding
             | module | import | expression
signature   := identifier "::" signature-type "."
binding     := identifier "=" expression "."
expression  := literal | identifier | qualified-name | list | tuple | block
             | lambda | pattern-lambda | if | case | application
             | type-application | operator-value | section | infix
case-arm    := "|" case-arm-pattern ("if" expression)? "->" expression
case-arm-pattern
            := pattern ("|" pattern)*
lambda-parameter
            := pattern ("|" pattern)*
pattern     := literal-pattern | identifier | "_" | constructor-pattern
             | list-pattern | cons-list-pattern | tuple-pattern | as-pattern
```

Primary expressions are integer, fractional, boolean, character, and text
literals; identifiers; lists; tuples; blocks; lambdas; conditionals; and cases.
Juxtaposition applies functions and binds tighter than infix operators.
`callable @Type` applies an explicit type argument. Qualified value lookup is
`Alias::member` with no whitespace inside the qualified name.

Lists are `[a, b]`; tuples are `()` or `(a, b, ...)`; parentheses group one
expression. `\(a, b) -> body` accepts one or more comma-separated lambda
parameters. Ordered pattern lambdas use `\|(patterns) -> body` followed by
additional `|(patterns) -> body` clauses of the same arity.

`if condition then yes else no` always has both branches. Cases use
`case expression { | pattern -> body ... }`; a guarded arm inserts
`if guard` before `->`. Patterns include literals other than fractional
literals, variables, wildcard `_`, constructors, lists, cons lists, tuples,
as-patterns, and alternatives.

The `pattern` production does not recursively include alternatives.
Alternatives are recognized only by the outer `case-arm-pattern` or
`lambda-parameter` production. Grouping an alternative or nesting it inside a
constructor, tuple, list, cons-list, or as-pattern is unsupported. Lambda
parameters do not accept guards. The optional `if` guard belongs only to a
complete case-arm pattern.

Function types are right-associative. Type atoms are primitive names, type
variables, named types, `Name(arguments)`, lists, tuples, and parenthesized
types. Constraints use `@{Capability(type), ...}: type`.

Built-in precedence and source-local operator declarations are documented in
[operators](../language/operators.md). Declaration scope restrictions are in
[source and blocks](../language/source-and-blocks.md).
