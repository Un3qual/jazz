---
title: Expression grammar
description: Reference implemented Jazz declarations, expressions, patterns, types, and precedence.
sidebar_position: 2
---

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

Function application binds tighter than infix operators. Explicit type
application selects a type argument for a callable. Qualified lookup requires
an imported alias and does not admit whitespace within the qualified name.

Parentheses around one expression group it; zero or at least two comma-separated
elements form a tuple. Every clause of an ordered pattern lambda has the same
arity.

Conditionals always include both branches. Fractional literals are not valid
patterns.

The `pattern` production does not recursively include alternatives.
Alternatives are recognized only by the outer `case-arm-pattern` or
`lambda-parameter` production. Grouping an alternative or nesting it inside a
constructor, tuple, list, cons-list, or as-pattern is unsupported. Lambda
parameters do not accept guards. The optional `if` guard belongs only to a
complete case-arm pattern.

Function types associate to the right. Constraints apply to the complete type
that follows them.

Built-in precedence and source-local operator declarations are documented in
[operators](../language/operators.md). Declaration scope restrictions are in
[source and blocks](../language/source-and-blocks.md).
