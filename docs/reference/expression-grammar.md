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
context     := "@{" constraint ("," constraint)* "}:"
constraint  := class-name "(" type ")"
class       := "class" context? identifier "(" identifier ")" "{"
               (signature | binding)* "}" "."
impl        := "impl" context? class-name "(" type ")" "{"
               binding* "}" "."
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
an imported alias for `Alias::name` and `Alias::Class::method`; a visible
class also permits `Class::method`. Qualified names do not admit internal
whitespace. Qualified class references have two components and method references
through an alias have three; longer qualification is rejected.

Signature constraints and impl heads accept `Alias::Class`. Class declarations
still introduce an unqualified class name. See
[module resolution](module-resolution.md#qualified-classes) for visibility and
identity rules.

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
that follows them. Class bodies contain method signatures and optional default
bindings. Implementation contexts apply prerequisite classes to variables bound
by the head; superclass contexts apply classes to the class parameter. Ordinary
method and value signatures may constrain compound types. See
[capabilities](../language/capabilities.md) for kind, head, and overlap rules.

Built-in precedence and source-local operator declarations are documented in
[operators](../language/operators.md). Declaration scope restrictions are in
[source and blocks](../language/source-and-blocks.md).
