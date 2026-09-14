# RFC 0020: Operators as functions

Status: Accepted
Date: 2026-09-14
Supersedes: Separate built-in operator dispatch, implicit structural operator equality, and operator-only mixed numeric promotion.

## Decision

`not :: Bool -> Bool` is an ordinary Prelude function. It is neither a keyword
nor a special prefix expression.

The parser retains precedence and associativity. Name resolution translates
operator expressions, values, and sections to ordinary calls in the surrounding
lexical scope:

| Notation | Function             |
| -------- | -------------------- |
| `+`      | `add`                |
| `-`      | `subtract`           |
| `*`      | `multiply`           |
| `/`      | `divide`             |
| `==`     | `equals`             |
| `!=`     | `differs`            |
| `<`      | `lessThan`           |
| `<=`     | `lessThanOrEqual`    |
| `>`      | `greaterThan`        |
| `>=`     | `greaterThanOrEqual` |
| `$`      | `apply`              |

The function name resolves exactly as it would in a written call. Ordinary
shadowing, imports, type inference, instance evidence, partial application and
execution apply. Operator notation does not bypass the selected implementation.
An explicit or absent Prelude must supply the functions it uses.

`Num(a)` owns add, subtract, multiply and divide, each with type `a -> a -> a`.
Primitive implementations call private kernel functions; user-defined types may
implement the same methods. Integral division retains its existing semantics.
The ordering functions are ordinary constrained functions over
`Comparable::compare`, so one comparison implementation supplies all four.
`Equatable::differs` retains its overridable default, now written with `not`.

Primitive equality implementations call kernel equality. Collection equality
uses element evidence. ADTs require explicit Equatable implementations; there
is no compiler-provided structural fallback. The private kernel remains usable
by explicit implementations. Concrete mixed numeric operands require conversion
for named calls and every operator spelling. Literals retain ordinary inference.

Sections retain argument order and evaluate their captured operand once when
the section is constructed. `$` retains its low right-associative precedence,
ordinary callable form, and supports sections like the other functions. `|` remains
pattern syntax, not an executable Boolean operator. Existing source-local custom
operator declarations, precedence, bindings and transport restrictions remain.

## Context

The maintainer requested Boolean `not` and one implementation shared by operator
notation and named function calls. They explicitly chose ordinary function rules,
including Equatable evidence for ADTs and explicit mixed numeric conversions,
and confirmed that this applies to all executable operators.

## Consequences

Reuse the existing operator-to-call resolution rather than creating another
operator registry for inference or runtime dispatch. Remove replaced operator
alias schemes, checked operand projections, mixed numeric promotion and runtime
operator values. Private kernel primitives own only primitive semantics.

The hosted frontend retains the same surface grammar and canonical lowered
representation as Haskell. This decision does not add hosted inference or a new
compiler backend. Update maintained programs and public contracts for the
intentional equality and numeric changes, and test custom implementations,
lexical resolution, first-class values, sections, numeric widths and diagnostics.
