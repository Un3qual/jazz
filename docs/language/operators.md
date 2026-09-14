---
title: Operators
description: Use built-in operators, sections, values, and source-local declarations.
sidebar_position: 8
---

## Built-in precedence

Precedence determines how an unparenthesized expression is grouped. Function
application binds more tightly than every infix operator. The built-in order,
from tightest to loosest, is:

| Operators                        | Associativity |
| -------------------------------- | ------------- |
| `*`, `/`                         | left          |
| `+`, `-`                         | left          |
| `\|`                             | left          |
| `==`, `!=`, `<`, `<=`, `>=`, `>` | left          |
| `$`                              | right         |

`$` applies the function on its left to the value on its right. Its low
precedence makes it useful for avoiding parentheses around the argument.

`|` participates in pattern alternatives and list patterns; it is not Boolean
OR. `True | False` is rejected with `E2003`.

## Functions behind operator notation

Operators are notation for ordinary functions. `left == right` means
`equals left right`, including lexical scope, type inference and implementation
selection. The parser supplies grouping; functions supply behavior.

| Operators            | Functions                                                          |
| -------------------- | ------------------------------------------------------------------ |
| `+`, `-`, `*`, `/`   | `add`, `subtract`, `multiply`, `divide`                            |
| `==`, `!=`           | `equals`, `differs`                                                |
| `<`, `<=`, `>`, `>=` | `lessThan`, `lessThanOrEqual`, `greaterThan`, `greaterThanOrEqual` |
| `$`                  | `apply`                                                            |

The Prelude defines arithmetic through `Num`, equality through `Equatable`,
and ordering through `Comparable::compare`. A user-defined implementation works
for both the named function and its operator spelling. Local bindings and
imports of those function names also affect operator notation.

Arithmetic operands have the same type. Convert concrete mixed numeric operands
explicitly; operators have no additional coercion rules. Equality requires
`Equatable`, including for ADTs. There is no implicit structural ADT equality.
See [Prelude](../standard-library/prelude.md) for the functions and capabilities.

Boolean negation is the ordinary function `not`: write `not condition`.

## Operator values and sections

Every executable operator can be used as a function value or a section. A section
captures one operand and returns a function. For example, `($ True) not` evaluates
to `False`.

Left and right sections capture one operand. Their argument order is exact:

- `(expr op) arg` means `op expr arg`;
- `(op expr) arg` means `op arg expr`; and
- `(op) left right` is ordinary curried application.

For subtraction, `(10 -) 3` evaluates as `10 - 3`, while `(- 10) 3`
evaluates as `3 - 10`. A right section is therefore different from ordinary
partial application: `((-) 10) 3` evaluates as `10 - 3`.

A declared operator becomes a callable value, an infix function, and a section
target after its binding is in scope.

## Source-local declarations

Programs can give a locally defined function infix notation and a precedence:

Fragment:

<!-- jazz-example: fragment -->

```jazz
operator %% tier 2.
(%%) :: Int -> Int -> Int.
(%%) = \(left, right) -> left + right.
1 %% 2.
```

The declaration affects only the current source unit and must precede use.
Associativity controls how adjacent operators at the same precedence group;
`nonassoc` requires explicit parentheses. Operator declarations are not allowed
inside expression blocks. See the [expression grammar](../reference/expression-grammar.md)
for tiers, numeric precedence, defaults, and valid symbols.
