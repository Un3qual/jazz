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

## Executable built-ins

Arithmetic on operands of the same numeric type returns that type. Built-in
arithmetic also accepts one integral operand with `Float` or `Float64`; the
integral operand is converted and the result has the float operand's type.
Other width changes require explicit conversion. Ordering and equality produce
`Bool`. See [Runtime values](../reference/runtime-values.md) for the supported
numeric domains.

## Operator values and sections

An executable built-in can be used as a callable value. Sections are available
for arithmetic `+`, `-`, `*`, `/`; ordering `<`, `<=`, `>`, `>=`; and equality
`==`, `!=`. A section captures one operand and returns a function. `$` is
callable but not sectionable.

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
