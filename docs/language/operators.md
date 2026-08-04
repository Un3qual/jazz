---
title: Operators
description: Use built-in operators, sections, values, and source-local declarations.
sidebar_position: 8
---

## Built-in precedence

The parser's built-in fixity order, from tightest to loosest, is:

| Operators                        | Associativity |
| -------------------------------- | ------------- |
| `*`, `/`                         | left          |
| `+`, `-`                         | left          |
| `\|`                             | left          |
| `==`, `!=`, `<`, `<=`, `>=`, `>` | left          |
| `$`                              | right         |

Function application binds tighter than every infix operator. `$` is ordinary
low-precedence application. This table is parser metadata: recognizing and
grouping an operator does not by itself give that operator executable
semantics.

`|` is parser/fixity metadata only. It has no executable built-in type rule;
`True | False` is rejected with `E2003`. The pipe token also separates pattern
alternatives and cons-list components where those grammars expect it.

## Executable built-ins

The executable built-in operators are arithmetic `+`, `-`, `*`, `/`; ordering
`<`, `<=`, `>`, `>=`; equality `==`, `!=`; and application `$`. The arithmetic
operators return their numeric operand type, ordering and equality return
`Bool`, and `$` applies its left function to its right argument.

## Operator values and sections

The executable built-ins can be parenthesized as callable values. Built-in
sections are supported only for arithmetic `+`, `-`, `*`, `/`; ordering `<`,
`<=`, `>`, `>=`; and equality `==`, `!=`. `$` is callable as `($)`, but it is
not sectionable. `|` is neither a callable built-in value nor a sectionable
built-in.

Left and right sections capture one operand. Their argument order is exact:

- `(expr op) arg` means `op expr arg`;
- `(op expr) arg` means `op arg expr`; and
- `(op) left right` is ordinary curried application.

For subtraction, `(10 -) 3` evaluates as `10 - 3`, while `(- 10) 3`
evaluates as `3 - 10`. A right section is therefore different from ordinary
partial application: `((-) 10) 3` evaluates as `10 - 3`.

A source-local declared operator becomes a callable value, infix function, and
section target after its ordinary function binding is in scope.

## Source-local declarations

Source units may declare a new operator before use:

Fragment:

<!-- jazz-example: fragment -->

```jazz
operator %% tier 2.
(%%) :: Int -> Int -> Int.
(%%) = \(left, right) -> left + right.
1 %% 2.
```

Tiers range from 1 to 5, or a declaration may use `precedence 1` through
`precedence 99`. Optional associativity is `left`, `right`, or `nonassoc`;
without an explicit associativity, tiers 1 through 4 default left and tier 5
defaults right, matching their built-in tiers. Custom precedence defaults
left. Operator declarations and bindings are source-local and not allowed in
nested expression blocks. See the exact [expression grammar](../reference/expression-grammar.md).
