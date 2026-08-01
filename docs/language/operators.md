---
title: Operators
description: Use built-in operators, sections, values, and source-local declarations.
sidebar_position: 8
---

# Operators

## Built-in precedence

The built-in precedence order, from tightest to loosest, is:

| Operators                        | Associativity |
| -------------------------------- | ------------- |
| `*`, `/`                         | left          |
| `+`, `-`                         | left          |
| `\|`                             | left          |
| `==`, `!=`, `<`, `<=`, `>=`, `>` | left          |
| `$`                              | right         |

Function application binds tighter than every infix operator. `$` is ordinary
low-precedence application.

## Operator values and sections

Parenthesized operators are callable values, and left or right sections capture
one operand.

## Source-local declarations

Source units may declare a new operator before use:

Fragment:

```jazz
operator %% tier 2.
(%%) :: Int -> Int -> Int.
(%%) = \(left, right) -> left + right.
1 %% 2.
```

Tiers range from 1 to 5, or a declaration may use `precedence 1` through
`precedence 99`. Optional associativity is `left`, `right`, or `nonassoc`;
custom precedence defaults left. Operator declarations and bindings are
source-local and not allowed in nested expression blocks. See the exact
[expression grammar](../reference/expression-grammar.md).
