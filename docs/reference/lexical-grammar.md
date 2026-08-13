---
title: Lexical grammar
description: Reference Jazz tokens, identifiers, literals, comments, escapes, and punctuation.
sidebar_position: 1
---

## Whitespace and comments

Unicode whitespace separates tokens. `#` starts a comment that continues to
the next newline. Jazz has no block-comment form.

## Identifiers

An identifier starts with a Unicode alphabetic character or `_`. Continuation
characters may also contain Unicode alphanumerics, `_`, `'`, and `!`. The `!`
suffix has [purity meaning](../language/purity.md), but is lexically part of the
identifier.

Reserved words are `module`, `import`, `as`, `data`, `value`, `if`,
`then`, `else`, and `case`. `True` and `False` are literal names. Declaration
words such as `class`, `impl`, and `operator` are contextual identifiers and
are recognized only in their declaration shapes.

## Literals

Integers are unsigned decimal digit sequences; negative values are expressed
with subtraction. Fractional literals require adjacent whole, dot, and
fractional digits, such as `1.25`. Optional adjacent suffixes `f16`, `f32`, and
`f64` select a concrete floating width. Other suffix spellings are not numeric
syntax.

Character literals use single quotes and contain exactly one Unicode scalar.
Text literals use double quotes. Both accept `\\`, `\'`, `\"`, `\n`, `\r`,
`\t`, `\0`, and `\u{HEX}` with one to six hexadecimal digits. Surrogates,
values above `0x10FFFF`, raw newlines, invalid escapes, and unterminated
literals are `E0001` errors.

## Symbols

Structural symbols are `::`, `:`, `@`, `=`, `->`, `.`, braces, parentheses,
brackets, comma, and backslash. Operator runs use characters from
`!%&*+-/<>?^|~`; `$` is also built in. Arrow and several comment-like symbols
are reserved from user-defined operators.

All tokens retain one-based line and column positions for
[diagnostics](diagnostics.md).
