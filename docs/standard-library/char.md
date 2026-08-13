---
title: Char
description: Convert, classify, and case-map Unicode scalar values.
---

`Char` is one Unicode scalar. It does not expose bytes or UTF-16 code units,
and its operations do not implicitly normalize text or apply locale-sensitive
rules.

## Conversion

`charToUInt32` returns the scalar value. `charFromUInt32` returns `Nothing` for
values outside Unicode or in the surrogate range.

## Classification

`charIsAlpha`, `charIsAlphaNum`, `charIsDigit`, `charIsSpace`,
`charIsHexDigit`, `charIsLower`, `charIsUpper`, and `charIsNewline` use Unicode
scalar properties.

## Case mapping

`charToLower` and `charToUpper` perform simple, locale-independent one-scalar
mapping. They never expand one scalar into multiple values. All Char operations
are logically `O(1)`.

Literal spelling and escapes are defined by the
[lexical grammar](../reference/lexical-grammar.md). Use [Text](text.md) for
immutable scalar sequences.
