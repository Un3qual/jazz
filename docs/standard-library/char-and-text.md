---
title: Char and Text
description: Work with Unicode scalars and immutable scalar-indexed text.
sidebar_position: 8
---

# Char and Text

`Char` is one Unicode scalar. `Text` is an immutable sequence of Unicode
scalars. Neither exposes bytes or UTF-16 code units, and operations do not
implicitly normalize or apply locale-sensitive rules.

## Char

`Char` exports `charToUInt32`, checked `charFromUInt32`, `charIsAlpha`,
`charIsAlphaNum`, `charIsDigit`, `charIsSpace`, `charIsHexDigit`,
`charIsLower`, `charIsUpper`, `charToLower`, `charToUpper`, and
`charIsNewline`.

`charFromUInt32` returns `Nothing` outside Unicode or in the surrogate range.
Classification is Unicode-aware. Case conversion is simple,
locale-independent one-scalar mapping and never expands a scalar. These
operations are logically `O(1)`.

## Text

Shape and traversal use `textEmpty`, `textLength`, `textIsEmpty`, `textUncons`,
`textAt`, `textTake`, `textDrop`, `textSlice`, `textToChars`, and
`textReverse`. Construction uses `textAppend`, `textAppendChar`,
`textFromChars`, `textRepeat`, `textConcat`, and `textJoin`.

Search uses `textStartsWith`, `textEndsWith`, `textContains`, and `textFind`.
Splitting and cleanup use `textSplit`, `textLines`, `textWords`,
`textReplaceAll`, `textTrim`, `textTrimStart`, `textTrimEnd`, `textPadLeft`,
and `textPadRight`.

Indices and widths count scalars. Negative indices return `Nothing`; negative
counts clamp to zero. Empty needles match at zero. Splitting on an empty
delimiter yields one text value per scalar; replacing an empty needle leaves
the input unchanged. Search and replacement are left-to-right and non-overlapping.
Prefix is `O(m)`; naive search/split/replace can be `O(n * m)` plus output;
traversal and cleanup are linear in input plus output.

Literal spelling and escapes are in the
[lexical grammar](../reference/lexical-grammar.md).
