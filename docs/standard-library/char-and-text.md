---
title: Char and Text
description: Work with Unicode scalars and immutable scalar-indexed text.
sidebar_position: 8
---

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

Indices and widths count scalars. `textAt` returns `Nothing` for a negative
index. Negative counts clamp to zero, and a negative `textDrop` or `textSlice`
start clamps to the beginning. Empty needles match at zero. Splitting on an
empty delimiter yields one text value per scalar; replacing an empty needle
leaves the input unchanged. Search and replacement are left-to-right and
non-overlapping.

`textIsEmpty` and `textUncons` are constant-time at the API boundary. Indexing,
slicing, conversion to characters, reversal, and other traversal are `O(n)`
worst case. `textConcat` and `textJoin` are linear in traversed input plus
produced output and avoid repeated pairwise-append chains.

Prefix testing is `O(m)`; naive search, split, and replacement can be
`O(n * m)` plus output. `textLines` recognizes LF, CRLF, and CR. `textWords`
uses Unicode whitespace and discards empty runs. Trimming is also
Unicode-whitespace-aware. Padding counts scalar width and never truncates text
already at or beyond the requested width.

Literal spelling and escapes are in the
[lexical grammar](../reference/lexical-grammar.md).
