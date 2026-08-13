---
title: Text
description: Work with immutable, Unicode-scalar-indexed text.
---

`Text` is an immutable sequence of Unicode scalars. Indices and widths count
scalars, not bytes or UTF-16 code units. Operations do not implicitly normalize
text or apply locale-sensitive rules.

## Shape and traversal

Use `textEmpty`, `textLength`, `textIsEmpty`, `textUncons`, `textAt`,
`textTake`, `textDrop`, `textSlice`, `textToChars`, and `textReverse`.
`textAt` returns `Nothing` for a negative or out-of-range index. Negative counts
clamp to zero; negative drop and slice starts clamp to the beginning.

## Construction

`textAppend`, `textAppendChar`, `textFromChars`, `textRepeat`, `textConcat`, and
`textJoin` construct text. `textConcat` and `textJoin` avoid repeated
pairwise-append chains.

## Search and cleanup

Search uses `textStartsWith`, `textEndsWith`, `textContains`, and `textFind`.
Splitting and cleanup use `textSplit`, `textLines`, `textWords`,
`textReplaceAll`, `textTrim`, `textTrimStart`, `textTrimEnd`, `textPadLeft`, and
`textPadRight`.

Empty needles match at zero. Splitting on an empty delimiter yields one value
per scalar; replacing an empty needle leaves the input unchanged. Search and
replacement proceed left to right without overlap. `textLines` recognizes LF,
CRLF, and CR. `textWords` and trimming use Unicode whitespace.

`textIsEmpty` and `textUncons` are constant-time at the API boundary. General
traversal is `O(n)`. Prefix testing is `O(m)`; naive search, split, and
replacement can be `O(n * m)` plus output. Literal spelling and escapes are in
the [lexical grammar](../reference/lexical-grammar.md).
