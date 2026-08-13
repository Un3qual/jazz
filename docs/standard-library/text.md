---
title: Text
description: Work with immutable, Unicode-scalar-indexed text.
---

`Text` is an immutable sequence of Unicode scalars. Indices, lengths, and
widths count scalars rather than bytes or UTF-16 code units. Operations do not
implicitly normalize text or apply locale-sensitive rules.

## Constants and shape

### `textEmpty`

<!-- jazz-signature -->

```jazz
textEmpty :: Text.
```

The empty text value, equivalent to `""`.

### `textLength`

<!-- jazz-signature -->

```jazz
textLength :: Text -> Int.
```

Returns the number of Unicode scalars in `O(n)`.

### `textIsEmpty`

<!-- jazz-signature -->

```jazz
textIsEmpty :: Text -> Bool.
```

Returns `True` only for empty text. This is constant-time at the API boundary.

### `textUncons`

<!-- jazz-signature -->

```jazz
textUncons :: Text -> Maybe((Char, Text)).
```

Returns the first scalar and remaining text as `Just`, or `Nothing` for empty
text. This is constant-time at the API boundary.

## Access and slicing

### `textAt`

<!-- jazz-signature -->

```jazz
textAt :: Int -> Text -> Maybe(Char).
```

Returns the scalar at a zero-based index. Negative and out-of-range indices
return `Nothing`. The cost is linear in the traversed prefix.

Example: `textAt 1 "Jazz"` produces `Just 'a'`.

### `textTake`

<!-- jazz-signature -->

```jazz
textTake :: Int -> Text -> Text.
```

Returns at most the first `count` scalars. Negative counts clamp to zero. The
cost is linear in the returned prefix.

### `textDrop`

<!-- jazz-signature -->

```jazz
textDrop :: Int -> Text -> Text.
```

Skips at most the first `count` scalars. Negative counts clamp to zero. The
cost is linear in the skipped prefix.

### `textSlice`

<!-- jazz-signature -->

```jazz
textSlice :: Int -> Int -> Text -> Text.
```

Drops `start` scalars and then takes `count` scalars. Negative starts and counts
clamp to zero. The cost is linear in the traversed prefix and output.

## Construction

### `textAppend`

<!-- jazz-signature -->

```jazz
textAppend :: Text -> Text -> Text.
```

Returns the first text followed by the second. The cost is linear in the
resulting text size.

### `textAppendChar`

<!-- jazz-signature -->

```jazz
textAppendChar :: Text -> Char -> Text.
```

Appends one scalar to the end of the text.

### `textFromChars`

<!-- jazz-signature -->

```jazz
textFromChars :: [Char] -> Text.
```

Constructs text from scalars in list order in `O(n)`.

### `textRepeat`

<!-- jazz-signature -->

```jazz
textRepeat :: Int -> Text -> Text.
```

Repeats text `count` times. Non-positive counts return `textEmpty`. The cost is
linear in the output size.

### `textConcat`

<!-- jazz-signature -->

```jazz
textConcat :: [Text] -> Text.
```

Concatenates fragments in list order without a repeated pairwise-append chain.
The cost is linear in fragment count and total output.

### `textJoin`

<!-- jazz-signature -->

```jazz
textJoin :: Text -> [Text] -> Text.
```

Places the delimiter between adjacent fragments and concatenates them. No
delimiter appears before the first or after the last fragment.

## Conversion and traversal

### `textToChars`

<!-- jazz-signature -->

```jazz
textToChars :: Text -> [Char].
```

Returns the Unicode scalars in source order in `O(n)`.

### `textReverse`

<!-- jazz-signature -->

```jazz
textReverse :: Text -> Text.
```

Reverses by Unicode scalar, not by grapheme cluster, in `O(n)`.

## Search

### `textStartsWith`

<!-- jazz-signature -->

```jazz
textStartsWith :: Text -> Text -> Bool.
```

Tests whether the second argument begins with the prefix supplied first. An
empty prefix always matches. The cost is `O(m)` in the prefix length.

### `textEndsWith`

<!-- jazz-signature -->

```jazz
textEndsWith :: Text -> Text -> Bool.
```

Tests whether the second argument ends with the suffix supplied first. An empty
suffix always matches. The implementation traverses the text to find the
suffix boundary.

### `textContains`

<!-- jazz-signature -->

```jazz
textContains :: Text -> Text -> Bool.
```

Tests whether the second argument contains the needle supplied first. An empty
needle matches. Naive search is `O(n × m)` worst case.

### `textFind`

<!-- jazz-signature -->

```jazz
textFind :: Text -> Text -> Maybe(Int).
```

Returns the scalar index of the first left-to-right match, or `Nothing`. An
empty needle returns `Just 0`. Naive search is `O(n × m)` worst case.

## Splitting

### `textSplit`

<!-- jazz-signature -->

```jazz
textSplit :: Text -> Text -> [Text].
```

Splits the second argument at non-overlapping occurrences of the delimiter
supplied first. An empty delimiter produces one text value per scalar.

Example: `textSplit "" "ab"` produces `["a", "b"]`.

### `textLines`

<!-- jazz-signature -->

```jazz
textLines :: Text -> [Text].
```

Splits lines at LF, CRLF, or CR. Line terminators are excluded. Empty input
returns `[]`, and a trailing terminator does not add a final empty line.

### `textWords`

<!-- jazz-signature -->

```jazz
textWords :: Text -> [Text].
```

Splits at runs of Unicode whitespace and omits empty words. Traversal is
`O(n)`.

## Replacement and cleanup

### `textReplaceAll`

<!-- jazz-signature -->

```jazz
textReplaceAll :: Text -> Text -> Text -> Text.
```

Replaces non-overlapping matches of the first argument with the second in the
third argument, scanning left to right. An empty needle returns the input
unchanged. Naive replacement is `O(n × m)` plus output work.

### `textTrim`

<!-- jazz-signature -->

```jazz
textTrim :: Text -> Text.
```

Removes Unicode whitespace from both ends in `O(n)`.

### `textTrimStart`

<!-- jazz-signature -->

```jazz
textTrimStart :: Text -> Text.
```

Removes Unicode whitespace from the beginning and leaves trailing whitespace
unchanged.

### `textTrimEnd`

<!-- jazz-signature -->

```jazz
textTrimEnd :: Text -> Text.
```

Removes Unicode whitespace from the end and leaves leading whitespace
unchanged.

## Padding

### `textPadLeft`

<!-- jazz-signature -->

```jazz
textPadLeft :: Int -> Char -> Text -> Text.
```

Prepends the padding scalar until the text reaches the requested scalar width.
If the text is already wide enough, it is returned unchanged.

Example: `textPadLeft 4 '0' "12"` produces `"0012"`.

### `textPadRight`

<!-- jazz-signature -->

```jazz
textPadRight :: Int -> Char -> Text -> Text.
```

Appends the padding scalar until the text reaches the requested scalar width.
If the text is already wide enough, it is returned unchanged.

Literal spelling and escapes are defined by the
[lexical grammar](../reference/lexical-grammar.md). Use [Char](char.md) for
single-scalar classification and case mapping.
