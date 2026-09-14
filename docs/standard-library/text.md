---
title: Text
description: Work with immutable, Unicode-scalar-indexed text.
---

`Text` is an immutable sequence of Unicode scalars. Indices, lengths, and
widths count scalars rather than bytes or UTF-16 code units. Operations do not
implicitly normalize text or apply locale-sensitive rules.

## Constants and shape

### `empty`

```jazz jazz-signature
empty :: Text.
```

The empty text value, equivalent to `""`.

### `length`

```jazz jazz-signature
length :: Text -> Int.
```

Returns the number of Unicode scalars in `O(n)`.

### `isEmpty`

```jazz jazz-signature
isEmpty :: Text -> Bool.
```

Returns `True` only for empty text. This is constant-time at the API boundary.

### `uncons`

```jazz jazz-signature
uncons :: Text -> Maybe::Maybe((Char, Text)).
```

Returns the first scalar and remaining text as `Just`, or `Nothing` for empty
text. This is constant-time at the API boundary.

## Access and slicing

### `at`

```jazz jazz-signature
at :: Int -> Text -> Maybe::Maybe(Char).
```

Returns the scalar at a zero-based index. Negative and out-of-range indices
return `Nothing`. The cost is linear in the traversed prefix.

### `take`

```jazz jazz-signature
take :: Int -> Text -> Text.
```

Returns at most the first `count` scalars. Negative counts clamp to zero. The
cost is linear in the returned prefix.

### `drop`

```jazz jazz-signature
drop :: Int -> Text -> Text.
```

Skips at most the first `count` scalars. Negative counts clamp to zero. The
cost is linear in the skipped prefix.

### `slice`

```jazz jazz-signature
slice :: Int -> Int -> Text -> Text.
```

Drops `start` scalars and then takes `count` scalars. Negative starts and counts
clamp to zero. The cost is linear in the traversed prefix and output.

## Construction

### `append`

```jazz jazz-signature
append :: Text -> Text -> Text.
```

Returns the first text followed by the second. The cost is linear in the
resulting text size.

### `appendChar`

```jazz jazz-signature
appendChar :: Text -> Char -> Text.
```

Appends one scalar to the end of the text.

### `fromChars`

```jazz jazz-signature
fromChars :: [Char] -> Text.
```

Constructs text from scalars in list order in `O(n)`.

### `repeat`

```jazz jazz-signature
repeat :: Int -> Text -> Text.
```

Repeats text `count` times. Non-positive counts return `empty` in `O(1)`
time and temporary space. For positive counts, time and temporary space are
linear in the repetition count plus the output size.

### `concat`

```jazz jazz-signature
concat :: [Text] -> Text.
```

Concatenates fragments in list order without a repeated pairwise-append chain.
The cost is linear in fragment count and total output.

### `join`

```jazz jazz-signature
join :: Text -> [Text] -> Text.
```

Places the delimiter between adjacent fragments and concatenates them. No
delimiter appears before the first or after the last fragment.

## Conversion and traversal

### `toChars`

```jazz jazz-signature
toChars :: Text -> [Char].
```

Returns the Unicode scalars in source order in `O(n)`.

### `reverse`

```jazz jazz-signature
reverse :: Text -> Text.
```

Reverses by Unicode scalar, not by grapheme cluster, in `O(n)`.

## Search

### `startsWith`

```jazz jazz-signature
startsWith :: Text -> Text -> Bool.
```

Tests whether the second argument begins with the prefix supplied first. An
empty prefix always matches. The cost is `O(m)` in the prefix length.

### `endsWith`

```jazz jazz-signature
endsWith :: Text -> Text -> Bool.
```

Tests whether the second argument ends with the suffix supplied first. An empty
suffix always matches. The implementation traverses the text to find the
suffix boundary.

### `contains`

```jazz jazz-signature
contains :: Text -> Text -> Bool.
```

Tests whether the second argument contains the needle supplied first. An empty
needle matches. Naive search is `O(n × m)` worst case.

### `find`

```jazz jazz-signature
find :: Text -> Text -> Maybe::Maybe(Int).
```

Returns the scalar index of the first left-to-right match, or `Nothing`. An
empty needle returns `Just 0`. Naive search is `O(n × m)` worst case.

## Splitting

### `split`

```jazz jazz-signature
split :: Text -> Text -> [Text].
```

Splits the second argument at non-overlapping occurrences of the delimiter
supplied first. An empty delimiter produces one text value per scalar.

Example: `split "" "ab"` produces `["a", "b"]`.

### `lines`

```jazz jazz-signature
lines :: Text -> [Text].
```

Splits lines at LF, CRLF, or CR. Line terminators are excluded. Empty input
returns `[]`, and a trailing terminator does not add a final empty line.

### `words`

```jazz jazz-signature
words :: Text -> [Text].
```

Splits at runs of Unicode whitespace and omits empty words. Traversal is
`O(n)`.

## Replacement and cleanup

### `replaceAll`

```jazz jazz-signature
replaceAll :: Text -> Text -> Text -> Text.
```

Replaces non-overlapping matches of the first argument with the second in the
third argument, scanning left to right. An empty needle returns the input
unchanged. Naive replacement is `O(n × m)` plus output work.

### `trim`

```jazz jazz-signature
trim :: Text -> Text.
```

Removes Unicode whitespace from both ends in `O(n)`.

### `trimStart`

```jazz jazz-signature
trimStart :: Text -> Text.
```

Removes Unicode whitespace from the beginning and leaves trailing whitespace
unchanged.

### `trimEnd`

```jazz jazz-signature
trimEnd :: Text -> Text.
```

Removes Unicode whitespace from the end and leaves leading whitespace
unchanged.

## Padding

### `padLeft`

```jazz jazz-signature
padLeft :: Int -> Char -> Text -> Text.
```

Prepends the padding scalar until the text reaches the requested scalar width.
If the text is already wide enough, it is returned unchanged.

### `padRight`

```jazz jazz-signature
padRight :: Int -> Char -> Text -> Text.
```

Appends the padding scalar until the text reaches the requested scalar width.
If the text is already wide enough, it is returned unchanged.

Literal spelling and escapes are defined by the
[lexical grammar](../reference/lexical-grammar.md). Use [Char](char.md) for
single-scalar classification and case mapping.

## `map`

```jazz jazz-signature
map :: (Char -> Char) -> Text -> Text.
```

Transforms Unicode scalars and returns Text. The callback is the first argument
and must return Char. The implementation converts to scalars, maps once, and
rebuilds Text in linear time. For another element type, explicitly use
`map change (Text::toChars text)` to produce a List.
