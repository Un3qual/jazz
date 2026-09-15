---
title: Char
description: Convert, classify, and case-map Unicode scalar values.
---

`Char` represents one Unicode scalar, not a byte or UTF-16 code unit. Char
operations do not normalize text or apply locale-sensitive rules.

## Conversion

### `toUInt32`

```jazz jazz-signature
toUInt32 :: Char -> UInt32.
```

Returns the scalar's numeric Unicode value. This is logically `O(1)`.

### `fromUInt32`

```jazz jazz-signature
fromUInt32 :: UInt32 -> Maybe::Maybe(Char).
```

Returns `Just` for a valid Unicode scalar. Values above `0x10FFFF` and values in
the surrogate range return `Nothing`. This is logically `O(1)`.

## Classification

### `isAlpha`

```jazz jazz-signature
isAlpha :: Char -> Bool.
```

Returns `True` for Unicode characters in the `UppercaseLetter`,
`LowercaseLetter`, `TitlecaseLetter`, `ModifierLetter`, or `OtherLetter`
general category.

### `isAlphaNum`

```jazz jazz-signature
isAlphaNum :: Char -> Bool.
```

Returns `True` for any category accepted by `isAlpha`, plus
`DecimalNumber`, `LetterNumber`, and `OtherNumber`.

### `isDigit`

```jazz jazz-signature
isDigit :: Char -> Bool.
```

Uses the Unicode digit property, not only ASCII `0`–`9`.

### `isSpace`

```jazz jazz-signature
isSpace :: Char -> Bool.
```

Uses the Unicode whitespace property.

### `isHexDigit`

```jazz jazz-signature
isHexDigit :: Char -> Bool.
```

Recognizes only ASCII `0`–`9`, `a`–`f`, and `A`–`F`.

### `isLower`

```jazz jazz-signature
isLower :: Char -> Bool.
```

Returns `True` only for the Unicode `LowercaseLetter` general category.

### `isUpper`

```jazz jazz-signature
isUpper :: Char -> Bool.
```

Returns `True` only for the Unicode `UppercaseLetter` general category.

### `isNewline`

```jazz jazz-signature
isNewline :: Char -> Bool.
```

Returns `True` for line feed (`'\n'`) or carriage return (`'\r'`) and `False`
for other scalars.

Classification functions are logically `O(1)`.

## Case mapping

### `toLower`

```jazz jazz-signature
toLower :: Char -> Char.
```

Performs simple, locale-independent lowercase mapping. It returns one scalar
and never expands a character into multiple values.

### `toUpper`

```jazz jazz-signature
toUpper :: Char -> Char.
```

Performs simple, locale-independent uppercase mapping. It returns one scalar
and never expands a character into multiple values.

Literal spelling and escapes are defined by the
[lexical grammar](../reference/lexical-grammar.md). Use [Text](text.md) for
immutable scalar sequences.
