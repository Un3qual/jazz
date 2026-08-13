---
title: Char
description: Convert, classify, and case-map Unicode scalar values.
---

`Char` represents one Unicode scalar, not a byte or UTF-16 code unit. Char
operations do not normalize text or apply locale-sensitive rules.

## Conversion

### `charToUInt32`

```jazz jazz-signature
charToUInt32 :: Char -> UInt32.
```

Returns the scalar's numeric Unicode value. This is logically `O(1)`.

### `charFromUInt32`

```jazz jazz-signature
charFromUInt32 :: UInt32 -> Maybe(Char).
```

Returns `Just` for a valid Unicode scalar. Values above `0x10FFFF` and values in
the surrogate range return `Nothing`. This is logically `O(1)`.

## Classification

### `charIsAlpha`

```jazz jazz-signature
charIsAlpha :: Char -> Bool.
```

Returns whether the scalar has a Unicode alphabetic property.

### `charIsAlphaNum`

```jazz jazz-signature
charIsAlphaNum :: Char -> Bool.
```

Returns whether the scalar is alphabetic or numeric under Unicode properties.

### `charIsDigit`

```jazz jazz-signature
charIsDigit :: Char -> Bool.
```

Returns whether the scalar is a Unicode digit.

### `charIsSpace`

```jazz jazz-signature
charIsSpace :: Char -> Bool.
```

Returns whether the scalar is Unicode whitespace.

### `charIsHexDigit`

```jazz jazz-signature
charIsHexDigit :: Char -> Bool.
```

Returns whether the scalar is an ASCII hexadecimal digit: `0`-`9`, `a`-`f`, or
`A`-`F`.

### `charIsLower`

```jazz jazz-signature
charIsLower :: Char -> Bool.
```

Returns whether the scalar has a Unicode lowercase property.

### `charIsUpper`

```jazz jazz-signature
charIsUpper :: Char -> Bool.
```

Returns whether the scalar has a Unicode uppercase property.

### `charIsNewline`

```jazz jazz-signature
charIsNewline :: Char -> Bool.
```

Returns `True` for line feed (`'\n'`) or carriage return (`'\r'`) and `False`
for other scalars.

Classification functions are logically `O(1)`.

## Case mapping

### `charToLower`

```jazz jazz-signature
charToLower :: Char -> Char.
```

Performs simple, locale-independent lowercase mapping. It returns one scalar
and never expands a character into multiple values.

### `charToUpper`

```jazz jazz-signature
charToUpper :: Char -> Char.
```

Performs simple, locale-independent uppercase mapping. It returns one scalar
and never expands a character into multiple values.

Literal spelling and escapes are defined by the
[lexical grammar](../reference/lexical-grammar.md). Use [Text](text.md) for
immutable scalar sequences.
