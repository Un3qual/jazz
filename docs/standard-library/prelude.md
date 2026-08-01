---
title: Prelude
description: Reference the implicit capability vocabulary, conversions, and compatibility helpers.
sidebar_position: 2
---

# Prelude

The Prelude is bundled into ordinary compilation and execution. It declares:

- `Ordering = LT | EQ | GT`;
- `Eq(a)` with `equals` and `Ord(a)` with `compare`;
- marker capabilities `Num(a)`, `Integral(a)`, and `Fractional(a)`;
- `Showable(a)` with `show`; and
- `Default(a)` with `defaultValue`.

Concrete implementations cover the appropriate built-in scalar and numeric
types. `Text` ordering is lexicographic by Unicode scalar and `Char` ordering
uses scalar value. `show` uses stable runtime value syntax; defaults are
zero-like values.

The Prelude also exposes `map`, `filter`, `hd`, `tl`, `print!`, target-named
numeric conversions from `toInt8` through `toFloat64`, and aliases `toInt` and
`toFloat`. Prefer the richer [List API](list.md) for new library-oriented code.

Numeric conversions are explicit and range checked. Integer narrowing and sign
changes must fit; float-to-integer conversion requires a finite integral value;
and floating conversion uses deterministic target-format rounding. Invalid
dynamic conversions report a runtime diagnostic.

Use `--no-prelude` only for compiler or runtime work. Its low-level support
surface is not a user API. For capability limits, see
[capabilities](../language/capabilities.md).
