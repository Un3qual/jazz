---
title: Prelude
description: Reference the implicit capability vocabulary, conversions, and compatibility helpers.
sidebar_position: 2
---

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

The Prelude also exposes these compatibility list helpers:

- `map :: (a -> b) -> [a] -> [b]` applies a function to every item and
  preserves order;
- `filter :: (a -> Bool) -> [a] -> [a]` keeps the items whose predicate is
  `True`, preserving order;
- `hd :: [a] -> a` returns the first item; and
- `tl :: [a] -> [a]` returns everything after the first item.

`hd` and `tl` are partial: an empty list fails fatally with `E3009` or `E3010`,
respectively. Prefer the richer [List API](list.md) for new library-oriented
code.

In stub-v1, `print! :: a -> a` emits no output and returns its evaluated
argument unchanged. Its bang suffix still classifies it as impure. Run mode may
render the final expression value; that rendering is separate from `print!`.

The Prelude also exposes target-named numeric conversions from `toInt8` through
`toFloat64`, plus aliases `toInt` and `toFloat`.

Numeric conversions are explicit and range checked. Integer narrowing and sign
changes must fit; float-to-integer conversion requires a finite integral value;
and floating conversion uses deterministic target-format rounding. Invalid
dynamic conversions report a runtime diagnostic.

Use `--no-prelude` only for compiler or runtime work. Its low-level support
surface is not a user API. For capability limits, see
[capabilities](../language/capabilities.md).
