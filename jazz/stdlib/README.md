# Jazz standard library

This directory is the user-facing, Jazz-authored standard library for the
active `jazz` compiler. `Prelude.jz` is bundled automatically unless the
compiler is run with `--no-prelude`; every other module requires an explicit
`import Module.` declaration.

The library favors task-oriented names such as `Text::join`, `List::foldLeft`, and
`Dictionary::update`. It does not add category-theory-named abstractions merely
to mirror another language's library. Most implementation code is Jazz. The
small `__kernel_*` substrate supplies primitive scalar, text, numeric, and host
operations that cannot yet be expressed in Jazz; those names are private.

## Module overview

| Module       | Public abstraction                    | Primary purpose                                                                             |
| ------------ | ------------------------------------- | ------------------------------------------------------------------------------------------- |
| `Prelude`    | `Ordering(..)` and capability classes | Implicit scalar and collection capabilities, numeric conversions, and compatibility helpers |
| `List`       | Built-in `[a]`                        | Total list queries, transformations, folds, search, grouping, and stable sorting            |
| `Maybe`      | `Maybe(..)`                           | Optional values and branch-preserving transformations                                       |
| `Result`     | `Result(..)`                          | Recoverable success/error values and transformations                                        |
| `NonEmpty`   | `NonEmpty(..)`                        | A list shape that statically contains at least one value                                    |
| `Reduce`     | Generic seedless fold returning Maybe | Safe seedless reduction across collections                                                  |
| `Dictionary` | Abstract `Dictionary(k, v)`           | Insertion-ordered lookup by `Equatable` keys                                                |
| `Queue`      | Abstract `Queue(a)`                   | Persistent first-in, first-out traversal                                                    |
| `Map`        | Abstract `Map(k, v)`                  | Persistent ordered lookup by `Comparable` keys                                              |
| `Set`        | Abstract `Set(a)`                     | Persistent ordered unique values and set operations                                         |
| `Char`       | Built-in `Char`                       | Unicode scalar conversion, classification, and simple case mapping                          |
| `Text`       | Built-in `Text`                       | Unicode-scalar text construction, traversal, search, splitting, and cleanup                 |
| `IOError`    | `IOErrorCategory` and `IOError`       | Stable, platform-neutral host-I/O failure data                                              |
| `IO`         | Host operations ending in `!`         | Strict UTF-8 files and streams, arguments, and process exit                                 |

`Maybe`, `Result`, and `NonEmpty` intentionally publish their constructors so
callers can pattern match and construct values directly. `Dictionary`,
`Queue`, `Map`, and `Set` publish only their type identities and operations;
their constructors and representation invariants are private. All collection
updates return new values and leave older versions usable.

Complexities below describe logical Jazz operations. Let `n` be the input or
collection size, `m` a second input size, and `k` the produced output size.
Callback cost is excluded unless stated otherwise.

## Prelude

The bundled prelude declares:

- `Ordering = LT | EQ | GT`;
- `Equatable(a)` with `equals` and default `differs`;
- `Comparable(a)` with `compare` and superclass `Equatable(a)`;
- `Mappable(f)` with `map`, `Reducible(f)` with `foldLeft` and `foldRight`,
  and `Combinable(a)` with `combine`;
- `Num(a)` with `add`, `subtract`, `multiply`, and `divide`;
- marker capabilities `Integral(a)` and `Fractional(a)`;
- `Showable(a)` with `show`, and `Default(a)` with `defaultValue`.

Concrete instances cover the built-in scalar and numeric types appropriate to
each capability. `Text` ordering is lexicographic by Unicode scalar value.
`Char` ordering uses its scalar value. `show` produces the same stable value
syntax used by runtime rendering, while defaults are zero-like values.

The prelude also exposes `filter`, `hd`, `tl`, `print!`, target-named
numeric conversions from `toInt8` through `toFloat64`, and the aliases `toInt`
and `toFloat`. Use generic methods for shared collection behavior and qualified
module functions for specialized operations. Kernel bridge names are implementation details and are visible
only through explicit no-prelude compiler entry points.

## List

All list APIs are total. Empty or out-of-range queries use `Maybe`, negative
counts clamp to zero, `List::any []` is `False`, and `List::all []` is `True`.
Transformations preserve input order. `List::zip` stops at the shorter input,
`List::indexed` starts at zero, `List::distinct` preserves first occurrence, and
`List::group` groups adjacent equal values rather than every equal value.
`List::sort` and `List::sortBy` are stable merge sorts.

| Operation family  | Public values                                                                            | Complexity                                                                                                                                                                                        |
| ----------------- | ---------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Shape             | `List::prepend`, `List::reverse`, `List::length`, `List::isEmpty`                        | Prepend and empty check `O(1)`; reverse and length `O(n)`                                                                                                                                         |
| Safe access       | `List::head`, `List::tail`, `List::last`, `List::init`, `List::at`                       | Head/tail `O(1)`; last/init `O(n)`; indexing `O(min(n, index))`                                                                                                                                   |
| Slicing           | `List::take`, `List::drop`, `List::splitAt`                                              | `O(min(n, count))`, plus produced prefix allocation                                                                                                                                               |
| Combining         | `List::append`, `List::concat`, `List::repeat`, `List::intersperse`, `List::intercalate` | Append is `O(n)` in its left input; concat is linear in fragment count plus total elements; repeat and intersperse are linear in output size; intercalate also counts inserted separator elements |
| Transforming      | `List::map`, `List::filter`, `List::filterMap`, `List::partition`                        | `O(n)` plus callback work                                                                                                                                                                         |
| Folding           | `List::foldLeft`, `List::foldRight`, `List::scanLeft`                                    | `O(n)`; scan returns the initial value followed by every prefix result                                                                                                                            |
| Predicates/search | `List::any`, `List::all`, `List::contains`, `List::find`, `List::findIndex`              | `O(n)` worst case with short-circuiting; `List::contains` requires `Equatable`                                                                                                                    |
| Pair/list views   | `List::zip`, `List::unzip`, `List::indexed`                                              | `O(n)` in the traversed/produced length                                                                                                                                                           |
| Normalization     | `List::distinct`, `List::group`, `List::groupBy`                                         | Distinct is `O(n^2)`; adjacent grouping is `O(n)`                                                                                                                                                 |
| Ordering          | `List::minimum`, `List::maximum`, `List::sort`, `List::sortBy`                           | Min/max `O(n)` and return `Nothing` on empty input; stable sorting `O(n log n)`                                                                                                                   |

## Maybe and Result

`Maybe(a)` is `Nothing | Just a`. Its public helpers are `Maybe::map`,
`Maybe::andThen`, `Maybe::withDefault`, `Maybe::orElse`, `Maybe::filter`,
`Maybe::isJust`, `Maybe::isNothing`, `Maybe::toList`, and `Maybe::fromList`.
`Maybe::fromList` returns the first value and ignores the remaining tail.

`Result(e, a)` is `Err e | Ok a`. Its public helpers are `Result::map`,
`Result::mapError`, `Result::andThen`, `Result::recover`, `Result::withDefault`,
`Result::isOk`, `Result::isErr`, `Result::toMaybe`, `Result::errorToMaybe`, and
`Result::fromMaybe`.

These operations are `O(1)` aside from a callback they invoke. Mapping and
chaining preserve the branch they do not target; defaults do not evaluate a
conversion of the absent/error branch.

## NonEmpty

`NonEmpty(a)` is publicly represented as `NonEmpty a [a]`. Construct it with
`NonEmpty::singleton` or convert a list with `NonEmpty::fromList`, which returns
`Nothing` only for `[]`. `NonEmpty::head` and `NonEmpty::tail` are total.

The module also exports `NonEmpty::toList`, `NonEmpty::last`, `NonEmpty::prepend`,
`NonEmpty::appendList`, `NonEmpty::map`, `NonEmpty::length`, `NonEmpty::foldLeft`, and
`NonEmpty::foldRight`. Head/tail/singleton are `O(1)`; traversal, conversion,
mapping, length, last, append, and folds are `O(n)`.

## Dictionary

`Dictionary(k, v)` is an insertion-ordered association structure requiring
`Equatable(k)` only for key operations. A new key is appended. Replacing or updating
an existing key retains its original position. Duplicate keys passed to
`Dictionary::fromList` therefore keep their first position and last value.
Iteration, lists, keys, values, filters, and folds all use insertion order.

| Operation family   | Public values                                                                                                                            | Complexity                                                                                                                       |
| ------------------ | ---------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| Construction/views | `Dictionary::empty`, `Dictionary::singleton`, `Dictionary::fromList`, `Dictionary::toList`                                               | Empty/singleton/to-list `O(1)`; from-list `O(n^2)` worst case                                                                    |
| Size               | `Dictionary::size`, `Dictionary::isEmpty`                                                                                                | `O(1)`                                                                                                                           |
| Lookup             | `Dictionary::lookup`, `Dictionary::getOr`, `Dictionary::containsKey`                                                                     | `O(n)` worst case                                                                                                                |
| Updates            | `Dictionary::insert`, `Dictionary::replace`, `Dictionary::remove`, `Dictionary::update`                                                  | `O(n)`; replace returns `Nothing` when absent, remove is unchanged when absent, and update uses `Maybe` to insert/replace/remove |
| Traversal          | `Dictionary::keys`, `Dictionary::values`, `Dictionary::mapValues`, `Dictionary::filter`, `Dictionary::foldLeft`, `Dictionary::foldRight` | `O(n)` plus callback work, preserving insertion order                                                                            |

Use `Dictionary` when insertion order and `Equatable`-only keys matter. Use `Map` when
ordered traversal and logarithmic lookup are more important.

## Queue

`Queue(a)` is a persistent two-list FIFO queue. `Queue::enqueue` adds at the rear;
`Queue::peek` and `Queue::dequeue` observe the oldest value. Empty observations
return `Nothing`. Older queue values are unaffected by later operations.

The public API is `Queue::empty`, `Queue::singleton`, `Queue::fromList`,
`Queue::toList`, `Queue::size`, `Queue::isEmpty`, `Queue::enqueue`, `Queue::enqueueAll`,
`Queue::peek`, `Queue::dequeue`, `Queue::map`, `Queue::foldLeft`, and
`Queue::foldRight`.

Size and empty checks are `O(1)`. Enqueue is `O(1)`. Peek and dequeue are
amortized `O(1)` but a normalization step can be `O(n)`. From/to-list, mapping,
folding, and enqueueing `m` values are `O(n)` or `O(m)` as appropriate, and all
views/folds use FIFO order.

## Map

`Map(k, v)` is a persistent AVL tree requiring `Comparable(k)` for key operations.
Inserting an existing key replaces its value. `Map::toList`, keys, values, and
folds use ascending key order. The tree's height, ordering, and cached-size
invariants are private and checked by the test harness after generated update
traces.

| Operation family   | Public values                                                                   | Complexity                                                                                                                           |
| ------------------ | ------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| Construction/views | `Map::empty`, `Map::singleton`, `Map::fromList`, `Map::toList`                  | Empty/singleton `O(1)`; from-list `O(n log n)`; ascending list `O(n)`                                                                |
| Size               | `Map::size`, `Map::isEmpty`                                                     | `O(1)`                                                                                                                               |
| Lookup             | `Map::lookup`, `Map::getOr`, `Map::containsKey`                                 | `O(log n)`                                                                                                                           |
| Updates            | `Map::insert`, `Map::replace`, `Map::remove`, `Map::update`                     | `O(log n)`; replace returns `Nothing` when absent, remove is unchanged when absent, and update uses `Maybe` to insert/replace/remove |
| Boundaries         | `Map::minimum`, `Map::maximum`, `Map::popMinimum`, `Map::popMaximum`            | `O(log n)` and `Nothing` on empty maps; pop also returns the remaining persistent map                                                |
| Traversal          | `Map::keys`, `Map::values`, `Map::mapValues`, `Map::foldLeft`, `Map::foldRight` | `O(n)` plus callback work in ascending key order                                                                                     |
| Filtering          | `Map::filter`                                                                   | `O(n log n)` worst case because retained entries rebuild an ordered map                                                              |

## Set

`Set(a)` is a persistent ordered set backed by `Map(a, ())`. Duplicate inserts
do not increase its size. `Set::toList` and folds use ascending value order.

The public API is `Set::empty`, `Set::singleton`, `Set::fromList`, `Set::toList`,
`Set::size`, `Set::isEmpty`, `Set::contains`, `Set::insert`, `Set::remove`, `Set::union`,
`Set::intersection`, `Set::difference`, `Set::isSubset`, `Set::filter`, `Set::map`,
`Set::foldLeft`, and `Set::foldRight`.

Empty/singleton and size checks are `O(1)`. Contains, insert, and remove are
`O(log n)`. From-list and value-changing `Set::map` are `O(n log n)`.
Materialization and folds are `O(n)`. Union is `O(m log(n + m))` for the
implementation's traversed right set. Subset is `O(n log m)`. Intersection and
difference are `O(n * (log n + log m))` worst case because they combine
membership checks with rebuilding; filter is `O(n log n)`.

## Char

`Char` operations use Unicode scalar values, not bytes or UTF-16 code units.
The module exports `Char::toUInt32`, total checked conversion
`Char::fromUInt32`, `Char::isAlpha`, `Char::isAlphaNum`, `Char::isDigit`,
`Char::isSpace`, `Char::isHexDigit`, `Char::isLower`, `Char::isUpper`, `Char::toLower`,
`Char::toUpper`, and `Char::isNewline`.

`Char::fromUInt32` returns `Nothing` for values outside Unicode or in the
surrogate range. Classification is Unicode-aware. Case conversion is simple,
locale-independent one-scalar mapping; it does not expand one scalar into
multiple characters. Scalar operations are logically `O(1)`.

## Text

`Text` indexes and counts Unicode scalar values. It does not implicitly
normalize text, expose bytes, or perform locale-sensitive conversion. Negative
indices return `Nothing`; negative take/drop/slice counts clamp to zero. Empty
needles match at index zero. Splitting on an empty delimiter produces one text
value per scalar, while replacing an empty needle leaves the input unchanged.
Search and replacement are left-to-right and replacements do not overlap.

| Operation family  | Public values                                                                                                                                           | Complexity                                                                                                                                 |
| ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| Shape/traversal   | `Text::empty`, `Text::length`, `Text::isEmpty`, `Text::uncons`, `Text::at`, `Text::take`, `Text::drop`, `Text::slice`, `Text::toChars`, `Text::reverse` | Empty check/uncons are constant at the API level; indexed/sliced/traversal operations are `O(n)` worst case                                |
| Construction      | `Text::append`, `Text::appendChar`, `Text::fromChars`, `Text::repeat`, `Text::concat`, `Text::join`                                                     | Linear in traversed input plus produced output; `Text::concat`/`Text::join` avoid repeated pairwise append chains                          |
| Predicates/search | `Text::startsWith`, `Text::endsWith`, `Text::contains`, `Text::find`                                                                                    | Prefix `O(m)`; the others are linear scans with naive matching, `O(n * m)` worst case                                                      |
| Splitting         | `Text::split`, `Text::lines`, `Text::words`                                                                                                             | Split is `O(n * m)` worst case; lines/words are `O(n)`. Lines accept LF, CRLF, and CR; words use Unicode whitespace and discard empty runs |
| Replacement       | `Text::replaceAll`                                                                                                                                      | Left-to-right, non-overlapping, `O(n * m + k)` worst case                                                                                  |
| Cleanup           | `Text::trim`, `Text::trimStart`, `Text::trimEnd`, `Text::padLeft`, `Text::padRight`                                                                     | `O(n + k)` and Unicode-whitespace-aware; padding never truncates text already at or beyond the requested scalar width                      |

## IOError and IO

`IOErrorCategory` publishes `NotFound`, `PermissionDenied`, `AlreadyExists`,
`InvalidData`, `ResourceExhausted`, `Interrupted`, `Unsupported`, and `Other`.
`IOError` publishes `IOError category maybePath message`. These stable Jazz
values hide Haskell exceptions, OS error numbers, and future native-runtime
details.

`IO` exports `readText!`, `writeText!`, `readStdin!`, `writeStdout!`,
`writeStderr!`, `arguments!`, and `exit!`. Recoverable file and stream
operations return `Result(IOError, a)`, use strict UTF-8, and attach paths
only to file operations. `arguments!` preserves process argument order.
`exit!` terminates through the installed runtime host. I/O cost is host- and
payload-dependent and is not covered by collection complexity promises.

## Verification and performance

The combined standard-library behavioral and invariant suite is
`stdlib-spec`. Production-shaped uses live in the shared
[`programs` corpus](../../programs/README.md). Deterministic runtime budgets,
recorded machine benchmarks, GHC compiler profiling, and Jazz semantic flame
graphs are documented in the [performance guide](../../PERFORMANCE.md).

The generic Prelude methods `map`, `foldLeft`, `foldRight`, and `combine` use
collection instances supplied by imports. The explicit-import `Reduce` module
provides a safe seedless fold returning Maybe.
