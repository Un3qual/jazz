# Jazz standard library

This directory is the user-facing, Jazz-authored standard library for the
active `jazz-next` compiler. `Prelude.jz` is bundled automatically unless the
compiler is run with `--no-prelude`; every other module requires an explicit
`import Module.` declaration.

The library favors task-oriented names such as `textJoin`, `listFoldLeft`, and
`dictionaryUpdate`. It does not add category-theory-named abstractions merely
to mirror another language's library. Most implementation code is Jazz. The
small `__kernel_*` substrate supplies primitive scalar, text, numeric, and host
operations that cannot yet be expressed in Jazz; those names are private.

## Module overview

| Module       | Public abstraction                    | Primary purpose                                                                      |
| ------------ | ------------------------------------- | ------------------------------------------------------------------------------------ |
| `Prelude`    | `Ordering(..)` and capability classes | Implicit scalar capabilities, numeric conversions, and minimal compatibility helpers |
| `List`       | Built-in `[a]`                        | Total list queries, transformations, folds, search, grouping, and stable sorting     |
| `Maybe`      | `Maybe(..)`                           | Optional values and branch-preserving transformations                                |
| `Result`     | `Result(..)`                          | Recoverable success/error values and transformations                                 |
| `NonEmpty`   | `NonEmpty(..)`                        | A list shape that statically contains at least one value                             |
| `Dictionary` | Abstract `Dictionary(k, v)`           | Insertion-ordered lookup by `Eq` keys                                                |
| `Queue`      | Abstract `Queue(a)`                   | Persistent first-in, first-out traversal                                             |
| `Map`        | Abstract `Map(k, v)`                  | Persistent ordered lookup by `Ord` keys                                              |
| `Set`        | Abstract `Set(a)`                     | Persistent ordered unique values and set operations                                  |
| `Char`       | Built-in `Char`                       | Unicode scalar conversion, classification, and simple case mapping                   |
| `Text`       | Built-in `Text`                       | Unicode-scalar text construction, traversal, search, splitting, and cleanup          |
| `IOError`    | `IOErrorCategory` and `IOError`       | Stable, platform-neutral host-I/O failure data                                       |
| `IO`         | Host operations ending in `!`         | Strict UTF-8 files and streams, arguments, and process exit                          |

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
- `Eq(a)` with `equals`, and `Ord(a)` with `compare`;
- marker capabilities `Num(a)`, `Integral(a)`, and `Fractional(a)`;
- `Showable(a)` with `show`, and `Default(a)` with `defaultValue`.

Concrete instances cover the built-in scalar and numeric types appropriate to
each capability. `Text` ordering is lexicographic by Unicode scalar value.
`Char` ordering uses its scalar value. `show` produces the same stable value
syntax used by runtime rendering, while defaults are zero-like values.

The prelude also exposes `map`, `filter`, `hd`, `tl`, `print!`, target-named
numeric conversions from `toInt8` through `toFloat64`, and the aliases `toInt`
and `toFloat`. The richer `List` API should be preferred in new Jazz-authored
library code. Kernel bridge names are implementation details and are visible
only through explicit no-prelude compiler entry points.

## List

All list APIs are total. Empty or out-of-range queries use `Maybe`, negative
counts clamp to zero, `listAny []` is `False`, and `listAll []` is `True`.
Transformations preserve input order. `listZip` stops at the shorter input,
`listIndexed` starts at zero, `listDistinct` preserves first occurrence, and
`listGroup` groups adjacent equal values rather than every equal value.
`listSort` and `listSortBy` are stable merge sorts.

| Operation family  | Public values                                                                  | Complexity                                                                                                                                |
| ----------------- | ------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------- |
| Shape             | `listPrepend`, `listReverse`, `listLength`, `listIsEmpty`                      | Prepend and empty check `O(1)`; reverse and length `O(n)`                                                                                 |
| Safe access       | `listHead`, `listTail`, `listLast`, `listInit`, `listAt`                       | Head/tail `O(1)`; last/init `O(n)`; indexing `O(min(n, index))`                                                                           |
| Slicing           | `listTake`, `listDrop`, `listSplitAt`                                          | `O(min(n, count))`, plus produced prefix allocation                                                                                       |
| Combining         | `listAppend`, `listConcat`, `listRepeat`, `listIntersperse`, `listIntercalate` | Append `O(n)` in its left input; repeat/intersperse `O(k)`; current left-folded concat/intercalate can be quadratic across many fragments |
| Transforming      | `listMap`, `listFilter`, `listFilterMap`, `listPartition`                      | `O(n)` plus callback work                                                                                                                 |
| Folding           | `listFoldLeft`, `listFoldRight`, `listScanLeft`                                | `O(n)`; scan returns the initial value followed by every prefix result                                                                    |
| Predicates/search | `listAny`, `listAll`, `listContains`, `listFind`, `listFindIndex`              | `O(n)` worst case with short-circuiting; `listContains` requires `Eq`                                                                     |
| Pair/list views   | `listZip`, `listUnzip`, `listIndexed`                                          | `O(n)` in the traversed/produced length                                                                                                   |
| Normalization     | `listDistinct`, `listGroup`, `listGroupBy`                                     | Distinct is `O(n^2)`; adjacent grouping is `O(n)`                                                                                         |
| Ordering          | `listMinimum`, `listMaximum`, `listSort`, `listSortBy`                         | Min/max `O(n)` and return `Nothing` on empty input; stable sorting `O(n log n)`                                                           |

## Maybe and Result

`Maybe(a)` is `Nothing | Just a`. Its public helpers are `maybeMap`,
`maybeAndThen`, `maybeWithDefault`, `maybeOrElse`, `maybeFilter`,
`maybeIsJust`, `maybeIsNothing`, `maybeToList`, and `maybeFromList`.
`maybeFromList` returns the first value and ignores the remaining tail.

`Result(e, a)` is `Err e | Ok a`. Its public helpers are `resultMap`,
`resultMapError`, `resultAndThen`, `resultRecover`, `resultWithDefault`,
`resultIsOk`, `resultIsErr`, `resultToMaybe`, `resultErrorToMaybe`, and
`resultFromMaybe`.

These operations are `O(1)` aside from a callback they invoke. Mapping and
chaining preserve the branch they do not target; defaults do not evaluate a
conversion of the absent/error branch.

## NonEmpty

`NonEmpty(a)` is publicly represented as `NonEmpty a [a]`. Construct it with
`nonEmptySingleton` or convert a list with `nonEmptyFromList`, which returns
`Nothing` only for `[]`. `nonEmptyHead` and `nonEmptyTail` are total.

The module also exports `nonEmptyToList`, `nonEmptyLast`, `nonEmptyPrepend`,
`nonEmptyAppendList`, `nonEmptyMap`, `nonEmptyLength`, `nonEmptyFoldLeft`, and
`nonEmptyFoldRight`. Head/tail/singleton are `O(1)`; traversal, conversion,
mapping, length, last, append, and folds are `O(n)`.

## Dictionary

`Dictionary(k, v)` is an insertion-ordered association structure requiring
`Eq(k)` only for key operations. A new key is appended. Replacing or updating
an existing key retains its original position. Duplicate keys passed to
`dictionaryFromList` therefore keep their first position and last value.
Iteration, lists, keys, values, filters, and folds all use insertion order.

| Operation family   | Public values                                                                                                                | Complexity                                                                                                                       |
| ------------------ | ---------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| Construction/views | `dictionaryEmpty`, `dictionarySingleton`, `dictionaryFromList`, `dictionaryToList`                                           | Empty/singleton/to-list `O(1)`; from-list `O(n^2)` worst case                                                                    |
| Size               | `dictionarySize`, `dictionaryIsEmpty`                                                                                        | `O(1)`                                                                                                                           |
| Lookup             | `dictionaryLookup`, `dictionaryGetOr`, `dictionaryContainsKey`                                                               | `O(n)` worst case                                                                                                                |
| Updates            | `dictionaryInsert`, `dictionaryReplace`, `dictionaryRemove`, `dictionaryUpdate`                                              | `O(n)`; replace returns `Nothing` when absent, remove is unchanged when absent, and update uses `Maybe` to insert/replace/remove |
| Traversal          | `dictionaryKeys`, `dictionaryValues`, `dictionaryMapValues`, `dictionaryFilter`, `dictionaryFoldLeft`, `dictionaryFoldRight` | `O(n)` plus callback work, preserving insertion order                                                                            |

Use `Dictionary` when insertion order and `Eq`-only keys matter. Use `Map` when
ordered traversal and logarithmic lookup are more important.

## Queue

`Queue(a)` is a persistent two-list FIFO queue. `queueEnqueue` adds at the rear;
`queuePeek` and `queueDequeue` observe the oldest value. Empty observations
return `Nothing`. Older queue values are unaffected by later operations.

The public API is `queueEmpty`, `queueSingleton`, `queueFromList`,
`queueToList`, `queueSize`, `queueIsEmpty`, `queueEnqueue`, `queueEnqueueAll`,
`queuePeek`, `queueDequeue`, `queueMap`, `queueFoldLeft`, and
`queueFoldRight`.

Size and empty checks are `O(1)`. Enqueue is `O(1)`. Peek and dequeue are
amortized `O(1)` but a normalization step can be `O(n)`. From/to-list, mapping,
folding, and enqueueing `m` values are `O(n)` or `O(m)` as appropriate, and all
views/folds use FIFO order.

## Map

`Map(k, v)` is a persistent AVL tree requiring `Ord(k)` for key operations.
Inserting an existing key replaces its value. `mapToList`, keys, values, and
folds use ascending key order. The tree's height, ordering, and cached-size
invariants are private and checked by the test harness after generated update
traces.

| Operation family   | Public values                                                         | Complexity                                                                                                                           |
| ------------------ | --------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------ |
| Construction/views | `mapEmpty`, `mapSingleton`, `mapFromList`, `mapToList`                | Empty/singleton `O(1)`; from-list `O(n log n)`; ascending list `O(n)`                                                                |
| Size               | `mapSize`, `mapIsEmpty`                                               | `O(1)`                                                                                                                               |
| Lookup             | `mapLookup`, `mapGetOr`, `mapContainsKey`                             | `O(log n)`                                                                                                                           |
| Updates            | `mapInsert`, `mapReplace`, `mapRemove`, `mapUpdate`                   | `O(log n)`; replace returns `Nothing` when absent, remove is unchanged when absent, and update uses `Maybe` to insert/replace/remove |
| Boundaries         | `mapMinimum`, `mapMaximum`, `mapPopMinimum`, `mapPopMaximum`          | `O(log n)` and `Nothing` on empty maps; pop also returns the remaining persistent map                                                |
| Traversal          | `mapKeys`, `mapValues`, `mapMapValues`, `mapFoldLeft`, `mapFoldRight` | `O(n)` plus callback work in ascending key order                                                                                     |
| Filtering          | `mapFilter`                                                           | `O(n log n)` worst case because retained entries rebuild an ordered map                                                              |

## Set

`Set(a)` is a persistent ordered set backed by `Map(a, ())`. Duplicate inserts
do not increase its size. `setToList` and folds use ascending value order.

The public API is `setEmpty`, `setSingleton`, `setFromList`, `setToList`,
`setSize`, `setIsEmpty`, `setContains`, `setInsert`, `setRemove`, `setUnion`,
`setIntersection`, `setDifference`, `setIsSubset`, `setFilter`, `setMap`,
`setFoldLeft`, and `setFoldRight`.

Empty/singleton and size checks are `O(1)`. Contains, insert, and remove are
`O(log n)`. From-list and value-changing `setMap` are `O(n log n)`.
Materialization and folds are `O(n)`. Union is `O(m log(n + m))` for the
implementation's traversed right set. Subset is `O(n log m)`. Intersection and
difference are `O(n * (log n + log m))` worst case because they combine
membership checks with rebuilding; filter is `O(n log n)`.

## Char

`Char` operations use Unicode scalar values, not bytes or UTF-16 code units.
The module exports `charToUInt32`, total checked conversion
`charFromUInt32`, `charIsAlpha`, `charIsAlphaNum`, `charIsDigit`,
`charIsSpace`, `charIsHexDigit`, `charIsLower`, `charIsUpper`, `charToLower`,
`charToUpper`, and `charIsNewline`.

`charFromUInt32` returns `Nothing` for values outside Unicode or in the
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

| Operation family  | Public values                                                                                                                       | Complexity                                                                                                                                 |
| ----------------- | ----------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| Shape/traversal   | `textEmpty`, `textLength`, `textIsEmpty`, `textUncons`, `textAt`, `textTake`, `textDrop`, `textSlice`, `textToChars`, `textReverse` | Empty check/uncons are constant at the API level; indexed/sliced/traversal operations are `O(n)` worst case                                |
| Construction      | `textAppend`, `textAppendChar`, `textFromChars`, `textRepeat`, `textConcat`, `textJoin`                                             | Linear in traversed input plus produced output; `textConcat`/`textJoin` avoid repeated pairwise append chains                              |
| Predicates/search | `textStartsWith`, `textEndsWith`, `textContains`, `textFind`                                                                        | Prefix `O(m)`; the others are linear scans with naive matching, `O(n * m)` worst case                                                      |
| Splitting         | `textSplit`, `textLines`, `textWords`                                                                                               | Split is `O(n * m)` worst case; lines/words are `O(n)`. Lines accept LF, CRLF, and CR; words use Unicode whitespace and discard empty runs |
| Replacement       | `textReplaceAll`                                                                                                                    | Left-to-right, non-overlapping, `O(n * m + k)` worst case                                                                                  |
| Cleanup           | `textTrim`, `textTrimStart`, `textTrimEnd`, `textPadLeft`, `textPadRight`                                                           | `O(n + k)` and Unicode-whitespace-aware; padding never truncates text already at or beyond the requested scalar width                      |

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
