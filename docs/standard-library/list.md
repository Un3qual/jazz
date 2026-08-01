---
title: List
description: Use total list access, transformation, folds, grouping, and stable sorting.
sidebar_position: 3
---

# List

Import `List` for the full `[a]` API. Empty or out-of-range queries use
`Maybe`; negative counts clamp to zero; `listAny []` is `False`; and
`listAll []` is `True`. Transformations preserve input order.

| Family        | Public values                                                                  | Complexity and behavior                                                                                                                                                                           |
| ------------- | ------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Shape         | `listPrepend`, `listReverse`, `listLength`, `listIsEmpty`                      | Prepend and empty check `O(1)`; reverse and length `O(n)`                                                                                                                                         |
| Safe access   | `listHead`, `listTail`, `listLast`, `listInit`, `listAt`                       | Head/tail `O(1)`; last/init `O(n)`; index `O(min(n, index))`                                                                                                                                      |
| Slicing       | `listTake`, `listDrop`, `listSplitAt`                                          | `O(min(n, count))` plus produced prefix allocation                                                                                                                                                |
| Combining     | `listAppend`, `listConcat`, `listRepeat`, `listIntersperse`, `listIntercalate` | Append is `O(n)` in its left input; concat is linear in fragment count plus total elements; repeat and intersperse are linear in output size; intercalate also counts inserted separator elements |
| Transforming  | `listMap`, `listFilter`, `listFilterMap`, `listPartition`                      | `O(n)` plus callback work                                                                                                                                                                         |
| Folding       | `listFoldLeft`, `listFoldRight`, `listScanLeft`                                | `O(n)`; scan includes the initial value and every prefix result                                                                                                                                   |
| Search        | `listAny`, `listAll`, `listContains`, `listFind`, `listFindIndex`              | `O(n)` worst case with short-circuiting; contains requires `Eq`                                                                                                                                   |
| Pair views    | `listZip`, `listUnzip`, `listIndexed`                                          | `O(n)` in traversed or produced length                                                                                                                                                            |
| Normalization | `listDistinct`, `listGroup`, `listGroupBy`                                     | Distinct is `O(n^2)`; adjacent grouping is `O(n)`                                                                                                                                                 |
| Ordering      | `listMinimum`, `listMaximum`, `listSort`, `listSortBy`                         | Min/max are `O(n)` and return `Nothing` on empty; stable sorting is `O(n log n)`                                                                                                                  |

`listZip` stops at the shorter input. `listIndexed` starts at zero.
`listDistinct` preserves first occurrence. `listGroup` groups adjacent equal
values, not every equal value. `listSort` and `listSortBy` are stable merge
sorts.

Fragment:

<!-- jazz-example: fragment -->

```jazz
import List.
listMap (\(item) -> item * 2) [1, 2, 3].
```

Use [Maybe and Result](maybe-result-nonempty.md) to handle total queries.
