---
title: Dictionary
description: Use an insertion-ordered persistent dictionary with Eq-only keys.
sidebar_position: 5
---

`Dictionary(k, v)` is an abstract insertion-ordered association structure.
Key operations require only `Eq(k)`. A new key is appended; replacing or
updating an existing key retains its position. Duplicate keys passed to
`dictionaryFromList` keep their first position and last value.

## Operations

| Family       | Public values                                                                                                                | Complexity and behavior                                                                                                           |
| ------------ | ---------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| Construction | `dictionaryEmpty`, `dictionarySingleton`, `dictionaryFromList`, `dictionaryToList`                                           | Empty, singleton, and to-list are `O(1)`; from-list is `O(n^2)` worst case                                                        |
| Size         | `dictionarySize`, `dictionaryIsEmpty`                                                                                        | `O(1)`                                                                                                                            |
| Lookup       | `dictionaryLookup`, `dictionaryGetOr`, `dictionaryContainsKey`                                                               | `O(n)` worst case                                                                                                                 |
| Updates      | `dictionaryInsert`, `dictionaryReplace`, `dictionaryRemove`, `dictionaryUpdate`                                              | `O(n)`; replace returns `Nothing` when absent; remove is unchanged when absent; update uses `Maybe` to insert, replace, or remove |
| Traversal    | `dictionaryKeys`, `dictionaryValues`, `dictionaryMapValues`, `dictionaryFilter`, `dictionaryFoldLeft`, `dictionaryFoldRight` | `O(n)` plus callbacks, preserving insertion order                                                                                 |

The representation and constructor are private. Use Dictionary when insertion
order and `Eq`-only keys matter. Use [Map](map.md) when ascending order
and logarithmic lookup are more important.
