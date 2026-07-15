# Jazz-Next Constructor Exports and Broad Standard Library Design

## Status

Approved in discussion on `2026-07-15`.

This design covers Batch 5 of
[`docs/jazz-improvement-backlog.md`](../../jazz-improvement-backlog.md): item 5,
concise data-constructor export groups, and item 8, broader Jazz-authored
standard-library APIs. It does not replace the separate Jazz-authored parser
curation target in [`docs/execution/queue.md`](../../execution/queue.md).

## Summary

Jazz module headers gain grouped selectors for exporting a data type with all
or selected constructors while retaining abstract types and the existing
per-namespace selectors. The parser preserves grouped selectors and their
spans; the resolver validates local data-declaration ownership and expands the
groups into the existing flat typed export inventory. Compiler interfaces and
runtime publication continue consuming that one inventory.

The Jazz-authored standard library grows from the current bootstrap-oriented
modules into a broader functional library. Existing `List`, `Maybe`, `Result`,
`Text`, `Char`, and prelude capabilities gain practical operations. New
`NonEmpty`, `Dictionary`, `Map`, `Set`, and `Queue` abstractions use immutable,
persistent representations with honest complexity guarantees. Algorithms are
written in Jazz wherever the current language can express them. Private kernel
bridges remain limited to representation-level work that Jazz cannot perform
safely or efficiently, notably single-allocation text concatenation and
primitive value rendering, plus Unicode scalar classification and simple case
mapping already owned by the host text boundary.

## Goals

- Keep public module declarations readable as datatype APIs grow.
- Preserve the ability to export an abstract type or only selected
  constructors.
- Reuse the existing typed export inventory rather than creating a parallel
  compiler/runtime export model.
- Provide a broad, approachable functional standard library for application
  and compiler code.
- Prefer plain, task-oriented public names over category-theory-heavy terms.
- Implement collection algorithms and abstractions in Jazz wherever practical.
- Make partial collection lookups total through `Maybe`.
- Provide immutable persistent collections with documented ordering and
  complexity behavior.
- Dogfood the new APIs in the Jazz-authored lexer and production-shaped corpus.
- Use the existing benchmark and observation facilities as performance
  evidence without turning noisy timing changes into fixed pass/fail gates.

## Non-Goals

- Re-exports, wildcard imports, hiding imports, or import-side namespace
  prefixes.
- A second function-declaration syntax or any change to compact lambda
  lowering.
- Mutable collections or Java-style builder objects.
- Hash maps or hash sets before Jazz has an accepted hashing contract.
- Finger trees, real-time queues, or worst-case constant-time persistent queue
  operations.
- Superclasses, default class methods, user-visible dictionaries, or a broad
  capability-system redesign.
- Filling every marker numeric capability with methods merely to mirror
  Haskell.
- Locale-sensitive casing or full multi-scalar Unicode case folding.
- Bytes, binary I/O, packages, LLVM lowering, or native-runtime collection
  implementations.
- Making all standard-library modules implicit prelude contents.

## Constructor-Group Export Syntax

### Surface Forms

Module export lists accept these three type forms:

```jazz
type Maybe
type Maybe(..)
type Maybe(Nothing, Just)
```

Their meanings are:

- `type T` exports only the type identity. Its constructors remain private.
- `type T(..)` exports the type identity and every constructor owned by that
  local data declaration.
- `type T(C1, C2)` exports the type identity and exactly the named constructors
  owned by that local data declaration.

For example:

```jazz
module Maybe (type Maybe(..), value maybeMap) {
  data Maybe a = Nothing | Just a.
}
```

The `type` prefix remains appropriate because a `data` declaration introduces
a named type plus constructor bindings. `data` is declaration syntax, not a
separate exported namespace, so grouped exports do not introduce a `data`
selector keyword.

### Compatibility

Existing bare selectors and the `value`, `constructor`, `type`, and `class`
prefixes remain accepted. An omitted export list continues to publish every
owned declaration, and `()` continues to publish nothing. Existing source may
continue spelling every constructor separately.

Grouped selectors do not change import syntax. They only determine the
exporting module's public typed inventory.

### Grammar and Validation

The constructor group is valid only after a prefixed `type` selector. An empty
group such as `type T()` is rejected; `type T` already expresses an abstract
type. A selected group contains one or more comma-separated constructor names.
`..` is the complete-constructor marker only in this module-export context.

Validation follows these rules:

- the named type must be owned by the current module;
- every selected constructor must be owned by that exact data declaration;
- imported declarations are not eligible;
- duplicate constructor names inside one group are parser errors;
- unknown types, unknown constructors, and wrong-owner constructors are
  resolver errors;
- overlapping selectors such as `type T(..), constructor C` are accepted and
  deduplicated by the typed inventory; and
- the order of selectors does not affect the published inventory.

Exact syntax and duplicate failures use `E0001`. Ownership, unknown-name, and
wrong-namespace failures retain `E4015`. The parser and lowering path preserve
the span of the grouped type and each selected constructor so diagnostics can
label the offending item instead of the whole module header.

### Internal Representation

The source-level selector model distinguishes an ordinary namespace/name
selector from a type selector carrying no constructors, all constructors, or a
selected non-empty constructor list. This structure survives parsing and
lowering long enough for module resolution to compare the group with locally
owned data-declaration metadata.

After validation, the resolver expands a group into ordinary `TypeNamespace`
and `ConstructorNamespace` entries in `ModuleExportInventory`. No grouped
selector reaches module compilation, interface publication, import filtering,
or runtime publication. The existing inventory remains the sole downstream
authority.

The TextMate grammar recognizes the grouped forms only inside module export
lists and scopes the type name separately from constructor names.

## Standard-Library Ownership and Naming

All new source lives under `jazz-next/jazz/stdlib/`. Standard-library modules
may import other standard-library modules and may not import modules under
`jazz-next/jazz/compiler/`. Compiler modules may import the standard library.
The existing repository audit continues enforcing this direction.

The expanded modules remain explicit imports rather than becoming one enormous
implicit prelude. Prelude capabilities and their primitive implementations stay
in `Prelude.jz`; reusable datatype and collection APIs stay in focused modules.

Public functions retain module-oriented prefixes so unqualified imports remain
usable without immediately colliding:

- `listContains`, not a second unqualified `contains`;
- `maybeWithDefault`, not a generic `withDefault`;
- `dictionaryLookup`, not a generic `lookup`; and
- `queueEnqueue`, not a generic `enqueue`.

The public vocabulary deliberately avoids adding `Semigroup`, `Monoid`, or
similar abstractions merely to match another language. Existing `Foldable`
vocabulary is not categorically forbidden, but this batch does not need a new
fold capability: concrete collection modules expose clear fold operations
directly.

## Prelude Capabilities and Ordering

The prelude defines:

```jazz
data Ordering = LT | EQ | GT.
```

`Ord(a)` gains `compare`, which returns `Ordering`. Existing primitive numeric
`Ord` implementations receive method bodies. `Char` gains scalar ordering and
`Text` gains lexicographic Unicode-scalar ordering. `Ord` does not inherit from
`Eq`; an ordered collection uses only `compare`, and `EQ` identifies one key.

`Showable(a)` gains `show`, returning deterministic source-readable `Text`.
Primitive instances use a private runtime rendering bridge where Jazz cannot
otherwise format the primitive representation. `Text` and `Char` rendering
retains deterministic escaping. `print!` remains the effectful output surface;
`show` only produces a value.

`Default(a)` gains `defaultValue`. Current primitive default instances receive
explicit Jazz method bodies, with empty `Text` and the null scalar added where
appropriate. No implicit defaulting behavior is added to expressions or
function calls.

`Eq::equals` remains the equality operation used by equality-based library
functions. `Num`, `Integral`, and `Fractional` are not expanded solely to make
the capability list look symmetrical.

## Foundational Modules

### `List`

`List.jz` retains `listPrepend`, `listReverse`, and `listLength` and adds these
public families:

- shape and safe access: `listIsEmpty`, `listHead`, `listTail`, `listLast`,
  `listInit`, and `listAt`;
- slicing and construction: `listTake`, `listDrop`, `listSplitAt`,
  `listAppend`, `listConcat`, `listRepeat`, `listIntersperse`, and
  `listIntercalate`;
- transformation: `listMap`, `listFilter`, `listFilterMap`, and
  `listPartition`;
- aggregation: `listFoldLeft`, `listFoldRight`, and `listScanLeft`;
- predicates and search: `listAny`, `listAll`, `listContains`, `listFind`, and
  `listFindIndex`;
- combination: `listZip`, `listUnzip`, and `listIndexed`;
- equality-based normalization: `listDistinct`, `listGroup`, and
  `listGroupBy`; and
- ordering: `listMinimum`, `listMaximum`, `listSort`, and `listSortBy`.

`listHead`, `listTail`, `listLast`, `listInit`, `listAt`, `listMinimum`, and
`listMaximum` return `Maybe`. Negative indexes return `Nothing`. Nonpositive
take and repeat counts return an empty list; nonpositive drop counts return the
original list. Counts beyond the list length clamp to the list boundary.
Sorting is stable. `listFoldRight` may be implemented through reverse plus a
stack-safe left fold rather than consuming one host stack frame per item.

The existing unprefixed `map`, `filter`, `hd`, and `tl` remain compatible.
New examples and modules prefer the prefixed total APIs.

### `Maybe`

`Maybe.jz` exports `type Maybe(..)` and adds `maybeMap`, `maybeAndThen`,
`maybeWithDefault`, `maybeOrElse`, `maybeFilter`, `maybeIsJust`,
`maybeIsNothing`, `maybeToList`, and `maybeFromList`.

The name `maybeAndThen` is preferred to a category-oriented bind name. It
accepts a function that already returns `Maybe` and avoids nested values.

### `Result`

`Result.jz` exports `type Result(..)` and adds `resultMap`, `resultMapError`,
`resultAndThen`, `resultRecover`, `resultWithDefault`, `resultIsOk`,
`resultIsErr`, `resultToMaybe`, `resultErrorToMaybe`, and `resultFromMaybe`.

`resultRecover` transforms an error into a replacement result. The conversion
from `Maybe` requires the caller to supply the error value used for `Nothing`.

### `NonEmpty`

`NonEmpty.jz` defines `NonEmpty(a)` as one head value plus a list tail and
exports its constructor through `type NonEmpty(..)`. Its API includes
`nonEmptySingleton`, `nonEmptyFromList`, `nonEmptyToList`, `nonEmptyHead`,
`nonEmptyTail`, `nonEmptyLast`, `nonEmptyPrepend`, `nonEmptyAppendList`,
`nonEmptyMap`, `nonEmptyLength`, `nonEmptyFoldLeft`, and
`nonEmptyFoldRight`.

Only conversion from an ordinary list is fallible. Operations on an existing
`NonEmpty` value remain total.

## Persistent Collections

### `Dictionary`

`Dictionary(k, v)` is an abstract equality-based collection backed initially
by a Jazz association list. The constructor is private. Public operations
include:

- `dictionaryEmpty`, `dictionarySingleton`, `dictionaryFromList`, and
  `dictionaryToList`;
- `dictionarySize` and `dictionaryIsEmpty`;
- `dictionaryLookup`, `dictionaryGetOr`, and `dictionaryContainsKey`;
- `dictionaryInsert`, `dictionaryReplace`, `dictionaryRemove`, and
  `dictionaryUpdate`;
- `dictionaryKeys`, `dictionaryValues`, `dictionaryMapValues`, and
  `dictionaryFilter`; and
- `dictionaryFoldLeft` and `dictionaryFoldRight`.

Key comparison uses `Eq::equals`. The first insertion fixes a key's iteration
position. Reinserting the key replaces its value without moving it.
`dictionaryFromList` therefore keeps the first position and last value for
duplicate keys. Conversion, keys, values, filtering, and folds preserve that
deterministic insertion order.

Abstract collection functions take the collection value first, followed by
the key, value, or callback arguments. `dictionaryReplace` returns `Nothing`
when the key is absent. `dictionaryUpdate` passes the current `Maybe(v)` to a
callback; returning `Nothing` removes an existing key or leaves an absent key
absent, while returning `Just` inserts or replaces the value.

Lookup and update operations are documented as `O(n)`. Updates rebuild only
the affected list prefix and structurally share the untouched suffix where the
operation permits it. The abstract type leaves room for a future internal
representation change without changing user code.

### `Map`

`Map(k, v)` is an abstract persistent AVL tree. Nodes store subtree height,
left subtree, key, value, and right subtree. Inserts and removals rebuild only
the search path and share untouched subtrees. Rotations allocate a bounded
number of replacement nodes.

Public operations include:

- `mapEmpty`, `mapSingleton`, `mapFromList`, and `mapToList`;
- `mapSize` and `mapIsEmpty`;
- `mapLookup`, `mapGetOr`, and `mapContainsKey`;
- `mapInsert`, `mapReplace`, `mapRemove`, and `mapUpdate`;
- `mapMinimum`, `mapMaximum`, `mapPopMinimum`, and `mapPopMaximum`;
- `mapKeys`, `mapValues`, `mapMapValues`, and `mapFilter`; and
- `mapFoldLeft` and `mapFoldRight`.

Keys use `Ord::compare`; `EQ` identifies an existing key. No separate `Eq`
constraint is required. Conversion and folds use ascending key order.
Duplicate keys in `mapFromList` keep the last value. Lookup, insert, remove,
minimum, and maximum operations are `O(log n)` when the supplied comparison is
a consistent total order. The compiler cannot prove user-defined comparison
transitivity, so that law is part of the `Ord` contract.

`mapReplace` and `mapUpdate` follow the same absence rules as their dictionary
counterparts. Pop operations return the selected key/value pair together with
the remaining map inside `Maybe`.

### `Set`

`Set(a)` is abstract and privately wraps `Map(a, ())`, reusing one balancing
implementation. Its public API includes `setEmpty`, `setSingleton`,
`setFromList`, `setToList`, `setSize`, `setIsEmpty`, `setContains`,
`setInsert`, `setRemove`, `setUnion`, `setIntersection`, `setDifference`,
`setIsSubset`, `setFilter`, `setMap`, `setFoldLeft`, and `setFoldRight`.

Iteration and conversion use ascending value order. `setMap` may collapse
distinct inputs when their mapped values compare as `EQ`.

### `Queue`

`Queue(a)` is abstract and stores a front list, a reversed rear list, and size.
Its public API includes `queueEmpty`, `queueSingleton`, `queueFromList`,
`queueToList`, `queueSize`, `queueIsEmpty`, `queueEnqueue`,
`queueEnqueueAll`, `queuePeek`, `queueDequeue`, `queueMap`,
`queueFoldLeft`, and `queueFoldRight`.

Enqueue is constant time. Peek and dequeue normalize the rear list when the
front is empty, giving amortized constant-time behavior along ordinary
operation sequences and an occasional linear reversal. The documentation does
not claim worst-case real-time behavior. Reusing the same unnormalized version
across many persistent branches can repeat that reversal cost.

## Text and Character APIs

### `Text`

`Text.jz` retains its current traversal and append functions and adds:

- scalar access and slicing: `textAt`, `textTake`, `textDrop`, and `textSlice`;
- conversion and construction: `textToChars`, `textReverse`, `textRepeat`,
  `textConcat`, and `textJoin`;
- search: `textStartsWith`, `textEndsWith`, `textContains`, and `textFind`;
- decomposition: `textSplit`, `textLines`, and `textWords`;
- replacement and cleanup: `textReplaceAll`, `textTrim`, `textTrimStart`, and
  `textTrimEnd`; and
- layout: `textPadLeft` and `textPadRight`.

Indexes and counts refer to Unicode scalar values, never UTF-8 code units or
bytes. Negative indexes return `Nothing`. Nonpositive take/repeat/padding
counts produce the natural empty or unchanged result, and dropping beyond the
end produces empty text.

Search and replacement proceed left-to-right with non-overlapping matches.
Splitting on empty text yields one-text-scalar fragments. Replacing an empty
search text leaves the input unchanged. `textLines` treats CRLF as one line
break. `textWords` uses the standard-library character whitespace predicate
and omits empty runs.

There is no public `TextBuilder`. Jazz code accumulates ordinary `[Text]`
fragments and finalizes them with `textConcat` or `textJoin`. Private
`__kernel_textConcat` performs one efficient final allocation for a list of
fragments. Control flow, searching, slicing, splitting, joining, and
replacement remain Jazz-authored.

### `Char`

`Char.jz` adds `charIsLower`, `charIsUpper`, `charToLower`, and `charToUpper`
to the existing classification and scalar-conversion operations. Case
conversion uses simple Unicode scalar mappings. Locale-sensitive behavior and
multi-scalar full case folding are deferred.

## Jazz-Authored Compiler Dogfooding

The hosted lexer imports and exercises the expanded library where it materially
improves clarity:

- keyword classification uses a `Dictionary(Text, CanonicalTokenKind)` rather
  than a nested conditional chain;
- operator-character classification uses `listContains`; and
- existing local recursion is replaced only when a library operation makes the
  ownership and intent clearer.

The lexer remains a compiler module and the collection implementations remain
stdlib modules. No dependency is introduced from stdlib back into compiler
source.

## Failure Model

Missing keys, empty collections, and checked indexing are normal outcomes and
use `Maybe`. Standard-library functions do not throw host exceptions for those
cases. Counts clamp according to the module-specific rules above.

The text-concatenation and primitive-rendering bridges must translate any host
failure into the existing structured runtime diagnostic boundary; host
exceptions must not escape. Pure collection invariant failures indicate a
compiler or stdlib defect and receive regression coverage rather than a public
recovery API.

## Complexity Contract

- ordinary list, `Maybe`, `Result`, and `NonEmpty` traversals are linear unless
  documented otherwise;
- stable list sorting is `O(n log n)`;
- `Dictionary` lookup and update are linear;
- balanced `Map` and `Set` lookup, insertion, and removal are `O(log n)` under
  a lawful total comparison;
- ordered map/set traversal is linear in collection size;
- queue enqueue and ordinary sequential dequeue are amortized constant time,
  subject to the persistent-branching caveat above;
- scalar text search and slicing are linear in traversed scalar values; and
- `textConcat` allocates the combined text once at the runtime representation
  boundary.

Large linear traversals use tail-recursive loops or the existing stack-safe
runtime path. AVL recursion is logarithmically bounded and need not be forced
into a tail-recursive shape at the expense of clarity.

## Verification

### Export Syntax and Publication

Coverage includes:

- abstract `type T` exports;
- `type T(..)` all-constructor exports;
- `type T(C1, C2)` selected-constructor exports;
- empty groups, malformed `..`, and duplicate group entries;
- unknown, wrong-owner, wrong-namespace, and imported-only selections;
- precise grouped-selector diagnostic spans;
- overlapping selector deduplication;
- local use of private constructors;
- downstream import visibility;
- compiler-interface filtering; and
- runtime publication of only the expanded public inventory.

### Standard Library

A Cabal-registered standard-library suite executes checked-in `.jz` fixtures
for every public operation and its boundary cases. Substantial programs remain
external Jazz files; embedded Haskell strings are reserved for focused syntax,
whitespace, source-span, and diagnostic tests according to the existing
`MultilineStrings` policy.

Collection verification includes:

- large-list stack-safety cases;
- deterministic dictionary duplicate and iteration behavior;
- AVL sorted traversal, stored heights, balance factors, and replacement and
  removal cases;
- persistence checks proving older collection values remain usable;
- randomized operation traces compared with simple Haskell reference models;
- set algebra laws over finite generated fixtures; and
- queue FIFO behavior, normalization boundaries, and persistent-version
  checks.

Text verification covers Unicode scalar indexing, empty and out-of-range
inputs, CR/LF/CRLF lines, whitespace words, empty delimiters, non-overlapping
replacement, casing boundaries, escaping, and large fragment concatenation.

### Integration and Performance Evidence

The production-shaped corpus gains programs for word frequency, sorted
indexing, queue traversal, text processing, and cross-module abstract
collections. The Jazz lexer parity suite continues to pass after dogfooding the
new APIs.

The existing benchmark suite gains focused list, dictionary, map/set, queue,
and text cases. Same-machine results and runtime statistics are recorded as
evidence. They are not fixed percentage gates: a deliberate language or
correctness improvement may change performance, but the evidence must make the
change visible and explainable.

The final gate includes:

```text
cabal test --project-dir=jazz-next all --test-show-details=failures
cabal bench --project-dir=jazz-next jazz-next-bench --benchmark-options='--jazz-smoke'
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

## Documentation

The implementation updates the active explicit-export specification, Jazz
language/feature status, standard-library reference material, editor fixture,
performance guide, and improvement backlog. Historical plans remain historical
evidence and are not rewritten as current specifications.

Generated performance artifacts remain ignored. The bootstrap execution queue
continues pointing at its accepted parser-design curation target; this
improvement batch does not silently promote or replace that independent work.

## Delivery Slices

The implementation plan should keep the broad batch reviewable through
milestone commits:

1. grouped export parser, validation, inventory expansion, diagnostics, and
   editor coverage;
2. `Ordering` and useful `Ord`, `Showable`, and `Default` methods;
3. expanded `List`, `Maybe`, `Result`, and new `NonEmpty` APIs;
4. `Dictionary` and `Queue`;
5. persistent AVL `Map` and `Set`;
6. expanded `Text` and `Char` APIs plus the narrow runtime bridges;
7. lexer dogfooding, corpus programs, benchmark coverage, and active docs; and
8. full verification and backlog closeout.

Each slice uses behavioral tests first and commits independently. The plan
describes interfaces and intended changes without embedding large final code
listings.
