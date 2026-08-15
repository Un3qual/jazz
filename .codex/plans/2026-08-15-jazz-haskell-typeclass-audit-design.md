# Jazz Haskell typeclass and structural evaluation design

**Date:** 2026-08-15

**Status:** Approved design; awaiting written-design review

## Purpose

Use standard Haskell classes where they remove bespoke compiler machinery,
make existing composition laws explicit, or make phase-boundary evaluation
safer to maintain. This is a code-quality refactor of the active Haskell paths;
it does not change Jazz syntax, language semantics, diagnostics, serialized
contracts, or runtime observability.

The governing rule is deliberately stricter than "GHC can derive it": an
instance must improve the code that exists now. A lawful but unused instance is
not sufficient unless it strengthens a clear public contract with one
unambiguous interpretation.

## Audit evidence

The active `src/` and `app/` trees contain 380 `data` or `newtype`
declarations. Only five declarations are parameterized:

- `CapabilityFailure failure`;
- `RuntimeHost m`;
- `RuntimeObservationResult value`;
- `RuntimeOutcome value`; and
- `PatternCaseArmResult result`.

That shape rules out a broad `Functor`/`Foldable`/`Traversable` refactor. The
more useful opportunities are monomorphic accumulators that already expose
parallel `empty`, `merge`, or `unions` helpers, plus the manually maintained
phase-forcing matrix.

The repository currently contains:

- lawful identity and composition operations for runtime requirements, ordered
  names, pattern typing, capability facts, imported interfaces, and export
  inventories;
- ordered concatenation for explicit runtime result hints;
- a hand-maintained exhaustive `SurfaceNumericType` fixture list; and
- 1,112 lines in `Jazz.Compiler.Force`, mostly structural recursion over pure
  compiler products that Haskell's `NFData` contract already represents.

The library uses GHC 9.14.1 and `base-4.22.0.0`. `deepseq` is already pinned by
repository benchmark and test components, but is not yet a dependency of the
private compiler library.

## Instance-admission policy

An instance is accepted only when all of these hold:

1. the operation is lawful;
2. the domain has one canonical interpretation at the use site;
3. the instance deletes helpers, simplifies current call sites, strengthens a
   tested contract, or makes future structural changes fail safely; and
4. the generic class vocabulary is at least as clear as the helper it replaces.

An instance is rejected when it merely reduces characters, makes constructor
order or textual representation accidentally observable, chooses among
multiple plausible domain operations, or creates a class contract with no
current consumer.

This policy applies to `base` classes and to classes in the repository's
existing dependency set. It does not justify adding a general abstraction
library or a compiler-wide typeclass hierarchy.

## Accepted composition instances

### Runtime requirements

`RuntimeRequirements` combines a Boolean requirement with a set of runtime
services. Logical disjunction and set union are associative and have the
existing `False`/empty-set identity.

It will gain `Semigroup` and `Monoid`. `LoweredIR.Lower.Requirements` will use
`foldMap`, `mempty`, and `(<>)`; `emptyRuntimeRequirements` and
`mergeRuntimeRequirements` will be removed.

### Ordered names

`OrderedNames` represents set union while preserving the first occurrence of
each name. The existing `orderedNamesUnion` is associative for values produced
by the private constructors, and `orderedNamesEmpty` is its identity.

It will gain `Semigroup` and `Monoid`. Recursive-binding analysis will use
`mconcat`/`foldMap` and `(<>)`; the parallel empty, binary-union, and
list-union helpers will be removed. Focused tests will lock first-occurrence
order and duplicate elimination, not merely set equality.

### Pattern bindings and typing

`PatternBindings` already combines maps with left-biased union.
`PatternTyping` combines those bindings while disjoining the skip-branch flag.
Both operations are associative and have existing identities.

Both private types will gain `Semigroup` and `Monoid`. Pattern traversal will
use the standard operations and remove `emptyPatternBindings`,
`mergePatternBindings`, `emptyPatternTyping`, and `mergePatternTyping` where a
named sentinel is not clearer. Tests will preserve left-biased collision
behavior because duplicate-binder reporting depends on traversal order.

### Scope capability facts

`ScopeCapabilityFacts` has one established merge:

- left-biased union for class and method maps;
- set union for generated-equality and concrete-implementation facts; and
- ordered list concatenation for implementation methods sharing a key.

It will gain `Semigroup` and `Monoid`. The exported `mergeCapabilityFacts`
helper will be removed and internal consumers will use `(<>)`.
`emptyScopeCapabilityFacts` may remain as a domain-named alias for `mempty`
where it improves record construction and test readability.

### Imported interfaces

`ImportedInterface` uses the same left-biased import precedence across maps and
sets and delegates capability composition to `ScopeCapabilityFacts`. It will
gain `Semigroup` and `Monoid`. Module compilation will combine the ambient
interface and selected dependency interfaces with `foldMap`, removing
`mergeModuleInterfaces` and its duplicated capability-field merge.

Tests will explicitly lock ambient-versus-import and earlier-versus-later
collision precedence before call sites are rewritten.

### Module export inventories

`ModuleExportInventory` is an abstract wrapper around a `Set ModuleExport`.
Set union is its only meaningful composition and has an empty identity. It will
gain `Semigroup` and `Monoid`.

Selector accumulation will operate on inventories through `foldMap` instead of
unwrapping, applying `Set.unions`, and rewrapping. The constructor remains
private, so the existing inventory invariant is unchanged.

### Explicit runtime result hints

`RuntimeExplicitResultHints` stores source-ordered hints in a `Seq` and already
concatenates outer hints before inner hints. It will gain `Semigroup`, and the
nested-hint normalization path will use `(<>)` rather than unpacking both
sequences. No `Monoid` instance is added because the runtime has no current
consumer for an empty hint collection.

## Exhaustive finite enumeration

`SurfaceNumericType` has a hand-maintained `allNumericTypes` list in the hosted
parser comparison tests. Its constructor order is already the intended stable
numeric catalog order.

It will derive `Enum` and `Bounded`, and the test list will become
`[minBound .. maxBound]`. This makes a newly added surface numeric type enter
the exhaustive comparison automatically. Other finite enums will not gain
these instances unless an active exhaustive catalog consumes them; explicit
precedence lists such as namespace lookup order must remain explicit.

## Structural phase forcing

### Problem

`Jazz.Compiler.Force` manually walks canonical AST, Typed Core, Lowered IR,
module graph/interface, inference result, and diagnostic structures. Most of
the module duplicates the generic definition of normal-form evaluation. Every
new field or constructor requires another synchronized edit in a distant
module.

The explicit force entry points are valuable: they mark phase ownership and
are used by profiling tests and stage timing. The duplicated recursive
implementation is not.

### NFData boundary

Pure compiler structures reachable from these roots will derive `Generic` and
`NFData` next to their declarations:

- canonical `Expr` and its structural children;
- `InferenceResult`, inference types, module interfaces, runtime-hint keys,
  diagnostics, names, and their structural children;
- `TypedProgram` and the complete Typed Core contract beneath it;
- `LoweredProgram` and the complete Lowered IR contract beneath it;
- `ResolvedModule`, `CompiledModule`, and `CompiledProgram` and their pure
  module metadata; and
- `ModuleExportInventory`.

`deepseq` will become an explicit `jazz-internal` dependency. Deriving will use
explicit stock/anyclass strategies so newtype behavior and generic behavior are
not left to inference.

`Jazz.Compiler.Force` will remain a small phase-boundary facade whose pure
entry points delegate to `rnf`. This retains useful names such as
`forceInferenceResult`, `forceTypedProgram`, and `forceLoweredProgram` while
removing the parallel structural traversal.

Unused forcing exports with no active source or test consumers, including
surface-parser/token forcing and generic list forcing, will be removed rather
than migrated speculatively.

### Selective forcing preserved

Not every current strictness helper means `NFData`:

- `forceRuntimeProgramOutputResult` intentionally forces the rendered program
  output only. It must not traverse runtime environments, closures, unused
  module exports, or unrendered partial-constructor arguments.
- inference's pre-finalization container ownership boundary intentionally
  materializes selected map/set entries only to weak head normal form while
  the finalizer still owns related state;
- diagnostic ownership remains a named boundary even if its structural
  implementation can share `NFData`; and
- runtime evaluator forcing is semantic evaluation, not structural
  strictness, and remains unchanged.

These paths will stay explicit and will not receive blanket `NFData` calls.

### Prevention and verification

Generic `NFData` gives new structural fields two protections: they are included
automatically in `rnf`, and a field whose type lacks a valid instance fails the
build. The existing poisoned-thunk profiling tests will continue to assert that
deep inference metadata, compiled modules, resolved modules, Typed Core, and
Lowered IR are forced at the same named boundaries.

The runtime rendering test will continue to assert the opposite boundary:
unused exports and unrendered partial-constructor arguments stay unforced.

## Rejected instances and derivations

### Parameterized result wrappers

`RuntimeOutcome`, `RuntimeObservationResult`, `CapabilityFailure`, and
`PatternCaseArmResult` can mechanically support some combination of `Functor`,
`Foldable`, or `Traversable`. Current code does not map or traverse those
wrappers in a way that removes bespoke logic. Their existing case analyses also
perform branch-specific work that those instances would not replace.

No instances will be added until a real consumer demonstrates a simpler
contract.

`Applicative` and `Monad` are also rejected for runtime outcomes. Although a
short-circuiting implementation can be lawful, current runtime control flow is
deliberately explicit about diagnostics and requested exits, and no call site
benefits from hiding it behind bind.

### Runtime host

`RuntimeHost m` is parameterized by an effect constructor of kind `Type ->
Type`; it is not a candidate for ordinary `Functor`. Its relevant operation is
a rank-2 natural-transformation hoist. There is only one current lift into the
host-evaluation transformer, so adding a generic hoist abstraction would add
more machinery than it removes.

### Statistics and other apparent monoids

`RuntimeStatistics` is not a simple counter monoid. Current continuation depth,
maximum depth, and trace-derived maxima do not compose as independent totals.
It keeps its named empty value and update functions.

`ModuleInterface`, constructor inventories, warning settings, solver state,
and runtime module accumulators have empty-looking values but no current
canonical merge, or their update semantics depend on ordering and replacement.
They will not receive speculative `Semigroup`/`Monoid` instances.

`IntegerLiteralRange` has an associative hull operation but no unambiguous
class-level meaning: intersection is also a reasonable range composition.
The named `combineIntegerLiteralRanges` operation remains clearer than `(<>)`.

### Representation and interoperability classes

The audit rejects unused `Read`, `Ix`, `Bits`, numeric, `IsList`, additional
`IsString`, `Storable`, generic Aeson, and exception instances. In particular:

- `Show` output is diagnostic/debug text, not a stable parser contract;
- identifier/path newtypes retain validated constructors rather than accepting
  overloaded literals broadly;
- ID and width types should not acquire nonsensical arithmetic;
- JSON observation schemas remain explicitly versioned and field-controlled;
  and
- compiler diagnostics remain values in explicit result channels rather than
  host exceptions.

## Test strategy

Tests are written before each implementation group.

1. Direct composition tests prove left/right identity and associativity for
   exported or constructible accumulator values. Private representations stay
   private: capture-order, pattern-inference, and module-import behavior tests
   lock their identity, ordering, and collision semantics through existing
   public operations rather than adding test-only exports.
2. Numeric catalog tests prove that `[minBound .. maxBound]` covers the exact
   existing surface constructors in stable order.
3. Existing profiling poison tests are run before the `NFData` migration and
   after each structural family is converted.
4. A focused compile with development warnings catches missing instances,
   ambiguous deriving strategies, redundant helpers, and orphan instances.
5. Closeout runs every Cabal suite serially in the checked-in Nix development
   environment, followed by `cabal check` and `git diff --check`.

## Delivery sequence

1. Add tests that lock accumulator laws, collision precedence, ordered unions,
   and numeric enumeration.
2. Introduce composition instances one domain at a time and remove the
   superseded helpers and imports.
3. Add `deepseq` to the private library and migrate one pure structural family
   at a time, keeping the suite compiling after each family.
4. Collapse `Jazz.Compiler.Force` to the retained phase facade and delete dead
   forcing exports.
5. Run focused and full verification, then report both accepted and rejected
   candidates so future contributors do not repeat the blanket-deriving audit.

Each independently compiling group receives its own commit. No public language
documentation or execution-queue row is needed because the work changes no
Jazz behavior and was directly requested as a repository-quality pass.

## Non-goals

- Adding instances merely because they are lawful or derivable.
- Introducing a compiler-wide algebra, visitor, recursion-schemes framework,
  or phase typeclass hierarchy.
- Changing import precedence, binder collision behavior, diagnostic ordering,
  phase timing ownership, or runtime evaluation.
- Generic serialization of public diagnostics or observation artifacts.
- Forcing runtime closures, environments, cells, or values structurally.
- Mirroring Haskell-only class instances into the Jazz-authored hosted sources.
