# RFC 0019: Generic capabilities and library names

Status: Accepted
Date: 2026-09-13
Supersedes: The concrete-implementation and instance-transport restrictions of RFC 0017; its nominal identities and qualified spellings remain.

## Decision

Give Jazz ordinary constrained polymorphism through its existing `class` and
`impl` syntax, with behavior-based names: `Equatable`, `Comparable`, `Mappable`,
`Reducible`, and `Combinable`. Support one parameter per class, generic
implementations with prerequisites, inferred constraints and kinds, ordinary
method values, superclasses, and default methods. User-defined classes receive
the same support as the bundled classes.

The maintainer approved the naming and generic-programming direction, then
accepted separate mapping for Text and Set on 2026-09-13. Generic mapping
preserves the collection constructor. This revision replaces the earlier
requested-destination mapping proposal; it does not add functional dependencies,
`determines` syntax, associated types, multi-parameter classes, or a matrix of
cross-collection mapping implementations.

The maintainer approved the revised contract, the initial simplifications,
and all seven compiler-reuse requirements on 2026-09-13, explicitly retaining
the Reduce module and its safe seedless helper. This is an accepted,
unimplemented contract delta. Public documentation continues to describe shipped
behavior until implementation lands. Keep Jazz's evaluation strategy, numeric
widths and promotion rules, purity boundary, and analyzed-core interpreter.
Automatic deriving, overlapping instances, explicit higher-rank
types, re-exports, new operator transport, and broader self-hosting/native work
remain outside this change.

## Context

The current compiler infers and transports some class constraints, but
`ConcreteImplFact` has a variable-free target, declaration parsing rejects
variable targets, signature normalization rejects type-variable application,
and methods use explicit class-qualified references. Class bodies contain
signatures only. The Prelude exposes a list-specific builtin `map`.

The library already has polymorphic functions for collections, optional values,
and results. Renaming `listMap` to `map` preserves element polymorphism but
does not let one helper work across collection implementations. Names must
resolve to declarations before types select evidence; unrelated same-named
functions do not become overloads.

The active owners are `src/Jazz/Compiler/TypeRepresentation.hs`,
`SemanticDeclarations.hs`, `Parser/CapabilityDeclaration.hs`,
`TypeInference/Capabilities.hs`, `ModuleInterface.hs`, `ModuleResolver.hs`,
`BuiltinCatalog.hs`, the analyzed runtime, and `jazz/stdlib/`.

## Type constructors and inference

Infer kinds from declarations and signatures. A complete value type has kind
`Type`; List and Queue have kind `Type -> Type`. Result has kind
`Type -> Type -> Type`; `Result(error)` fixes its first argument. Kinds remain
implicit in source code.

Allow a variable as an application head: `f(a)` and `f(a, b)`. Retain the
existing named-head surface grammar; defer parenthesized application heads
such as `(Result(error))(a)`. Normalize `Result(error, a)` as successive
applications internally. `List(a)` and `[a]` denote the same builtin list type.
Allow partial named type application such as `Result(error)` when the expected
kind permits it. Term bindings and datatype fields require complete types.
Reject kind mismatches, overapplication, and infinite kinds. Extend semantic
normalization without replacing the existing surface type-application shape.

A class still has exactly one parameter, whose kind is inferred. Variables in
a method signature other than that parameter are independently generalized on
each use. Method signatures may add prerequisites using the existing constraint
prefix; implementations and defaults are checked under those assumptions.

```jazz
class Mappable(f) {
  map :: (a -> b) -> f(a) -> f(b).
}.

convert = \(change, values) -> map change values.
```

The helper infers `@{Mappable(f)}: (a -> b) -> f(a) -> f(b)`. An input of
`[Int]` fixes `f = List` and `a = Int`; the callback fixes `b`. Mapping a list
returns a list, a Queue returns a Queue, and a Maybe returns a Maybe. An output
annotation can constrain element types but cannot turn the result into another
collection constructor. No new destination annotation is needed for ordinary
list mapping.

Generic helpers retain their constraints when exported, stored, partially
applied, or passed as monomorphic callable arguments. This does not add
rank-two argument polymorphism. Expected types participate in inference,
including for existing result-only methods such as `defaultValue :: a`.
Unresolved choices receive an ambiguity diagnostic. Preserve existing numeric
defaulting and add no collection defaulting or declaration-order selection.

## Generic implementations and evidence

Reuse the existing constraint prefix for implementation prerequisites and
superclasses:

```jazz
impl @{Equatable(a)}: Equatable([a]) {
  equals = \(left, right) -> case (left, right) {
    | ([], []) -> True
    | ([x | xs], [y | ys]) -> if equals x y then equals xs ys else False
    | _ -> False
  }.
}.

class @{Equatable(a)}: Comparable(a) {
  compare :: a -> a -> Ordering.
}.
```

Implementation variables are implicitly bound by the single head argument.
Every prerequisite variable must occur in that head. Accept existing concrete
targets and constructor-headed generic targets such as `Equatable([a])`,
`Mappable(Queue)`, and `Mappable(Result(error))`. Generic constructor argument
positions contain distinct variables rather than nested specialized patterns.
Reject a bare-variable catch-all head. Function targets retain their current
rejection in this release; named, list, and tuple targets cover the initial
library families.

Each implementation prerequisite must have the form `C(a)`, where `a` is an
individual variable bound by the head. Each superclass prerequisite must have
the form `C(a)`, where `a` is the class parameter. Reject compound declaration
prerequisites such as `Equatable([a])` and prerequisites with unbound variables.
Together with constructor-headed instances, these restrictions make instance
search descend through the target without general size or occurrence counting.
This restriction applies only to implementation and superclass declarations;
inferred constraints, use-site constraints such as `Equatable([Int])`, and
method-local prerequisites continue to use ordinary constrained schemes.

Two visible heads for the same class overlap if their targets unify after
freshening. Reject overlap independently of prerequisites. A generic list
implementation cannot coexist with an `[Int]` specialization. Repeated aliases
of one declaration deduplicate by implementation identity.

Check generic method bodies once under their prerequisites and their own
implementation evidence. Callers select evidence and recursively solve its
prerequisites. Generic functions carry evidence parameters; concrete calls
supply the selected `ImplId` and `MethodId`. Extend existing analyzed facts and
runtime method cells. No second interpreter, runtime type inspection, or
same-name method fallback is introduced.

## Methods, defaults, and module boundaries

A class method introduces an ordinary overloaded value in its defining scope.
It can be called, stored, and partially applied without a wrapper. Its plain
name, `Class::method`, and `Alias::Class::method` refer to the same identity.
Normal lexical shadowing and import collision rules apply. Types do not resolve
an ambiguous source name.

Keep the existing module selectors. Exporting/importing `class C` makes its
methods available with the class, including ordinary method names for an
unqualified import. Aliased imports expose `Alias::method` and
`Alias::Class::method` without leaking plain names. The existing `value method`
selector may expose a method alone, preserving hidden supporting class metadata
without exposing the class name. An omitted export list includes owned method
values. Do not add `C(..)` or per-method class selectors in this release.
Selecting a class exposes all its methods; there is no per-method privacy within
that class selection. Imported declarations remain ineligible for re-export.

Import edges transport the complete checked instance environment, including
transitive instances, independently of value/class selection and aliasing.
This changes RFC 0017's instance filtering. It is needed so an ordinary library
module can implement a Prelude class for its collection: importing Queue must
make `Mappable(Queue)` usable without re-exporting the Prelude class. An import
with an empty selection can supply instances. This does not expose private
class/type names. Reject conflicting visible heads before entry evaluation.

An imported generic function receives caller evidence for its constraints;
a concrete exported binding retains its defining-module evidence. Preserve
nominal identity across both cases and repeated imports. Third-party instances
use the same overlap rules; there is no separate instance-import syntax.

Superclass declarations form an acyclic graph. A subclass constraint supplies
its superclass evidence. Each implementation must satisfy those superclass
constraints; declaring a subclass implementation does not create missing
superclass implementations.

Allow a default body alongside a method signature. Defaults may call other
methods and are checked under class, superclass, and method prerequisites.
Explicit implementations override defaults. A method with neither a supplied
body nor a default is a compile error. Method recursion follows ordinary Jazz
recursion rules.

```jazz
class Equatable(a) {
  equals :: a -> a -> Bool.
  differs :: a -> a -> Bool.
  differs = \(left, right) -> if equals left right then False else True.
}.
```

## Bundled capabilities

Define the five classes in the Prelude. Collection-owned instances live in
their library modules; list instances live in the Prelude. Avoid a Prelude
dependency on those modules. Implement the classes in ordinary Jazz code.

| Class           | Methods                                                                                       | Initial instances                                                                                                  |
| --------------- | --------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| `Equatable(a)`  | `equals`; default `differs`                                                                   | Existing scalar equality targets; generic lists, tuples, Maybe, Result, NonEmpty, Queue with element prerequisites |
| `Comparable(a)` | `compare`; superclass Equatable                                                               | Existing scalar ordering targets                                                                                   |
| `Mappable(f)`   | `map :: (a -> b) -> f(a) -> f(b)`                                                             | List, Queue, Maybe, NonEmpty, Result(error), Map(key), Dictionary(key)                                             |
| `Reducible(f)`  | `foldLeft :: (b -> a -> b) -> b -> f(a) -> b`; `foldRight :: (a -> b -> b) -> b -> f(a) -> b` | The Mappable families plus Set                                                                                     |
| `Combinable(a)` | `combine :: a -> a -> a`, with associative behavior                                           | Text, lists, Queue, NonEmpty; Set with Comparable element evidence                                                 |

Mapping preserves element positions, optional absence, Result errors, and map
keys, as applicable. Same-constructor mapping obeys identity and composition.
Reduction visits list/nonempty order, Queue FIFO order, zero/one success values
for Maybe/Result, map/set key order, and Dictionary's documented entry order.
Set reduction does not construct a new Set and needs no ordering prerequisite.
Text uses explicit character conversion for generic reduction.

Provide a safe, no-seed helper in an explicit-import `Reduce` module:
`reduce :: @{Reducible(f)}: (a -> a -> a) -> f(a) -> Maybe(a)`. It returns
Nothing for an empty input and otherwise left-folds from the first element.
Keeping it outside the Prelude avoids a dependency on Maybe. Implement it once
with `foldLeft` and a Maybe accumulator; do not convert to an intermediate List
or add a primitive or another class. Its order is the collection's documented
reduction order. A singleton returns its element without invoking the callback.

Defer the additional `Empty` class. Existing collection-specific empty values
and the separate `Default` class remain. NonEmpty can implement Combinable
without having an empty value. Add no arbitrary numeric combination instance.

## Separate Text and Set mapping

Neither Text nor Set implements the generic Mappable class in this release.
Both have ordinary module functions. Text gains a function-first map:

```jazz
map :: (Char -> Char) -> Text -> Text.
```

Set's existing `setMap` is renamed, preserving its collection-first order:

```jazz
map :: @{Comparable(b)}: Set(a) -> (a -> b) -> Set(b).
```

`Text::map` visits Unicode scalars and returns Text. `Set::map` maps values and
orders/deduplicates its result. Use qualified imports where these names collide
with Prelude map. A Text-to-integer mapping converts through `Text::toChars`;
a Set-to-List mapping converts through `Set::toList`. Destination construction
is explicit when a different collection kind is wanted. Existing conversions
keep their signatures.

Illustrative module body, with `import Text as Text.` and
`import Set as Set.`:

```jazz
convert = \(change, values) -> map change values.
counts :: [Int].
counts = convert (\(character) -> 1) (Text::toChars "abc").
unchanged = Text::map (\(character) -> character) "abc".
unique = Set::map (Set::fromList [1, 2, 3]) (\(value) -> 0).
```

Expect `[1, 1, 1]`, `"abc"`, and a singleton Set containing 0. There is no
cross-collection `Mapping` module or automatically selected destination.

## Library migration

Rename Eq to Equatable and Ord to Comparable across declarations, compiler-owned
obligations, diagnostics, examples, and fixtures. Preserve builtin structural
equality and numeric semantics. Num, Integral, Fractional, Showable, and Default
retain their behavior and names.

Remove redundant lower-camel module prefixes from public values in List, Queue,
Maybe, Result, NonEmpty, Map, Set, Dictionary, Char, Text, and IOError. The
inventory under `.codex/plans/2026-09-13-stdlib-api-renames.csv` records the 183
current prefixed exports. For example, `listAppend` becomes `append` and
`mapMapValues` becomes `mapValues`. New Text map and generic class methods are
additions outside that rename inventory.

Preserve existing specialized argument orders and return contracts. Generic
Prelude map and folds are function-first. Specialized same-named functions are
separate values, so migrate consumers with qualified imports where needed.
Replace the public list-specific builtin map with the Mappable method; keep a
private list primitive only where used. Ordinary list mapping keeps its List
result without extra collection annotations. No-prelude mode has no implicit
public method or instance fallback.

Update all authored Jazz consumers, Haskell-embedded fixtures, programs,
examples, API inventories, docs, and affected syntax highlighting. Remove old
public spellings without duplicate compatibility aliases. Historical RFCs
remain historical. Keep filter, hd, tl, host operations, and purity unchanged.

After the compiler core is complete, migrate each library module's names,
instances, consumers, and documentation together. New Text and Reduce functions
use final names immediately; there is no intermediate library batch that adds
capabilities under old prefixes and then rewrites the same consumers.

## Compiler implementation approach

Keep one parameterized Haskell `Kind variable` tree, with inference variables
during solving and `Void` after solving/defaulting. Derived traversals handle
variable operations. Normalize semantic type applications into one form, with
Haskell pattern synonyms preserving useful list, function, and named-data views
where needed. This does not add pattern synonyms to Jazz source syntax.

Store each class parameter's kind once in class metadata. Represent method
polymorphism with ordinary `SemanticScheme` values and reuse their instantiation
path, preserving nominal method identity and explicit class-parameter binding
order. Do not create a separate method generalization mechanism.

Use a local `StateT InferState Maybe` adapter for candidate trials. Every trial
starts from the same state; failures discard trial changes. Inspect all successful
matches and require a unique instance, retaining declaration overlap checks
independently of prerequisites. Keep diagnostics outside silent trials and
defer obligations whose generic targets remain unknown. This requires no
whole-compiler monad migration or additional dependency.

Apply the seven approved reuse requirements within those existing owners:

1. Share one evidence representation between inference and analyzed facts.
   Replace the duplicate `ExpressionEvidenceSeed` record with the shared
   `EvidenceReference` representation before adding generic evidence. Apply
   solved substitutions through the existing draft finalizer; add no separate
   evidence-lowering pass or runtime instruction sequence.
2. Extend `DeferredExplicitConstraint`, the existing constraint queue, and
   statement-local finalization/entailment for instance prerequisites and
   superclasses. Keep one obligation-solving path, including generic head
   matching, deferred targets, and superclass entailment.
3. Extend `ModuleInterface` and `ImportedInterface` publication, selection,
   and merging for complete instance metadata and runtime method-cell transport.
   Their existing public inventories continue to govern source visibility;
   add no separate instance-import graph or registry.
4. Use the existing `CapabilityMethodReference` for ordinary method values.
   Generalize `ModuleValueBinding`'s lexical-binder-only identity field to
   `ResolvedReference`, so all method spellings publish the same method cell
   without synthetic wrapper bindings or a parallel method-value export table.
5. Factor the shared expected-type and constraint checking in
   `checkImplMethodBodies` for defaults checked under class assumptions.
   Defaults remain ordinary expressions with their defining scope and use
   existing runtime method cells. Add no separate default-body representation,
   checker, or evaluator; supplied implementations override defaults.
6. Integrate kind skeletons and implementation-template preparation into
   `prepareScope` and its checked declaration cache. Any additional local
   dependency traversal needs a concrete requirement; add no separate compiler
   phase or prepared-module representation by default.
7. Share the existing constraint-prefix parser and `SignatureConstraint`
   representation for declaration contexts. Reuse method expression-binding
   parsing for defaults; enforce declaration-specific restrictions in validation.

These requirements preserve the new kind tree and isolated candidate trials.
`previewInference` is not a replacement for those trials: its contract discards
outputs and outstanding constraints. Keep that distinct speculation behavior.

## Acceptance evidence

1. One unannotated map helper runs on List, Queue, Maybe, Result, NonEmpty, Map,
   Dictionary, and a user-defined collection, with changed element types and
   preserved collection structure. No destination annotations are introduced.
2. Generic constrained implementations recursively obtain element evidence.
   Missing prerequisites, overlapping heads, escaping variables, bare-variable
   targets, kind errors, and compound declaration prerequisites produce diagnostics.
3. Stored, partial, higher-order, exported, and result-constrained methods use
   correct evidence, including on empty collections and in returned closures.
4. Defaults, overrides, missing bodies, superclass evidence, and superclass
   cycles have compile/run coverage. A user-defined chaining/traversal class
   exercises nested constructor applications and method-local constraints.
5. Class and value selectors, ordinary and qualified methods, aliases, private
   names, repeated imports, and transitive instances retain the stated identity
   and visibility rules. A Queue-owned Prelude-class instance works on import.
6. Text map preserves Text and rejects a non-character callback result. Set map
   orders/deduplicates and rejects unavailable output ordering. Explicit Text
   and Set conversions permit element-type changes through generic List map.
7. Reducible, safe reduce, and Combinable execute on representative values,
   including empty inputs and NonEmpty. Preserve numeric, purity, and host rules.
8. All 183 renamed exports and their consumers agree. Existing supported hosted
   syntax/lowering comparisons are extended for declaration contexts and default
   bodies; the surface type-application encoding is retained. This does not
   resume the separate hosted semantic compiler project.

Run the compiler/stdlib/module suites, retained hosted frontend comparisons,
examples, quality gates, and repository checks in the implementation plan.
Previous run-specific full-scale-test waivers do not apply automatically.

## Consequences

The release gains reusable generic classes with ordinary inference. Separate
Text/Set functions and explicit conversions keep collection restrictions in
library signatures. Functional dependencies, associated types, multi-parameter
classes, automatic cross-collection mapping, the Empty class, parenthesized
application heads, and new class export selectors are deferred. Generic library
instance transport remains necessary and is retained in the compiler batch.

Implementation follows this accepted contract under RFC 0001.
The execution plan remains internal coordination state and defines no public
behavior by itself.

## References

- [Haskell 2010 declarations, classes, and kinds](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html)
- [Haskell Functor](https://hackage.haskell.org/package/base/docs/Data-Functor.html)
- [Haskell Text mapping](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Haskell Set mapping](https://hackage.haskell.org/package/containers/docs/Data-Set.html)
- [Current Jazz capability contract](../../docs/language/capabilities.md)
- [RFC 0017](../accepted/0017-alias-qualified-classes.md)
- [RFC 0018 analyzed runtime ownership](../accepted/0018-direct-analyzed-runtime-facts.md)
