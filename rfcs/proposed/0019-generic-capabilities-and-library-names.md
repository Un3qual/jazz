# RFC 0019: Generic capabilities and library names

Status: Proposed
Date: 2026-09-13
Supersedes: On acceptance, the concrete-implementation and class-method visibility restrictions of RFC 0017; its nominal identities and qualified spellings remain.

## Decision

Give Jazz ordinary Haskell-style constrained polymorphism through its existing
`class` and `impl` syntax, with capability names that describe useful behavior.
The maintainer approved this direction and the names `Equatable`, `Comparable`,
`Mappable`, `Reducible`, and `Combinable` on 2026-09-13. This document supplies
the detailed semantic and migration decisions for review. It is not a claim
that these features are implemented or that these additional decisions have
already been accepted.

The contract includes generic implementations with prerequisites, inferred
constraints, type-constructor parameters, methods usable as ordinary values,
superclasses, default methods, and deterministic evidence across modules.
The maintainer additionally requires both `Set` and `Text` to support `map`,
with the destination collection inferred from the calling context. Mapping
therefore relates an input type to an output type; it is not restricted to
preserving a type constructor. Multi-parameter classes with declared type
dependencies are included to express that relation in library code.
Implement the compiler mechanism generally: user-defined classes receive the
same support as the bundled classes.

Use familiar operation names such as `map`, `foldLeft`, `foldRight`, `reduce`,
`combine`, and `empty`. Public names and introductory documentation describe
behavior, with examples and explicit rules. Mathematical terminology may
appear in contributor references but is not required vocabulary for users.

This is the class-programming model, not a promise to implement every Haskell
or GHC feature. Jazz keeps its evaluation strategy, numeric widths and promotion
rules, effect boundary, source syntax, and interpreter. Automatic deriving,
associated types, overlapping instances, explicit higher-rank types, and new operator transport are
outside this contract. Existing accepted behavior outside these changes remains
in force.

## Context

The current compiler infers and transports some class constraints, but
`ConcreteImplFact` has a variable-free target, declaration parsing rejects
variable targets, signature normalization rejects type-variable application,
and methods use explicit class-qualified references. Class bodies contain
signatures only. The Prelude also exposes a list-specific builtin `map`.

The library already has polymorphic functions for lists, queues, optional
values, results, and other structures. Renaming `listMap` to `map` preserves
element polymorphism but does not give it a generic collection implementation.
Names must resolve to declarations before types select implementation evidence;
unrelated functions with matching names do not become overloads.

The source owners are `src/Jazz/Compiler/TypeRepresentation.hs`,
`SemanticDeclarations.hs`, `Parser/CapabilityDeclaration.hs`,
`TypeInference/Capabilities.hs`, `ModuleInterface.hs`, `ModuleResolver.hs`,
`BuiltinCatalog.hs`, the analyzed runtime, and `jazz/stdlib/`.

## Type constructors and inference

Infer kinds from declaration and signature use. A complete value type has kind
`Type`; a constructor such as `List` or `Queue` has kind `Type -> Type`.
`Result` has kind `Type -> Type -> Type`, and `Result(error)` fixes its first
argument. Kinds remain implicit in source code.

Allow a type variable as an application head: `f(a)` and `f(a, b)`.
Applications associate to the left; `Result(error, a)` and
`(Result(error))(a)` denote the same type. `List(a)` and `[a]` denote the same
builtin list type. A named type may be partially applied when the expected
kind permits it; term bindings and datatype fields must have complete types.
Reject kind mismatches, overapplication, and infinite kinds before method
selection. Do not introduce an independently normalized parallel type tree.

Class parameters have inferred kinds. Method-local variables are independently
generalized for each method: the `a` and `b` below are not additional class
parameters. Method signatures may declare additional constraints using the
existing signature prefix. Callers supply both class and method prerequisites;
implementations and defaults are checked under those same assumptions.
Superclass arguments and implementation contexts must be well-kinded.

```jazz
class Transforming(f) {
  transform :: (a -> b) -> f(a) -> f(b).
}.
```

This custom class verifies higher-kinded support independently of the bundled
mapping policy. A helper calling `transform` can infer
`@{Transforming(f)}: (a -> b) -> f(a) -> f(b)`. It remains generic when exported,
stored, passed as a monomorphic callable argument, or partially applied. This
does not add rank-two argument polymorphism. Explicit signatures constrain
inference rather than being required for ordinary generic helpers.

Use expected types as well as argument types to solve method obligations.
A method such as `empty :: a` must work with an explicit result type or a
surrounding constraint. Unresolved choices receive an ambiguity diagnostic;
the compiler must not pick an instance from declaration order or runtime data.
Retain existing numeric defaulting rules; add no collection defaulting rule.

## Mapping inputs to requested outputs

Use one ordinary class declaration to describe the input/output relation.
The example assumes the `Mapping` instances described below are imported:

```jazz
class Mappable(source, target, a, b)
  determines (source -> a, target -> b) {
  map :: (a -> b) -> source -> target.
}.

convert = \(change, values) -> map change values.
codes :: [Int].
codes = convert (\(character) -> 1) "abc".
```

`codes` is `[1, 1, 1]`. The inferred type of `convert` is
`@{Mappable(source, target, a, b)}: (a -> b) -> source -> target`.
The output type is solved from expected types, explicit signatures/type
application, or subsequent operations that determine it. With an output of
`Text`, the callback result must be `Char`. With an output of `Set(b)`, the
selected implementation requires `Comparable(b)` and removes duplicates.
These are instance prerequisites, not compiler exceptions for builtin types.

`determines (source -> a, target -> b)` means that the source type fixes the
input element type and the target type fixes the output element type. A
dependency side names one parameter or a parenthesized nonempty parameter list;
multiple dependencies are comma-separated. Names must refer to class parameters.
This is the functional-dependency mechanism with descriptive surface spelling.
It introduces no runtime metadata or overload search by value.

During inference, equal determinant types improve their dependent types.
Freshened instance heads with unifiable determinants must agree on their
dependents, even if their other class arguments differ. Check dependency
coverage: all variables in dependent positions must be determined by the
determinant positions, closing that set through declared dependencies in the
instance prerequisites. Reject violations at the declaration. Instance heads
still obey the overlap and termination rules below.

An unannotated helper may retain its output constraint polymorphically.
A concrete evaluated expression whose destination remains unconstrained is
ambiguous; report the unresolved target and suggest a result annotation.
Do not silently choose a List, preserve the input type by default, or choose
whatever instance happens to be available first. Thus existing result-ambiguous
`map f list` programs may need a result annotation in this migration.

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

Implementation variables are implicitly bound by the head's arguments. Every
prerequisite variable must occur in that head. Accept existing concrete targets
and constructor-headed generic targets, including `Equatable([a])`,
`Transforming(Queue)`, and `Mappable(Text, [b], Char, b)`. Variables may repeat
across arguments where they express a relationship, but a constructor's generic
argument positions contain distinct variables, not nested specialized patterns.
At least one head argument must have a concrete constructor; reject an
all-variable catch-all head. Function targets retain their
existing rejection in this increment; the library instances below use named
and list constructors.

For each prerequisite, count every type constructor, application, and variable
in its arguments. Require a strictly smaller total than the instance head,
and no greater occurrence count for any variable. Reject recursive contexts
that fail this structural decrease rule. This admits `Equatable(a)` as a
prerequisite of `Equatable([a])` without arbitrary solver fuel or undecidable
instance search.

Two visible implementation heads for the same class overlap if all their
arguments can unify after freshening their variables. Reject overlap even when
their prerequisite lists differ.
In particular, a generic list implementation cannot coexist with a special
`[Int]` implementation. Different import aliases for the same declaration
deduplicate by implementation identity and do not create overlap.

Check generic method bodies once under their declared prerequisites and the
instance's own evidence. Select evidence at the caller, recursively solving
prerequisites. Checked generic functions carry evidence parameters; concrete
calls supply evidence identified by the defining `ImplId` and `MethodId`.
Default methods and superclass projections use that same evidence. Extend
the current analyzed expression/binder facts and runtime method cells; do not
add a second interpreter, alternate IR, global name lookup, or runtime type
inspection to choose implementations.

## Method names and module boundaries

A class method introduces one overloaded value declaration in its defining
module. `map` can therefore be imported, stored, partially applied, and used
without writing a wrapper. Resolve that value's identity before type inference.
Same-spelled methods in distinct classes are distinct values and obey ordinary
lexical shadowing and explicit-import collision rules. Types do not resolve
an otherwise ambiguous name.

Preserve `Class::method` and `Alias::Class::method` as references to the same
method identity. An alias may also expose the method as `Alias::method` when
that method value is exported. An alias-only import introduces no unqualified
class or method names.

Use Haskell-style separation of class names and exported methods:

- `class C` selects the class for constraints and implementation heads.
- `class C(..)` selects the class and all its declared methods.
- `class C(method1, method2)` selects the class and named methods.
- `value method` selects the ordinary method value, retaining hidden class
  metadata required by its inferred type without exposing a private class name.
- An omitted export list publishes all owned public declarations, including
  method values. Imports support the corresponding explicit selectors.

All method spellings honor the same selection: `C::method` cannot bypass an
export list that hides the method. Update existing explicit `class C` exports
that intend to expose methods to `class C(..)` in the migration. Re-exporting
imported declarations remains disallowed.

Instance availability follows the Haskell model: import edges carry the
dependency's complete checked instance environment, including transitive
instances, independently of value/class selection and aliasing. Importing a
module with an empty selection can therefore make its instances available.
This does not make private type or class names source-accessible. Resolve
nominal identities and reject conflicting visible heads before evaluating
the entry module. A third-party module may define an implementation for an
imported class/type; no separate instance-import syntax is introduced.

This explicitly changes RFC 0017's non-transitive instance policy. An imported
generic function receives the caller's evidence for its constraints; a
monomorphic exported binding retains evidence already selected in its defining
module. There is no call-site replacement of fixed evidence.

## Superclasses and defaults

Superclass declarations form an acyclic graph. A subclass constraint provides
its superclass evidence, including through imported and aliased classes.
Declaring an implementation requires satisfying every superclass constraint;
it does not manufacture missing superclass implementations.

Allow one default body alongside a method's signature in a class declaration.
Defaults may call other methods and are checked under the class and superclass
constraints. An explicit implementation overrides that default. A method with
neither an implementation nor a default is a compile error. Recursive method
bodies follow ordinary Jazz recursion rules; do not promise termination or
replace method checking with heuristic cycle rejection.

```jazz
class Equatable(a) {
  equals :: a -> a -> Bool.
  differs :: a -> a -> Bool.
  differs = \(left, right) -> if equals left right then False else True.
}.
```

## Bundled capabilities

Define the foundational classes in the Prelude so collection modules can
implement them without a Prelude-to-library dependency cycle. New classes
are ordinary Jazz declarations. Preserve the separate default-value concept.

| Class                            | Methods and requirements                                                                                                               | First supported targets                                                                                                                  |
| -------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------- |
| `Equatable(a)`                   | `equals :: a -> a -> Bool`; default `differs`                                                                                          | Existing scalar equality targets; lists, tuples, `Maybe(a)`, `Result(e, a)`, `NonEmpty(a)`, `Queue(a)` with required element constraints |
| `Comparable(a)`                  | Superclass `Equatable(a)`; `compare :: a -> a -> Ordering`                                                                             | Existing scalar ordering targets                                                                                                         |
| `Mappable(source, target, a, b)` | `source` determines `a`, `target` determines `b`; `map :: (a -> b) -> source -> target`                                                | Input/output pairs in the mapping matrix below, including `Text` and `Set`                                                               |
| `Reducible(collection, a)`       | `collection` determines `a`; `foldLeft :: (b -> a -> b) -> b -> collection -> b`; `foldRight :: (a -> b -> b) -> b -> collection -> b` | Lists, Queue, Maybe, Result, NonEmpty, Map, Dictionary, Set, Text; maps/dictionaries visit values and Text visits Unicode scalars        |
| `Combinable(a)`                  | `combine :: a -> a -> a`; regrouping combinations preserves the result                                                                 | `Text`, lists, `Queue(a)`, `NonEmpty(a)`, and `Set(a)` with `Comparable(a)`                                                              |
| `Empty(a)`                       | Superclass `Combinable(a)`; `empty :: a`; combining with `empty` on either side preserves the other value                              | The combinable targets except `NonEmpty(a)`                                                                                              |

`Empty` is the proposed name for the additional identity-bearing capability;
it was not in the five-name approval. Its meaning is an identity for `combine`,
not an arbitrary `Default` value. A nonempty collection can be combinable
without having an empty value. Numeric sum/product choices require distinct
wrapper types; this contract adds no arbitrary numeric combination instance.

Add a library function
`reduce :: @{Reducible(collection, a)}: (a -> a -> a) -> collection -> Maybe(a)` after `Maybe`
is available, in a new explicit-import `Reduce` module. It has no initial value,
returns `Nothing` for an empty structure, and uses the first element as the
left-fold seed otherwise. Keep folds in the Prelude independent of `Maybe`.

The initial mapping pairs are explicit ordinary library implementations:

| Input                                  | Supported requested outputs                | Element and structural behavior                                                                                 |
| -------------------------------------- | ------------------------------------------ | --------------------------------------------------------------------------------------------------------------- |
| `[a]`, `Queue(a)`, `Set(a)`, or `Text` | `[b]`, `Queue(b)`, `Set(b)`, or `Text`     | Text input fixes `a = Char`; Text output fixes `b = Char`; Set output requires `Comparable(b)` and deduplicates |
| `NonEmpty(a)`                          | The four outputs above, plus `NonEmpty(b)` | Input guarantees at least one callback result; preserve that guarantee in NonEmpty output                       |
| `Maybe(a)`                             | `Maybe(b)`                                 | Preserve absence                                                                                                |
| `Result(e, a)`                         | `Result(e, b)`                             | Preserve the error type and error value                                                                         |
| `Map(k, a)`                            | `Map(k, b)`                                | Preserve keys and transform values                                                                              |
| `Dictionary(k, a)`                     | `Dictionary(k, b)`                         | Preserve keys and transform values                                                                              |

The matrix defines supported conversions rather than promising every pair of
types has an implementation. An empty List cannot be mapped totally into
NonEmpty, and a Result is not implicitly converted into a container that would
discard its error. New types can define additional mapping pairs with the same
class mechanism. Share traversal/construction helpers where they remove
repetition; do not generate a compiler-owned matrix.

Expose this matrix through a new explicit-import `Mapping` module, which imports
the participating library modules and declares the cross-module implementations.
This avoids adding dependency cycles between List, Queue, Set, and Text. The
Prelude supplies list-to-list mapping so ordinary list code keeps its existing
import requirements; Mapping does not redeclare that instance. Library-owned
same-family instances may be defined in their own module and arrive transitively
through Mapping. Class definitions remain in the Prelude.

Each implementation visits input elements in its documented order and
constructs the requested target from the callback results. The target's rules
apply: a Set sorts/deduplicates; Text accepts only characters. Do not impose a
universal structure-preservation or map-composition law across these conversions.
For same-family List/Queue/Maybe/Result mappings, document and verify those
stronger properties where they hold. For `Reducible`,
document a deterministic element order: list/nonempty order, queue FIFO order,
zero/one success value for Maybe/Result, map/set key order, and the dictionary's
documented entry order. No iteration order is inferred from implementation
incidents or runtime hash layout.

Both `Set` and `Text` implement generic `Mappable`. Retain the named,
collection-specific `Set::map` returning a Set. A Text-to-integer mapping
returns a requested collection of integers, never a malformed Text value.
Map/dictionary mapping changes values, never keys. Failure values in `Result`
pass through unchanged.

Generic mechanism support must also be exercised by user-defined classes for
chaining and traversing parameterized values. This verifies that the compiler
does not special-case the bundled five names. Adding every Haskell library
class or convenience operator is not the criterion for this migration.

## Library migration

Rename `Eq` to `Equatable` and `Ord` to `Comparable`, including inferred
diagnostics, compiler-owned equality obligations, visible declarations,
examples, and fixtures. Keep nominal identity authoritative. Existing primitive
equality/numeric restrictions remain; a user-defined equality implementation
must not silently change builtin structural equality or width conversion.
`Num`, `Integral`, `Fractional`, `Showable`, and `Default` retain their current
behavior and names in this migration.

Remove the redundant lower-camel module prefix from public values in `List`,
`Queue`, `Maybe`, `Result`, `NonEmpty`, `Map`, `Set`, `Dictionary`, `Char`,
`Text`, and `IOError`. For example, `listAppend` becomes `append`,
`queueEnqueueAll` becomes `enqueueAll`, and `mapMapValues` becomes `mapValues`.
Preserve type/constructor names and already-unprefixed I/O operations.

Ordinary renamed functions keep their argument order and return contract.
An operation that already matches a class method may expose that same method
identity through its owning class; a specialized module function is still a
separate value and never joins an overload set by spelling. In particular,
retain collection-specific signatures where Queue/Set/Map currently put the
collection first. The generic Prelude `map` and folds put the function first.
Use qualified imports in migrated code wherever ordinary names collide.

Replace the public list-specific builtin `map` with the `Mappable` method.
Retain a private list primitive only where the implementation actually uses
it. Existing `map f list` programs with a result type fixed to a List retain
their result; migrate previously unconstrained uses with the intended result
type. Keep `filter`, `hd`,
`tl`, and the current runtime host operations unchanged. No-prelude mode has
no implicit public method or instance fallback.

Update Jazz-authored consumers, Haskell-embedded source fixtures, programs,
examples, docs, public API inventories, and formatting/highlighting support
in the same completed migration. Remove obsolete public spellings without a
duplicate compatibility API. Existing historical RFCs remain historical.

## Acceptance evidence

The implementation must demonstrate these observable cases:

1. One unannotated `map` helper executes with context-selected outputs for lists,
   queues, sets, text, optional values, and results, including an element-type
   change and preserved result error. Text-to-List(Int), List(Char)-to-Text,
   Set-to-List, List-to-Set, and Text-to-Text receive explicit output contexts.
2. A constrained generic list or wrapper implementation recursively obtains
   its element evidence; unavailable evidence reports the unresolved constraint.
3. Stored, partially applied, higher-order, exported, and result-constrained
   methods retain the correct evidence, including on empty collections.
4. A custom parameterized datatype and custom class use the same machinery
   as bundled types. A custom chaining class and a custom traversal class
   demonstrate nested constructor applications and method-local constraints.
5. Class method values, class-qualified references, aliases, explicit selectors,
   private names, repeated imports, and transitive instances follow one identity
   and visibility contract. Overlap errors are independent of import order.
6. Superclass prerequisites, supplied defaults, overrides, missing methods,
   kind errors, all-variable heads, escaping instance variables, conflicting
   type dependencies, uncovered dependent variables, and non-decreasing contexts
   receive compile-time diagnostics.
7. Mapping, reduction, combination, and empty-value behavior are checked on
   representative values and nested types. A non-character Text output, a Set
   output without ordering evidence, and an unconstrained concrete output are
   rejected. Preserve existing numeric, purity, and host/runtime behavior.
8. All renamed public exports have matching signatures and updated consumers.
   Existing supported hosted lexer/parser/lowering comparisons stay valid;
   extend their declaration/type encodings for this syntax. This does not
   resume the separate hosted semantic compiler or native backend projects.

Do not claim completion after parser acceptance, a name-only migration, or
concrete per-element instances. Run the supported compiler/stdlib/module and
hosted frontend suites, executable examples, quality gate, and repository
checks named in the implementation plan. A previous batch's full-scale-test
waiver is not automatically a waiver for these grammar changes.

## Alternatives

Keeping class-qualified concrete implementations would simplify the work but
would not meet the approved generic-programming requirement. Selecting unrelated
same-named functions by argument type would add a different name-resolution
model and is rejected. A single oversized collection class would exclude
optional/result values and impose operations that some structures cannot
support. Mathematical public names would violate the approved naming goal.

## Consequences

This is a substantial compiler and API migration, not a mechanical rename.
The additional decisions requiring review are the `Empty` capability name,
the exact constraint/default/dependency syntax, and Haskell-style instance
propagation and method export selection. Set/Text participation and inference
of a requested output collection are maintainer requirements. The approved naming and generic-programming
direction is already recorded; it does not need to be selected again.

Implementation work follows acceptance of this semantic contract under RFC 0001. Public documentation continues describing shipped behavior until each
coherent implementation lands. The execution plan and the public-name inventory
live under `.codex/plans/`; they do not define language behavior.

## References

- [Haskell 2010 declarations, classes, and kinds](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html)
- [Haskell 2010 method and instance visibility](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html)
- [Swift protocol naming](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/protocols/)
- [Kotlin text mapping into a result collection](https://kotlinlang.org/api/core/kotlin-stdlib/kotlin.text/map.html)
- [GHC functional dependencies](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/functional_dependencies.html)
- [Current Jazz capability contract](../../docs/language/capabilities.md)
- [RFC 0017](../accepted/0017-alias-qualified-classes.md)
- [RFC 0018 analyzed runtime ownership](../accepted/0018-direct-analyzed-runtime-facts.md)
