---
id: JN-GENERIC-CAPABILITIES-CORE-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Implementation"
target_paths:
  - src/Jazz/Compiler/TypeRepresentation.hs
  - src/Jazz/Compiler/SemanticDeclarations.hs
  - src/Jazz/Compiler/Parser/CapabilityDeclaration.hs
  - src/Jazz/Compiler/TypeInference/Capabilities.hs
  - src/Jazz/Compiler/ModuleInterface.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/ConstraintsTests.hs
verification:
  - cabal build all --jobs=1
  - cabal test all --jobs=1 --test-show-details=failures
  - JAZZ_CABAL_JOBS=1 bash scripts/ci/haskell-quality.sh
  - bash scripts/check-examples.sh --jazz-bin "$(cabal list-bin jazz)"
  - bash scripts/check-docs.sh
  - bash scripts/check-execution-queue.sh
  - git diff --check
deliverable: "General generic capability dispatch, kind inference, ordinary method values, superclass/default evidence, and module transport through the analyzed interpreter."
last_verified: 2026-09-13
---

# Generic Capabilities and Library Names Implementation Plan

> For agentic workers: use `superpowers:executing-plans` to implement this plan
> inline, task by task. No agent fan-out is requested for this work.

**Goal:** Implement the approved generic-programming and naming direction,
with generic mapping that preserves the collection constructor and separate
Text/Set mapping, then complete the standard-library rename and consumer migration.

**Architecture:** Extend the existing shared type representation, declaration
templates, solver, nominal method identities, and analyzed evidence. Class
methods become ordinary value references. Each class has one parameter, which
may be a collection constructor. Mappable and Reducible express element types
through ordinary type application; specialized Text/Set operations remain
library functions. Imported modules transport generic instance evidence.

**Tech stack:** Haskell and Jazz, existing Cabal suites and Python/shell gates,
the repository-pinned Nix development/quality shells.

**Spec:** [RFC 0019](../../rfcs/accepted/0019-generic-capabilities-and-library-names.md).
The revised contract and seven simplifications are approved on 2026-09-13.
The maintainer explicitly retained the Reduce module and safe seedless helper.
This accepted future contract is not yet implemented.

## Global constraints

- Implement only in the active root `src/`, `jazz/`, `app/`, and `test/` owners.
- Public capability names describe behavior. The approved five are
  `Equatable`, `Comparable`, `Mappable`, `Reducible`, and `Combinable`.
- Generic `map` preserves the collection constructor and can change its element
  type. Text uses a separate Char-to-Char map; Set uses its existing constrained
  map. Other destinations use explicit conversion functions.
- Defer functional dependencies, `determines`, associated types, multi-parameter
  classes, automatic cross-collection mapping, Empty, parenthesized application
  heads, and new class selectors. Keep the explicit-import Reduce module.
- Keep the single analyzed-core interpreter and nominal class/impl identities.
- Preserve numeric widths, builtin structural equality semantics, direct-call
  purity rules, host behavior, and permanent rejection of `trait` syntax.
- Extend retained hosted syntax/lowering conformance for changed grammar;
  broader self-hosting, native work, profiling, and benchmarks remain deferred.
- Use `--jobs=1` for Cabal. Previous full-scale-test waivers were run-specific.
- Update public behavior, consumer fixtures, queue state, and documentation with
  each coherent implementation. Commit verified milestones.
- Do not weaken existing checks, add same-name runtime fallback, or enumerate
  concrete element types as a substitute for generic implementations.

## Promotion and delivery

RFC 0019 is accepted and this plan is ready. The queue row uses exactly this
ordered target and verification list. The first queue child covers Tasks 1-4
as one complete compiler deliverable; do not close it at parser-only support.

Task 5 is the dependent library migration, child
`JN-GENERIC-CAPABILITIES-LIBRARY-001`. At core closeout, create its ready plan
from that task, naming the concrete stdlib, catalog, and test files. Preserve
the approved scope and continue to that milestone without reopening it for
permission. The separate child is an integration/verification boundary, not a
request to leave the rename undone.

## Implementation

### Task 1: Kinded applications and generic declaration templates

**Files:** `src/Jazz/Compiler/TypeRepresentation.hs`,
`SemanticDeclarations.hs`, `AST.hs`, `Parser/AST.hs`,
`Parser/CapabilityDeclaration.hs`, `Parser/Lower.hs`, `SignatureRendering.hs`,
`TypeInference/Signature.hs`, `TypeInference/Solver.hs`,
`jazz/compiler/ParserDeclaration.jz`, and
the canonical/hosted adapters that consume those declarations.

**Interfaces:** Keep surface `SignatureType` applications as `TypeApplication
name [arguments]`. Existing parsing already accepts `f(a)`, `f(a, b)`, and
`Result(error)`; extend name resolution and kind-aware normalization to give
them meaning. Defer parenthesized application heads without changing the
surface AST or hosted type-application encoding.

Use one shared kind tree:

```haskell
data Kind variable
  = TypeKind
  | FunctionKind (Kind variable) (Kind variable)
  | KindVariable variable
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
```

Inference uses `Kind KindVariableId`; solved kinds use `Kind Void`. Derive
traversals for renaming and collecting variables and checking that solving and
defaulting leave no unresolved variables. Keep substitutions local to kind
inference; do not introduce a general solver framework or a second kind tree.

Normalize named and variable applications, including partial constructors and
List, into one semantic application form. Use bidirectional Haskell pattern
synonyms for existing list/function/named-data views where they simplify current
callers. Do not store duplicate equivalent forms or add Jazz pattern-synonym
syntax. Preserve numeric primitives and source-oriented rendering/diagnostics.

Store the class parameter kind once on class declaration metadata. Reuse
`SemanticScheme` for method quantifiers, prerequisites, and instantiation,
retaining class/method identities and the class parameter's binding order for
explicit `Class::method@[Type]`. Do not add a second quantification mechanism
to `ClassMethodType` or recursively embed a full class environment in each
method scheme. One checked implementation template carries `ImplId`,
quantifiers/kinds, one head target, prerequisites, and method identities,
replacing the concrete-only catalog.

- [ ] In parser/signature and binding suites, add focused source cases:

  ```jazz
  data Wrapped f a = Wrapped f(a).
  keep :: Wrapped(List, Int) -> Wrapped(List, Int).
  keep = \(value) -> value.

  class Transforming(f) {
    transform :: (a -> b) -> f(a) -> f(b).
  }.
  ```

  Reject `Wrapped(Int, Int)`, applying Int as a constructor, excessive Result
  arguments, and kind-occurs-check cycles. Accept partial `Result(error)` and
  normalize `[a]` and `List(a)` identically. Check error locations as behavior,
  not serialized private representation strings.

- [ ] Run `cabal test parser-foundation-spec declaration-parser-spec
binding-signature-coherence-spec signature-rendering-spec --jobs=1
--test-show-details=failures`; confirm the new behavior fails on the baseline.
- [ ] Extend parsing/lowering for `@{...}:` declaration contexts and class
      default bodies. Retain the one-parameter class/impl arity checks. Reject
      multi-parameter declarations and dependency clauses in focused syntax tests.
      Normalize the existing named-head applications through the shared owner;
      substitutions and renderers must agree on one semantic shape. Validate
      implementation contexts as `C(a)` for head-bound variables and superclass
      contexts as `C(classParameter)`. Keep method-local and use-site constraints
      governed by ordinary schemes, including compound targets.
- [ ] Extend hosted declaration encodings for contexts/default bodies and complete
      their exhaustive adapters; reuse existing type-application encodings and
      retain structural differential checks for the changed declaration syntax.
      Run the focused suites plus `canonical-parser-comparison-spec`,
      `canonical-core-comparison-spec`, and
      `jazz-parser-types-declarations-modules-spec`. Commit the coherent type and
      declaration representation migration once all components build.

### Task 2: Generic resolution and runtime evidence

**Files:** `src/Jazz/Compiler/CapabilityFacts.hs`, `SemanticDeclarations.hs`,
`TypeInference/Capabilities.hs`, `TypeInference/ImplChecking.hs`,
`TypeInference/Solver.hs`, `TypeInference/Instantiation.hs`,
`TypeInference/State.hs`, `TypeInference/Analyzed.hs`, `CoreIdentity.hs`,
`SemanticFacts.hs`, `Runtime/Types.hs`, `Runtime/ScopePlan.hs`,
`Runtime/Engine.hs`, and `Runtime/Semantics.hs`.

**Interfaces:** The template from Task 1 is the sole declaration environment.
Class obligations contain the class identity and one kinded target.
Selected evidence identifies an implementation plus substitution and prerequisite
evidence; deferred evidence refers to an enclosing scheme parameter. Extend
existing analyzed facts and method cells with these forms. Preserve the public
driver result APIs.

- [ ] Add compile/run cases using the existing `assertSourceOkWithoutPrelude`
      helper in `BindingSignature/ConstraintsTests.hs` and driver-based runtime
      cases in `Runtime/CapabilitiesTests.hs`. Start with this complete program:

  ```jazz
  class Same(a) { same :: a -> a -> Bool. }.
  impl Same(Int) { same = \(x, y) -> x == y. }.
  impl @{Same(a)}: Same([a]) {
    same = \(left, right) -> case (left, right) {
      | ([], []) -> True
      | ([x | xs], [y | ys]) -> if Same::same x y then Same::same xs ys else False
      | _ -> False
    }.
  }.
  check = \(x, y) -> Same::same x y.
  (check [[1]] [[1]], check [[1]] [[2]]).
  ```

  Expect `(True, False)`. Missing `Same(Bool)` must fail at the use site.
  Also implement `Transforming(List)` and `Transforming(Result(error))` for
  the Task 1 custom class. Check element-type changes, preserved Result errors,
  method-local quantifier reuse at different types, and the inferred unchanged
  collection constructor. Reject a Queue result annotation on a List mapping.

- [ ] Run binding/runtime suites and confirm failures concern unsupported new
      semantics, not fixture/import mistakes.
- [ ] Freshen each instance's variables, unify the entire head against the
      obligation, and solve its variable-only declaration prerequisites. The
      declaration restrictions provide descent; add no size/occurrence accounting.
      Reject overlapping heads independently of prerequisites and runtime values.
      Reject compound/unbound declaration prerequisites, while accepting inferred
      and use-site compound obligations such as `Same([[Int]])`.
- [ ] Wrap candidate trials locally in `StateT InferState Maybe`, using existing
      `transformers` and unification functions. Run every candidate from the same
      immutable pre-trial state, discard failures, and examine all successes
      before accepting a unique match. Preserve inference allocation/rollback
      policies and report diagnostics outside silent trials. Keep unknown generic
      targets deferred; do not select the first plausible concrete implementation.
      Cover failed-trial isolation through observable inference/dispatch behavior
      and reject overlapping heads even when one prerequisite is unavailable.
- [ ] Check bodies under declared prerequisites. Register method types as ordinary
      constrained `SemanticScheme` values and reuse `instantiateTypeScheme` in
      `TypeInference/Instantiation.hs`; instantiate method-local variables freshly
      per use while preserving the explicit class-parameter binding order.
- [ ] Pass selected/deferred evidence through analyzed binders and callables.
      Cover stored/partial methods, expected-result methods, empty collections,
      returned closures, and recursive dictionaries. Assert distinguishable results
      for competing test types so an incorrect instance cannot accidentally pass.
      Run `binding-signature-coherence-spec`, `runtime-semantics-spec`,
      `recursive-bindings-spec`, and `haskell-typeclass-contracts-spec`; commit.

### Task 3: Ordinary method values, superclass evidence, and defaults

**Files:** `src/Jazz/Compiler/ModuleResolver.hs`,
`ModuleResolver/Names.hs`, `CoreIdentity.hs`, `TypeInference.hs`,
`TypeInference/Capabilities.hs`, `TypeInference/ImplChecking.hs`,
`Runtime/ScopePlan.hs`, and the binding/runtime/purity suites.

**Interfaces:** A plain method value, `Class::method`, and qualified alias
spelling resolve to the same `CapabilityMethodKey`. Evidence from Task 2
supports superclass projection and omitted-method default bodies.

- [ ] Change the generic helper above to call plain `same`; add stored aliases
      and local shadowing cases. Add a class with `same` and default `different`,
      and a subclass that calls a superclass method. Execute default and overridden
      methods with different results; reject missing methods and superclass cycles.
- [ ] Run binding/runtime and `purity-semantics-spec` tests to establish the
      new cases' failures.
- [ ] Publish class methods with the ordinary schemes from Tasks 1-2 into the
      normal local value environment and resolve them before inference. Do not
      add a separate method generalization or value-lookup path. Follow current
      lexical rules and reject same-scope
      duplicate values. Resolve defaults through the implementation's evidence,
      with supplied methods overriding defaults. Derive superclass evidence from
      its declared graph, never from coincidental same-spelled facts.
- [ ] Confirm class-method aliases preserve bang-name purity checks. Run the
      three focused suites and `source-ranges-spec`; update public capability and
      grammar documentation for this completed behavior and commit.

### Task 4: Existing module selectors and generic instance transport

**Files:** `src/Jazz/Compiler/ModuleExports.hs`, `ModuleInterface.hs`,
`ModuleResolver.hs`, `ModuleResolver/Imports.hs`, `ModuleAnalysis.hs`,
`test/Jazz/Compiler/Modules/Loader/CapabilitiesTests.hs`,
`Loader/AliasClassTests.hs`, `ModuleExportsSpec.hs`,
`ModulePipelineContractSpec.hs`. Existing module selector grammar is unchanged.

**Interfaces:** Keep existing `class C` and `value method` selectors. Selecting
a class includes its ordinary method values. Separate that source-visible
inventory from the dependency instance environment, which includes instances
for imported classes. Preserve hidden supporting type/class metadata and
original declaration identities.

- [ ] Add an A -> B -> C graph: A exports a class/method; B declares an
      implementation; C imports B with no values and uses A's method. Expect
      successful dispatch. Repeated aliases must deduplicate the same instance;
      an additional overlapping instance must fail regardless of import order.
- [ ] Add fixtures for existing `class C` and method-value-only selections.
      Class selection provides its method names, including alias-qualified
      spellings; value-only selection preserves hidden class metadata. Assert
      private names remain inaccessible, exported helpers receive caller evidence,
      and concrete exported bindings retain defining-module evidence.
- [ ] Add a Prelude-owned test class implemented in a Queue-like library module.
      Importing that library, even by alias, must make its generic instance usable.
      This case prevents retaining the current public-class filter on instances.
- [ ] Implement the interface/resolver changes with focused loader tests.
      Retain re-export rejection and add no new selector grammar. Run
      `module-exports-spec`, `module-resolution-spec`, `loader-spec`, and
      `module-pipeline-contract-spec`.
- [ ] Complete the core queue child only after Tasks 1-4 work end to end and
      the frontmatter verification commands pass. Publish matching public contract
      changes, promote the library child from Task 5, and commit closeout.

### Task 5: Bundled capabilities, final library names, and consumers

**Files:** `jazz/stdlib/Prelude.jz`, `List.jz`, `Queue.jz`, `Maybe.jz`,
`Result.jz`, `NonEmpty.jz`, `Map.jz`, `Dictionary.jz`, `Set.jz`, `Text.jz`,
new `jazz/stdlib/Reduce.jz`, `src/Jazz/Compiler/BuiltinCatalog.hs`,
`PreludeContract.hs`, `Prelude.hs`, `TypeInference/Capabilities.hs`,
`test/Jazz/Compiler/Stdlib/{LinearCollectionsTests,OrderedCollectionsTests,TextTests,FoundationsTests}.hs`,
`test/Jazz/Repository/AuditSpec.hs`, and new `docs/standard-library/reduce.md`.
Also every source/export in `2026-09-13-stdlib-api-renames.csv`,
`jazz/compiler/`, `test/fixtures/stdlib/`, Haskell-embedded Jazz fixtures,
`programs/`, `examples/`, `docs/standard-library/`, public examples/signatures,
`jazz/stdlib/README.md`, `scripts/check-stdlib-api-docs.py`,
`scripts/test-check-stdlib-api-docs.py`, and affected editor grammar owners.

**Interfaces:** Define the five RFC classes in the Prelude. `Mappable(f)` uses
`map :: (a -> b) -> f(a) -> f(b)`; `Reducible(f)` uses ordinary `f(a)` folds.
Collection-owned instances arrive on import through Task 4; list instances
remain in the Prelude. The public list-only builtin map becomes the class
method. Add Text map and retain Set's existing map signature/argument order.
The CSV maps 183 existing exports without changing their argument orders; the
two class renames and new exports are tracked separately. Migrate a module's
public names, instances, consumers, and documentation together, using final
names from the start. Generic Prelude methods remain distinct from specialized
module values. There is no separate second pass to rename newly added adapters.

- [ ] Reconcile the CSV with live exports before editing. Rename Eq/Ord to
      Equatable/Comparable with their compiler consumers, fixtures, and public
      documentation. Update the public builtin map binding and hardcoded class
      inventories with the Prelude classes/instances. Keep `Default` separate.
- [ ] Work through modules in dependency order, keeping related modules together
      when needed for a compiling milestone. For each group, add behavioral
      fixtures with final names, implement its instances and renames, and update
      consumers and API docs before committing. Resolve references by module
      ownership; do not globally replace common names such as map or empty.
      Use `import List as List` and equivalent qualification to avoid collisions.

- [ ] In module fixtures, use `import Queue as Queue.`, `import Maybe as Maybe.`,
      `import Text as Text.`, and `import Set as Set.` with this helper:

  ```jazz
  convert = \(change, values) -> map change values.
  listResult = convert (\(value) -> value == 1) [1, 2].
  queueResult = convert (\(value) -> value == 1) (Queue::fromList [1, 2]).
  maybeResult = convert (\(value) -> value == 1) (Maybe::Just 1).
  unchanged = Text::map (\(character) -> character) "abc".
  unique = Set::map (Set::fromList [1, 2]) (\(value) -> 0).
  ```

  Expect `[True, False]`, a Queue with those values, `Just True`, `"abc"`, and a
  singleton Set. No helper signature or destination-collection annotation is
  required. Add Result error preservation, NonEmpty, and Map/Dictionary key
  preservation cases, plus a collection absent from the stdlib.

- [ ] Cover Text-to-integer mapping explicitly through `Text::toChars` and List
      map, and Set-to-List through `Set::toList`. Reject a non-Char Text callback,
      Set output without Comparable evidence, generic Mappable(Text/Set) uses,
      and an output annotation that changes a List map into a Queue. Verify Set
      remains Reducible without element ordering evidence; Text reduction goes
      through its character conversion.
- [ ] Run `stdlib-spec` and `binding-signature-coherence-spec` to establish
      failures in the new class and Text-map behavior before implementation.
- [ ] Implement ordinary Jazz instances using existing traversals. Add the new
      function-first Text map with type `(Char -> Char) -> Text -> Text` and
      Unicode scalar coverage. Do not create a Mapping module or Empty class.
      Text implementation may use `toChars`, `Mappable::map`, and `fromChars`,
      with the existing linear cost and no new kernel operation. Qualify the
      generic map reference so Text's local `map` does not shadow it.
- [ ] Implement the RFC's Reducible and Combinable families while migrating their
      owning modules. Keep existing empty values. Check mapping identity/composition,
      FIFO/key order, and empty/NonEmpty behavior.
- [ ] Keep `Reduce.jz` as a small explicit-import module after Maybe is available.
      Implement `reduce :: @{Reducible(f)}: (a -> a -> a) -> f(a) -> Maybe(a)`
      once using `foldLeft` with a Maybe accumulator. Nothing takes the first
      element; Just combines the accumulator with the next element. Do not add
      intermediate List conversion, a runtime primitive, or another capability.
      Test empty and singleton inputs plus a non-associative callback that shows
      left-fold order on List and Queue through one generic helper. Register the
      module and its documentation in the authored module/API inventories.
- [ ] Run `stdlib-spec`, `prelude-loading-spec`, `builtin-catalog-spec`,
      `binding-signature-coherence-spec`, `runtime-semantics-spec`, and `loader-spec`.
      Update public capability/stdlib documentation and commit.

- [ ] Run `stdlib-spec`, `repository-audit-spec`, the complete retained hosted
      parser/core suites, and the API-doc checker. Assert old public prefix names
      are absent from active exports and consumer code, allowing historical records
      and migration documentation. Add no compiler workaround unless a focused
      source case demonstrates a real required compatibility issue.
- [ ] Run the complete frontmatter verification commands in pinned shells.
      Record each command/result and any explicit new waiver. Also run the four
      `jazz-parser-scale-full-{expression,declarations,control-flow,operator}-spec`
      suites with `-ffull-parser-scale --jobs=1 --test-show-details=failures`
      because declaration grammar changed; they are disabled in the default
      Cabal configuration. No performance claim follows from functional tests.
- [ ] Review the full RFC acceptance matrix against observed behavior. Update
      shipped status and public API docs, close the library queue child, refresh
      curation/blocker state, and commit the completed migration.

## Verification environment

Use `/nix/var/nix/profiles/default/bin/nix --extra-experimental-features
'nix-command flakes' develop --command ...` when Nix is absent from PATH.
Use the quality shell selected by the repository for the Haskell quality gate.
No compiler tests are claimed by this planning change. Run RFC, authority,
queue, docs, and whitespace checks for the design documents themselves.

## Design review record

- Approved direction: behavior-based naming, inferred generic helpers and
  implementations, constructor parameters, superclass/default support, and
  separate Text/Set mapping.
- Revised scope: one-parameter classes and constructor-preserving map; no
  functional dependencies, associated types, automatic destination selection,
  Empty class, or new class export selectors. Reducible uses a constructor
  parameter too, so Set participates and Text uses character conversion.
- Retained module change: importing a collection supplies its generic instances,
  including implementations of Prelude-owned classes.
- Review decisions approved on 2026-09-13, in the original audit order:
  1. Defer parenthesized application heads; retain the surface application AST
     and implement constructor variables through semantic normalization (Task 1).
  2. Restrict implementation/superclass declaration prerequisites to their
     bound variables instead of general termination accounting (Tasks 1-2).
  3. Combine capabilities, renames, consumers, and docs per module (Task 5).
  4. Maintainer correction: retain Reduce and the seedless helper; implement it
     with one fold and a Maybe accumulator (Task 5).
  5. Reuse ordinary method schemes and instantiation; store the parameter kind
     once in class metadata (Tasks 1-3).
  6. Use one parameterized Kind tree with derived traversals (Task 1).
  7. Use canonical semantic applications with Haskell pattern-synonym views
     where useful, without changing Jazz pattern syntax (Task 1).
  8. Use local `StateT InferState Maybe` candidate trials with isolated state,
     all-match uniqueness, and independent overlap checks (Task 2).
- RFC acceptance and ready plan metadata record this approval. Compiler and
  library implementation are not claimed by the documentation change.
