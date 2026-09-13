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
  - src/Jazz/Compiler/AST.hs
  - src/Jazz/Compiler/Parser/AST.hs
  - src/Jazz/Compiler/Parser/Signature.hs
  - src/Jazz/Compiler/Parser/CapabilityDeclaration.hs
  - src/Jazz/Compiler/Parser/Lower.hs
  - src/Jazz/Compiler/SignatureRendering.hs
  - src/Jazz/Compiler/TypeInference/Signature.hs
  - src/Jazz/Compiler/TypeInference/Solver.hs
  - src/Jazz/Compiler/TypeInference/Scope.hs
  - src/Jazz/Compiler/CapabilityFacts.hs
  - src/Jazz/Compiler/TypeInference.hs
  - src/Jazz/Compiler/TypeInference/Capabilities.hs
  - src/Jazz/Compiler/TypeInference/ImplChecking.hs
  - src/Jazz/Compiler/TypeInference/Instantiation.hs
  - src/Jazz/Compiler/TypeInference/State.hs
  - src/Jazz/Compiler/TypeInference/Analyzed.hs
  - src/Jazz/Compiler/CoreIdentity.hs
  - src/Jazz/Compiler/SemanticFacts.hs
  - src/Jazz/Compiler/Runtime/Types.hs
  - src/Jazz/Compiler/Runtime/Engine.hs
  - src/Jazz/Compiler/Runtime/Semantics.hs
  - src/Jazz/Compiler/ModuleResolver.hs
  - src/Jazz/Compiler/ModuleResolver/Names.hs
  - src/Jazz/Compiler/ModuleResolver/Imports.hs
  - src/Jazz/Compiler/ModuleExports.hs
  - src/Jazz/Compiler/ModuleInterface.hs
  - src/Jazz/Compiler/ModuleAnalysis.hs
  - src/Jazz/Compiler/ModuleRuntime.hs
  - src/Jazz/Compiler/TypeInference/Interface.hs
  - jazz/compiler/ParserDeclaration.jz
  - jazz/compiler/ParserTypes.jz
  - jazz/compiler/CoreTypes.jz
  - jazz/compiler/CoreLower.jz
  - test/Jazz/Compiler/Parser/Foundation/SignaturesTests.hs
  - test/Jazz/Compiler/Parser/Foundation/InvalidSyntaxTests.hs
  - test/Jazz/Compiler/Parser/DeclarationParserSpec.hs
  - test/Jazz/Compiler/Diagnostics/SignatureRenderingSpec.hs
  - test/Jazz/Compiler/Parser/SourceRangesSpec.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/ConstraintsTests.hs
  - test/Jazz/Compiler/Semantics/BindingSignature/InferenceOwnershipTests.hs
  - test/Jazz/Compiler/Semantics/Runtime/CapabilitiesTests.hs
  - test/Jazz/Compiler/Semantics/PuritySemanticsSpec.hs
  - test/Jazz/Compiler/Modules/Loader/CapabilitiesTests.hs
  - test/Jazz/Compiler/Modules/Loader/AliasClassTests.hs
  - test/Jazz/Compiler/Modules/ModuleExportsSpec.hs
  - test/Jazz/Compiler/Modules/ModuleResolutionSpec.hs
  - test/Jazz/Compiler/Modules/ModulePipelineContractSpec.hs
  - test/Jazz/Compiler/ProfilingSpec.hs
  - test/Jazz/Compiler/HaskellTypeclassContractsSpec.hs
  - test/Jazz/Compiler/Bootstrap/CanonicalParserComparison.hs
  - test/Jazz/Compiler/Bootstrap/CanonicalCoreComparison.hs
  - test/Jazz/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs
  - test/Jazz/Compiler/Bootstrap/CanonicalCoreComparisonSpec.hs
  - test/Jazz/Compiler/Bootstrap/JazzParserTypesDeclarationsModulesSpec.hs
  - docs/language/capabilities.md
  - docs/language/types-and-signatures.md
  - docs/language/modules.md
  - docs/reference/expression-grammar.md
  - docs/reference/module-resolution.md
verification:
  - cabal build all --jobs=1
  - cabal test all --jobs=1 --test-show-details=failures
  - cabal test jazz-parser-scale-full-expression-spec jazz-parser-scale-full-declarations-spec jazz-parser-scale-full-control-flow-spec jazz-parser-scale-full-operator-spec -ffull-parser-scale --jobs=1 --test-show-details=failures
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
The revised contract, initial simplifications, compiler-reuse requirements,
and final correctness refinements are approved on 2026-09-13.
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
- Reuse the existing scope preparation, constraint queue, draft finalization,
  module interfaces, reference identities, and method-body checking. Implement
  the seven reuse requirements within Tasks 1-4, without separate setup tasks
  or parallel representations.
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
`JN-GENERIC-CAPABILITIES-LIBRARY-001`, recorded in Next Curation Target behind
`JN-GENERIC-CAPABILITIES-CORE-001`. At core closeout, create its ready plan
from Task 5 and the rename CSV, naming the concrete stdlib, catalog, consumer,
and test files. Preserve
the approved scope and continue to that milestone without reopening it for
permission. The separate child is an integration/verification boundary, not a
request to leave the rename undone.

## Implementation

### Task 1: Kinded applications and generic declaration templates

**Files:** `src/Jazz/Compiler/TypeRepresentation.hs`,
`SemanticDeclarations.hs`, `AST.hs`, `Parser/AST.hs`, `Parser/Signature.hs`,
`Parser/CapabilityDeclaration.hs`, `Parser/Lower.hs`, `SignatureRendering.hs`,
`TypeInference/Signature.hs`, `TypeInference/Solver.hs`, `TypeInference/Scope.hs`,
`jazz/compiler/{ParserDeclaration,ParserTypes,CoreTypes,CoreLower}.jz`, and
`test/Jazz/Compiler/Bootstrap/{CanonicalParserComparison,CanonicalCoreComparison}.hs`.
The frontmatter includes the corresponding fixture and public-documentation
owners for Tasks 1-4; these paths describe the same approved core scope.

**Interfaces:** Keep surface `SignatureType` applications as `TypeApplication
name [arguments]`. Existing parsing already accepts `f(a)`, `f(a, b)`, and
`Result(error)`; extend name resolution and kind-aware normalization to give
them meaning. Defer parenthesized application heads without changing the
surface application AST or hosted type-application encoding. Changes to
`Parser/Signature.hs` extract shared constraint-prefix parsing only.

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
After solving each declaration group's kind constraints, default any remaining
unconstrained kind variables to `Type`, then publish fixed kinds. Reuse that
metadata across imports; do not infer kinds from later uses or add kind
polymorphism.

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

Use `prepareScope`, `ScopePreparation.preparedDeclarations`, and
`PreparedDeclaration` in `TypeInference/Scope.hs` to prepare kind skeletons and
checked implementation templates. Extend the existing registration/cache flow
so real checking consumes those prepared declarations. Add a local dependency
traversal only where kind dependencies require it; do not introduce another
compiler phase or prepared-module representation.

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
  not serialized private representation strings. Include an unused marker/phantom
  parameter that defaults to `Type` and a constructor parameter whose inferred
  kind survives module transport; extend the Task 4 import fixture for the latter.

- [ ] Run `cabal test parser-foundation-spec declaration-parser-spec
binding-signature-coherence-spec signature-rendering-spec --jobs=1
--test-show-details=failures`; confirm the new behavior fails on the baseline.
- [ ] Extract the existing `@{...}:` prefix from
      `constrainedSignaturePayloadParser`/`constraintBlockParser` in
      `Parser/Signature.hs` for use by both signatures and declaration headers.
      Retain `SignatureConstraint` and its shared lowering/traversal. Factor
      expression-binding parsing from `parseImplBody` for class default bodies,
      retaining ordinary expressions and their source spans. Apply class/impl
      context restrictions in declaration validation, not a second constraint
      grammar. Retain the one-parameter class/impl arity checks. Reject
      multi-parameter declarations and dependency clauses in focused syntax tests.
      Normalize the existing named-head applications through the shared owner;
      substitutions and renderers must agree on one semantic shape. Validate
      implementation contexts as `C(a)` for head-bound variables and superclass
      contexts as `C(classParameter)`. Keep method-local and use-site constraints
      governed by ordinary schemes, including compound targets.
- [ ] Extend `prepareScope` and its declaration cache for kind/template
      preparation, retaining forward declaration behavior and diagnostic ownership.
- [ ] Extend hosted declaration encodings for contexts/default bodies and complete
      their exhaustive adapters; reuse existing type-application encodings and
      retain structural differential checks for the changed declaration syntax.
      Run the focused suites plus `canonical-parser-comparison-spec`,
      `canonical-core-comparison-spec`, and
      `jazz-parser-types-declarations-modules-spec`. Commit the coherent type and
      declaration representation migration once all components build.

### Task 2: Generic resolution and runtime evidence

**Files:** `src/Jazz/Compiler/CapabilityFacts.hs`, `SemanticDeclarations.hs`,
`TypeInference.hs`, `TypeInference/Capabilities.hs`, `TypeInference/ImplChecking.hs`,
`TypeInference/Solver.hs`, `TypeInference/Instantiation.hs`,
`TypeInference/State.hs`, `TypeInference/Analyzed.hs`, `CoreIdentity.hs`,
`SemanticFacts.hs`, `Runtime/Types.hs`,
`Runtime/Engine.hs`, and `Runtime/Semantics.hs`.

**Interfaces:** The template from Task 1 is the sole declaration environment.
Class obligations retain `DeferredExplicitConstraint` and the existing
constraint queue, with the class identity and one kinded target.
Selected evidence identifies an implementation plus substitution and prerequisite
evidence; deferred evidence refers to an enclosing scheme parameter. Replace
the duplicate `ExpressionEvidenceSeed` record with shared `EvidenceReference`
values before extending evidence. Both current records have the same fields
and underlying target type. `expressionEvidenceFacts` resolves their types
through the existing `Draft`/`finalizeCheckedExpression` path, including nested
prerequisite evidence once added. Extend existing analyzed facts and method
cells; do not add another evidence-lowering pass or runtime instruction plan.
Preserve the public driver result APIs.

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

  Expect `(True, False)`. This also checks that one implementation body can use
  the same overloaded method at both element and list types. Missing `Same(Bool)`
  must fail at the use site.
  Also implement `Transforming(List)` and `Transforming(Result(error))` for
  the Task 1 custom class. Check element-type changes, preserved Result errors,
  method-local quantifier reuse at different types, and the inferred unchanged
  collection constructor. Reject a Queue result annotation on a List mapping.
  Reject `transform = \(change, values) -> values` for `Transforming(List)`,
  which incorrectly equates the method's independent element variables. Also
  reject an `impl Keeping([a])` of `class Keeping(t) { keep :: t -> t. }`
  with `keep = \(values) -> [True]`, specializing its instance parameter to Bool.

- [ ] Run binding/runtime suites and confirm failures concern unsupported new
      semantics, not fixture/import mistakes.
- [ ] Freshen each instance's variables and match the entire head against the
      obligation. Use exact constructor identity for overlap and exact-match
      preference: Int/Int64 and Float/Float64 heads remain distinct. Reuse the
      solver's structural traversal, variable binding, and occurs checks with
      narrowly scoped exact primitive matching; do not add a second unifier or
      change ordinary expression numeric compatibility. Reject `[a]`/`[Int]`
      overlap regardless of prerequisites. Retain the existing numeric dispatch
      fixtures and Prelude loading, extending them only for gaps in exact-head
      preference and numeric-compatible fallback.
      Select the head before solving its variable-only prerequisites; failure
      must not select a different head. The declaration restrictions provide
      descent; add no size/occurrence accounting.
      Reject compound/unbound declaration prerequisites, while accepting inferred
      and use-site compound obligations such as `Same([[Int]])`.
- [ ] Extend `resolveDeferredExplicitConstraint`,
      `finalizeDeferredExplicitConstraintsAtWithEntailments`, and
      `deferredConstraintIsEntailed` for generic prerequisites and superclass
      entailment. Reuse the existing queue and statement checkpoints, preserving
      source diagnostics and generalization of unresolved constraints. Do not
      introduce a second obligation type, work queue, or solving pass.
- [ ] Wrap candidate trials locally in `StateT InferState Maybe`, using existing
      `transformers` and unification functions. Run every candidate from the same
      immutable pre-trial state, discard failures, and examine all matching heads.
      Apply exact-match preference and require a unique selection before checking
      prerequisites. Preserve inference allocation/rollback policies and report
      diagnostics outside silent trials. Defer obligations whose instance head
      is not yet determined; do not select the first plausible implementation.
      Cover failed-trial isolation through observable inference/dispatch behavior
      and reject overlapping heads even when one prerequisite is unavailable.
      Do not substitute `previewInference`: it deliberately discards outputs and
      outstanding constraints and has a different allocation/continuation contract.
- [ ] Check bodies under declared prerequisites and self evidence. Reuse
      `inferRigidTypeVars` and the signed-binding checker's rigidity discipline
      for instance parameters and method-local quantified variables. Restore the
      surrounding rigid set after checking and constraint finalization, including
      failures. Register method types as ordinary constrained `SemanticScheme`
      values and reuse `instantiateTypeScheme` in
      `TypeInference/Instantiation.hs`; instantiate method-local variables freshly
      per use while preserving the explicit class-parameter binding order.
      Remove `implMethodEnv`'s target-specialized `PlainTypeBinding` treatment
      of method names; references in bodies use the same overloaded schemes,
      with prerequisites and self evidence supplied through the entailment path.
      Reuse the recursive `Same([a])` fixture above to verify both evidence targets.
- [ ] Unify the two evidence records and update their producers/consumers, then
      pass selected/deferred evidence through analyzed binders and callables.
      Finalize substitutions in the existing draft finalizer and consume the
      attached facts directly at runtime, as required by RFC 0018.
      Cover stored/partial methods, expected-result methods, empty collections,
      returned closures, and recursive dictionaries. Assert distinguishable results
      for competing test types so an incorrect instance cannot accidentally pass.
      Run `binding-signature-coherence-spec`, `runtime-semantics-spec`,
      `recursive-bindings-spec`, and `haskell-typeclass-contracts-spec`; commit.

### Task 3: Ordinary method values, superclass evidence, and defaults

**Files:** `src/Jazz/Compiler/ModuleResolver.hs`,
`ModuleResolver/Names.hs`, `CoreIdentity.hs`, `TypeInference.hs`,
`TypeInference/Capabilities.hs`, `TypeInference/ImplChecking.hs`,
`Runtime/Engine.hs`, `Runtime/Types.hs`, and the binding/runtime/purity suites.

**Interfaces:** A plain method value, `Class::method`, and qualified alias
spelling resolve to the same `CapabilityMethodKey`. Evidence from Task 2
supports superclass projection and omitted-method default bodies. Use the
existing `CapabilityMethodReference` identity for every spelling. Defaults
remain ordinary `Expr` values checked with shared method-body checking and
executed through existing method cells; preserve their defining lexical scope.

- [ ] Change the generic helper above to call plain `same`; add stored aliases
      and local shadowing cases. Add a class with `same` and default `different`,
      and a subclass that calls a superclass method. Execute default and overridden
      methods with different results; reject missing methods and superclass cycles.
- [ ] Run binding/runtime and `purity-semantics-spec` tests to establish the
      new cases' failures.
- [ ] Publish class methods with the ordinary schemes from Tasks 1-2 into the
      normal local value environment and resolve them before inference. Do not
      add a separate method generalization or value-lookup path. Follow current
      lexical rules and reject same-scope duplicate values. Do not synthesize
      wrapper bindings or fresh lexical identities for ordinary method spellings.
- [ ] Factor expected-type and constraint checking from `checkImplMethodBodies`
      into one shared body-checking operation. Use implementation prerequisites
      for supplied bodies and class/superclass/method assumptions for defaults.
      Check a default once in its defining scope, then select it with the
      implementation's evidence when no supplied body overrides it. Reuse Task 2's
      rigidity handling for class and method-local variables; include a default
      that illegally specializes a quantified variable in the existing default
      checks. Reuse ordinary expression inference and runtime method cells,
      including recursion; do not
      add a default-body AST, separate checker, or evaluator.
- [ ] Supply superclass evidence through the existing entailment path extended
      in Task 2, using declared nominal superclass relationships. Preserve the
      acyclic superclass requirement and missing-superclass diagnostics.
- [ ] Confirm class-method aliases preserve bang-name purity checks. Run the
      three focused suites and `source-ranges-spec`; update public capability and
      grammar documentation for this completed behavior and commit.

### Task 4: Existing module selectors and generic instance transport

**Files:** `src/Jazz/Compiler/ModuleExports.hs`, `ModuleInterface.hs`,
`ModuleResolver.hs`, `ModuleResolver/Imports.hs`, `ModuleAnalysis.hs`, `ModuleRuntime.hs`,
`TypeInference/Interface.hs`,
`test/Jazz/Compiler/Modules/Loader/CapabilitiesTests.hs`,
`Loader/AliasClassTests.hs`, `ModuleExportsSpec.hs`,
`ModulePipelineContractSpec.hs`. Existing module selector grammar is unchanged.

**Interfaces:** Keep existing `class C` and `value method` selectors. Selecting
a class includes its ordinary method values. Separate that source-visible
inventory from the dependency instance environment, which includes instances
for imported classes. Preserve hidden supporting type/class metadata and
original declaration identities.

Extend `ModuleInterface.interfaceCapabilities` and
`ImportedInterface.importedCapabilities` through their existing publication,
selection, and merge functions. Keep source visibility governed by
`interfacePublicExports`, selected inventories, and `importedClassNames`.
Generalize `ModuleValueBinding.interfaceBindingId` from `CoreBinderId` to an
`interfaceBindingReference :: ResolvedReference` field. Ordinary bindings use
`LexicalReference`; method values use `CapabilityMethodReference`. Update
inference imports and runtime `exportReference` consumers together. Reuse this
value export map rather than adding a parallel method-value table. Update
`closeModuleBindings` in `TypeInference/Interface.hs` and existing constructor
fixtures, including `test/Jazz/Compiler/ProfilingSpec.hs`, for the reference field;
this is fixture maintenance, not new profiling work.

- [ ] Add modules A, B, and C: A exports a class/method; B imports A and
      declares an implementation. C imports A for the method and B with an empty
      selection for its instance, then calls A's method. The empty B selection
      exposes no names. Expect successful dispatch. Repeated aliases must deduplicate the same instance;
      an additional overlapping instance must fail regardless of import order.
- [ ] Add fixtures for existing `class C` and method-value-only selections.
      Class selection provides its method names, including alias-qualified
      spellings; value-only selection preserves hidden class metadata. Assert
      private names remain inaccessible, exported helpers receive caller evidence,
      and concrete exported bindings retain defining-module evidence.
      Include an imported default that calls a private helper in its defining
      module, with a same-named helper in the implementing module, to verify
      that default reuse preserves lexical ownership.
- [ ] Add a Prelude-owned test class implemented in a Queue-like library module.
      Importing that library, even by alias, must make its generic instance usable.
      This case prevents retaining the current public-class filter on instances.
- [ ] Extend `publishModuleInterface`, `importSelectedInterface`, and existing
      capability merges for transitive instances and supporting metadata, including
      empty-selection imports. Update `ModuleRuntime` publication, selection, and
      merging to transport the corresponding method cells and deduplicate by
      nominal identity. Retain the existing module dependency order; add no
      instance-import graph, registry, or separate transport pass.
- [ ] Complete the `ModuleValueBinding` reference-field migration and method
      export inventory changes with the existing class/value/alias loader cases.
      Retain re-export rejection and add no new selector grammar. Run
      `module-exports-spec`, `module-resolution-spec`, `loader-spec`, and
      `module-pipeline-contract-spec`.
- [ ] Complete the core queue child only after Tasks 1-4 work end to end and
      the frontmatter verification commands pass, including execution of all
      four full parser-scale suites with `-ffull-parser-scale`. The quality gate
      only builds those components; it does not run them. Publish matching public
      contract changes, promote the library child from Task 5, and commit closeout.

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
- [ ] Implement collection equality through element `Equatable` methods, not
      structural `==`. Queue compares FIFO contents using existing `toList`
      and list equality; do not change its representation or normalization.
      Check queues built with `fromList [1, 2]` and `enqueue (fromList [1]) 2`
      compare equal, and use a custom element equality to catch structural
      fallback across the listed generic equality instances. Implement ordinary
      Prelude tuple instances for pairs and triples only, with one prerequisite
      per component. Preserve existing builtin tuple equality at every supported
      arity; add no variadic instances, generator, or deriving mechanism.
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
      Record each command/result and any explicit new waiver. This includes
      full parser-scale coverage after migrating the library used by the hosted
      parser. No performance claim follows from functional tests.
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
- Second review: all seven compiler-reuse requirements approved on 2026-09-13:
  1. Share inference/analyzed evidence and use existing draft finalization (Task 2).
  2. Extend the current constraint queue and entailment checks (Tasks 2-3).
  3. Transport instances through existing module interfaces and runtime cells (Task 4).
  4. Export method values through existing resolved reference identities (Tasks 3-4).
  5. Share implementation/default body checking and ordinary expressions (Task 3).
  6. Prepare kinds and templates in the existing scope preparation/cache (Task 1).
  7. Share constraint-prefix and method expression-binding parsing (Task 1).
- Final review fixes approved on 2026-09-13:
  1. Distinguish exact instance heads from numeric compatibility (Task 2).
  2. Reuse rigid-variable checking for implementation/default definitions (Tasks 2-3).
  3. Keep method references overloaded within implementations (Tasks 2-3).
  4. Default unconstrained kinds at declaration finalization (Task 1).
  5. Compare logical collection contents through element equality (Task 5).
  6. Bound initial tuple instances to ordinary pairs and triples (Task 5).
  7. Execute full parser-scale checks before core closeout (Task 4/frontmatter).
- RFC acceptance and ready plan metadata record this approval. Compiler and
  library implementation are not claimed by the documentation change.
