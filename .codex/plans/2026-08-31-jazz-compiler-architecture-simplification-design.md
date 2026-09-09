# Jazz compiler architecture simplification design

> Backend scope superseded on 2026-09-07 by
> [RFC 0016](../../rfcs/accepted/0016-optional-backend-removal.md) and the
> [approved removal plan](2026-09-07-remove-optional-backend.md). Backend
> representations, mirrors, and future Typed Core interpreter migration below
> are historical proposals, not remaining tasks. Retained compiler and runtime
> simplifications remain in place.

**Date:** 2026-08-31

**Status:** Implemented and verified at `daea3d1a` on 2026-09-04.

## Purpose

Simplify the active Haskell compiler architecture without changing Jazz language
behavior. The design reduces duplicate representations, makes compiler-phase
invariants visible to GHC, gives the existing interpreter complete analyzed
input, and removes runtime type-information side channels.

This is an architectural migration, not a compiler rewrite. Ordinary
`compile` and `run` behavior remains interpreter-backed throughout the work.
The interpreter is a long-lived reference engine; deleting it is not part of
this design.

The design specifically addresses:

- too many representations of types;
- the underannotated canonical core AST;
- overlapping outcome wrappers;
- duplicate module and import carriers;
- unresolved and resolved names sharing one representation;
- span-keyed runtime hint maps carrying semantic information out of band;
- provisional Typed Core nodes that can contain unsupported placeholders; and
- source-level signature syntax leaking into runtime values and evidence.

## Scope and constraints

The active implementation remains in `src/`, `app/`, `jazz/`, and `test/`.
This document is internal architecture state and does not define public Jazz
language behavior.

The migration must preserve:

- public syntax, type-system behavior, diagnostics, and runtime behavior;
- the current interpreter-backed ordinary compile/run path at every milestone;
- deterministic module, Typed Core, and Lowered IR artifacts;
- the opaque `ValidatedTypedProgram` and `ValidatedLoweredProgram` proof
  boundaries;
- Haskell/hosted-Jazz contract parity where a portable schema is required;
- the explicit opt-in status of incomplete Typed Core and Lowered IR profiles;
- the existing execution queue's accepted feature ordering; and
- focused and full repository verification after each coherent milestone.

The migration must not:

- remove the interpreter;
- cut ordinary execution over to Typed Core before full parity and a separate
  approval;
- add public language features;
- introduce a universal extensible AST framework;
- create one global compiler context;
- replace explicit pass APIs with an abstract category of compiler passes;
- use type-level machinery for runtime-selected backend support profiles;
- type-index every heterogeneous runtime value; or
- preserve temporary adapters as permanent compatibility layers.

## Current architecture

The ordinary production path currently carries the program through this
sequence:

```text
Text
  -> Token
  -> SurfaceExpr
  -> Expr
  -> CoreModule / resolved-module carriers
  -> InferenceResult / ModuleInterface
  -> CompiledProgram (still containing Expr)
  -> interpreter EvaluationMachine
  -> RuntimeValue / RuntimeProgram
```

The opt-in backend-preparation path is separate:

```text
resolved Expr
  -> InferredExpr / ProvisionalTypedExpr
  -> TypedProgram
  -> ValidatedTypedProgram
  -> LoweringAnalysis
  -> LoweredProgram
  -> ValidatedLoweredProgram
```

This separation was useful while the backend profile was being established,
but it has left several architectural costs:

1. Inner `Expr` nodes generally do not carry a stable identity, complete span,
   resolved semantic type, or evidence decisions.
2. Inference transports runtime decisions through maps keyed by module path,
   source span, and textual name.
3. `Name` represents unresolved source names, qualified names, resolved names,
   builtins, and generated names at once.
4. Source, inference, Typed Core, and representation types have overlapping
   structure without clear abstraction boundaries.
5. Parsed, resolved, compiled, and runtime module records repeat identity,
   import, ordering, and interface data.
6. Typed Core production exposes status, private outcome, public result, and
   portable outcome wrappers for closely related states.
7. Runtime values retain source-level `SignatureType` wrappers because the
   analyzed core does not carry the semantic decisions directly.

The simplification keeps representations that express genuinely different
abstraction levels and removes representations that merely compensate for
missing phase information.

## Selected architecture

### 1. Phase-indexed canonical core

The canonical core becomes one phase-indexed family:

```haskell
data CorePhase
  = Lowered
  | Resolved
  | Analyzed

type family CoreNameAt (phase :: CorePhase) where
  CoreNameAt 'Lowered  = UnresolvedName
  CoreNameAt 'Resolved = ResolvedName
  CoreNameAt 'Analyzed = ResolvedName
```

`Lowered` here means lowered from surface syntax into canonical core. It is not
the backend's Lowered IR.

The core family includes:

```haskell
data Expr (phase :: CorePhase)
data Pattern (phase :: CorePhase)
data Statement (phase :: CorePhase)
data CoreModule (phase :: CorePhase)
data CoreProgram (phase :: CorePhase)
```

Every core node carries:

- a deterministic `CoreNodeId`;
- a complete `SourceSpan`; and
- phase- and node-category-specific facts.

Canonical lowering allocates identities. Resolution and analysis preserve the
same identity and span, so diagnostics, profiling, and semantic facts refer to
one node without span-based re-identification.

Facts are indexed by phase and node category rather than stored in one loose
annotation record:

```haskell
data CoreSort
  = ExpressionSort
  | PatternSort
  | StatementSort

type family FactsAt (phase :: CorePhase) (sort :: CoreSort) where
  FactsAt 'Lowered  sort = ()
  FactsAt 'Resolved sort = ()
  FactsAt 'Analyzed 'ExpressionSort = ExpressionFacts
  FactsAt 'Analyzed 'PatternSort    = PatternFacts
  FactsAt 'Analyzed 'StatementSort  = StatementFacts
```

The resolver accepts only `CoreProgram 'Lowered` and returns only
`CoreProgram 'Resolved`. Analysis accepts only resolved input and returns only
analyzed output. The ordinary interpreter and Typed Core builder accept only
analyzed input.

Nominal roles prevent `coerce` from bypassing phase transitions. Constructors
that establish whole-program invariants remain private.

This use of data kinds, closed type families, and indexed ADTs makes illegal
pass composition unrepresentable without introducing a generic visitor or
extensible-AST framework. GADT constructors are reserved for cases where a
constructor genuinely refines its result phase or node kind; the migration
will not use GADT syntax merely to make an otherwise uniform constructor look
more advanced.

### 2. Narrow name types

The mixed-state `Name` sum is replaced with distinct types:

```haskell
data UnresolvedName
  = UnqualifiedName Identifier
  | QualifiedName ModuleQualifier Identifier

data ResolvedName = ResolvedName
  { resolvedOrigin :: ResolvedNameOrigin
  , resolvedIdentifier :: Identifier
  }

data ResolvedNameOrigin
  = CurrentModule
  | ImportedModule ModulePath
  | AmbientPrelude
```

Builtin and compiler-generated identities receive dedicated, explicit
constructors or identifiers within the resolved domain; they are not confused
with unresolved spelling.

Name traversal uses `Bifunctor`, `Bifoldable`, and `Bitraversable` where syntax
is genuinely parametric in both type-constructor names and variables. It does
not use an open name-family hierarchy or reflection of module paths at the
type level.

### 3. Consolidated type representations

The design retains four abstraction levels because each answers a different
question:

1. `SignatureType`: what type syntax did the author write?
2. `SemanticType`: what type did analysis infer or instantiate?
3. `TypedRepresentationRecipe`: how should a typed value be represented?
4. `LoweredRepresentation`: what concrete backend representation is emitted?

All other type-like structures must be a view, constraint, or alias of one of
these levels rather than another independently recursive type tree.

#### Source signatures

Source signatures become name-parametric:

```haskell
data SignatureType typeName variable
  = SignatureVariable variable
  | SignatureConstructor
      typeName
      [SignatureType typeName variable]
  | SignatureFunction
      (SignatureType typeName variable)
      (SignatureType typeName variable)
  | SignatureTuple [SignatureType typeName variable]
  | SignatureList (SignatureType typeName variable)
  deriving stock
    (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

type SurfaceSignatureType =
  SignatureType Identifier TypeVariableName

type CoreSignatureType phase =
  SignatureType (CoreNameAt phase) TypeVariableName
```

Constraint syntax, payload types, and related signature structures follow the
same pattern. A shared `NumericType` replaces surface-specific numeric type
duplicates. The shared tree has one `Bifunctor`, `Bifoldable`, and
`Bitraversable` implementation for transformations that must change both type
constructor names and type variables; ordinary `Functor` and `Traversable`
operate on the variable parameter.

#### Semantic types

Inference and Typed Core share one parametric semantic tree:

```haskell
data SemanticType typeName variable
  = SemanticVariable variable
  | SemanticConstructor
      typeName
      [SemanticType typeName variable]
  | SemanticFunction
      (SemanticType typeName variable)
      (SemanticType typeName variable)
  | SemanticTuple [SemanticType typeName variable]
  | SemanticList (SemanticType typeName variable)
  deriving stock
    (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
  deriving anyclass (NFData)

type InferenceType =
  SemanticType ResolvedName InferenceVariable

type TypedType =
  SemanticType TypedCoreName TypedTypeParameterId

type AnalyzedType =
  SemanticType ResolvedName AnalyzedTypeVariable
```

Inference variables, substitutions, generalization, and skolemization remain
in inference-specific wrappers. Typed binder identities and fully elaborated
type parameters remain Typed Core concerns. Sharing the recursive tree does
not erase those stage invariants.

Integer-literal ranges move out of the semantic type tree and into solver
numeric constraints. They describe admissible values, not a distinct type
constructor.

`Generic`, standalone deriving, and `DerivingVia` remove mechanical instance
code. `Traversable` and `Bitraversable` supply real variable and name/type
transformations. A recursion-scheme framework is not introduced.

### 4. Phase-indexed modules, imports, and programs

One indexed module record replaces the overlapping core, resolver, and
compiled module carriers:

```haskell
data CoreModule (phase :: CorePhase) = CoreModule
  { coreModuleIdentity :: ModuleIdentity
  , coreModuleImports :: [ModuleImport phase]
  , coreModuleStatements :: [Statement phase]
  , coreModuleFacts :: ModuleFactsAt phase
  }
```

Imports are indexed by the same phase:

```haskell
type family ImportExposureAt (phase :: CorePhase) where
  ImportExposureAt 'Lowered  = DeclaredImportExposure
  ImportExposureAt 'Resolved = ImportExposure
  ImportExposureAt 'Analyzed = ImportExposure

data ModuleImport phase = ModuleImport
  { importedModule :: ModulePath
  , importAlias :: Maybe ModuleQualifier
  , importExposure :: ImportExposureAt phase
  }
```

This replaces `ParsedImport`, `CoreResolvedImport`, and `ResolvedImport`.
Declared exposure remains distinct from validated resolved exposure.

Foundational module identities become opaque types:

```haskell
newtype ModulePath = ModulePath (NonEmpty Identifier)
newtype SourceFile = SourceFile FilePath

data ModuleIdentity = ModuleIdentity
  { modulePath :: ModulePath
  , moduleSource :: SourceFile
  }
```

Smart constructors validate module paths. The bundled prelude has an explicit
identity and no longer relies on an empty-path sentinel.

The whole program is an invariant-bearing value:

```haskell
data CoreProgram phase = CoreProgram
  { coreProgramPrelude :: PreludeArtifact phase
  , coreProgramEntry :: ModulePath
  , coreProgramModules :: NonEmpty (CoreModule phase)
  }
```

Its private constructor guarantees:

- exactly one module for each path;
- the entry module exists;
- dependency-first deterministic ordering; and
- every resolved import refers to an earlier module or the explicit prelude.

Resolution uses temporary `ModuleDiscoveryFacts` and `ReferenceInventory`
values while discovering the graph. It accumulates resolved modules in a
`Seq`, preserving dependency order without reverse-list accumulation and a
final reversal.

`CompiledProgram` and `CompiledDependency` disappear. The interpreter consumes
`CoreProgram 'Analyzed` directly. `CompiledPrelude` becomes
`PreludeArtifact 'Analyzed`. A direct `Map ModulePath` supplies dependency
lookup. `ImportedInterface` remains only if profiling proves that its merged
projection provides material value.

Runtime-owned `RuntimeModule` and `RuntimeProgram` remain separate because they
hold evaluated values rather than compiler syntax.

### 5. Concrete compiler build outcomes

Maintainer-approved revision (2026-09-04): consolidate the duplicated Typed Core
outcomes without introducing a generic compiler-result abstraction.

```haskell
data TypedCoreBuildResult
  = TypedCoreProductionBlockedByDiagnostics
  | TypedCoreProductionUnsupported (NonEmpty TypedCoreProductionFailure)
  | TypedCoreProductionInvariantFailures (NonEmpty TypedCoreValidationFailure)
  | TypedCoreProductionSucceeded ValidatedTypedProgram
  deriving (Eq, Show)
```

`TypedCoreProductionResult` retains its private constructor, containing
`InferenceResult` and `TypedCoreBuildResult`. Callers observe the concrete build
result directly. Delete the duplicate private outcome, constructor adapters, and
status conversion. Preserve the validated-program observation projection.

Keep `LoweredIRLoweringResult` and its domain-specific constructors. There is no
current shared caller requiring polymorphic result processing, so do not add
`CheckedBuild`, generic instances, or tests of compiler-derived machinery.

Move the existing hosted outcome schema into `TypedCore.Portable` and name its
type `PortableTypedCoreOutcome` in Haskell and Jazz. Preserve constructor names
and encoded values. Keep its existing canonical encoder in the contract layer;
do not invent an unused conversion from production failures to the narrower
portable schema.

`RuntimeOutcome` and `RunExecution` remain distinct: they describe program
termination and host/driver orchestration.

### 6. Complete analyzed-node facts

`ExpressionFacts` includes:

- the inferred `SemanticType`;
- resolved type instantiations;
- selected capability evidence;
- numeric-literal specialization; and
- a compact interpreter runtime plan.

`PatternFacts` includes binding types, constructor identity, and refutability
or coverage information already established by analysis. `StatementFacts`
includes declaration-level schemes, binder identities, and exported semantic
information.

Facts must record decisions already made by analysis. They must not become a
second mutable inference environment or a bag of optional fields.

### 7. Node-local runtime plans

The span-keyed `BindingRuntimeHintKey -> SignatureType` side channel is
replaced by a plan on the relevant analyzed expression:

```haskell
newtype RuntimePlan =
  RuntimePlan (Seq RuntimeObligation)
  deriving newtype (Semigroup, Monoid)

data RuntimeObligation
  = InstantiateTypes (NonEmpty AnalyzedType)
  | SupplyEvidence (NonEmpty EvidenceReference)
  | SpecializeNumericLiteral NumericTarget
  | ConstrainResult AnalyzedType
```

`Semigroup` and `Monoid` give analysis a standard ordered accumulation model.
The sequence is not promoted to the type level because obligations are
program-derived runtime data and their heterogeneous ordering is already
validated by analysis.

After migration, runtime-hint fields disappear from:

- `InferenceResult`;
- module interfaces and prelude artifacts;
- runtime expression and scope requests;
- `EvaluationContext`;
- closures; and
- deferred host-call state.

Source spans remain attached for diagnostics, traces, and profiling. They are
not semantic lookup keys.

### 8. Interpreter migration

The first new interpreter boundary is:

```haskell
interpretAnalyzedProgram
  :: CoreProgram 'Analyzed
  -> RuntimeRequest
  -> RuntimeOutcome
```

Closures capture `Expr 'Analyzed`, and runtime environments use
`ResolvedName`. The existing evaluation machine and heap-safe control model
remain intact.

After Typed Core has full ordinary-execution parity, a second entry point is
added:

```haskell
interpretTypedProgram
  :: ValidatedTypedProgram
  -> RuntimeRequest
  -> RuntimeOutcome
```

The two interpreters run through a differential period. Typed Core may become
the reference interpreter input only after all supported language profiles,
multi-module execution, runtime primitives, diagnostics, and performance have
met the cutover criteria.

The two entry points remain explicit functions. An `Interpretable` typeclass or
generic compiler-pass category would obscure their intentionally different
migration roles and would not eliminate invalid states.

### 9. Runtime value and evidence cleanup

Before this migration, `VTyped`, `VExplicitTypeApplication`, and
`VRuntimeExplicitResultHints` influenced evaluation and primitive selection.
They were consolidated into semantic annotations, preserving deferred obligations
with the values that own them. Runtime values and method candidates now consume
analyzed types and `EvidenceReference` directly; source-signature conversion and
optional method-signature handling are removed.

The required order is:

1. move their decisions into analyzed expression facts and runtime
   obligations;
2. make primitive dispatch and result specialization consume those semantic
   obligations;
3. prove parity with focused and differential tests; and
4. remove only wrappers that are then semantically inert.

Maintainer-approved clarification (2026-09-04): deferred obligations may need to
remain attached to first-class values after their expression has finished
executing. Simplify redundant conversions and repeated annotation handling;
retaining necessary value-associated metadata is allowed. The acceptance
criterion is clearer ownership and less duplicated code, not zero wrapper
constructors.

Runtime evidence uses stable semantic identity rather than source syntax:

```haskell
data EvidenceReference = EvidenceReference
  { evidenceCapability :: CapabilityId
  , evidenceImplementation :: ImplId
  , evidenceMethod :: Maybe MethodId
  , evidenceType :: AnalyzedType
  }
```

It does not retain free-form textual identity or source-level
`SignatureType`.

The completed pass retains the existing closure, builtin, operator, constructor,
and method variants. A `RuntimeCallable` abstraction is outside this pass; any
future proposal must demonstrate less repeated dispatch and annotation handling.
A fully type-indexed `RuntimeValue` GADT is rejected because Jazz values are
heterogeneous and runtime checked; the additional existential packaging would
not remove those checks.

### 10. Checked Typed Core construction

Maintainer-approved revision (2026-09-04): construct Typed Core directly from
analyzed core and report unsupported features during construction. Keep checks
that require constructed expressions alongside construction, including recursive
support and capture checks. Preliminary function information and multiple passes
are allowed where needed; do not force a single traversal or add an eligibility
wrapper that discards preparation results.

The producer returns the concrete `TypedCoreBuildResult`. Error diagnostics
block production first. Checked construction accumulates unsupported failures in
the existing order; only a successfully constructed raw `TypedProgram` reaches
the independent validator. Validation failures remain distinct from unsupported
features. No failed construction exposes a successful partial artifact.

Remove provisional expression/statement trees and inference-owned production
failures. Preserve analyzed semantic decisions needed by construction without
retaining the solver state. Reuse earlier work only when capture and
specialization contexts match; do not add a speculative cache or another IR.

Advanced Haskell features and extensions are permitted whenever they simplify
code or improve maintainability. Earlier blanket exclusions do not override
this maintainer-approved criterion.

### Boundary coverage

The migration adds or retains focused coverage for:

- deterministic node identity and complete source spans;
- lowered-to-resolved and resolved-to-analyzed phase transitions;
- unresolved/resolved name separation and module origins;
- semantic-type normalization and conversion laws;
- module-program uniqueness, entry existence, and dependency order;
- temporary analyzed-facts-to-runtime-hints parity;
- old/new interpreter differential results and diagnostics;
- capability evidence selection and scoping;
- numeric defaulting, specialization, and explicit type applications;
- recursion, closures, patterns, imported values, and prelude behavior;
- Typed Core production and validation;
- Lowered IR production and validation; and
- portable hosted schema projection.

Temporary bridge tests are removed with their bridge. The implementation does
not add tests that merely restate GADT constructor shapes or prove that code
rejected by GHC fails to compile.

### Focused and repository gates

Each milestone runs its directly affected suites and, before closure:

```sh
cabal build all --jobs=1
cabal test all --test-show-details=direct --jobs=1
cabal check
```

Representation-heavy and interpreter milestones also run
`benchmark-stage-spec` and `profiling-spec`. The final architecture gate uses
`scripts/ci/main-functional.sh`, including repository validation, executable
examples, and `nix flake check`.

Authoritative commands run inside the checked-in Nix development environment.

## Deletion criteria

An old representation may be removed only when:

1. no production constructor or consumer remains outside its named adapter;
2. structural or behavioral parity exists at its replacement boundary;
3. focused and full compiler tests pass;
4. hosted/bootstrap projections remain stable where applicable; and
5. deletion requires no unapproved language or backend-capability decision.

Specific gates are:

- The mixed-state `Name` disappears after all resolver outputs and downstream
  environments use `ResolvedName` and all earlier phases use
  `UnresolvedName`.
- Duplicate type trees disappear after their conversions become aliases or
  views and no stage-specific invariant depends on a separate recursive tree.
- Old module carriers disappear after resolver, compiler, loader, and tests use
  `CoreProgram phase` or a runtime-owned value.
- Runtime hint maps disappear after analyzed facts drive identical interpreter
  behavior across single- and multi-module execution.
- Overlapping outcome wrappers disappear after all consumers use
  the concrete build result or the portable schema.
- Runtime value wrappers disappear only after runtime obligations fully own
  their behavior.
- The raw-core interpreter can be retired only after full Typed Core ordinary
  execution coverage, multi-module parity, all supported patterns and
  primitives, stable diagnostics, accepted performance, a soak period, and
  separate user approval.

## Advanced Haskell policy

Advanced features are selected when they remove invalid states or handwritten
code:

- data kinds, closed type families, indexed ADTs, and nominal roles for phase
  safety;
- GADTs where a constructor genuinely refines its result phase or node kind;
- parametric recursive types plus `Traversable` and `Bitraversable` for real
  transformations;
- opaque newtypes and smart constructors for validated invariants;
- `NonEmpty` for failures and paths that cannot validly be empty;
- `Semigroup` and `Monoid` for ordered plan accumulation;
- applicative validation where independent errors genuinely accumulate; and
- `Generic`, `NFData`, standalone deriving, and `DerivingVia` for boilerplate.

The design rejects advanced features whose cost exceeds their proof value:

- open data or type families for this closed pipeline;
- a generic recursion-scheme or visitor framework;
- type-level module paths or namespaces;
- singleton reflection without a real runtime witness requirement;
- a categorical compiler-pass wrapper over passes with different contexts and
  recovery semantics;
- a fully indexed heterogeneous runtime-value GADT;
- linear types for the current persistent compiler data; and
- phantom validation markers in place of opaque validated constructors.

## Expected result

After the architecture milestones, Jazz has:

- one phase-indexed canonical program family from canonical lowering through
  analysis;
- one clear recursive representation at each of the source, semantic, typed
  representation, and lowered representation levels;
- complete node-local semantic facts;
- no span-keyed semantic side channel;
- one checked-build outcome vocabulary;
- fewer module and import carriers;
- checked Typed Core construction from analyzed facts;
- a preserved interpreter with better input invariants; and
- explicit, testable criteria for any eventual execution cutover.

The reduction in type count is a consequence of clearer invariant ownership,
not an objective to collapse semantically distinct compiler stages.
