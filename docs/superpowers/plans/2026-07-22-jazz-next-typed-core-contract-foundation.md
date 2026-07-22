---
id: JN-BOOTSTRAP-TYPED-CORE-CONTRACT-FOUNDATION-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-LOWERED-IR-CONTRACT-FOUNDATION-001
last_verified: 2026-07-22
plan_section: "Implementation Batch: Typed-Core Contract Foundation"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-22-jazz-next-typed-core-elaboration-design.md
  - docs/superpowers/plans/2026-07-22-jazz-next-typed-core-contract-foundation.md
  - jazz-next/README.md
  - jazz-next/jazz-next.cabal
  - jazz-next/jazz/compiler/TypedCoreTypes.jz
  - jazz-next/jazz/compiler/TypedCoreValidate.jz
  - jazz-next/src/JazzNext/Compiler/TypedCore.hs
  - jazz-next/src/JazzNext/Compiler/TypedCore/Validate.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-core-modules-corpus-closure-spec jazz-core-signatures-declarations-operators-spec jazz-core-control-flow-patterns-spec jazz-core-expression-foundation-spec repository-audit-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Establish the permanent typed-core contract with matching Haskell and Jazz schemas, complete structured validators, checked canonical comparison, and repeated exact parity over 16 valid and 28 invalid fixed fixtures without changing inference, canonical-core interpretation, or lowered-IR production."
---

# Jazz-Next Typed-Core Contract Foundation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox syntax for tracking. This plan follows the repository's contract-
> foundation convention: it records exact schemas, constructor inventories,
> observable behavior, commands, and commit boundaries without embedding full
> validator bodies.

**Goal:** Implement the permanent typed-core data and validation contract for
both the Haskell stage-0 compiler and the later Jazz-authored compiler path.

**Architecture:** Mirror one reviewed typed-core schema in Haskell and ordinary
Jazz ADTs. Both implementations validate complete untrusted contract values
into the same ordered failure model; a checked test adapter renders complete
programs and failures for exact repeated comparison. This child stops before
inference produces typed core or any typed value is lowered to IR.

**Tech Stack:** GHC 9.14.1, Haskell 2010 with existing package extensions,
ordinary Jazz `.jz` modules, the stack-safe Jazz interpreter, canonical runtime
values, Cabal test components, and the Nix-pinned development environment.

**Design checkpoint:**
[`2026-07-22-jazz-next-typed-core-elaboration-design.md`](../specs/2026-07-22-jazz-next-typed-core-elaboration-design.md)

## Global Constraints

- Modify compiler behavior only under `jazz-next/`; `jazz-hs/` and `jazz2/`
  remain read-only references.
- Keep canonical core, parser lowering, name resolution, inference, module
  compilation, runtime hints, and the reference interpreter unchanged.
- Do not add an inference/elaboration entry point, provisional typed tree,
  source-to-typed-core adapter, or core-to-IR lowering entry point.
- Keep semantic typed-core types distinct from mutable solver
  `ExpressionType` values and from concrete `LoweredRepresentation` values.
- Permit representation parameters only under their owning generalized scheme;
  never guess universal boxing or a host pointer representation.
- Preserve module, statement, expression, pattern, parameter, instantiation,
  evidence, candidate, and failure order in every canonical value.
- Use structured resolved name origins and namespaces. Rendered text alone is
  not symbol identity.
- Use structured validation results as the parity contract; rendered prose is
  not canonical evidence.
- Keep the Haskell/Jazz schema mirrors exact. The checked comparison adapter
  must reject unknown or malformed runtime values rather than supplying
  defaults.
- Keep checked-in `.jz` indentation at exactly two spaces.
- Write and run each failing behavior test before its production change.
- Run compiler and test commands through the Nix-pinned environment.
- Do not run opt-in exhaustive parser-scale components. Routine Cabal `all`
  may run only the bounded `jazz-parser-scale-spec` component.
- Commit each independently reviewable green milestone.

## File and Responsibility Map

| File | Responsibility in this child |
| --- | --- |
| `jazz-next/src/JazzNext/Compiler/TypedCore.hs` | Stage-0 typed names, stable binder/evidence identities, semantic types, representation recipes, schemes, evidence, annotations, patterns, expressions, declarations, interfaces, modules, programs, and validation-result data. |
| `jazz-next/src/JazzNext/Compiler/TypedCore/Validate.hs` | Stable complete Haskell validation without inference, runtime, lowering, or backend decisions. |
| `jazz-next/jazz/compiler/TypedCoreTypes.jz` | Exact ordinary Jazz mirror of the semantic typed-core and validation-result schema. |
| `jazz-next/jazz/compiler/TypedCoreValidate.jz` | Stable complete Jazz validation using ordinary ADTs and list traversal. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs` | Checked structural conversion and canonical rendering; no inference, evidence selection, or invariant decisions. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs` | Fixed 16-valid / 28-invalid fixture inventory, manifest audits, Haskell expectations, hosted executions, repetition, and exact assertions. |
| `jazz-next/jazz-next.cabal` | Register production Haskell modules, checked-in Jazz sources, and the focused test component. |
| Coordination/status paths in frontmatter | Promote, document, close, archive, and expose typed-core expression production plus direct-call lowering as a later gate. |

## Exact Contract Schema

`TypedCore.hs` must expose the following constructor inventory. Record syntax
may replace positional constructors only when field order in canonical
comparison remains exactly the order shown here.

```haskell
newtype TypedTypeParameterId = TypedTypeParameterId Int
newtype TypedEvidenceParameterId = TypedEvidenceParameterId Int
newtype TypedBinderId = TypedBinderId ([Text], [Int], TypedCoreName)
newtype TypedSourcePath = TypedSourcePath Text

data TypedNameOrigin
  = TypedCurrentModule
  | TypedImportedModule [Text]
  | TypedAmbientPrelude

data TypedNameNamespace
  = TypedValueNamespace
  | TypedConstructorNamespace
  | TypedTypeNamespace
  | TypedCapabilityNamespace

data TypedGeneratedNameKind
  = TypedLambdaPatternArgument Int
  | TypedOperatorBinding Text
  | TypedOperatorSectionFunction
  | TypedOperatorSectionLeft
  | TypedOperatorSectionRight

data TypedCoreName
  = TypedUnresolvedSourceName Text
  | TypedUnresolvedQualifiedName Text Text
  | TypedResolvedName TypedNameOrigin TypedNameNamespace Text
  | TypedBuiltinName Text
  | TypedGeneratedName TypedGeneratedNameKind

data TypedOperatorRef
  = TypedBuiltinOperator Text
  | TypedResolvedOperator TypedCoreName Text

data TypedSpan = TypedSpan Int Int

data TypedNumericType
  = TypedInt8Type | TypedInt16Type | TypedInt32Type | TypedInt64Type
  | TypedUInt8Type | TypedUInt16Type | TypedUInt32Type | TypedUInt64Type
  | TypedFloat16Type | TypedFloat32Type | TypedFloat64Type

data TypedType
  = TypedIntType
  | TypedFloatType
  | TypedNumericType TypedNumericType
  | TypedBoolType
  | TypedCharType
  | TypedTextType
  | TypedListType TypedType
  | TypedTupleType [TypedType]
  | TypedDataType TypedCoreName [TypedType]
  | TypedFunctionType TypedType TypedType
  | TypedTypeParameterType TypedTypeParameterId

data TypedRepresentationRecipe
  = TypedUnitRecipe
  | TypedBoolRecipe
  | TypedSignedIntegerRecipe Int
  | TypedUnsignedIntegerRecipe Int
  | TypedFloatRecipe Int
  | TypedCharRecipe
  | TypedManagedTextRecipe
  | TypedManagedListRecipe TypedRepresentationRecipe
  | TypedManagedProductRecipe [TypedRepresentationRecipe]
  | TypedManagedVariantRecipe TypedCoreName [TypedType]
  | TypedClosureRecipe [TypedRepresentationRecipe] TypedRepresentationRecipe
  | TypedRepresentationParameterRecipe TypedTypeParameterId

data TypedNumericConstraint
  = TypedAnyNumericConstraint
  | TypedRuntimeArithmeticNumericConstraint
  | TypedRuntimeComparisonNumericConstraint
  | TypedIntegralNumericConstraint
  | TypedIntegralLiteralNumericConstraint Text Text

data TypedPrimitiveConstraint
  = TypedNumericPrimitiveConstraint TypedNumericConstraint TypedType
  | TypedStrictEqualityPrimitiveConstraint TypedType

data TypedCapabilityConstraint = TypedCapabilityConstraint
  Text                 -- capability/class name
  (Maybe Text)         -- qualified method key
  TypedType            -- target type

data TypedEvidenceParameter = TypedEvidenceParameter
  TypedEvidenceParameterId
  TypedCapabilityConstraint

data TypedScheme = TypedScheme
  TypedBinderId
  [TypedTypeParameterId]
  [TypedEvidenceParameter]
  [TypedPrimitiveConstraint]
  TypedType
  TypedRepresentationRecipe

data TypedTypeArgument = TypedTypeArgument TypedTypeParameterId TypedType

data TypedInstantiation = TypedInstantiation
  TypedBinderId
  [TypedTypeArgument]
  (Maybe TypedSpan)

data TypedImplId = TypedImplId
  [Text]               -- defining module path
  TypedCoreName        -- capability/class name
  [TypedType]          -- concrete target arguments

data TypedMethodId = TypedMethodId TypedImplId Text

data TypedEvidenceUse = TypedEvidenceUse
  (Maybe TypedEvidenceParameterId)
  TypedCapabilityConstraint
  TypedImplId
  (Maybe TypedMethodId)

data TypedEvidenceCandidate = TypedEvidenceCandidate
  TypedImplId
  (Maybe TypedMethodId)

data TypedEvidenceSelection
  = TypedSelectedEvidence TypedEvidenceUse
  | TypedEvidenceCandidates TypedCapabilityConstraint [TypedEvidenceCandidate]

data TypedNodeInfo = TypedNodeInfo
  TypedType
  TypedRepresentationRecipe
  [TypedInstantiation]
  [TypedEvidenceSelection]

data TypedLiteral
  = TypedIntegerLiteral Text
  | TypedFractionalLiteral Text Text (Maybe TypedNumericType)
  | TypedBooleanLiteral Bool
  | TypedCharacterLiteral Char
  | TypedTextLiteral Text

data TypedPattern
  = TypedWildcardPattern TypedNodeInfo
  | TypedVariablePattern TypedNodeInfo TypedBinderId TypedCoreName
  | TypedLiteralPattern TypedNodeInfo TypedLiteral
  | TypedConstructorPattern TypedNodeInfo TypedCoreName [TypedPattern]
  | TypedListPattern TypedNodeInfo [TypedPattern]
  | TypedConsListPattern TypedNodeInfo TypedPattern TypedPattern
  | TypedTuplePattern TypedNodeInfo [TypedPattern]
  | TypedAsPattern TypedNodeInfo TypedBinderId TypedCoreName TypedPattern
  | TypedOrPattern TypedNodeInfo [TypedPattern]

data TypedCaseArm = TypedCaseArm
  TypedPattern
  (Maybe TypedExpr)
  TypedExpr

data TypedExpr
  = TypedLiteralExpr TypedNodeInfo TypedLiteral
  | TypedVariableExpr TypedNodeInfo TypedCoreName
  | TypedLambdaExpr TypedNodeInfo TypedBinderId TypedCoreName TypedExpr
  | TypedOperatorValueExpr TypedNodeInfo TypedOperatorRef
  | TypedListExpr TypedNodeInfo [TypedExpr]
  | TypedTupleExpr TypedNodeInfo [TypedExpr]
  | TypedApplyExpr TypedNodeInfo TypedExpr TypedExpr
  | TypedTypeApplicationExpr TypedNodeInfo TypedExpr TypedSpan TypedType
  | TypedIfExpr TypedNodeInfo TypedExpr TypedExpr TypedExpr
  | TypedPatternCaseExpr TypedNodeInfo TypedExpr [TypedCaseArm]
  | TypedBinaryExpr TypedNodeInfo TypedOperatorRef TypedExpr TypedExpr
  | TypedLeftSectionExpr TypedNodeInfo TypedExpr TypedOperatorRef
  | TypedRightSectionExpr TypedNodeInfo TypedOperatorRef TypedExpr
  | TypedBlockExpr TypedNodeInfo [TypedStatement]

data TypedConstructorDeclaration = TypedConstructorDeclaration
  TypedBinderId
  TypedCoreName
  [TypedType]
  [TypedRepresentationRecipe]

data TypedDataDeclaration = TypedDataDeclaration
  TypedSpan
  TypedCoreName
  [TypedTypeParameterId]
  [TypedConstructorDeclaration]

data TypedMethodSignature = TypedMethodSignature
  TypedCoreName
  TypedSpan
  TypedScheme

data TypedClassDeclaration = TypedClassDeclaration
  TypedSpan
  TypedCoreName
  [TypedTypeParameterId]
  [TypedMethodSignature]

data TypedMethodDefinition = TypedMethodDefinition
  TypedMethodId
  TypedBinderId
  TypedCoreName
  TypedSpan
  TypedExpr

data TypedImplDeclaration = TypedImplDeclaration
  TypedSpan
  TypedImplId
  [TypedMethodDefinition]

data TypedStatement
  = TypedLetStatement TypedBinderId TypedCoreName TypedSpan TypedScheme TypedExpr
  | TypedSignatureStatement TypedBinderId TypedCoreName TypedSpan TypedScheme
  | TypedDataStatement TypedDataDeclaration
  | TypedClassStatement TypedClassDeclaration
  | TypedImplStatement TypedImplDeclaration
  | TypedExpressionStatement TypedSpan TypedExpr

data TypedResolvedImport = TypedResolvedImport
  TypedSpan
  [Text]
  (Maybe Text)
  (Maybe [Text])

data TypedModuleExport = TypedModuleExport TypedNameNamespace Text

data TypedValueInterface = TypedValueInterface TypedCoreName TypedScheme
data TypedDataInterface = TypedDataInterface TypedDataDeclaration
data TypedClassInterface = TypedClassInterface TypedClassDeclaration
data TypedImplInterface = TypedImplInterface TypedImplId

data TypedModuleInterface = TypedModuleInterface
  [TypedValueInterface]
  [TypedDataInterface]
  [TypedClassInterface]
  [TypedImplInterface]

data TypedModule = TypedModule
  [Text]
  TypedSourcePath
  [TypedResolvedImport]
  [TypedModuleExport]
  TypedModuleInterface
  [TypedStatement]
  TypedNodeInfo

data TypedProgram = TypedProgram
  (Maybe TypedModule)
  [TypedModule]
  [Text]
```

`TypedSignedIntegerRecipe`, `TypedUnsignedIntegerRecipe`, and
`TypedFloatRecipe` accept only widths in `{8,16,32,64}`, `{8,16,32,64}`, and
`{16,32,64}` respectively. Keeping widths as `Int` makes malformed widths
constructible and therefore validator-testable; valid canonical values remain
equivalent to `TypedNumericType`.

The Jazz schema uses the same constructor names and field order. Haskell
tuples used only to define newtype payloads become ordinary multi-field Jazz
constructors. The `Maybe` and list shapes remain explicit.

## Exact Validation Contract

`TypedCore.hs` also owns these result types:

```haskell
data TypedCoreValidationPath
  = TypedProgramPath
  | TypedPreludePath
  | TypedModulePath [Text]
  | TypedInterfacePath [Text]
  | TypedStatementPath [Text] Int
  | TypedExpressionPath [Text] Int [Int]
  | TypedPatternPath [Text] Int [Int]

data TypedCoreValidationKind
  = TypedUnresolvedName
  | TypedInvalidSourcePath
  | TypedDuplicateModule
  | TypedUnknownEntryModule
  | TypedDuplicateBinder
  | TypedUnknownBinder
  | TypedDuplicateTypeParameter
  | TypedInvalidTypeParameterOrder
  | TypedUnboundTypeParameter
  | TypedUnboundRepresentationParameter
  | TypedInvalidRepresentationWidth
  | TypedTypeRepresentationMismatch
  | TypedApplicationFunctionMismatch
  | TypedApplicationArgumentMismatch
  | TypedApplicationResultMismatch
  | TypedConditionalConditionMismatch
  | TypedConditionalBranchMismatch
  | TypedPatternScrutineeMismatch
  | TypedPatternGuardMismatch
  | TypedPatternArmResultMismatch
  | TypedOrPatternBinderMismatch
  | TypedDuplicateEvidenceParameter
  | TypedInvalidEvidenceParameterOrder
  | TypedInstantiationMismatch
  | TypedMissingEvidence
  | TypedDuplicateEvidence
  | TypedAmbiguousEvidence
  | TypedInvisibleImpl
  | TypedMethodSelectionMismatch
  | TypedDataRecipeMismatch
  | TypedCallableRecipeMismatch
  | TypedModuleInterfaceMismatch

data TypedCoreValidationDetail
  = TypedNoValidationDetail
  | TypedTextDetail Text
  | TypedIndexDetail Int
  | TypedArityDetail Int Int
  | TypedNameDetail TypedCoreName
  | TypedBinderDetail TypedBinderId
  | TypedTypeDetail TypedType TypedType
  | TypedRecipeDetail TypedRepresentationRecipe TypedRepresentationRecipe
  | TypedTypeParameterDetail TypedTypeParameterId
  | TypedEvidenceParameterDetail TypedEvidenceParameterId
  | TypedImplDetail TypedImplId

data TypedCoreValidationFailure = TypedCoreValidationFailure
  TypedCoreValidationPath
  TypedCoreValidationKind
  TypedCoreValidationDetail

data TypedCoreOutcome
  = TypedCoreBlockedByDiagnostics
  | TypedCoreInvariantFailures [TypedCoreValidationFailure]
  | TypedCoreSucceeded TypedProgram
```

Stable public interfaces are:

```haskell
validateTypedProgram :: TypedProgram -> [TypedCoreValidationFailure]

canonicalTypedProgramRuntimeValue :: TypedProgram -> RuntimeValue
canonicalTypedCoreOutcomeRuntimeValue :: TypedCoreOutcome -> RuntimeValue
canonicalTypedValidationFailuresRuntimeValue
  :: [TypedCoreValidationFailure] -> RuntimeValue
decodeCanonicalTypedValidationFailuresRuntimeValue
  :: RuntimeValue -> Either Text [TypedCoreValidationFailure]
```

The matching Jazz interface is:

```jazz
validateProgram :: TypedProgram -> [TypedCoreValidationFailure].
```

The Haskell and Jazz validators traverse the whole program and append failures
in program, prelude, module, interface, statement, expression, pattern, and
child order. Internal maps or sets may answer lookups but may not determine
output order.

## Fixed Fixture Inventory

The valid family contains exactly these 16 fixtures:

| Fixture | Required coverage |
| --- | --- |
| `scalar-aliases-widths` | `Int`/`Float` defaults, all explicit numeric widths, `Bool`, `Char`, and matching scalar recipes. |
| `resolved-name-origins` | Current-module, imported-module, and ambient-prelude names across all namespaces. |
| `builtin-generated-names` | Builtin and every active generated-name form without display-text identity shortcuts. |
| `list-tuple-data-recipes` | Empty/non-empty tuples, lists, resolved data applications, product/list/variant recipes. |
| `callable-recipes` | Curried semantic function type and flattened closure recipe agreement. |
| `monomorphic-binding` | Binder identity, scheme with no parameters/evidence, typed value, and interface publication. |
| `generalized-binding` | Ordered type parameters, representation parameters, primitive constraints, and binding identity. |
| `implicit-instantiation` | Complete ordered implicit type arguments with no explicit span. |
| `explicit-instantiation` | Complete ordered explicit type arguments with retained source span. |
| `explicit-capability-evidence` | Source-ordered evidence parameter and selected impl use. |
| `inferred-capability-evidence` | Inferred evidence after explicit parameters with deterministic obligation order. |
| `qualified-method-selection` | Fully applied method with one selected impl and method-body identity. |
| `partial-method-candidates` | Permitted ordered candidate set before full application. |
| `patterns-binders` | Every pattern form, binder annotations, guard typing, and arm result typing. |
| `or-pattern-alignment` | Identical binder ids, order, types, and recipes across alternatives. |
| `multi-module-interface` | Relative source paths, imports, exports, typed interfaces, prelude ownership, dependency order, and entry path. |

The invalid family contains exactly these 28 fixtures:

| Group | Fixtures |
| --- | --- |
| Names and modules | `unresolved-source-name`, `unresolved-qualified-name`, `absolute-source-path`, `duplicate-module-path`, `unknown-entry-module`. |
| Binder and parameter scope | `duplicate-binder`, `unknown-binder`, `duplicate-or-noncanonical-type-parameter`, `free-type-parameter`, `free-representation-parameter`. |
| Representation shape | `invalid-integer-width`, `type-representation-mismatch`, `data-recipe-declaration`, `callable-recipe-signature`. |
| Application and control flow | `application-function-shape`, `application-argument-type`, `application-result-type`, `if-condition-type`, `if-branch-type`. |
| Patterns | `pattern-scrutinee-type`, `pattern-guard-type`, `pattern-arm-result-type`, `or-pattern-binder-contract`. |
| Instantiation and evidence | `duplicate-or-noncanonical-evidence-parameter`, `instantiation-contract`, `missing-or-duplicate-evidence`, `ambiguous-or-invisible-evidence`, `method-or-interface-identity`. |

Each combined fixture in the final two evidence rows must contain two
independent failures in a fixed order so all four validation kinds
(`TypedMissingEvidence`, `TypedDuplicateEvidence`, `TypedAmbiguousEvidence`,
and `TypedInvisibleImpl`) plus method/interface mismatches are asserted without
changing the exact 28-case manifest.

Fixture names and ordering are fixed in the test manifest. Tests reject
missing, duplicate, unknown, or reordered cases and assert exact `16`, `28`,
and `44` counts. Adapter-hardening cases for unknown constructors, wrong arity,
wrong field category, host/runtime values, and malformed nested identities are
separate from the fixed validation manifest because they are schema-decoding
failures rather than constructible typed-core values.

## Implementation Batch: Typed-Core Contract Foundation

### Task 0: Promote the reviewed child

**Files:** this plan, `docs/execution/queue.md`,
`docs/execution/blocker-contracts.md`, and the approved design.

**Interfaces:**

- Consumes: approved design plus reviewed `status: proposed` plan.
- Produces: one exact P1/L `Ready Now` row whose plan/frontmatter metadata
  match and whose parent blocker names only this child as active.

- [x] **Step 1: Change reviewed metadata to executable metadata**

  Set frontmatter `status: ready` and `autonomous_ready: yes`. Add the child to
  `Ready Now` with the exact dependency, plan section, target paths,
  deliverable, verification commands, and `2026-07-22` verification date from
  this plan.

- [x] **Step 2: Preserve the next gate**

  Keep `Next Curation Target` empty. Update the bootstrap blocker and typed-core
  design to name only the contract foundation as active; keep inference
  elaboration, core-to-IR lowering, LLVM, object/link, and native runtime
  unpromoted.

- [x] **Step 3: Verify promotion metadata**

  Run:

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: queue checks, regression checks, docs status checks, and whitespace
  checks pass. A tool-version guard may skip Prettier outside the Nix shell but
  must not skip queue or docs status validation.

- [x] **Step 4: Commit promotion**

  ```bash
  git add docs/execution/queue.md docs/execution/blocker-contracts.md docs/superpowers/specs/2026-07-22-jazz-next-typed-core-elaboration-design.md docs/superpowers/plans/2026-07-22-jazz-next-typed-core-contract-foundation.md
  git commit -m "docs: promote typed core contract foundation"
  ```

### Task 1: Establish the Haskell schema and canonical boundary

**Files:**

- Create: `jazz-next/src/JazzNext/Compiler/TypedCore.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**

- Consumes: the exact constructor inventories above and existing
  `CanonicalValue`/`RuntimeValue` helpers.
- Produces: constructible complete Haskell typed-core values plus
  `canonicalTypedProgramRuntimeValue`, without validation decisions.

- [x] **Step 1: Register the failing focused test**

  Add `JazzNext.Compiler.TypedCore` to library `exposed-modules`. Add test suite
  `jazz-typed-core-contract-spec` with main
  `JazzNext/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs` and other modules
  `CanonicalTypedCoreComparison`, `CanonicalValue`, repository root/source
  layout, and `TestSource`.

  The initial test list must include manifest audits and deterministic
  canonical rendering for all 16 valid fixtures and all three
  `TypedCoreOutcome` constructors.

- [x] **Step 2: Prove the schema is absent**

  Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-typed-core-contract-spec --test-show-details=failures
  ```

  Expected: build failure naming missing module
  `JazzNext.Compiler.TypedCore` or missing exported constructors.

- [x] **Step 3: Implement the exact Haskell schema**

  Add every constructor in `Exact Contract Schema` and `Exact Validation
  Contract`, derive `Eq` and `Show`, and export all constructors plus the four
  stable identifier newtypes. Do not import `TypeInference.Types`, `LoweredIR`,
  `Runtime`, or backend modules.

- [x] **Step 4: Implement canonical structural rendering**

  Convert every typed-core constructor to the identically named canonical
  constructor. Render lists in stored order, `Maybe` as `Nothing`/`Just`,
  integer widths and ordinals through `runtimeIntValue`, and literal integer or
  fractional payloads as stored `Text`. Do not call Haskell `Show`.

- [x] **Step 5: Make the valid manifest green twice**

  Run the focused suite twice. Expected: all 16 names occur once in fixed
  order; the count is `16`; complete canonical program output is identical
  across both in-process constructions and both suite executions.

- [x] **Step 6: Commit the Haskell boundary**

  ```bash
  git add jazz-next/jazz-next.cabal jazz-next/src/JazzNext/Compiler/TypedCore.hs jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs jazz-next/test/JazzNext/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs
  git commit -m "feat: define typed core contract"
  ```

### Task 2: Validate the complete Haskell contract

**Files:**

- Create: `jazz-next/src/JazzNext/Compiler/TypedCore/Validate.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**

- Consumes: complete Haskell schema and fixed 28-invalid fixture manifest.
- Produces: `validateTypedProgram :: TypedProgram ->
  [TypedCoreValidationFailure]` with exact paths, kinds, details, and order.

- [x] **Step 1: Add complete failing invalid expectations**

  Materialize all 28 invalid programs and full expected failure lists. Assert
  the exact manifest order, exact count `28`, combined count `44`, and complete
  multiple-failure order for the two combined evidence fixtures.

- [x] **Step 2: Prove the validator is absent**

  Register `JazzNext.Compiler.TypedCore.Validate`, import
  `validateTypedProgram`, and run the focused suite.

  Expected: build failure naming the absent module or function while all Task 1
  canonical rendering assertions remain unchanged.

- [x] **Step 3: Implement declaration and scope validation**

  Validate relative `TypedSourcePath`, unique module paths, existing entry
  module, unique binder ids, binder references, scheme-owned type parameters,
  representation parameters, and evidence parameters. Type-parameter and
  evidence-parameter ids must be the exact zero-based sequence in stored
  order. Emit failures at the first structural path that owns the invalid value
  while continuing traversal.

- [x] **Step 4: Implement type and representation validation**

  Enforce the exact semantic mapping: `Int -> signed 64`, `Float -> float 64`,
  concrete numeric widths to matching recipes, `Bool`, `Char`, `Text`, list,
  empty/non-empty tuple, data, function, and scheme parameter mappings. Reject
  malformed numeric widths and disagreement between declarations and managed
  variant recipes.

- [x] **Step 5: Implement expression and pattern validation**

  Check function/argument/result application types, boolean conditions, equal
  branches, pattern/scrutinee types, boolean guards, equal arm results, and
  identical ordered binder contracts across or-pattern alternatives. Validate
  callable recipe parameter/result flattening against right-associated
  function types.

- [x] **Step 6: Implement instantiation, evidence, method, and interface validation**

  Check owning binder, complete ordered type-argument mappings, evidence
  parameter/use cardinality, selected versus candidate evidence, visible impl
  identities, selected method identity, and interface equality with exported
  declarations. Preserve explicit-before-inferred evidence order.

- [x] **Step 7: Make Haskell validation green twice**

  Run the focused suite twice. Expected: all 16 valid programs return `[]`; all
  28 invalid programs return their complete expected ordered lists; repeated
  output is identical.

- [x] **Step 8: Commit Haskell validation**

  ```bash
  git add jazz-next/jazz-next.cabal jazz-next/src/JazzNext/Compiler/TypedCore/Validate.hs jazz-next/test/JazzNext/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs
  git commit -m "feat: validate typed core contract"
  ```

### Task 3: Mirror and validate the contract in Jazz

**Files:**

- Create: `jazz-next/jazz/compiler/TypedCoreTypes.jz`
- Create: `jazz-next/jazz/compiler/TypedCoreValidate.jz`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**

- Consumes: green Haskell schema/validator and fixed 44-fixture manifest.
- Produces: ordinary Jazz construction and `validateProgram` with exact
  canonical parity to stage 0.

- [x] **Step 1: Add failing hosted execution**

  Add hosted batch construction for all 44 fixtures through checked-in
  `TypedCoreTypes` and `TypedCoreValidate` modules. Compare a list of tuples
  containing each complete canonical program and complete validation result.

- [x] **Step 2: Prove Jazz modules are absent**

  Run the focused suite. Expected: hosted compile failure for missing
  `TypedCoreTypes` or `TypedCoreValidate`; Haskell schema and validator
  assertions remain green.

- [x] **Step 3: Implement the exact Jazz schema**

  Add every constructor and field from `Exact Contract Schema` and `Exact
  Validation Contract`, importing only ordinary shared modules such as
  `Maybe`. Preserve constructor and field order exactly and use two-space
  indentation.

- [x] **Step 4: Implement the Jazz validator**

  Mirror every Haskell validation rule using deterministic list traversal and
  compiler-local association-list lookup. Accumulate all failures in structural
  order; do not depend on map iteration or Haskell host callbacks.

- [x] **Step 5: Decode hosted failure results strictly**

  Implement `decodeCanonicalTypedValidationFailuresRuntimeValue` to accept
  only the exact constructor names, arities, nested field categories, and
  identities in the contract. Return descriptive `Left Text` on the first
  schema error without guessing a default.

- [x] **Step 6: Make all 44 parity cases green twice**

  Run the focused suite twice. Expected: no hosted compile/runtime diagnostics;
  complete Haskell and Jazz programs and ordered failure lists match exactly on
  both runs.

- [x] **Step 7: Commit Jazz parity**

  ```bash
  git add jazz-next/jazz-next.cabal jazz-next/jazz/compiler/TypedCoreTypes.jz jazz-next/jazz/compiler/TypedCoreValidate.jz jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs jazz-next/test/JazzNext/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs
  git commit -m "feat: validate typed core in Jazz"
  ```

### Task 4: Harden schema and determinism evidence

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`

**Interfaces:**

- Consumes: both green validators and checked decoder.
- Produces: adapter rejection evidence, batch determinism, and focused
  canonical-core/lowered-IR regression evidence.

- [x] **Step 1: Add checked-adapter negative tests**

  Assert exact `Left Text` fragments for unknown validation constructor, wrong
  top-level arity, wrong field category, malformed nested binder/impl identity,
  a host-specific name identity, attempted runtime closure/value fields, and an
  absolute source path that bypasses typed construction through a malformed
  canonical value.

- [x] **Step 2: Add complete repeated batch assertions**

  Execute the full 44-program hosted batch twice in one test process and compare
  byte-identical rendered canonical output plus exact failure order. Audit that
  fixtures contain no duplicate names and cover every validation kind at least
  once.

- [x] **Step 3: Run focused regressions**

  Run the first verification command from frontmatter. Expected: typed-core,
  lowered-IR, all four hosted canonical-core suites, and repository audit pass.

- [x] **Step 4: Confirm bounded test topology**

  Confirm the new suite defines no synthetic scale generator, enables no
  `full-parser-scale` flag, and imports no inference, runtime-semantics,
  `LoweredIR.Lower`, LLVM, or backend module.

- [x] **Step 5: Commit hardening evidence**

  ```bash
  git add jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs jazz-next/test/JazzNext/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs
  git commit -m "test: prove typed core contract parity"
  ```

### Task 5: Verify and close the child

**Files:** all coordination/status paths listed in frontmatter.

**Interfaces:**

- Consumes: green contract implementation and complete verification output.
- Produces: archived contract-foundation closure and typed-core production plus
  direct-call lowering returned as the separate next gate.

- [ ] **Step 1: Run focused verification**

  Run the exact first verification command from frontmatter. Expected: every
  named component passes with failure details empty.

- [ ] **Step 2: Run routine repository verification**

  Run the warning-clean development build, routine Cabal `all`, and `cabal
  check` commands from frontmatter. Confirm only bounded
  `jazz-parser-scale-spec` runs; do not enable or invoke any
  `jazz-parser-scale-full-*` component.

- [ ] **Step 3: Record exact contract status**

  Update the typed-core design, bootstrap profile, and `jazz-next/README.md`
  with exact modules, interfaces, fixture counts, validator behavior, and the
  statement that no inference producer or lowerer exists yet.

- [ ] **Step 4: Close queue metadata**

  Mark this plan done, archive the child with concrete command evidence, empty
  `Ready Now`, and leave `Next Curation Target` empty unless a separately
  reviewed typed-core expression/direct-call candidate exists. Update the
  parent blocker to name typed-core expression production plus direct-call
  lowering as the next design/implementation gate.

- [ ] **Step 5: Run metadata gates**

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: all queue, regression, docs-status, and whitespace checks pass.

- [ ] **Step 6: Commit closure**

  ```bash
  git add docs/execution/blocker-contracts.md docs/execution/done-archive.md docs/execution/queue.md docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md docs/superpowers/specs/2026-07-22-jazz-next-typed-core-elaboration-design.md docs/superpowers/plans/2026-07-22-jazz-next-typed-core-contract-foundation.md jazz-next/README.md
  git commit -m "docs: close typed core contract foundation"
  ```

## Done Criteria

- Haskell and Jazz expose the same complete typed-core and validation data,
  with no solver-state, runtime-value, lowered-IR, LLVM, or host representation
  leaks.
- Both validators report complete structured failures in stable program order.
- Semantic types, representation recipes, schemes, instantiations, evidence,
  names, patterns, declarations, interfaces, modules, and programs obey the
  reviewed contract.
- All 16 valid and 28 invalid fixtures are explicitly audited, run twice, and
  match exactly across Haskell and Jazz.
- Checked comparison rejects malformed hosted values rather than guessing or
  crashing.
- Canonical core, inference results, module compilation, runtime hints,
  interpreter behavior, and lowered IR remain unchanged.
- Focused tests, warning-clean build, routine Cabal matrix, package check,
  queue/docs validation, and whitespace checks pass.
- No opt-in exhaustive parser-scale component is run.
