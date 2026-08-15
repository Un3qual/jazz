# Jazz Haskell Maintainability Milestone 4 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split Typed Core validation and its largest contract suites by semantic ownership, then add narrow partial-source and touched-file tooling that prevents the audited maintainability regressions from returning.

**Architecture:** Extract validator modules in dependency order—internal contracts, types/recipes, evidence, patterns, non-recursive declaration contracts, the expression/statement recursion closure, and program orchestration—while retaining one public proof-carrying façade. Rehome tests and fixtures by contract domain behind small aggregators, and add a lexical repository policy for only the explicit partial constructs removed by this refactor.

**Tech Stack:** Haskell 2010 with existing local extensions, GHC 9.14.1, Cabal private/internal modules and test suites, `containers`, `directory`, `filepath`, `text`, Ormolu and HLint supplied by the locked Nix flake.

## Global Constraints

- Milestones 1-3 must be complete and green before this plan starts.
- Preserve public Jazz syntax/semantics, validation failure values and order, hosted parity, exact Typed Core and Lowered IR fixtures, and `ValidatedTypedProgram` proof ownership.
- `Jazz.Compiler.TypedCore.Validate` remains the only exposed validation façade; its internal constructor module is a Cabal `other-modules` entry, not exposed to dependent test components.
- Keep one coherent read-only validation context; do not create a context per submodule or repeatedly rebuild partial contexts.
- Do not add generic visitors, a universal validator typeclass, a fixture DSL, line-count tests, or implementation-module-count tests.
- Source policy covers only active compiler Haskell and only exact partial constructs, while ignoring comments and literals.
- Formatter enforcement applies to touched paths; existing unrelated format drift is not part of this milestone.
- HLint is not a gate until its parser accepts all supported GHC 9.14 source syntax.
- Use the checked-in Nix development shell, run large suites serially, and commit each task after focused verification.

**Design source:** `.codex/plans/2026-08-15-jazz-haskell-maintainability-design.md`

---

## File map

| File | Responsibility after this milestone |
| --- | --- |
| `src/Jazz/Compiler/TypedCore/Validate/Internal.hs` | Private proof constructor, context, contracts, paths, lookup/name primitives |
| `src/Jazz/Compiler/TypedCore/Validate/TypeRecipes.hs` | Types, recipes, schemes, literals, numeric/primitive constraints |
| `src/Jazz/Compiler/TypedCore/Validate/Evidence.hs` | Capabilities, impl identities, instantiations, evidence selections, node metadata |
| `src/Jazz/Compiler/TypedCore/Validate/Patterns.hs` | Pattern shapes, binders, constructor fields, or-pattern agreement |
| `src/Jazz/Compiler/TypedCore/Validate/Expressions.hs` | Mutually recursive expression and ordered-statement traversal, applications/operators, lexical binders, impl bodies |
| `src/Jazz/Compiler/TypedCore/Validate/Declarations.hs` | Non-recursive signature, scheme, data/class/impl, and method contracts |
| `src/Jazz/Compiler/TypedCore/Validate/Program.hs` | Program/module order, imports/exports/interfaces, context construction, recursion orchestration |
| `src/Jazz/Compiler/TypedCore/Validate.hs` | Three-function public façade and abstract proof export |
| `test/Jazz/Compiler/Bootstrap/TypedCoreContract/*` | Domain-owned Typed Core contract tests and fixtures |
| `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/*` | Domain-owned producer/lowerer tests |
| `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/*` | Domain-owned source and exact artifact fixtures |
| `test/Jazz/Repository/HaskellSourcePolicy.hs` | Comment/literal-aware checks for `error` and qualified partial map lookup |
| `scripts/check-haskell-format.sh` | Touched-file Ormolu check with an explicit path list |
| `CONTRIBUTING.md` | Authoritative Haskell maintenance commands and HLint compatibility status |
| `jazz.cabal` | Registers validator internals and split test modules |

### Task 1: Establish the validator’s private internal contract

**Files:**

- Create: `src/Jazz/Compiler/TypedCore/Validate/Internal.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate.hs:1-110,1260-1348,5630-5669`
- Modify: `jazz.cabal` private library
- Characterization test: `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`

**Interfaces:**

- `Internal` owns `ModuleContext`, `ForwardSignedFunctionContext`,
`ResolvedNameKey`, all `*Contract` records, `BinderOccurrence`, and the proof:

```haskell
newtype ValidatedTypedProgram = ValidatedTypedProgram TypedProgram
  deriving (Eq, Show)

validatedTypedProgram :: ValidatedTypedProgram -> TypedProgram
validatedTypedProgram (ValidatedTypedProgram typedProgram) = typedProgram
```

- It also owns context-only transformations, name-key conversion, path/name
rendering, `failure`, `maybeToList`, and `firstJust`.
- `Internal` is listed under `library jazz-internal.other-modules`; only
`Jazz.Compiler.TypedCore.Validate` remains exposed.

- [ ] **Step 1: Run the complete validator characterization suite**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-contract-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: PASS across every fixed/review fixture and two hosted parity runs.

- [ ] **Step 2: Move private types and context-only operations**

Move the exact declarations above and helpers whose bodies only inspect or
update context/contracts. Export constructors needed by sibling validator
modules, including `ValidatedTypedProgram (..)`, from `Internal`; the Cabal
`other-modules` boundary prevents dependent test suites from importing it.

Keep these public names exported abstractly by the façade:

```haskell
ValidatedTypedProgram
validateTypedProgram
validateTypedProgramOnce
validatedTypedProgram
```

- [ ] **Step 3: Preserve the façade proof construction**

Until `Program` exists, leave `validateTypedProgram` and
`validateTypedProgramOnce` in the façade, importing the internal constructor.
Verify no test can import `Validate.Internal` through the private library
interface by running the Cabal build after registering it as `other-modules`.

- [ ] **Step 4: Verify behavior and package visibility**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-contract-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/TypedCore/Validate.hs src/Jazz/Compiler/TypedCore/Validate/Internal.hs
git commit -m "refactor: establish typed-core validator internals"
```

### Task 2: Extract type, recipe, scheme, and literal validation

**Files:**

- Create: `src/Jazz/Compiler/TypedCore/Validate/TypeRecipes.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate.hs`
- Modify: `jazz.cabal`
- Characterization test: Typed Core contract suite

**Interfaces:**

- Produces the existing focused functions used by later validator modules:

```haskell
validateType :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedType -> [TypedCoreValidationFailure]
validateRecipe :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedRepresentationRecipe -> [TypedCoreValidationFailure]
validateTypeRecipe :: TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedType -> TypedRepresentationRecipe -> [TypedCoreValidationFailure]
validateLiteral :: TypedCoreValidationPath -> TypedNodeInfo -> TypedLiteral -> [TypedCoreValidationFailure]
validateOrderedTypeParameters :: TypedCoreValidationPath -> [TypedTypeParameterId] -> [TypedCoreValidationFailure]
validatePrimitiveConstraint :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedPrimitiveConstraint -> [TypedCoreValidationFailure]
expectedRecipe :: TypedType -> Maybe TypedRepresentationRecipe
expectedValueRecipe :: TypedType -> Maybe TypedRepresentationRecipe
typeRecipeCompatible :: TypedType -> TypedRepresentationRecipe -> Bool
nodeInfoHasCompatibleIntrinsicContract :: TypedNodeInfo -> Bool
recipeContractFailures :: TypedCoreValidationPath -> TypedCoreValidationKind -> TypedRepresentationRecipe -> TypedNodeInfo -> [TypedCoreValidationFailure]
```

- [ ] **Step 1: Identify and move the complete low-level closure**

Move type-parameter ordering, primitive/numeric constraints, numeric bounds,
type/recipe validation, callable staging compatibility, literal validation,
Unicode/numeric literal helpers, expected recipe construction, and intrinsic
node/recipe contract helpers.

Move a private helper with its public parent whenever it has no other consumer;
do not duplicate a helper to avoid an import.

- [ ] **Step 2: Keep context-dependent equality checks in the same owner**

Move strict-equality support and data-parameter contribution helpers with
primitive constraints. They may read `ModuleContext` and `DataContract`, but
must not import declarations, patterns, expressions, or program orchestration.

- [ ] **Step 3: Register as an internal module and repair imports**

Add `TypeRecipes` to `jazz-internal.other-modules`. Import its exact functions
from the remaining façade. Remove now-unused `Data.Char`, `Data.Ratio`, and
numeric catalog imports from the façade when their last owner moved.

- [ ] **Step 4: Verify validation values and ordering**

```bash
rg -n 'validateExpression|validatePattern|validateStatement|validateModule' src/Jazz/Compiler/TypedCore/Validate/TypeRecipes.hs
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-contract-spec -fdevelopment --test-show-details=direct --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: the first command has no matches; suite/build PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/TypedCore/Validate.hs src/Jazz/Compiler/TypedCore/Validate/Internal.hs src/Jazz/Compiler/TypedCore/Validate/TypeRecipes.hs
git commit -m "refactor: extract typed-core type recipe validation"
```

### Task 3: Extract evidence and pattern validation

**Files:**

- Create: `src/Jazz/Compiler/TypedCore/Validate/Evidence.hs`
- Create: `src/Jazz/Compiler/TypedCore/Validate/Patterns.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate.hs`
- Modify: `jazz.cabal`

**Interfaces:**

- `Evidence` produces:

```haskell
validateNodeInfo :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> Bool -> Maybe Text -> Maybe Text -> TypedNodeInfo -> [TypedCoreValidationFailure]
validateCapabilityConstraint :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedCapabilityConstraint -> [TypedCoreValidationFailure]
validateEvidenceSelections :: ModuleContext -> TypedCoreValidationPath -> Bool -> Maybe Text -> Maybe Text -> [TypedEvidenceSelection] -> [TypedCoreValidationFailure]
validateImplId :: ModuleContext -> TypedCoreValidationPath -> Set TypedTypeParameterId -> TypedImplId -> [TypedCoreValidationFailure]
```

- `Patterns` produces:

```haskell
validatePattern :: ModuleContext -> [Int] -> [Int] -> ValueContract -> TypedPattern -> [TypedCoreValidationFailure]
patternBoundContracts :: TypedPattern -> [BinderContract]
patternBinderContract :: TypedPattern -> [PatternBinderContract]
patternInfo :: TypedPattern -> TypedNodeInfo
```

- [ ] **Step 1: Move evidence/node-info validation**

Move capability constraints, data-type application checks, instantiation
contracts, evidence parameter bindings/uses/candidates/selections, impl/method
IDs, qualified-method key inspection, and `validateNodeInfo` to `Evidence`.

`Evidence` imports `Internal` and `TypeRecipes`. Its Typed-expression helpers
only inspect an expression to identify application/candidate keys; they must
not call `validateExpression` or import `Expressions`.

- [ ] **Step 2: Verify evidence extraction before pattern moves**

```bash
rg -n 'validateExpression|validatePattern|validateStatement' src/Jazz/Compiler/TypedCore/Validate/Evidence.hs
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-contract-spec -fdevelopment --test-show-details=direct --jobs=1
```

Expected: `rg` has no matches; suite PASS.

- [ ] **Step 3: Move pattern validation**

Move pattern metadata, shape/child contracts, literals, constructor-field
substitution, binder contracts, duplicate binder agreement, and or-pattern
agreement to `Patterns`. It imports `Evidence.validateNodeInfo` and
`TypeRecipes` literal/recipe helpers. `Evidence` must not import `Patterns`.

- [ ] **Step 4: Verify the dependency direction and exact contracts**

```bash
rg -n 'TypedCore\.Validate\.Patterns' src/Jazz/Compiler/TypedCore/Validate/Evidence.hs
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-contract-spec jazz-typed-core-expression-direct-call-spec -fdevelopment --test-show-details=direct --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: no reverse import; suites/build PASS.

- [ ] **Step 5: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/TypedCore/Validate.hs src/Jazz/Compiler/TypedCore/Validate/Internal.hs src/Jazz/Compiler/TypedCore/Validate/TypeRecipes.hs src/Jazz/Compiler/TypedCore/Validate/Evidence.hs src/Jazz/Compiler/TypedCore/Validate/Patterns.hs
git commit -m "refactor: extract typed-core evidence and patterns"
```

### Task 4: Complete the validator split and façade

**Files:**

- Create: `src/Jazz/Compiler/TypedCore/Validate/Expressions.hs`
- Create: `src/Jazz/Compiler/TypedCore/Validate/Declarations.hs`
- Create: `src/Jazz/Compiler/TypedCore/Validate/Program.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate.hs`
- Modify: `jazz.cabal`

**Interfaces:**

- `Declarations` produces the non-recursive declaration-contract helpers used
  while traversing statements: signatures, schemes, data/class/impl headers,
  and method contracts.
- `Expressions` owns the mutually recursive closure containing
  `validateExpression`, ordered scope traversal, statement validation, block
  context extension, and impl-method body validation.
- `Program` produces:

```haskell
validateTypedProgramInternal :: TypedProgram -> [TypedCoreValidationFailure]
```

- The façade produces exactly:

```haskell
validateTypedProgram :: TypedProgram -> [TypedCoreValidationFailure]
validateTypedProgramOnce :: TypedProgram -> Either [TypedCoreValidationFailure] ValidatedTypedProgram
validatedTypedProgram :: ValidatedTypedProgram -> TypedProgram
```

and exports `ValidatedTypedProgram` abstractly.

- [ ] **Step 1: Move non-recursive declaration contracts**

Move signature/scheme validation, data/class/impl declaration headers, method
contracts, binder occurrence/duplicate checks, and context transformations
that do not inspect statement bodies to `Declarations`.

The dependency direction at this point is:

```text
Internal <- TypeRecipes <- Evidence <- Patterns <- Declarations
```

`Declarations` may import every module to its left. It must not call
`validateExpression`, traverse ordered statements, or validate impl-method
bodies, so no earlier module needs a reverse import.

- [ ] **Step 2: Move the expression/statement recursion closure**

Move expression traversal, application spines, variable/binder contracts,
explicit type application, lambda/list/tuple/block/case checks, builtin/native
callable contracts, operator value/binary/section checks, and expression-owned
literal/collection checks to `Expressions`.

Move ordered scope traversal, forward-signed function handling, statement
validation, block declaration context extension, and impl-method body
validation in the same step. These functions are mutually recursive in the
live validator: expressions validate block statements, while statements and
impl methods validate expressions. Keep that strongly connected component in
one module instead of introducing callbacks solely to force a split.

`Expressions` imports `Declarations`, `Patterns`, `Evidence`, `TypeRecipes`,
and `Internal`; none of those modules imports `Expressions`.

- [ ] **Step 3: Move program/module/interface orchestration**

Move program ordering/cycles/entry checks, module path/source/import/export
checks, module context construction, visibility/interface construction,
recursive-group orchestration, module-result checks, and interface dependency
closure to `Program`. It calls `Declarations` and `Expressions` and constructs
one coherent `ModuleContext` per module.

- [ ] **Step 4: Reduce and verify the public façade**

Implement:

```haskell
validateTypedProgram = validateTypedProgramInternal

validateTypedProgramOnce typedProgram =
  case validateTypedProgramInternal typedProgram of
    [] -> Right (ValidatedTypedProgram typedProgram)
    failures -> Left failures
```

The façade imports the private constructor from `Internal`, but exports the
type without `( .. )`. Run:

```bash
wc -l src/Jazz/Compiler/TypedCore/Validate.hs
rg -n '^validate(Expression|Pattern|Statement|Scheme|Type|Recipe|NodeInfo|ModuleInterface) ::' src/Jazz/Compiler/TypedCore/Validate.hs
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-contract-spec jazz-typed-core-expression-direct-call-spec jazz-lowered-ir-contract-spec -fdevelopment --test-show-details=direct --jobs=1
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
```

Expected: façade is a small entry-point module; the signature search has no
matches; all suites/build PASS. The line count is review information only, not
a gate.

- [ ] **Step 5: Commit**

```bash
git add jazz.cabal src/Jazz/Compiler/TypedCore/Validate.hs src/Jazz/Compiler/TypedCore/Validate/Internal.hs src/Jazz/Compiler/TypedCore/Validate/TypeRecipes.hs src/Jazz/Compiler/TypedCore/Validate/Evidence.hs src/Jazz/Compiler/TypedCore/Validate/Patterns.hs src/Jazz/Compiler/TypedCore/Validate/Expressions.hs src/Jazz/Compiler/TypedCore/Validate/Declarations.hs src/Jazz/Compiler/TypedCore/Validate/Program.hs
git commit -m "refactor: split typed-core validation responsibilities"
```

### Task 5: Split the Typed Core contract suite by domain

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`
- Create: `test/Jazz/Compiler/Bootstrap/TypedCoreContract/Shared.hs`
- Create: `test/Jazz/Compiler/Bootstrap/TypedCoreContract/ManifestTests.hs`
- Create: `test/Jazz/Compiler/Bootstrap/TypedCoreContract/ModuleTests.hs`
- Create: `test/Jazz/Compiler/Bootstrap/TypedCoreContract/TypeRecipeTests.hs`
- Create: `test/Jazz/Compiler/Bootstrap/TypedCoreContract/ExpressionPatternTests.hs`
- Create: `test/Jazz/Compiler/Bootstrap/TypedCoreContract/EvidenceTests.hs`
- Create: `test/Jazz/Compiler/Bootstrap/TypedCoreContract/ParityTests.hs`
- Create domain fixture modules under `test/Jazz/Compiler/Bootstrap/TypedCoreContract/Fixtures/`
- Modify: `jazz.cabal` `jazz-typed-core-contract-spec.other-modules`

**Interfaces:**

- Every `*Tests` module produces `tests :: [NamedTest]`.
- `ParityTests` consumes `allContractPrograms :: [TypedProgram]` from domain fixture modules and owns hosted comparison/batch execution.
- `Shared` owns only repeated assertion helpers and primitive constructors used by at least two domain modules; it does not define a fixture DSL.

- [ ] **Step 1: Add the aggregator shape before moving tests**

Change `Main.tests` to concatenate domain lists in the exact current order:

```haskell
tests =
  ManifestTests.tests
    <> ModuleTests.tests
    <> TypeRecipeTests.tests
    <> ExpressionPatternTests.tests
    <> EvidenceTests.tests
    <> ParityTests.tests
```

Create modules initially exporting empty lists, register them, and run the
suite. Expected: FAIL because the audited fixed test count/order is incomplete;
do not commit this intermediate state.

- [ ] **Step 2: Move manifest and parity ownership**

Move fixed valid/invalid manifest audits, adapter decoding failures, coverage,
fixture uniqueness/counts to `ManifestTests`. Move Jazz batch execution,
double-run determinism, canonical encode/decode, checked project source loading,
and timeouts to `ParityTests`.

Keep the current parity program order: valid fixtures, invalid fixtures,
review regressions, direct-recursion artifacts, then closure-recursion artifacts.

- [ ] **Step 3: Move module and type/recipe domains**

Move program/module paths, source paths, imports, exports, interfaces,
visibility, recursive groups, declaration ordering, and module result fixtures
to `ModuleTests` and `Fixtures.Modules`.

Move schemes, type parameters, primitive constraints, numeric bounds, recipes,
callable staging, literals, lists, tuples, data shapes, and type applications
to `TypeRecipeTests` and `Fixtures.TypeRecipes`.

- [ ] **Step 4: Move expression/pattern and evidence domains**

Move scope/binder references, applications, lambdas, blocks, conditionals,
operators, pattern paths/shapes/binders/or-patterns to
`ExpressionPatternTests` and `Fixtures.ExpressionsPatterns`.

Move capabilities, classes, impls, methods, instantiations, evidence
parameters/selections/candidates, and interface evidence closure to
`EvidenceTests` and `Fixtures.Evidence`.

Promote a constructor to `Shared` only when at least two fixture domains import
it. Otherwise keep it in its domain module.

- [ ] **Step 5: Verify complete test inventory and hosted parity**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-contract-spec -fdevelopment --test-show-details=direct --jobs=1
rg -n '^test[A-Z].*:: IO \(\)|^[a-zA-Z].*Program :: TypedProgram' test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs
```

Expected: suite PASS with existing fixed counts and two parity runs; aggregator
contains no test bodies or fixture programs.

- [ ] **Step 6: Commit**

```bash
git add jazz.cabal test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreContract
git commit -m "refactor: split typed-core contract tests by domain"
```

### Task 6: Split direct-call fixtures and producer/lowerer tests

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Create fixture modules under `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures/`
- Create test modules under `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec/`
- Modify: `jazz.cabal` direct-call and both contract-suite `other-modules`

**Interfaces:**

- Original `TypedCoreExpressionDirectCallFixtures` remains a compatibility façade re-exporting active fixture groups used by three suites.
- Test modules each produce `tests :: [NamedTest]`:
  `ManifestTests`, `ScalarTests`, `TextTests`, `CallTests`,
  `CaptureRecursionTests`, and `BoundaryTests`.
- Fixture modules own: `Source`, `Scalar`, `ManagedText`, `Calls`,
  `CapturesRecursion`, and `LowererBoundary`.

- [ ] **Step 1: Split source fixture identity and resolution**

Move `Fixture`, fixture names/manifests, source maps, `sourceFixture` builders,
`resolveFixture`, and `resolveFixtureWithLookup` to `Fixtures.Source`. Keep
source insertion/order unchanged. Re-export these through the original module.

- [ ] **Step 2: Split exact artifact fixture domains**

Move unit/scalar/numeric/binding/pattern-case expected programs to
`Fixtures.Scalar`; managed Text producer/operation/exclusion artifacts to
`Fixtures.ManagedText`; direct/curried/closed/higher-order call artifacts to
`Fixtures.Calls`; lexical capture/direct recursion/closure recursion artifacts
to `Fixtures.CapturesRecursion`; and independent invalid/profile/lowerer
boundary programs to `Fixtures.LowererBoundary`.

Keep shared primitive constructors in the narrowest domain. A constructor used
by multiple domains moves to `Fixtures.Shared`, which exports constructors only
and no scenario DSL.

- [ ] **Step 3: Split the producer/lowerer test list in current order**

Make the original spec a small `Main` that concatenates:

```haskell
tests =
  ManifestTests.tests
    <> ScalarTests.tests
    <> TextTests.tests
    <> CallTests.tests
    <> CaptureRecursionTests.tests
    <> BoundaryTests.tests
```

Move each test with the fixtures/assertion helpers it owns. Shared source-to-
producer execution helpers go in `Spec.Shared`; exact semantic assertions stay
in their domain test module.

- [ ] **Step 4: Register every shared fixture module in all consuming suites**

Update `jazz-typed-core-expression-direct-call-spec`,
`jazz-typed-core-contract-spec`, and `jazz-lowered-ir-contract-spec`
`other-modules` lists. Do not duplicate a fixture module to avoid Cabal
registration.

- [ ] **Step 5: Verify all three artifact consumers**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec -fdevelopment --test-show-details=direct --jobs=1
rg -n '^test[A-Z].*:: IO \(\)|^[a-zA-Z].*Expected.*::' test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
```

Expected: all suites PASS; the original files are aggregating façades without
test bodies or expected artifact definitions.

- [ ] **Step 6: Commit**

```bash
git add jazz.cabal test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec
git commit -m "refactor: split typed-core producer fixtures and tests"
```

### Task 7: Add narrow partial-source and touched-format policy

**Files:**

- Create: `test/Jazz/Repository/HaskellSourcePolicy.hs`
- Modify: `test/Jazz/Repository/AuditSpec.hs`
- Create: `scripts/check-haskell-format.sh`
- Modify: `CONTRIBUTING.md`
- Modify: `jazz.cabal` repository audit `other-modules`

**Interfaces:**

- Produces:

```haskell
data HaskellSourcePolicyViolation
  = PartialErrorIdentifier FilePath Int
  | PartialQualifiedMapLookup FilePath Int
  deriving (Eq, Show)

validateHaskellSourcePolicy ::
  FilePath -> Text -> [HaskellSourcePolicyViolation]

readCompilerHaskellPolicyViolations ::
  FilePath -> IO [HaskellSourcePolicyViolation]
```

- The shell script requires one or more explicit `.hs` paths and runs the locked Nix Ormolu in check mode on only those paths.

- [ ] **Step 1: Add failing lexical-policy tests**

Add repository-audit cases proving:

```haskell
validateHaskellSourcePolicy "Example.hs" "value = error \"boom\""
  == [PartialErrorIdentifier "Example.hs" 1]

validateHaskellSourcePolicy "Example.hs" "value = table Map.! key"
  == [PartialQualifiedMapLookup "Example.hs" 1]

validateHaskellSourcePolicy
  "Example.hs"
  "-- error and Map.! are documentation\nmessage = \"error Map.!\"\ncharacter = '!'"
  == []
```

Also add `testCheckedInCompilerHaskellPolicy`, which scans only
`src/Jazz/Compiler/**/*.hs`. Run `repository-audit-spec`; expected: FAIL because
`HaskellSourcePolicy` does not exist.

- [ ] **Step 2: Implement a comment/literal-aware lexical scanner**

Use a single pass with states:

```haskell
data LexicalState
  = InCode
  | InLineComment
  | InBlockComment Int
  | InStringLiteral Bool
  | InCharacterLiteral Bool
```

Track the 1-based line number. Support nested `{- -}` comments, escaped string
and character literals, and newline termination for line comments. In
`InCode`, recognize identifier boundaries around `error` and the exact
qualified token sequence `Map.!`. Do not flag substrings such as `errors`,
`someMap.!`, comments, strings, or characters. Return violations in path then
source order.

Recursively scan regular `.hs` files below `src/Jazz/Compiler`, sort directory
entries before traversal, and ignore `dist-newstyle`, generated, app, benchmark,
and test paths by construction.

- [ ] **Step 3: Run the repository policy red-green cycle**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test repository-audit-spec -fdevelopment --test-show-details=direct
```

Expected: PASS; milestone 1 removed all active-source violations.

- [ ] **Step 4: Add explicit touched-file formatting command**

Create executable `scripts/check-haskell-format.sh` with:

```bash
#!/usr/bin/env bash
set -euo pipefail

if (( $# == 0 )); then
  printf 'usage: scripts/check-haskell-format.sh PATH.hs [PATH.hs ...]\n' >&2
  exit 2
fi

for source_path in "$@"; do
  case "$source_path" in
    *.hs) ;;
    *) printf 'not a Haskell source path: %s\n' "$source_path" >&2; exit 2 ;;
  esac
done

exec nix --extra-experimental-features 'nix-command flakes' develop --command ormolu --mode check "$@"
```

Document this exact command in `CONTRIBUTING.md`. State that the locked HLint
is advisory until it parses all GHC 9.14 syntax; do not suppress parse failures
or present its hint count as a clean gate.

- [ ] **Step 5: Verify policy and script behavior**

```bash
bash scripts/check-haskell-format.sh src/Jazz/Compiler/TypedCore/Validate.hs
bash -n scripts/check-haskell-format.sh
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test repository-audit-spec -fdevelopment --test-show-details=direct
git diff --check
```

Expected: PASS. If the façade needs formatting, format that touched file with
the same locked Ormolu and re-run check mode; do not format unrelated source.

- [ ] **Step 6: Commit**

```bash
git add jazz.cabal test/Jazz/Repository/AuditSpec.hs test/Jazz/Repository/HaskellSourcePolicy.hs scripts/check-haskell-format.sh CONTRIBUTING.md src/Jazz/Compiler/TypedCore/Validate.hs
git commit -m "chore: guard Haskell maintenance invariants"
```

### Task 8: Full closeout and repository state

**Files:**

- Modify: `.codex/execution/queue.md` only to close an active maintenance entry created for execution.
- Verify: the entire repository.

**Interfaces:**

- Consumes: all four completed milestone plans.
- Produces: verified maintainability refactor with clean execution state.

- [ ] **Step 1: Run every touched-file format check**

Pass the exact tracked `.hs` paths changed across milestones 1-4 to
`scripts/check-haskell-format.sh`. Obtain the list from `git diff --name-only`
against the pre-refactor base, inspect it, and pass only `.hs` paths. Do not use
a repository-wide Ormolu rewrite.

- [ ] **Step 2: Run the full suite serially in Nix**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all -fdevelopment --test-show-details=direct --jobs=1
```

Expected: every test suite PASS.

- [ ] **Step 3: Run build, package, repository, documentation, and whitespace gates**

```bash
nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment
nix --extra-experimental-features 'nix-command flakes' develop --command cabal check
nix --extra-experimental-features 'nix-command flakes' develop --command cabal test repository-audit-spec -fdevelopment --test-show-details=direct
nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
python3 scripts/check-execution-queue.py
git diff --check
```

Expected: all commands PASS.

- [ ] **Step 4: Run final ownership and regression scans**

```bash
rg -n '\berror\s*(\(|")|Map\.!' src -g '*.hs'
rg -n '^data ResolvedModule|resolvedGraphModulesRevState' src/Jazz/Compiler/ModuleResolver.hs
rg -n '^(runtimeControlOutcome|diagnosticResultOutcome|runtimeOutcomeAsDiagnosticResult|forceDiagnostic) ::' src/Jazz/Compiler
rg -n 'inferScopeTypeInternal :: Bool|finalizeExpression scalarCaptureTypes|finalizeExpression recursiveScalarCaptureTypes' src/Jazz/Compiler/TypeInference
git status --short
```

Expected: no explicit partials or compatibility resolver artifacts; each shared
adapter has one owner; positional signatures are gone; only intentional
closeout changes are present.

- [ ] **Step 5: Close execution state and commit**

If an active queue entry was created at execution start, mark it completed in
`.codex/execution/queue.md` and run `python3 scripts/check-execution-queue.py`.
Then:

```bash
git add .codex/execution/queue.md
git commit -m "chore: close Haskell maintainability refactor"
```

If execution did not create a queue entry, do not edit or commit the queue and
do not create an empty closeout commit.
