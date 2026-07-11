---
id: JN-BOOTSTRAP-GENERIC-NAMED-TYPES-001
status: done
completed_on: 2026-07-10
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-07-10
plan_section: "Implementation Batch: Generic Named Types"
target_paths:
  - docs/feature-status.md
  - docs/jazz-language-state.md
  - docs/execution/queue.md
  - docs/execution/done-archive.md
  - docs/superpowers/specs/2026-07-10-jazz-next-generic-named-types-design.md
  - jazz-next/scripts/test-warning-config.sh
  - jazz-next/src/JazzNext/Compiler/AST.hs
  - jazz-next/src/JazzNext/Compiler/Analyzer.hs
  - jazz-next/src/JazzNext/Compiler/CapabilityFacts.hs
  - jazz-next/src/JazzNext/Compiler/Driver.hs
  - jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs
  - jazz-next/src/JazzNext/Compiler/ModuleGraph.hs
  - jazz-next/src/JazzNext/Compiler/ModuleInterface.hs
  - jazz-next/src/JazzNext/Compiler/ModuleResolver.hs
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Expression.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Signature.hs
  - jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference/Capabilities.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference/State.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference/Types.hs
  - jazz-next/test/JazzNext/Compiler/Modules/Loader/VisibilityTests.hs
  - jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
  - jazz-next/test/JazzNext/Compiler/Parser/Foundation/ExpressionsTests.hs
  - jazz-next/test/JazzNext/Compiler/Parser/Foundation/InvalidSyntaxTests.hs
  - jazz-next/test/JazzNext/Compiler/Parser/Foundation/ModulesTests.hs
  - jazz-next/test/JazzNext/Compiler/Parser/Foundation/Shared.hs
  - jazz-next/test/JazzNext/Compiler/Parser/Foundation/SignaturesTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/BasicsTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/ConstraintsTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/DiagnosticsTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/GeneralizationTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/RecursionTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/Shared.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/Runtime/CapabilitiesTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/Runtime/ControlFlowTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/Runtime/NumericTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RenderingTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/Runtime/Shared.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Add full rank-1 generic named types to jazz-next signatures, constraints, impl targets, explicit type applications, module interfaces, and concrete runtime hints through one permanent recursive signature tree; preserve nominal identity and exact arity without adding higher-kinded types, bootstrap libraries, bytecode, or LLVM lowering."
---

# Jazz-Next Generic Named Types Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add full rank-1 named generic types to Jazz signatures and module interfaces so APIs can state types such as `Maybe(Char)`, `Result(IOError, Text)`, and `(a -> b) -> List(a) -> List(b)`.

**Architecture:** Replace the parallel ordinary and constrained signature trees with one recursive `SurfaceSignatureType` and one recursive `SignatureType`. Resolve named constructors through the existing type namespace, lower exact-arity applications to `TDataType`, quantify lower-case variables in first-occurrence order, and carry the resulting schemes and concrete hints through existing module and runtime boundaries. The Haskell interpreter remains stage 0; the typed core stays independent of future backend-neutral IR and LLVM lowering.

**Tech Stack:** Haskell 2010, Megaparsec token parsing, the canonical `jazz-next` surface/core AST, the existing rank-1 type-scheme solver, module interfaces, the stage-0 interpreter, focused `runghc` suites, and repository queue/docs gates.

## Global Constraints

- Modify only `jazz-next/` and active documentation; `jazz-hs/` and `jazz2/` remain read-only.
- Delete `SurfaceConstrainedSignatureType` and `ConstraintSignatureType`; do not leave aliases, mirror trees, or conversion shims.
- Preserve dedicated primitive, numeric, list, tuple, function, and Unit constructors.
- Accept both `[a]` and `List(a)` and normalize both to `TypeList`.
- Quantify lower-case variables implicitly in deterministic first-occurrence order; do not add explicit `forall`, higher-rank types, higher-kinded variables, or partial type application.
- Resolve user-defined names through `TypeNamespace`, preserve nominal module origin, and require exact declared arity.
- Preserve existing declaration-order visibility, explicit export rules, constraint policy, dispatch policy, and all current diagnostics outside named-type failures.
- Use `E2009` for semantic named-type failures at the adjacent-signature span or explicit type-argument span.
- Emit runtime hints only after a signature instantiation is concrete.
- Do not add `Maybe`/`Result` library definitions, text traversal, host I/O, bytecode, LLVM IR, object generation, linking, or native runtime work.
- Implement behavior test-first and commit each independently reviewable task.

---

## File Map

- `JazzNext.Compiler.Parser.{AST,Signature,Declaration,Lower}` owns the one recursive surface grammar and its canonical lowering.
- `JazzNext.Compiler.AST` owns the one recursive core `SignatureType` used by payloads, constraints, impls, explicit applications, runtime hints, and interfaces.
- `JazzNext.Compiler.{ModuleResolver,ModuleGraph}` resolves and traverses every nested type name through `TypeNamespace`.
- `JazzNext.Compiler.TypeInference.{Capabilities,Scope,Diagnostics}` validates named arity, allocates rank-1 variables, constructs schemes, and reports `E2009` failures.
- `JazzNext.Compiler.{CapabilityFacts,Analyzer}` consumes the unified tree for class/impl checking and evidence keys.
- `JazzNext.Compiler.{ModuleCompiler,ModuleInterface,TypeInference.Types}` carries schemes, class metadata, impl targets, and concrete hints across modules with nominal origin intact.
- `JazzNext.Compiler.{Runtime,Driver,TypeInference.State,TypeInference}` uses unified concrete runtime hints without exposing unresolved variables.
- Existing parser, binding-signature, module, and runtime component suites receive focused regression coverage; the warning-config runner gains a shell guard that prevents either removed type name from returning.

## Implementation Batch: Generic Named Types

### Task 1: Replace the Parallel Surface and Core Trees

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Signature.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Expression.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/RecursiveBindings.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/SignaturesTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/ExpressionsTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/InvalidSyntaxTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/Shared.hs`

**Interfaces:**

- Consumes: identifier tokens, `parseSignaturePayload`, `parseSignatureTypePrefix`, and existing primitive/list/tuple/function parsing.
- Produces: the only two recursive trees, `SurfaceSignatureType` and `SignatureType`; every `SurfaceSignaturePayload`, `SignaturePayload`, constraint, impl target, and type application contains one of those trees, while expression type applications also retain the `@` token span.

- [x] **Step 1: Add parser assertions for the accepted recursive grammar**

Register focused cases that assert these exact normalized trees:

```haskell
value :: Maybe(Char).
result :: Result(IOError, List(Maybe(Token))).
map :: (a -> b) -> List(a) -> [b].
equalsAll :: @{Eq(a)}: List(a) -> List(a) -> Bool.
class Foldable(a) { fold :: (b -> a -> b) -> b -> List(a) -> b. }.
impl Eq(Maybe(Char)) { equals = true. }.
identity @Maybe(Char) value.
```

Expected nodes include `SurfaceTypeVariable`, `SurfaceTypeName`, and `SurfaceTypeApplication`; both `List(a)` and `[a]` must contain `SurfaceTypeList (SurfaceTypeVariable (mkIdentifier "a"))`.

- [x] **Step 2: Add invalid grammar assertions**

Add parser cases for `Maybe()`, `Maybe(Char,)`, and an empty explicit type argument. Expect the existing parser diagnostic family, while `f(Char)` must parse structurally so inference can issue `E2009` for a variable-headed application.

- [x] **Step 3: Run the parser suite and verify the new cases fail**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
```

Expected: FAIL because the retained trees do not yet contain variable, name, or application constructors and ordinary signatures reject generic syntax.

- [x] **Step 4: Define the unified surface and core types**

Make the payload shapes and recursive constructors exactly:

```haskell
data SurfaceSignaturePayload
  = SurfaceSignatureType SurfaceSignatureType
  | SurfaceConstrainedSignature [SurfaceSignatureConstraint] SurfaceSignatureType
  | SurfaceUnsupportedSignature [SurfaceSignatureToken]

data SurfaceSignatureConstraint
  = SurfaceSignatureConstraint Identifier [SurfaceSignatureType]

data SignaturePayload
  = SignatureType SignatureType
  | ConstrainedSignature [SignatureConstraint] SignatureType
  | UnsupportedSignature [SignatureToken]

data SignatureConstraint = SignatureConstraint Name [SignatureType]

data SurfaceExpr
  = SETypeApplication SurfaceExpr SourceSpan SurfaceSignatureType

data Expr
  = ETypeApplication Expr SourceSpan SignatureType
```

The shown expression constructors replace their existing arity within the full existing sum types; do not create second `SurfaceExpr` or `Expr` declarations. Add `SurfaceTypeVariable`, `SurfaceTypeName`, `SurfaceTypeApplication`, `TypeVariable`, `TypeName`, and `TypeApplication` to the retained trees. Change `SSImpl` and `SImpl` target arguments to the retained type. Remove both constrained tree declarations.

- [x] **Step 5: Collapse parsing and lowering onto one recursive path**

Use one right-associative parser for ordinary bodies, constrained bodies, constraint arguments, impl targets, and explicit type arguments. Parse a lower-case bare identifier as `SurfaceTypeVariable`, an upper-case bare identifier as a primitive or `SurfaceTypeName`, an identifier with nonempty parenthesized arguments as `SurfaceTypeApplication`, and normalize `List(x)` directly to `SurfaceTypeList x`. Capture `tokenSpan typeApplicationToken` in `SETypeApplication`, lower it into `ETypeApplication`, and update every expression traversal to preserve that span. Replace `lowerSurfaceConstrainedSignatureType` with recursive `lowerSurfaceSignatureType` handling every retained constructor.

- [x] **Step 6: Run the parser suite and verify it passes**

Run the Task 1 command. Expected: PASS, including identical trees for `[a]` and `List(a)`.

- [x] **Step 7: Commit the parser and AST boundary**

```bash
git add jazz-next/src/JazzNext/Compiler/AST.hs jazz-next/src/JazzNext/Compiler/Parser jazz-next/test/JazzNext/Compiler/Parser/Foundation
git commit -m "refactor: unify Jazz signature type syntax"
```

### Task 2: Resolve Nested Type Names and Enforce Exact Arity

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleGraph.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Capabilities.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/BasicsTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/DiagnosticsTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs`

**Interfaces:**

- Consumes: `TypeNamespace`, `inferDataTypes :: InferState -> Map Text DataTypeBinding`, and `DataTypeBinding [Name] [[ConstructorArgumentType]]`.
- Produces: recursively resolved `SignatureType` nodes and `TDataType resolvedName arguments`, or a structured named-type failure rendered as `E2009`.

- [x] **Step 1: Add semantic success and failure cases**

Add success cases for a declared `Maybe(a)`, a two-parameter `Result(e, a)`, nested applications, a zero-parameter data type used bare, and two imported same-text types retaining different resolved origins. Add a declaration-order case where a signature before its local data declaration rejects and the same signature after that declaration succeeds. Add failures whose rendered diagnostics contain these exact facts:

```text
unknown named type 'Unknown'
type 'Maybe' expects 1 argument(s), found 2
type 'Result' expects 2 argument(s), found 1
type variable 'f' cannot be used as an application head
type 'Maybe' expects 1 argument(s), found 0
type 'List' expects 1 argument(s), found 2
```

Assert code `E2009` and the owning signature span. Add an explicit-application unknown-name case asserting the explicit type-argument span.

- [x] **Step 2: Run binding and module-resolution suites and verify failure**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
```

Expected: FAIL because nested signature names are not resolved or validated.

- [x] **Step 3: Resolve every retained type node recursively**

Add one `resolveSignatureType` traversal in `ModuleResolver.hs`. Preserve `TypeVariable`; resolve `TypeName` and `TypeApplication` heads with `resolveName TypeNamespace`; recurse through application arguments, lists, tuples, and functions. Use it for ordinary/constrained payloads, constraints, impl targets, class methods, and `ETypeApplication`. Replace `constraintTypeNames` in `ModuleGraph.hs` with a single `signatureTypeNames` traversal.

- [x] **Step 4: Convert named types with a structured failure result**

Use one conversion boundary shaped as:

```haskell
data SignatureTypeFailure
  = UnknownNamedType Name
  | NamedTypeArityMismatch Name Int Int
  | VariableApplicationHead Name

signatureTypeToExpressionTypeWithState ::
  InferState ->
  Map Text ExpressionType ->
  SignatureType ->
  Either SignatureTypeFailure ExpressionType
```

Primitive constructors map as today. `TypeVariable` looks up its allocated entry. `TypeName` accepts only a visible zero-parameter `DataTypeBinding`; `TypeApplication` validates built-in `List` at arity one and otherwise looks up the visible `DataTypeBinding`, compares argument count to `length typeParameters`, recursively converts arguments, and returns `TDataType name arguments`. A lower-case application head returns `VariableApplicationHead`. `TypeList`, `TypeTuple`, and `TypeFunction` recurse.

- [x] **Step 5: Render each structured failure as deterministic `E2009`**

Thread the failure from `signaturePayloadToSignatureType` to `mkInvalidSignatureTypeError` so the message distinguishes unknown name, expected/received arity, variable head, and partial application. Preserve existing mismatch, constraint, and `E2017` behavior after a type argument has resolved successfully.

- [x] **Step 6: Run both focused suites and verify they pass**

Run the two Task 2 commands. Expected: PASS with nominal origins and exact error text asserted.

- [x] **Step 7: Commit recursive resolution and arity checking**

```bash
git add jazz-next/src/JazzNext/Compiler/ModuleResolver.hs jazz-next/src/JazzNext/Compiler/ModuleGraph.hs jazz-next/src/JazzNext/Compiler/TypeInference/Capabilities.hs jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
git commit -m "feat: resolve generic named signature types"
```

### Task 3: Turn Ordinary Generic Signatures into Rank-1 Schemes

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Capabilities.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Types.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/BasicsTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/GeneralizationTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/RecursionTests.hs`

**Interfaces:**

- Consumes: the Task 2 conversion boundary and existing `TypeScheme`, `instantiateTypeScheme`, and `orderedSchemeVariables` behavior.
- Produces: `SignaturePayloadType` with a declared expression type and first-occurrence variable order for both ordinary and constrained signatures.

- [x] **Step 1: Add rank-1 behavior tests**

Add a signed identity used once at `Int` and once at `Text`, a signed two-variable mapper where `a` and `b` instantiate independently, repeated-variable mismatch coverage for `a -> a`, nested `Result(e, List(Maybe(a)))`, and a recursively signed generic binding. Assert the binding scheme order for `(b -> a) -> b -> a` is `b`, then `a`.

- [x] **Step 2: Run the binding-signature suite and verify failure**

Run the BindingSignatureCoherence command. Expected: FAIL because ordinary signatures do not allocate or quantify `TypeVariable` nodes.

- [x] **Step 3: Allocate variables in first-occurrence order**

Replace constrained-only variable collectors with unified helpers:

```haskell
signatureTypeVariableNamesInOrder :: SignatureType -> [Text]
signatureTypeVariableNames :: SignatureType -> Set Text
```

Traverse application arguments, lists, tuples, and function arguments/results left to right. De-duplicate without sorting. Allocate one fresh `TVarType` per name and pass that map through the Task 2 conversion function.

- [x] **Step 4: Build a declared `TypeScheme` for any generic adjacent signature**

Make both `SignatureType signatureType` and `ConstrainedSignature constraints signatureType` return `SignaturePayloadType` with `signaturePayloadVariableOrder`. During adjacent-signature checking, unify the RHS with an instantiation of the declared type, then store `SchemeTypeBinding` whose quantified set/order comes from the declared variables. Keep existing inferred constraints, primitive constraints, defining capability facts, recursion staging, and per-use `instantiateTypeScheme` behavior.

- [x] **Step 5: Run the binding-signature suite and verify it passes**

Run the Task 3 command. Expected: PASS, including incompatible concrete instantiations of the same signed generic binding in one scope.

- [x] **Step 6: Commit declared generic schemes**

```bash
git add jazz-next/src/JazzNext/Compiler/TypeInference jazz-next/src/JazzNext/Compiler/TypeInference.hs jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature
git commit -m "feat: infer rank-one signed generic schemes"
```

### Task 4: Migrate Constraints, Impl Targets, Evidence, and Diagnostics

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/CapabilityFacts.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Capabilities.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Types.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/ConstraintsTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature/DiagnosticsTests.hs`

**Interfaces:**

- Consumes: unified `SignatureType`, rank-1 variable maps, current class arity facts, concrete impl facts, and current overlap/dispatch rules.
- Produces: `ImplMethodType SignatureType` plus unified render, compatibility, substitution, variable collection, fact-key, and function-argument helpers.

- [x] **Step 1: Add constrained generic and impl regression tests**

Cover `@{Eq(a)}: List(a) -> List(a) -> Bool`, a nested concrete `Eq(Maybe(Char))` impl target, repeated variable consistency, class arity errors, duplicate constraints, constrained variables missing from the body, and unchanged concrete missing-impl diagnostics.

- [x] **Step 2: Run binding and runtime suites and verify failure**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
```

Expected: FAIL while capability helpers still require the removed constrained tree.

- [x] **Step 3: Rename capability helpers around the retained type**

Replace `constraintSignature*` helpers with `signatureType*` helpers over `SignatureType`. Preserve the existing behavior of fact keys, alias variants, concrete compatibility, class-parameter occurrence, ordered variable collection, function argument extraction, structural equality checks, and diagnostic rendering. Ensure `TypeApplication` recursively participates in every helper.

- [x] **Step 4: Migrate analyzer and inference metadata**

Change `ImplMethodType`, analyzer impl checks, deferred constraints, exact-evidence matching, and inferred constraint runtime selection to `SignatureType`. Delete conversions whose only purpose was translating between the former trees; keep one expression-type conversion and one expression-type-to-concrete-hint conversion.

- [x] **Step 5: Run the Task 4 suites and verify they pass**

Run both Task 4 commands. Expected: PASS with existing constraint and dispatch semantics unchanged.

- [x] **Step 6: Commit capability migration**

```bash
git add jazz-next/src/JazzNext/Compiler/Analyzer.hs jazz-next/src/JazzNext/Compiler/CapabilityFacts.hs jazz-next/src/JazzNext/Compiler/TypeInference jazz-next/test/JazzNext/Compiler/Semantics/BindingSignature
git commit -m "refactor: unify Jazz capability signature types"
```

### Task 5: Rebase Generic Schemes and Nominal Names Across Modules

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleInterface.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Types.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/VisibilityTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/ModulesTests.hs`

**Interfaces:**

- Consumes: `ModuleInterface`, imported `TypeScheme`, data-type inventories, explicit export filtering, and `ResolvedNameOrigin`.
- Produces: imported generic schemes and signature metadata whose nested `TDataType` and `SignatureType` names point to their defining module.

- [x] **Step 1: Add cross-module generic interface tests**

Compile a module exporting a generic data type and a generic value, import it into a consumer, and use the value at two concrete instantiations. Add same-text types in two modules and assert they do not unify. Add opaque value export coverage proving a value can expose its nominal type without implicitly exporting the type or constructors.

- [x] **Step 2: Run loader and parser module suites and verify failure**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
```

Expected: FAIL because unified nested signature metadata is not yet rebased across the interface.

- [x] **Step 3: Replace parallel rebasing with one recursive traversal**

Implement `rebaseSignatureType :: ResolvedNameOrigin -> Set Text -> SignatureType -> SignatureType`. Rebase `TypeName` and `TypeApplication` heads only when present in the module's data-type inventory, recurse through all children, preserve `TypeVariable`, and use the traversal for payloads, constraints, class methods, impl targets, and runtime hints. Continue rebasing `TypeScheme` through its existing `ExpressionType` traversal so imported quantified variables freshen at use sites.

- [x] **Step 4: Run both Task 5 suites and verify they pass**

Run both Task 5 commands. Expected: PASS with independent nominal identities and unchanged export visibility.

- [x] **Step 5: Commit module transport**

```bash
git add jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs jazz-next/src/JazzNext/Compiler/ModuleInterface.hs jazz-next/src/JazzNext/Compiler/TypeInference/Types.hs jazz-next/test/JazzNext/Compiler/Modules jazz-next/test/JazzNext/Compiler/Parser/Foundation/ModulesTests.hs
git commit -m "feat: transport generic named types across modules"
```

### Task 6: Preserve Concrete Runtime Hints and Explicit Type Application

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleInterface.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Capabilities.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/State.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/CapabilitiesTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/ControlFlowTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/NumericTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RecursionTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/RenderingTests.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/Shared.hs`

**Interfaces:**

- Consumes: generalized `TypeScheme`, explicit application's quantified-variable order, and `Map BindingRuntimeHintKey SignatureType`.
- Produces: explicit named-type instantiation and runtime values/hints that contain only concrete unified signature nodes.

- [x] **Step 1: Add explicit-application and runtime-hint tests**

Cover `identity @Maybe(Char) value`, a nested `Result(IOError, Text)` argument, monomorphic target rejection, excess argument rejection, incompatible explicit argument rejection, a polymorphic binding producing no unresolved runtime hint, and its concrete instantiation producing a nominal `TypeApplication` hint.

- [x] **Step 2: Run runtime and binding suites and verify failure**

Run the RuntimeSemantics and BindingSignatureCoherence commands. Expected: FAIL because runtime metadata still uses the removed constrained type and explicit arguments cannot lower named applications.

- [x] **Step 3: Change runtime-hint storage to `SignatureType`**

Update inference output, module interfaces, compiled prelude data, driver results, `RuntimeValue`, and runtime helper signatures from `ConstraintSignatureType` to `SignatureType`. Make `expressionTypeToRuntimeHint` return `Nothing` if any `TVarType` remains; otherwise recurse through `TDataType name arguments` as `TypeApplication name hints`, preserving zero-argument nominal types as `TypeName name`.

- [x] **Step 4: Apply explicit named arguments through the existing scheme order**

Convert the resolved explicit `SignatureType` with the Task 2 boundary, bind it to the first quantified variable, freshen remaining variables, and retain current `E2017` behavior for monomorphic targets, excess applications, and incompatibility. Runtime matching must compare nominal names and application arguments recursively.

- [x] **Step 5: Run the Task 6 suites and verify they pass**

Run both Task 6 commands. Expected: PASS with primitive, numeric, list, tuple, function, ADT, and qualified dispatch regressions unchanged.

- [x] **Step 6: Commit concrete runtime hints**

```bash
git add jazz-next/src/JazzNext/Compiler/Driver.hs jazz-next/src/JazzNext/Compiler/ModuleInterface.hs jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/src/JazzNext/Compiler/TypeInference jazz-next/src/JazzNext/Compiler/TypeInference.hs jazz-next/test/JazzNext/Compiler/Semantics/Runtime
git commit -m "refactor: use unified runtime type hints"
```

### Task 7: Prove the Old Representation Is Gone and Close the Queue Child

**Files:**

- Modify: `jazz-next/scripts/test-warning-config.sh`
- Modify: `docs/feature-status.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/done-archive.md`
- Modify: `docs/superpowers/specs/2026-07-10-jazz-next-generic-named-types-design.md`

**Interfaces:**

- Consumes: all Task 1-6 behavior and repository queue/archive conventions.
- Produces: an executable architecture guard, current language-status docs, and archived closure evidence for `JN-BOOTSTRAP-GENERIC-NAMED-TYPES-001`.

- [x] **Step 1: Add the architecture guard**

Add this guard after the suite matrix in the existing executable script so it fails if either removed name appears in production, tests, or Cabal metadata:

```bash
if rg -n 'SurfaceConstrainedSignatureType|ConstraintSignatureType' \
  jazz-next/src jazz-next/test jazz-next/jazz-next.cabal; then
  echo "parallel constrained signature type representation remains" >&2
  exit 1
fi

echo "signature type unification checks passed"
```

- [x] **Step 2: Run the architecture guard and verify it passes**

Run:

```bash
bash jazz-next/scripts/test-warning-config.sh
```

Expected: the full suite matrix passes, followed by `signature type unification checks passed`, with exit 0.

- [x] **Step 3: Run the complete verification contract**

Run every command in the frontmatter `verification` list in order. Expected: all focused suites, the full warning-config matrix, queue checker, docs checker, and diff check exit 0.

- [x] **Step 4: Update status and dispatcher documents**

Mark generic named signatures, exact arity, rank-1 signed schemes, module transport, and concrete hints implemented in `docs/feature-status.md` and `docs/jazz-language-state.md`. Change this plan to `status: done`, add `completed_on: 2026-07-10`, move the queue row to `docs/execution/done-archive.md` with verification evidence, and leave `Ready Now` empty unless a separately reviewed child has been promoted. Keep Jazz-authored `Maybe`/`Result` libraries as a separate unpromoted child.

- [x] **Step 5: Re-run queue and docs gates after closeout**

Run:

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: all three commands exit 0.

- [x] **Step 6: Commit verified closeout**

```bash
git add jazz-next/scripts/test-warning-config.sh docs/feature-status.md docs/jazz-language-state.md docs/execution/queue.md docs/execution/done-archive.md docs/superpowers/specs/2026-07-10-jazz-next-generic-named-types-design.md docs/superpowers/plans/2026-07-10-jazz-next-generic-named-types.md
git commit -m "docs: close generic named types child"
```
