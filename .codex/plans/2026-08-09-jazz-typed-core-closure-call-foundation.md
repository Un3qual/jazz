---
id: JN-BOOTSTRAP-TYPED-CORE-CLOSURE-CALL-FOUNDATION-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-08-09
plan_section: "Task 1"
target_paths:
  - src/Jazz/Compiler/TypeInference/Elaboration.hs
  - src/Jazz/Compiler/TypedCore.hs
  - src/Jazz/Compiler/TypedCore/Validate.hs
  - jazz/compiler/TypedCoreTypes.jz
  - jazz/compiler/TypedCoreValidate.jz
  - src/Jazz/Compiler/LoweredIR/Lower.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  - test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs
  - test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs
  - test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs
  - test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs
verification:
  - cabal test jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  - nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/ci/main-functional.sh
  - git diff --check
deliverable: "Produce and lower closed named function values, empty-environment unary closures, and higher-order closure calls while preserving the scalar/direct-call profile."
supersedes: []
---

# Jazz Typed-Core Closure-Call Foundation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the opt-in typed-core producer and backend-neutral lowerer so closed named functions can be transported as unary closure values, accepted as closure parameters or results, and called indirectly through explicit empty environments without changing ordinary Jazz compile/run behavior.

**Architecture:** Typed-core finalization classifies every local named callable once and records that classification in its `TypedScheme`; variable nodes carry the binder identity selected during finalization. The Haskell and Jazz typed-core mirrors validate those facts before the lowerer runs. The lowerer consumes the transported shape, retains the current flattened direct ABI for complete known direct calls, and emits one-argument closure signatures, empty environment layouts, closure construction, and `LoweredClosureCall` for closure-shaped values. Existing Lowered IR version 1 already owns every required representation and operation, so this child changes no Lowered IR schema.

**Tech Stack:** Haskell, GHC 9.14.1, Cabal, Jazz-authored contract modules, backend-neutral Lowered IR v1, Nix

## Global Constraints

- Implement only `JN-BOOTSTRAP-TYPED-CORE-CLOSURE-CALL-FOUNDATION-001` from accepted RFC 0009.
- Keep the producer and lowerer opt-in through `finalizeTypedCoreExpressionDirectCall` and `lowerTypedCoreExpressionDirectCall`; do not change the canonical-core/interpreter compile or run path.
- Preserve every existing scalar/direct-call fixture and the flattened direct-call recipe/ABI for statically known complete calls.
- Accept only concrete monomorphic callable values whose function body is capture-free and whose closure use needs one source argument at a time.
- Use an explicit `LoweredClosureEnvironmentLayout []` and a real environment operand for every capture-free closure; do not use null, an implicit environment, or a target-specific shortcut.
- Add no scalar `let` lowering, anonymous/nested lambda values, lexical capture, partial application, oversaturation, recursive groups, imports, managed values, runtime services, tail calls, LLVM, object/link, or native-runtime behavior.
- Keep Haskell `Jazz.Compiler.TypedCore.Validate` and Jazz `TypedCoreValidate.jz` behaviorally identical, including constructor order, validation paths, failure kinds/details, and failure ordering.
- Extend the existing contract runners and canonical adapters in place. Do not duplicate the Jazz evaluation harness, encoders, renderers, or fixture orchestration.
- Return no partial typed-core or lowered-IR artifact after any production, typed-core validation, profile, or lowered-IR validation failure.
- Commit after each green task using the commit message named in that task.

---

### Task 1: Transport callable shape and binder references through typed core

**Files:**

- Modify: `src/Jazz/Compiler/TypedCore.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`
- Modify: `jazz/compiler/TypedCoreTypes.jz`
- Modify: `jazz/compiler/TypedCoreValidate.jz`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`

**Interfaces:**

- Add `TypedCallableShape = TypedDirectCallableShape | TypedClosureCallableShape` to both typed-core type modules.
- Extend `TypedScheme` with a final `Maybe TypedCallableShape` field.
- Extend `TypedVariableExpr` with a final `Maybe TypedBinderId` field.
- Add mirrored validation failures for callable-shape mismatch and binder-reference mismatch, reusing `TypedBinderDetail` and the existing typed-core paths.
- Preserve `Nothing` for builtins and other values whose declaration is not present in the artifact.

- [ ] **Step 1: Add red Haskell/Jazz contract fixtures for the new schema and invariants.** Add exact cases for a direct callable scheme, a closure callable scheme, a non-callable scheme with `Nothing`, a callable scheme missing its shape, a scalar scheme carrying a shape, a local function reference with the correct binder, a local lambda-parameter reference with the correct binder, a missing binder reference, an unknown binder reference, and a binder whose type/recipe contract disagrees with the variable node. Run:

  ```bash
  cabal test jazz-typed-core-contract-spec --test-show-details=failures --jobs=1
  ```

  Expected: compilation fails because `TypedCallableShape` and the new constructor fields do not exist yet.

- [ ] **Step 2: Add the mirrored type constructors.** Change the durable shapes to:

  ```haskell
  data TypedCallableShape
    = TypedDirectCallableShape
    | TypedClosureCallableShape

  data TypedScheme
    = TypedScheme
        TypedBinderId
        [TypedTypeParameterId]
        [TypedEvidenceParameter]
        [TypedPrimitiveConstraint]
        TypedType
        TypedRepresentationRecipe
        (Maybe TypedCallableShape)

  data TypedExpr
    = ...
    | TypedVariableExpr TypedNodeInfo TypedCoreName (Maybe TypedBinderId)
  ```

  Make the same constructor-order change in `TypedCoreTypes.jz`. Update every constructor consumer named in this task so the library compiles: existing producer schemes receive the current direct shape, existing variable construction receives a provisional binder field, and existing hand-built fixtures receive their exact binder/shape contract. Task 2 replaces the producer’s provisional binder field with whole-module binder-aware finalization. Do not add compatibility patterns or alternate constructors.

- [ ] **Step 3: Teach both validators the same scheme rule.** For a non-function type require `Nothing`. For `TypedFunctionType`, require a concrete `TypedClosureRecipe` whose flattened arguments match the arrow chain and require exactly one shape. Preserve flattened recipes for `TypedDirectCallableShape`; accept recursively nested unary recipes for `TypedClosureCallableShape`.

- [ ] **Step 4: Teach both validators the same binder-reference rule.** Extend lexical contexts with `(TypedBinderId, TypedCoreName, type, recipe)` contracts. At `TypedVariableExpr`, require `Just binder` whenever lookup resolves to a declaration in the artifact, require `Nothing` only when no declaration exists there, and validate visibility plus type/recipe equality against the referenced binder rather than a suffix-only name match.

- [ ] **Step 5: Extend canonical transport.** Encode the new shape constructors, the scheme field, and `Maybe TypedBinderId` in `CanonicalTypedCoreComparison.hs`; keep its constructor names and field order exactly aligned with `TypedCoreTypes.jz`.

- [ ] **Step 6: Run the focused contract suite twice.**

  ```bash
  cabal test jazz-typed-core-contract-spec --test-show-details=failures --jobs=1
  cabal test jazz-typed-core-contract-spec --test-show-details=failures --jobs=1
  ```

  Expected: both runs pass with identical ordered valid/invalid manifest observations.

- [ ] **Step 7: Commit the contract milestone.**

  ```bash
  git add src/Jazz/Compiler/TypedCore.hs src/Jazz/Compiler/TypedCore/Validate.hs src/Jazz/Compiler/TypeInference/Elaboration.hs src/Jazz/Compiler/LoweredIR/Lower.hs jazz/compiler/TypedCoreTypes.jz jazz/compiler/TypedCoreValidate.jz test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs
  git commit -m "feat: transport typed callable contracts"
  ```

### Task 2: Classify named callables once during typed-core finalization

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`

**Interfaces:**

- Build `Map Name TypedCallableShape` once beside the existing `functionTable` before statements are finalized.
- Replace the parameter-name `Set` threaded through `finalizeExpression` with binder-aware lexical contracts so every produced local variable node can carry `Just TypedBinderId`.
- Make `scheme` accept a callable shape and append `Nothing` for non-callable schemes.
- Classify a named function as closure-shaped when any reference is not the callee of a statically known complete leading-lambda call; keep it direct-shaped only when every reference is such a complete call.

- [ ] **Step 1: Add source-to-typed-core fixtures before changing the producer.** Promote the unary closed `bare-function-value` boundary into an accepted named-function-value fixture and add exact accepted programs for:

  ```jazz
  identity :: Bool -> Bool.
  identity = \(item) -> item.
  identity.
  ```

  ```jazz
  apply :: (Bool -> Bool) -> Bool.
  apply = \(function) -> function True.
  identity :: Bool -> Bool.
  identity = \(item) -> item.
  apply identity.
  ```

  ```jazz
  identity :: Bool -> Bool.
  identity = \(item) -> item.
  choose :: Bool -> Bool -> Bool.
  choose = \(ignored) -> identity.
  choose False.
  ```

  Expected typed programs must mark value-used named functions closure-shaped, direct-only callers direct-shaped, and every local function/parameter use with its exact binder reference. The `choose` body returns a named unary closure; it does not introduce a nested lambda or accept general partial application. Retain partial application, oversaturation, capture, and recursion in the rejected manifest.

- [ ] **Step 2: Run the producer suite and record the intended red failures.**

  ```bash
  cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: the new accepted cases report `TypedCoreCallableValueUnsupported`, and exact typed programs lack callable shapes and binder references.

- [ ] **Step 3: Add whole-module callable-use classification.** Traverse provisional statements and expressions in source/preorder order. Record value use separately from a complete known application-spine call. Collapse multiple reasons to one final shape per binder, with closure shape winning. Do not inspect lowerer state and do not classify anonymous lambdas in this child.

- [ ] **Step 4: Thread binder-aware lexical contracts through finalization.** The top-level binder comes from `binderAt statementIndex [] typedName`; each leading lambda parameter comes from its existing statement/expression path. Emit `TypedVariableExpr info name (Just binder)` for local named functions and parameters, and `Nothing` only for declarations absent from the produced artifact.

- [ ] **Step 5: Preserve unary closure recipe staging.** Keep flattened `TypedClosureRecipe [ARep, BRep] CRep` for direct shapes. For a closure-shaped unary function use `TypedClosureRecipe [ARep] BRep`. When the result is itself callable in an accepted higher-order-result fixture, retain the recursively nested result recipe instead of flattening it.

- [ ] **Step 6: Make callable application role-sensitive.** A complete statically named direct call keeps the existing application-spine path. A call whose callee is a closure-valued parameter consumes exactly one operand. A named closure value is allowed in scalar/value position. Any underapplication or oversaturation still produces the existing ordered arity/profile failure in this child.

- [ ] **Step 7: Run the producer suite twice and inspect exact artifacts.**

  ```bash
  cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: all prior scalar/direct-call cases and all new source fixtures pass twice with stable ordering.

- [ ] **Step 8: Commit the producer milestone.**

  ```bash
  git add src/Jazz/Compiler/TypeInference/Elaboration.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: produce closed callable values"
  ```

### Task 3: Consume transported shape and recursively lower closure representations

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`

**Interfaces:**

- Extend `FunctionShape` with the defining binder and `TypedCallableShape`.
- Replace `monomorphicSchemeContract :: TypedScheme -> Maybe (TypedType, TypedRepresentationRecipe)` with a contract that also returns the transported callable shape.
- Extend `loweredRepresentation` recursively for `TypedClosureRecipe`.
- Keep `flattenLeadingLambdas` only for `TypedDirectCallableShape`; add a unary closure-shape collector that consumes exactly one leading lambda and permits a closure representation in its parameter or result.

- [ ] **Step 1: Add independent valid typed-core lowerer fixtures.** Construct programs directly, without passing through the producer, for a closure-valued parameter, a closure-valued result, a closure-shaped named function, and a direct function with the unchanged flattened recipe. Every variable node must carry the correct binder reference and every callable scheme its explicit shape.

- [ ] **Step 2: Add invalid profile fixtures.** Cover a closure shape with a flattened multi-argument recipe, a direct shape with a staged recipe that cannot match its complete leading lambdas, a non-concrete closure argument/result representation, a shape/body disagreement, and a variable binder reference that the typed-core validator must reject before lowering.

- [ ] **Step 3: Run the lowerer boundary tests.**

  ```bash
  cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: valid closure programs stop at `LoweredIRUnsupportedRepresentation`, `LoweredIRCallableValueUnsupported`, or `LoweredIRInvalidFunctionShape`; the invalid binder case is rejected as `LoweredIRTypedCoreFailures`.

- [ ] **Step 4: Lower closure recipes recursively.** Implement the closure case as:

  ```haskell
  TypedClosureRecipe arguments result -> do
    argumentRepresentations <- traverse loweredRepresentation arguments
    resultRepresentation <- loweredRepresentation result
    pure
      (LoweredClosureRepresentation
        (LoweredCallSignature argumentRepresentations resultRepresentation))
  ```

  The child accepts only one argument for closure-callable values, but the representation function stays structurally total for any validator-approved recipe.

- [ ] **Step 5: Make shape collection consume, never infer, callable shape.** Direct shape uses the existing complete leading-lambda flattening. Closure shape requires one leading lambda for the emitted function ABI and reads nested result representations recursively. If the typed scheme, lambda node, and body node disagree, return the owning statement’s `LoweredIRInvalidFunctionShape` before expression descent.

- [ ] **Step 6: Run the focused suite twice.**

  ```bash
  cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: every Task 3 representation/shape fixture passes twice. Do not introduce closure-emission fixtures until Task 4, so this checkpoint remains fully green.

- [ ] **Step 7: Commit the representation milestone.**

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: lower typed closure representations"
  ```

### Task 4: Emit empty-environment closures and unary higher-order calls

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs`

**Interfaces:**

- Generate one deterministic `LoweredClosureEnvironmentLayout []` for each closure-shaped named function.
- Keep the source-bound function id as `module::name`; generate only the environment layout id with RFC 0009’s `$jz1$closure-env$...` grammar.
- Give every closure-shaped lowered function a concrete environment parameter of `LoweredManagedReferenceRepresentation layoutId`.
- Emit `LoweredConstructProduct layoutId []`, then `LoweredConstructClosure functionId environmentOperand`, at each named function value site.
- Emit `LoweredClosureCall callableOperand [argumentOperand]` for a closure-valued parameter/result call.

- [ ] **Step 1: Write exact lowered programs before emission code.** The named-function-value fixture must include an empty environment layout, an environment construction instruction, a closure construction instruction, and a closure-represented entry result. The higher-order-call fixture must show its argument closure construction before the direct call to a direct-shaped `apply`, and `apply` must use `LoweredClosureCall` on its closure parameter. The closure-result fixture must return a closure representation without flattening it.

- [ ] **Step 2: Extend Lowered IR contract coverage.** Feed the new exact lowered programs through the existing Haskell/Jazz validation harness twice. Add no new Lowered IR constructors or version. Update `CanonicalLoweredIRComparison.hs` only where the shared expected-program manifest needs transport; reuse existing layout, representation, construct-closure, and closure-call encoders.

- [ ] **Step 3: Run the closure emission and Lowered IR suites.**

  ```bash
  cabal test jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: exact new programs fail because the lowerer still omits layouts, environments, closure construction, or indirect calls.

- [ ] **Step 4: Add deterministic environment identities.** Implement one private encoder for the RFC grammar and call it from layout collection. For module `Main`, binder path `[0]`, and name `identity`, the form must be length/count encoded, for example:

  ```text
  $jz1$closure-env$m1$4:Main$p1$0$n8:identity
  ```

  Reject a duplicate generated identity at the owning statement before `LoweredProgram` construction.

- [ ] **Step 5: Emit closure-shaped functions and value construction.** Add the empty layouts to `LoweredProgram`, set the function environment parameter, and construct environment plus closure in expression order. Do not cache or hoist a closure across a source expression boundary in this child.

- [ ] **Step 6: Emit unary closure calls.** Lower the callee expression first, then its one argument, then emit `LoweredClosureCall`. Use the typed application node’s recursively lowered result representation. Keep complete named direct calls on `LoweredDirectCall` and preserve their existing multi-operand order.

- [ ] **Step 7: Run both suites twice.**

  ```bash
  cabal test jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  cabal test jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: both repetitions produce identical valid IR and exact invalid outcomes.

- [ ] **Step 8: Commit the emission milestone.**

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs
  git commit -m "feat: emit empty-environment closure calls"
  ```

### Task 5: Lock failure ordering and complete parity manifests

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate.hs`
- Modify: `jazz/compiler/TypedCoreValidate.jz`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`

**Interfaces:**

- Preserve RFC 0009 ordering: input/program, module, statements in source order, then expression preorder; classification/identity/shape failures at a statement precede that statement’s descendant failures.
- Keep source-production manifests and independently constructed typed-core lowerer manifests separate.
- Keep malformed typed core out of the valid lowerer manifest.

- [ ] **Step 1: Add combined red failure cases.** Cover a callable-shape failure plus an unsupported descendant, a closure-use reason plus an unsupported supplied operand, several closure-shape reasons collapsing to one function classification, and an unaffected later sibling failure. Assert exact paths, kinds, details, and list order on two runs.

- [ ] **Step 2: Add manifest integrity assertions.** Assert fixture names are unique, accepted and rejected source sets are disjoint and exhaustive, valid/invalid independently constructed typed-core sets are disjoint and exhaustive, and every prior scalar/direct-call fixture remains present.

- [ ] **Step 3: Run G1 and confirm only ordering/inventory assertions are red.**

  ```bash
  cabal test jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

- [ ] **Step 4: Centralize ordered accumulation.** Classify the owning statement before descending, retain child failure chunks in traversal order, and remove callable/capture/arity failures only where the new child makes that exact behavior reachable. Do not sort failures or derive order from `Map`/`Set` iteration.

- [ ] **Step 5: Run G1 twice.**

  ```bash
  cabal test jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  cabal test jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: all three suites pass twice with identical ordered observations.

- [ ] **Step 6: Commit the failure-order milestone.**

  ```bash
  git add src/Jazz/Compiler/TypeInference/Elaboration.hs src/Jazz/Compiler/TypedCore/Validate.hs jazz/compiler/TypedCoreValidate.jz src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs
  git commit -m "test: lock closure call failure ordering"
  ```

### Task 6: Verify the child and close its dispatcher state

**Files:**

- Modify after implementation is green: `docs/compiler/pipeline.md`
- Modify after implementation is green: `docs/compiler/bootstrapping.md`
- Modify after implementation is green: `docs/project/status.md`
- Modify after implementation is green: `.codex/execution/queue.md`
- Modify after implementation is green: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Public/project docs describe an expanded opt-in typed-core/lowering profile, not shipped normal compile/run behavior.
- Queue closeout removes this child, records the verified boundary in current executor status and blocker evidence, and promotes no later child until that child has its own matching ready-plan frontmatter.

- [ ] **Step 1: Run the exact focused gate from RFC 0009.**

  ```bash
  cabal test jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

- [ ] **Step 2: Run the full checked-in Nix gate.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/ci/main-functional.sh
  ```

  Expected: warning-clean build, full ordinary test suite, repository audit, Cabal checks, documentation and queue checks, example smoke tests, flake checks, and diff checks all pass.

- [ ] **Step 3: Update the three status owners with the exact landed boundary.** State that closed named function values, closure parameters/results, explicit empty environments, and higher-order unary closure calls work only through the opt-in path. State that scalar bindings, capture, currying, recursion, and normal compile/run cutover remain unavailable.

- [ ] **Step 4: Close the queue row without over-promoting.** Remove this row, update the blocker’s completed-child evidence, and place `JN-BOOTSTRAP-TYPED-CORE-SCALAR-BINDING-001` only in the state supported by a newly written matching plan. If that plan has not been written and validated in the same closeout, leave it as the sole Next Curation Target rather than fabricating a ready row.

- [ ] **Step 5: Re-run coordination gates.**

  ```bash
  bash scripts/check-docs.sh
  bash scripts/check-execution-queue.sh
  git diff --check
  ```

- [ ] **Step 6: Review the final diff for scope creep.** Confirm no Lowered IR schema/version, canonical-core/interpreter path, scalar binding, anonymous lambda, capture, partial application, recursion, managed layout, runtime service, LLVM, or native-runtime behavior changed.

- [ ] **Step 7: Commit the verified closeout.**

  ```bash
  git add docs/compiler/pipeline.md docs/compiler/bootstrapping.md docs/project/status.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "docs: close closure call foundation batch"
  ```

## Execution Handoff

Execute Tasks 1-6 in order. Do not begin Task 2 until Task 1 is green and committed, and do not close or promote the dispatcher until the full Task 6 gate passes.
