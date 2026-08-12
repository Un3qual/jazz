---
id: JN-BOOTSTRAP-TYPED-CORE-LEXICAL-CAPTURE-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Full plan"
target_paths:
  - src/Jazz/Compiler/RecursiveBindings.hs
  - test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs
  - src/Jazz/Compiler/TypeInference/Elaboration.hs
  - src/Jazz/Compiler/LoweredIR/Lower.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  - test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs
  - test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs
  - test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs
  - test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs
verification:
  - cabal test recursive-bindings-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Produce binder-resolved inline and nested lexical closures and lower deterministic scalar or closure capture environments with stable lifted identities while retaining exact unsupported-capture failures."
last_verified: 2026-08-12
---

# Jazz Typed-Core Lexical Capture Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the opt-in typed-core producer and backend-neutral lowerer with
binder-resolved inline and nested closures whose scalar and closure-valued
captures use deterministic environment layouts and lifted identities.

**Architecture:** Typed-core finalization resolves every variable to its exact
visible binder while admitting anonymous lambdas and marking named functions
closure-shaped when they capture a prior lexical scalar. The lowerer discovers
root and nested lambda functions from validated typed core, computes ordered free
binder references in canonical expression traversal, creates one immutable
environment layout per closure, projects captures at function entry, and
constructs environments from the operands visible at each closure site. Existing
Typed Core and Lowered IR v1 constructors remain unchanged.

**Tech Stack:** Haskell, GHC 9.14.1, Cabal, backend-neutral Lowered IR v1, Nix

## Global Constraints

- Implement only `JN-BOOTSTRAP-TYPED-CORE-LEXICAL-CAPTURE-001` from accepted
  RFC 0009.
- Keep production and lowering opt-in through
  `finalizeTypedCoreExpressionDirectCall` and
  `lowerTypedCoreExpressionDirectCall`; ordinary compile/run remains on
  canonical core and the reference interpreter.
- Accept inline and nested unary lambdas plus concrete scalar and closure-valued
  captures; resolve every capture by `TypedBinderId`, never textual name in the
  lowerer.
- Order environment fields by first occurrence during canonical left-to-right
  typed-expression traversal and deduplicate repeated binder references without
  map or set iteration.
- Keep source-bound named functions on `module::name`; generate only lifted
  lambda and environment identities with RFC 0009's injective `$jz1$...`
  grammar and the `lambda-fn` or `closure-env` domain.
- Retain structured rejection for unsupported managed or unresolved captures,
  currying, partial application, oversaturation, recursive groups, control flow,
  patterns, nested blocks, imports, runtime services, and native behavior.
- Preserve every existing scalar, direct-call, closure-call, recursion,
  validator, canonical-comparison, and ordered-failure fixture.
- Add no Typed Core or Lowered IR constructor or version, global scalar storage,
  name-based lowering fallback, mutable environment, placeholder, adapter ABI,
  cache, or duplicate validator/encoder harness.
- Run Cabal only inside the checked-in Nix development shell and use
  `--jobs=1`.
- Commit after each green milestone using the commit message named below.

---

### Task 1: Promote the executor-ready lexical-capture child

**Files:**

- Create: `.codex/plans/2026-08-12-jazz-typed-core-lexical-capture.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Promote the accepted RFC 0009 child with the exact ten required target paths
  and G3 gate from the RFC ownership matrix.
- Remove lexical capture from `Next Curation Target` while its ready row is
  active.
- Keep currying, recursion, normal compile/run cutover, and every other blocked
  umbrella unpromoted.

- [x] **Step 1: Add the matching plan and Ready Now row.** Use priority `P1`,
      size `L`, kind `impl`, `autonomous_ready: yes`, this plan's exact target
      paths, and the four frontmatter verification commands.

- [x] **Step 2: Update the bootstrap blocker handoff.** State that the smallest
      unblocker is execution of the validated lexical-capture row and that no
      semantic decision remains.

- [x] **Step 3: Validate plan metadata and queue consistency.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  git diff --check
  ```

  Expected: both commands exit zero.

- [x] **Step 4: Commit the curation milestone.**

  ```bash
  git add .codex/plans/2026-08-12-jazz-typed-core-lexical-capture.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "docs: ready typed-core lexical capture"
  ```

### Task 2: Lock ordered capture evidence and producer behavior

**Files:**

- Modify: `src/Jazz/Compiler/RecursiveBindings.hs`
- Modify: `test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`

**Interfaces:**

- Add
  `lookupLambdaCapturedNamesOrdered :: LambdaCaptureHints -> Maybe ([Name], LambdaCaptureHints)`
  for stable first-occurrence `Name` values while retaining the existing
  set-returning `lookupLambdaCapturedNames` projection consumed by the reference
  runtime.
- Make finalization thread prior root scalar binders into named function bodies
  and nested lambdas, with the nearest lambda parameter or scalar binder winning
  after shadowing.
- Anonymous lambdas always receive staged unary closure recipes and exact
  parameter binder paths. Named functions that capture a prior scalar receive
  `TypedClosureCallableShape`; capture-free named functions retain their current
  shape classification.
- Typed core carries only existing lambda nodes and binder references; capture
  lists remain a lowerer-derived property, not a new schema or sidecar map.

- [ ] **Step 1: Add ordered capture-hint tests before implementation.** Extend
      `testLambdaCapturePlans` with literal expected name lists proving
      left-to-right order, repeated-reference deduplication, nested-lambda
      isolation, parameter shadowing, and block-local rebinding. The production
      mutation these tests catch is any set/map-derived or scope-insensitive
      capture order.

- [ ] **Step 2: Add exact producer fixtures before implementation.** Move
      `capturing-function` and `anonymous-lambda-result` into the accepted
      manifest and add hand-derived typed programs for:

  ```jazz
  seed :: Int.
  seed = 1.
  addSeed :: Int -> Int.
  addSeed = \(item) -> item + seed.
  addSeed 41.
  ```

  ```jazz
  apply :: (Int -> Int) -> Int.
  apply = \(function) -> function 1.
  seed :: Int.
  seed = 41.
  apply (\(item) -> seed + item).
  ```

  ```jazz
  consume :: (Int -> Int) -> Int.
  consume = \(function) -> function 1.
  forward :: (Int -> Int) -> Int.
  forward = \(function) -> consume (\(item) -> function item).
  identity :: Int -> Int.
  identity = \(item) -> item.
  forward identity.
  ```

  Add the shadowing fixture:

  ```jazz
  apply :: (Int -> Int) -> Int.
  apply = \(function) -> function 1.
  seed :: Int.
  seed = 99.
  apply (\(seed) -> seed + 1).
  ```

  Add the first-occurrence ordering fixture:

  ```jazz
  apply :: (Int -> Int) -> Int.
  apply = \(function) -> function 1.
  left :: Int.
  left = 20.
  right :: Int.
  right = 21.
  apply (\(item) -> right + left + right + item).
  ```

  Expected programs must use exact `TypedBinderId` values and nested unary
  recipes; shadowing binds the lambda parameter, while the second closure's
  environment order is `[right, left]`.

- [ ] **Step 3: Preserve the rejected boundary.** Add exact repeated producer
      failures for an unsupported managed capture, a capture combined with a
      later unsupported sibling, partial application, oversaturation, and
      recursive capture. Failures remain ordered by statement and expression
      path with no partial typed artifact.

- [ ] **Step 4: Run the producer and recursion suites and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test recursive-bindings-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: the ordered-hint assertion fails against the current set-derived
  projection, and the new accepted fixtures fail with the current
  `TypedCoreCaptureUnsupported` or `TypedCoreCallableValueUnsupported` results.

- [ ] **Step 5: Implement stable capture-hint order.** Retain a first-occurrence
      list beside set membership during the existing single traversal; preserve
      `lookupLambdaCapturedNames` for the runtime and expose the ordered
      projection for compiler production. Do not add a second AST walk.

- [ ] **Step 6: Resolve lexical binders during typed-core finalization.** Replace
      the parameter-only variable environment with an exact visible-binder
      environment that distinguishes leading parameters for duplicate checks
      from outer scalars and nested shadowing. Pass prior scalars into named
      function bodies, admit anonymous lambdas with closure recipes, and make
      capture-driven callable shape classification source-order aware.

- [ ] **Step 7: Run the Task 2 focused command twice and verify GREEN.** Expected:
      both repetitions pass with identical accepted artifacts and rejected
      failure order.

- [ ] **Step 8: Commit the producer milestone.**

  ```bash
  git add src/Jazz/Compiler/RecursiveBindings.hs test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs src/Jazz/Compiler/TypeInference/Elaboration.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  git commit -m "feat: produce typed lexical captures"
  ```

### Task 3: Lift lambdas and lower immutable capture environments

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`

**Interfaces:**

- Add private
  `CaptureShape { captureShapeBinder :: TypedBinderId, captureShapeName :: TypedCoreName, captureShapeRepresentation :: LoweredRepresentation }`
  and `functionShapeCaptures :: [CaptureShape]` to private `FunctionShape`.
- Index source-bound functions by declaration binder and lifted lambdas by their
  parameter binder. Derive nested shapes recursively from validated typed core;
  do not inspect inference state or source names to resolve operands.
- Generate `lambda-fn` identities for lifted functions and `closure-env`
  identities for their layouts from the owning lambda parameter binder's module,
  expression path, and name. Named functions retain `module::name` and derive
  their environment layout from the named scheme binder.
- Preload each emitted function's local binder map by projecting ordered capture
  fields from its environment parameter. At each named or anonymous closure
  site, construct the immutable product from currently visible binder operands,
  then construct the closure.

- [ ] **Step 1: Add exact lowerer expectations before implementation.** For each
      accepted Task 2 fixture, hand-build the complete `LoweredProgram` with
      literal layout order, generated IDs, projection instruction order,
      temporary IDs, function order, environment operands, calls, and terminal
      result. Include named scalar capture, empty anonymous closure, nested
      scalar capture, closure-parameter capture, shadowing, and repeated
      multi-capture order.

- [ ] **Step 2: Add independent valid typed-core lowerer fixtures.** Construct
      valid programs for the same boundaries without calling the producer, run
      each twice, and require exact lowered output plus permanent Lowered IR
      validation. These fixtures catch producer/lowerer coupling.

- [ ] **Step 3: Add independent lowerer rejection fixtures.** Require exact
      `LoweredIRCaptureUnsupported`, `LoweredIRDuplicateGeneratedIdentity`, or
      representation failures for a non-closure managed capture, a missing
      capture operand, an invalid generated-owner shape, and combined capture
      plus descendant failures. Every fixture must first pass typed-core
      validation.

- [ ] **Step 4: Run the direct-call suite and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: valid capture fixtures fail at the current lowerer capture or
  unsupported-lambda branches while prior boundaries remain green.

- [ ] **Step 5: Collect root and lifted function shapes.** Traverse statements
      and expressions in canonical order, exclude each function's own binders,
      deduplicate free binder references on first occurrence, retain transitive
      captures needed to construct nested closures, and reject unsupported
      representations at the owning path.

- [ ] **Step 6: Emit layouts, lifted functions, and projections.** Emit layouts
      and functions in source/preorder, seed function-local lowering state with
      environment projections, and ensure each generated ID participates in one
      duplicate-identity check before artifact construction.

- [ ] **Step 7: Construct captured closures from visible operands.** Generalize
      the existing empty-environment closure helper so named and anonymous sites
      supply ordered capture operands from exact local binder or parameter maps.
      Missing operands fail closed; no name lookup or empty-environment fallback
      is permitted.

- [ ] **Step 8: Run the Task 3 focused command twice and verify GREEN.** Expected:
      both repetitions pass with deterministic layouts, temporaries, lifted
      function order, and all existing empty-closure output unchanged.

- [ ] **Step 9: Commit the lowering milestone.**

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  git commit -m "feat: lower lexical capture environments"
  ```

### Task 4: Extend cross-language parity and invariant pressure

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`

**Interfaces:**

- Reuse the existing Haskell and Jazz validator runners and canonical encoders;
  add no alternate serialization or orchestration path.
- Add valid nested-lambda binder visibility and capture-environment artifacts to
  the exact parity manifests.
- Add malformed binder visibility, environment field order/representation,
  projection index, closure/environment mismatch, and duplicate generated
  identity cases to the existing invalid manifests.

- [ ] **Step 1: Add valid typed-core parity cases.** Feed exact inline, nested,
      shadowed, scalar-capture, and closure-capture programs through Haskell and
      hosted-Jazz validation twice and compare complete ordered canonical values.

- [ ] **Step 2: Add valid lowered-IR parity cases.** Feed the exact capture
      layouts, projections, constructed environments, lifted functions, and
      closure calls through both validators twice.

- [ ] **Step 3: Add invalid invariant cases.** Use hand-built artifacts whose
      single realistic mutation breaks binder visibility, field order or
      representation, projection bounds, closure environment identity, or
      generated uniqueness. Assert exact Haskell/Jazz failure parity.

- [ ] **Step 4: Run G3 twice and verify GREEN.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test recursive-bindings-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: both complete runs pass with stable manifest counts and ordering.

- [ ] **Step 5: Commit the parity milestone.**

  ```bash
  git add test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  git commit -m "test: cover lexical capture parity"
  ```

### Task 5: Close the child and verify the repository boundary

**Files:**

- Modify: `.codex/plans/2026-08-12-jazz-typed-core-lexical-capture.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`
- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/project/status.md`
- Modify: `rfcs/accepted/0009-typed-core-closure-and-recursion.md`

**Interfaces:**

- Mark this plan complete only after fresh focused, full serialized, queue, docs,
  and diff gates.
- Remove the completed row from `Ready Now`, place only
  `JN-BOOTSTRAP-TYPED-CORE-CURRIED-APPLICATION-001` in `Next Curation Target`,
  and update the blocker handoff without pre-promoting currying.
- State publicly that the opt-in profile supports deterministic inline and
  nested lexical closures while currying, oversaturation, and recursion remain
  absent and ordinary compile/run stays unchanged.

- [ ] **Step 1: Run fresh G3 verification.** Run the Task 4 Step 4 command once
      after the final source edit and read the complete exit status.

- [ ] **Step 2: Run the full serialized compiler suite.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  ```

  Expected: every registered test suite passes without parallel timeout noise.

- [ ] **Step 3: Update public compiler-boundary pages.** Add inline/nested
      closures, binder-resolved scalar and closure capture, deterministic
      environments, and lifted identities to the supported opt-in profile;
      retain all later RFC 0009 exclusions and the normal compile/run disclaimer.

- [ ] **Step 4: Synchronize plan, queue, blocker, and RFC state.** Check every
      task box, set frontmatter `status: complete` and `autonomous_ready: no`,
      use plan section `Full closeout`, remove the ready row, and add currying as
      the sole next curation candidate using RFC 0009's required paths and G1
      gate.

- [ ] **Step 5: Run closeout gates.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: every command exits zero.

- [ ] **Step 6: Perform the anti-slop review.** Enumerate every new private
      record, state field, helper, branch, and fallback. Remove any item without
      a concrete binder-resolution, ordered-capture, lifting, environment,
      invariant, or fixture responsibility. Confirm the final diff contains no
      parallel name-based environment, compatibility alias, dead abstraction,
      or implementation outside the RFC target paths and closeout owners.

- [ ] **Step 7: Commit closeout.**

  ```bash
  git add .codex/plans/2026-08-12-jazz-typed-core-lexical-capture.md .codex/execution/queue.md .codex/execution/blocker-contracts.md docs/compiler/bootstrapping.md docs/compiler/pipeline.md docs/project/status.md rfcs/accepted/0009-typed-core-closure-and-recursion.md
  git commit -m "docs: close typed-core lexical capture"
  ```
