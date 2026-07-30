---
id: JN-BOOTSTRAP-TYPED-CORE-EXPRESSION-DIRECT-CALL-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-TYPED-CORE-CONTRACT-FOUNDATION-001
  - JN-BOOTSTRAP-LOWERED-IR-CONTRACT-FOUNDATION-001
last_verified: 2026-07-30
plan_section: "Implementation Batch: Typed-Core Expression Production and Direct-Call Lowering"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-30-jazz-next-typed-core-expression-direct-call-design.md
  - docs/superpowers/plans/2026-07-30-jazz-next-typed-core-expression-direct-call.md
  - jazz-next/README.md
  - jazz-next/jazz-next.cabal
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --jobs=1 --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all --jobs=1
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --jobs=1 --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Produce and validate one opt-in single-module typed-core scalar/direct-call profile during the existing inference traversal, lower it to validated backend-neutral IR, and prove exact deterministic behavior over the fixed 16-accepted / 20-rejected manifest without changing normal compile/run behavior or permanent mirrored contracts."
---

# Jazz-Next Typed-Core Expression and Direct-Call Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:subagent-driven-development` (recommended) or
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce the first validated `TypedProgram` during the existing
stage-0 inference traversal and lower its closed scalar expressions and
non-capturing local direct calls to the permanent backend-neutral
`LoweredProgram` contract.

**Architecture:** Add an opt-in production mode to the existing analyzer and
inference path. Its recursive result pairs the current inferred type with an
optional provisional typed node and ordered profile failures, so finalization
uses the accepted final solver state without a second semantic traversal.
Normal inference entry points stay inference-only. A separate typed-core
lowerer validates its input, emits deterministic one-block scalar/direct-call
IR, and validates that result.

**Tech Stack:** GHC 9.14.1, Haskell 2010 with the package's existing
extensions, the active Jazz parser/resolver/analyzer/type-inference pipeline,
the permanent typed-core and lowered-IR contracts, Cabal test components, and
the Nix-pinned development environment.

**Design checkpoint:**
[`2026-07-30-jazz-next-typed-core-expression-direct-call-design.md`](../specs/2026-07-30-jazz-next-typed-core-expression-direct-call-design.md)

## Global Constraints

- Modify compiler behavior only under `jazz-next/`; `jazz-hs/` and `jazz2/`
  remain read-only references.
- Do not change permanent constructors in `TypedCore.hs` or `LoweredIR.hs`.
  Stop for a reviewed design amendment if an actual contract defect prevents
  this slice.
- Preserve the public shape and complete behavior of `InferenceResult`,
  ordinary inference entry points, module compilation, runtime hints, and the
  interpreter.
- Run analyzer and inference exactly once for the opt-in producer. Do not
  reconstruct semantic decisions in a second canonical-core traversal and do
  not add a structural-path sidecar annotation map.
- Inference-only mode must not allocate provisional typed nodes or production
  failures.
- Finalize once from the final solver state, after ordinary diagnostics and
  profile checks, and validate the complete `TypedProgram`.
- Keep source diagnostics, producer profile failures, typed-core invariant
  failures, lowerer profile failures, and lowered-IR invariant failures as
  distinct structured outcomes.
- Accept one resolved module only: no imports, imported types or capabilities,
  ambient prelude, multiple modules, managed values, closures, recursion,
  control flow, layouts, runtime services, tail calls, LLVM, object emission,
  linking, or native-runtime work.
- Emit deterministic identifiers only from resolved module paths and stable
  structural positions. Never use host paths, hashes, pointers, or map
  iteration order.
- Use canonical implemented Jazz syntax in every source fixture. Function
  values use pattern lambdas such as `increment = \(value) -> value + 1.` or
  ordered `\|` clauses when multiple bodies are needed. Haskell-style function
  equations are forbidden.
- Use `$` in the dedicated accepted fixture and exercise applicable canonical
  language features without decorative syntax.
- Tests compare complete structured values, not rendered diagnostics,
  constructor source text, `Show` output, absolute paths, or unordered
  collections.
- Run each failing behavior test before its production change and commit every
  independently reviewable green milestone.

## File and Responsibility Map

| File | Responsibility in this child |
| --- | --- |
| `jazz-next/src/JazzNext/Compiler/TypeInference.hs` | Public opt-in producer entry point, unchanged inference-only projection, production mode threading, and pairing of `InferenceResult` with production status. |
| `jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs` | Update the shared recursive inference function type to carry the richer internal expression result without changing diagnostic ownership. |
| `jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs` | Retain supported root-scope signatures, bindings, and terminal expression in source order while preserving existing inference/generalization/runtime-hint behavior. |
| `jazz-next/src/JazzNext/Compiler/TypeInference/Elaboration.hs` | Own production profiles, provisional typed nodes/statements, structured producer failures, solver-state finalization, deterministic ids, typed module/interface construction, and typed-core validation. |
| `jazz-next/src/JazzNext/Compiler/LoweredIR/Lower.hs` | Validate typed input, reject forms outside the slice, lower scalar primitives and direct calls deterministically, and validate the produced IR. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs` | Own the exact accepted/rejected manifests, canonical sources and inputs, explicit expected production results, explicit expected IR, and coverage audits. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs` | Run each source through parse, resolve, analyze/infer, finalize, validate, lower, and validate twice; assert exact results and inference-only compatibility. |
| `jazz-next/jazz-next.cabal` | Expose the two production modules and register the focused test component. |
| Coordination and status paths in frontmatter | Record implementation state, verification, closeout, and the next closure/recursion design gate. |

## Fixed Public and Internal Boundaries

`JazzNext.Compiler.TypeInference.Elaboration` owns the opt-in profile and
status types. `JazzNext.Compiler.TypeInference` re-exports the public types and
owns the result that includes the existing `InferenceResult`:

```haskell
data TypedCoreProductionProfile
  = TypedCoreExpressionDirectCallProfile
  deriving (Eq, Show)

data TypedCoreProductionStatus
  = TypedCoreProductionBlockedByDiagnostics
  | TypedCoreProductionUnsupported [TypedCoreProductionFailure]
  | TypedCoreProductionInvariantFailures [TypedCoreValidationFailure]
  | TypedCoreProductionSucceeded TypedProgram
  deriving (Eq, Show)

data TypedCoreProductionResult = TypedCoreProductionResult
  { typedCoreProductionInferenceResult :: InferenceResult,
    typedCoreProductionStatus :: TypedCoreProductionStatus
  }
  deriving (Eq, Show)

inferResolvedModuleTypedCoreWithProfile ::
  TypedCoreProductionProfile ->
  InferenceInputs ->
  TypedSourcePath ->
  ModuleGraph.ResolvedModule ->
  IO TypedCoreProductionResult
```

The internal recursive result replaces only the private
`(Maybe ExpressionType, InferState)` plumbing:

```haskell
data TypedCoreProductionMode
  = InferenceOnly
  | ProduceTypedCoreExpressionDirectCall
  deriving (Eq, Show)

data InferredExpr = InferredExpr
  { inferredExpressionType :: Maybe ExpressionType,
    inferredProvisionalExpr :: Maybe ProvisionalTypedExpr,
    inferredProductionFailures :: [TypedCoreProductionFailure]
  }

type InferExprFn =
  TypedCoreProductionMode ->
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  (InferredExpr, InferState)
```

`ProvisionalTypedExpr` and `ProvisionalTypedScope` are internal return values,
not permanent compiler contracts. Their constructors cover only literal,
unit, variable, lambda, application, listed binary operator, root signature,
root function binding, and terminal-expression shapes. Each retained node
carries its unresolved `ExpressionType`, resolved identity, stable structural
path, and any instantiation/evidence decision already selected by inference.

Finalization has no dependency on the public `InferenceResult`:

```haskell
finalizeTypedCoreExpressionDirectCall ::
  TypedSourcePath ->
  ModuleGraph.ResolvedModule ->
  InferState ->
  ProvisionalTypedScope ->
  TypedCoreProductionStatus
```

The lowerer owns an equally explicit four-way outcome:

```haskell
data LoweredIRLoweringResult
  = LoweredIRTypedCoreFailures [TypedCoreValidationFailure]
  | LoweredIRUnsupported [LoweredIRLoweringFailure]
  | LoweredIRInvariantFailures [LoweredIRValidationFailure]
  | LoweredIRSucceeded LoweredProgram
  deriving (Eq, Show)

lowerTypedCoreExpressionDirectCall ::
  TypedProgram ->
  LoweredIRLoweringResult
```

Status precedence is part of the contract:

```text
producer: source diagnostics -> profile failures -> typed validation -> success
lowerer:  typed validation -> profile failures -> IR validation -> success
```

## Fixed Fixture Inventory

The accepted manifest has exactly these 16 unique names in this order:

```haskell
acceptedFixtureNames =
  [ "unit-entry",
    "bool-entry",
    "char-entry",
    "default-int-entry",
    "default-float-entry",
    "explicit-numeric-widths",
    "arithmetic-operators",
    "ordering-operators",
    "equality-operators",
    "scalar-parameter-return",
    "single-argument-direct-call",
    "curried-multi-argument-direct-call",
    "forward-direct-call-dag",
    "nested-direct-calls",
    "dollar-direct-call",
    "exported-direct-function"
  ]
```

The rejected manifest has exactly these 20 unique names in this order:

```haskell
rejectedFixtureNames =
  [ "source-diagnostic",
    "invalid-portable-source-path",
    "resolved-import",
    "ambient-prelude-input",
    "text-value",
    "list-value",
    "non-unit-tuple",
    "data-value",
    "conditional",
    "pattern-case",
    "local-block-binding",
    "bare-function-value",
    "partial-direct-call",
    "oversaturated-direct-call",
    "capturing-function",
    "self-recursive-function",
    "mutually-recursive-functions",
    "polymorphic-or-evidence-function",
    "imported-direct-call",
    "user-defined-operator-call"
  ]
```

The source fixtures must use the implemented language surface. Representative
accepted direct-call sources use pattern-lambda bindings, including the
dedicated `$` case:

```jazz
module App::Main {
  increment :: Int -> Int.
  increment = \(value) -> value + 1.

  increment $ 41.
}
```

A function that needs distinct pattern bodies uses the old multiple-body form,
not function equations:

```jazz
classify = \|(0) -> 0
  |(value) -> value + 1.
```

The first profile does not accept that second example because literal-pattern
dispatch is control flow; it exists here solely to lock the authored-source
syntax rule for any future fixture support.

## Implementation Batch: Typed-Core Expression Production and Direct-Call Lowering

### Task 0: Promote the approved implementation child

**Files:**

- Create: `docs/superpowers/plans/2026-07-30-jazz-next-typed-core-expression-direct-call.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md`
- Modify: `docs/superpowers/specs/2026-07-30-jazz-next-typed-core-expression-direct-call-design.md`

**Interfaces:**

- Consumes: the approved written design and completed typed-core/lowered-IR
  contract foundations.
- Produces: one exact P1/L implementation row in `Ready Now`.
- Preserves: closure/recursion, control flow, multi-module integration, LLVM,
  object/link, and native-runtime work as unpromoted later gates.

- [x] **Step 1: Create matching plan frontmatter and queue metadata**

  Copy the design's existing concrete ownership paths, dependency ids, fixed
  manifests, deliverable, non-goals, and verification commands into this
  plan. The file/responsibility map and owning tasks fix the four exact
  file-creation paths, which cannot appear in a promoted queue row until they
  exist. Add one matching `Ready Now` row and remove the completed design
  candidate from `Next Curation Target`.

- [x] **Step 2: Record written approval**

  Mark the written design approved on `2026-07-30`. Update the bootstrap
  profile and blocker contract so the implementation child is the sole active
  unblocker.

- [x] **Step 3: Verify and commit the promotion**

  Run:

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: queue/frontmatter parity, docs status, and whitespace checks pass.
  Outside Nix, the docs script may report its documented Prettier version skip;
  queue and docs status checks must still run.

  Commit:

  ```bash
  git add docs/execution/queue.md docs/execution/blocker-contracts.md \
    docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md \
    docs/superpowers/specs/2026-07-30-jazz-next-typed-core-expression-direct-call-design.md \
    docs/superpowers/plans/2026-07-30-jazz-next-typed-core-expression-direct-call.md
  git commit -m "docs: plan typed-core direct-call slice"
  ```

### Task 1: Establish the opt-in single-pass production foundation

**Files:**

- Create: `jazz-next/src/JazzNext/Compiler/TypeInference/Elaboration.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Interfaces:**

- Produces: the public producer/profile/status boundary above.
- Produces: private mode-aware `InferredExpr` plumbing and provisional root
  scope.
- Initially closes: `unit-entry`, `source-diagnostic`,
  `invalid-portable-source-path`, `resolved-import`, and
  `ambient-prelude-input`.
- Preserves: exact ordinary `InferenceResult` values and diagnostics.

- [ ] **Step 1: Register the focused suite with the first failing cases**

  Expose `JazzNext.Compiler.TypeInference.Elaboration`. Register
  `jazz-typed-core-expression-direct-call-spec` with main
  `JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs` and the
  fixture module as `other-modules`.

  Add an audited partial fixture table containing the five names above. Assert:

  ```haskell
  assertEqual "unit production"
    (TypedCoreProductionSucceeded expectedUnitProgram)
    (typedCoreProductionStatus actualUnitResult)

  assertEqual "diagnostics take precedence"
    TypedCoreProductionBlockedByDiagnostics
    (typedCoreProductionStatus sourceDiagnosticResult)

  assertEqual "ordinary inference is unchanged"
    beforeInferenceResult
    afterInferenceResult
  ```

- [ ] **Step 2: Run the focused suite and confirm the API is absent**

  Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c \
    cabal test --project-dir=jazz-next \
      jazz-typed-core-expression-direct-call-spec \
      --jobs=1 --test-show-details=failures
  ```

  Expected: FAIL at compile time because the producer, status types, and
  elaboration module do not exist.

- [ ] **Step 3: Generalize private inference return plumbing**

  Add `TypedCoreProductionMode`, `InferredExpr`, provisional unit/root-scope
  nodes, and structured input failure kinds in `Elaboration.hs`. Change
  `InferExprFn`, `inferExprType`, and `inferScopeType` to return `InferredExpr`
  while preserving the current inferred type and state transitions.

  Every existing public inference entry point must pass `InferenceOnly` and
  project:

  ```haskell
  inferredExpressionType inferredResult
  ```

  In `InferenceOnly`, helper constructors must return:

  ```haskell
  InferredExpr expressionType Nothing []
  ```

  so the ordinary path does not retain typed nodes or production failures.

- [ ] **Step 4: Add the opt-in root entry point and unit finalization**

  Validate before finalization:

  - `inferenceCurrentModulePath` exactly equals
    `resolvedModulePath`;
  - `TypedSourcePath` is relative and satisfies the permanent contract;
  - `resolvedModuleImports` is empty;
  - imported types/data/capabilities/class names are empty; and
  - ambient-prelude resolution is absent.

  Run analyzer and inference once. Return diagnostics before profile failures.
  For a root `EBlock` with a terminal unit expression, apply final
  substitutions/defaulting, construct:

  ```haskell
  TypedProgram Nothing [entryModule] (resolvedModulePath resolvedModule)
  ```

  then call `validateTypedProgram`.

- [ ] **Step 5: Run focused and existing inference tests**

  Run the focused suite, then:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c \
    cabal test --project-dir=jazz-next \
      jazz-typed-core-expression-direct-call-spec \
      jazz-typed-core-contract-spec \
      --jobs=1 --test-show-details=failures
  ```

  Expected: the five foundation cases pass, complete `InferenceResult`
  equality holds for representative valid and invalid sources, and the
  permanent typed-core suite remains green.

- [ ] **Step 6: Commit the green foundation**

  ```bash
  git add jazz-next/jazz-next.cabal \
    jazz-next/src/JazzNext/Compiler/TypeInference.hs \
    jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs \
    jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs \
    jazz-next/src/JazzNext/Compiler/TypeInference/Elaboration.hs \
    jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs \
    jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: add typed-core production foundation"
  ```

### Task 2: Finalize the source-representable root scalar profile

> **Approved ordering amendment (`2026-07-30`):** Canonical integer literals
> are `LInt Integer` and carry no explicit width. Existing inference obtains
> `Int8`/`UInt*` types only from an expected-type context. To avoid invented
> syntax, fabricated semantic information, or premature application support,
> `explicit-numeric-widths` and `user-defined-operator-call` close in Task 3,
> where concrete signatures, bindings, and application spines are already
> owned. The overall exact 16-accepted / 20-rejected manifest is unchanged.

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Elaboration.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`

**Interfaces:**

- Produces: final typed unit, Bool, Char, default `Int`/`Float`, and all 10
  listed primitive operators.
- Rejects: managed and structured values, control flow, patterns, nested
  blocks, and other unsupported root expressions with ordered structured
  failures.
- Preserves: ordinary inference and permanent typed-core constructors.

- [ ] **Step 1: Add failing accepted scalar fixtures**

  Add accepted cases `bool-entry`, `char-entry`, `default-int-entry`,
  `default-float-entry`, `arithmetic-operators`, `ordering-operators`, and
  `equality-operators`.

  The manifest audit must prove:

  ```haskell
  admittedOperators ==
    ["+", "-", "*", "/", "<", "<=", ">", ">=", "==", "!="]
  ```

  Each fixture carries an explicit complete `TypedProgram`; operator fixtures
  assert exact child order, `TypedType`, and representation recipe at every
  node.

- [ ] **Step 2: Add failing rejected scalar/profile fixtures**

  Add `text-value`, `list-value`, `non-unit-tuple`, `data-value`,
  `conditional`, `pattern-case`, and `local-block-binding`. Assert complete
  ordered production failures including module/statement/expression paths and
  stable kind/detail fields.

- [ ] **Step 3: Run and confirm unsupported scalar nodes**

  Run the focused suite. Expected: FAIL because literals other than unit,
  binary nodes, and the new structured failure kinds are not finalized.

- [ ] **Step 4: Retain and finalize scalar provisional nodes**

  During the existing recursive visits, retain literal and supported binary
  nodes with unresolved `ExpressionType`. Finalization must:

  - apply final solver substitution and existing defaults once;
  - map `TIntegerLiteralType` and `TIntType` to semantic `Int` with signed
    64-bit representation;
  - map `TFloatType` to semantic `Float` with 64-bit float representation;
  - map unit, Bool, and Char to their exact semantic type and recipe;
  - resolve only the listed builtin binary operator identities; and
  - reject unresolved variables, managed recipes, non-scalar structures, and
    unsupported expressions without fabricating typed nodes.

  Accumulate failures in structural preorder and continue ordinary inference
  through unsupported children.

- [ ] **Step 5: Prove exact scalar results and repeatability**

  Run each accepted and rejected fixture twice. Compare the complete
  production status and complete `TypedProgram` values. Assert the two runs
  are equal before comparing expectations.

  Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c \
    cabal test --project-dir=jazz-next \
      jazz-typed-core-expression-direct-call-spec \
      jazz-typed-core-contract-spec \
      --jobs=1 --test-show-details=failures
  ```

  Expected: all currently registered cases pass twice with exact values.

- [ ] **Step 6: Commit scalar elaboration**

  ```bash
  git add jazz-next/src/JazzNext/Compiler/TypeInference.hs \
    jazz-next/src/JazzNext/Compiler/TypeInference/Elaboration.hs \
    jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs \
    jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: elaborate scalar typed core"
  ```

### Task 3: Produce monomorphic functions and fully saturated direct calls

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Elaboration.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`

**Interfaces:**

- Produces: concrete non-capturing, nonrecursive local functions and fully
  saturated direct-call spines.
- Produces: every explicit signed, unsigned, and floating numeric width from
  existing concrete signature/binding expected-type contexts, without new
  literal syntax or AST fields.
- Produces: exact typed module interface entries selected by the resolved
  public value-export inventory.
- Rejects: callable values, partial/over-application, captures, recursion,
  generalized/evidence-bearing functions, imported calls, declared
  user-defined operator calls, and unsupported exports.

- [ ] **Step 1: Add the eight remaining accepted fixtures**

  Add `explicit-numeric-widths`, `scalar-parameter-return`,
  `single-argument-direct-call`, `curried-multi-argument-direct-call`,
  `forward-direct-call-dag`, `nested-direct-calls`, `dollar-direct-call`, and
  `exported-direct-function`. Together with the root scalar fixtures, the
  accepted manifest must now equal the exact 16-name inventory above.

  `explicit-numeric-widths` must cover exactly:

  ```haskell
  explicitNumericTypes ==
    [ "Int8", "Int16", "Int32", "Int64",
      "UInt8", "UInt16", "UInt32", "UInt64",
      "Float16", "Float32", "Float64"
    ]
  ```

  Use concrete monomorphic function signatures and pattern-lambda bindings to
  provide the existing expected-type context for each literal. Do not add an
  integer suffix, change `LInt`, use explicit type application/conversion, or
  fabricate a width during finalization.

  Author function values only with canonical pattern lambdas:

  ```jazz
  combine :: Int -> Int -> Int.
  combine = \(left, right) -> left + right.

  combine 20 22.
  ```

  The `$` fixture must reach the same canonical `EApply` spine as ordinary
  application and must not receive a separate elaboration rule.

- [ ] **Step 2: Add the remaining callable/operator rejection fixtures**

  Add `bare-function-value`, `partial-direct-call`,
  `oversaturated-direct-call`, `capturing-function`,
  `self-recursive-function`, `mutually-recursive-functions`,
  `polymorphic-or-evidence-function`, `imported-direct-call`, and
  `user-defined-operator-call`. The rejected manifest must now equal the exact
  20-name inventory above.

  The operator source must declare and bind its operator with the canonical
  implemented operator declaration plus a pattern-lambda value, so ordinary
  analysis/inference succeeds and the producer—not an unbound-name
  diagnostic—owns the structured rejection.

- [ ] **Step 3: Run and confirm callable/profile failures**

  Run the focused suite. Expected: FAIL because scope production does not yet
  retain function statements, applications do not resolve a local direct
  target/arity, and call-graph/capture checks are absent.

- [ ] **Step 4: Retain root-scope signatures, functions, and calls**

  In `Scope.hs`, preserve existing binding seeds, adjacent-signature checking,
  generalization, runtime hints, and module-interface inference. In production
  mode only, additionally return source-ordered provisional root statements:

  ```haskell
  data ProvisionalTypedStatement
    = ProvisionalSignature ...
    | ProvisionalFunctionBinding ...
    | ProvisionalTerminalExpression ...
  ```

  Flatten leading lambdas into ordered parameters while retaining canonical
  nested `TypedLambdaExpr` nodes. Record resolved current-module target
  identity, concrete signature expected types, and application-spine argument
  order during existing `EApply` visits.

- [ ] **Step 5: Finalize and validate callable restrictions**

  Before constructing the permanent tree:

  - require concrete monomorphic scalar parameter/result types and recipes;
  - preserve every explicit signed, unsigned, and floating width selected by
    the existing signature/binding expected-type path;
  - require exact full arity at every use;
  - reject functions used outside callee position;
  - compute lexical free-value captures from resolved binder identities;
  - build the complete local direct-call graph in source order;
  - reject self loops and multi-node strongly connected components;
  - allow forward edges in an acyclic graph;
  - reject non-local, imported, prelude, constructor, method, and
    user-defined-operator targets; and
  - include only supported public value exports in the typed interface.

  Binder ids derive from zero-based module, statement, parameter, and
  expression paths. The interface preserves resolved export inventory order;
  statement/function order preserves source order.

- [ ] **Step 6: Prove both full manifests and inference compatibility**

  Audit exact counts (`16` accepted, `20` rejected), uniqueness, order, width
  coverage, operator coverage, and failure-kind coverage. Run every case twice
  and compare complete structured statuses.

  For representative scalar, direct-call, diagnostic, and unsupported
  sources, call the ordinary inference entry point separately and assert its
  complete `InferenceResult` equals the opt-in result's paired
  `InferenceResult`.

  Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c \
    cabal test --project-dir=jazz-next \
      jazz-typed-core-expression-direct-call-spec \
      jazz-typed-core-contract-spec \
      --jobs=1 --test-show-details=failures
  ```

  Expected: exact 16/20 production results pass twice and inference-only
  compatibility remains exact.

- [ ] **Step 7: Commit direct-call production**

  ```bash
  git add jazz-next/src/JazzNext/Compiler/TypeInference.hs \
    jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs \
    jazz-next/src/JazzNext/Compiler/TypeInference/Elaboration.hs \
    jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs \
    jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: elaborate direct-call functions"
  ```

### Task 4: Lower scalar values and primitive operators

**Files:**

- Create: `jazz-next/src/JazzNext/Compiler/LoweredIR/Lower.hs`
- Modify: `jazz-next/jazz-next.cabal`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`

**Interfaces:**

- Produces: `LoweredIRLoweringResult` and
  `lowerTypedCoreExpressionDirectCall`.
- Lowers: unit, Bool, Char, default/explicit numeric literals, parameters, and
  the 10 listed primitive operators.
- Preserves: permanent lowered-IR constructors and validator ownership.

- [ ] **Step 1: Add explicit expected scalar `LoweredProgram` values**

  Extend the accepted scalar fixtures with complete expected IR. Assert:

  - program version `1`;
  - empty layouts and runtime services;
  - synthetic function id `App::Main::$entry`;
  - one block id `entry`;
  - left-to-right temporary ids `t1`, `t2`, and so on;
  - exact immediate widths/representations; and
  - one `LoweredReturn` terminator.

  Add direct unit tests for lowering precedence using an intentionally invalid
  `TypedProgram`; it must return `LoweredIRTypedCoreFailures` before any
  profile result.

- [ ] **Step 2: Run and confirm the lowerer is absent**

  Run the focused suite. Expected: FAIL at compile time because
  `JazzNext.Compiler.LoweredIR.Lower` and its result types do not exist.

- [ ] **Step 3: Implement typed validation and scalar operand lowering**

  Call `validateTypedProgram` first. For a valid supported module, use an
  internal state containing the current function id, next block-local
  temporary index, emitted instructions in order, and parameter operands.

  Map recipes exactly:

  ```haskell
  TypedUnitRecipe
    -> LoweredImmediateOperand LoweredUnitImmediate
  TypedBoolRecipe
    -> LoweredImmediateOperand (LoweredBoolImmediate value)
  TypedCharRecipe
    -> LoweredImmediateOperand (LoweredCharImmediate value)
  ```

  Signed/unsigned integers and floats retain exact permanent widths. Map
  `+ - * /` to arithmetic primitives and `< <= > >= == !=` to comparison
  primitives. Emit one instruction/temporary per primitive and lower both
  operands left to right.

- [ ] **Step 4: Reject unsupported valid typed trees structurally**

  For permanent typed-core values outside this profile, return complete
  ordered `LoweredIRLoweringFailure` values with typed module/statement/
  expression paths. Do not rely on the producer having filtered them.

  Only after a complete supported program is emitted, call
  `validateLoweredProgram`; return invariant failures before success.

- [ ] **Step 5: Run scalar lowering and permanent contract suites**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c \
    cabal test --project-dir=jazz-next \
      jazz-typed-core-expression-direct-call-spec \
      jazz-typed-core-contract-spec \
      jazz-lowered-ir-contract-spec \
      --jobs=1 --test-show-details=failures
  ```

  Expected: every accepted scalar fixture matches its complete expected IR
  twice; the two permanent schema/validator suites remain green.

- [ ] **Step 6: Commit scalar lowering**

  ```bash
  git add jazz-next/jazz-next.cabal \
    jazz-next/src/JazzNext/Compiler/LoweredIR/Lower.hs \
    jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs \
    jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: lower scalar typed core"
  ```

### Task 5: Lower functions and fully saturated direct calls

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/LoweredIR/Lower.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`

**Interfaces:**

- Produces: source-ordered local lowered functions followed by one synthetic
  entry function.
- Lowers: flattened leading lambda parameters, parameter operands, nested
  fully saturated local direct calls, and `$`-originated canonical calls.
- Rejects independently: invalid function shapes, captures, recursion,
  callable values, arity errors, and non-local targets.

- [ ] **Step 1: Add complete expected function/direct-call IR**

  Extend the seven function/direct-call accepted fixtures with explicit
  `LoweredProgram` values. Assert:

  ```text
  local function id: <module segments joined by "::">::<source name>
  entry function id: <module segments joined by "::">::$entry
  block id: entry
  parameter ids: arg1, arg2, ...
  temporary ids: t1, t2, ... per block
  ```

  Assert local functions remain in source order even when the acyclic call
  graph contains forward references; the entry function comes last.

- [ ] **Step 2: Run and confirm direct calls are unsupported**

  Run the focused suite. Expected: FAIL with explicit lowerer profile failures
  for typed lambdas/applications and missing expected functions.

- [ ] **Step 3: Collect function shapes and stable call signatures**

  Validate all typed statements before emission. Flatten leading
  `TypedLambdaExpr` nodes into parameters while preserving each annotation's
  concrete scalar type and recipe. Build a source-ordered map from exact
  `TypedCoreName` identity to stable `LoweredFunctionId`, arity, argument
  representations, and result representation.

  Recheck capture, recursion, bare-value, and arity restrictions at the
  typed-core boundary. This is lowerer profile validation, not an assumption
  inherited from the producer.

- [ ] **Step 4: Emit parameter operands and ordinary direct calls**

  Lower a parameter reference to:

  ```haskell
  LoweredFunctionParameterOperand parameterId representation
  ```

  Flatten nested `TypedApplyExpr` nodes only when the callee resolves to the
  local function table and the spine has exact arity. Lower arguments left to
  right, emit:

  ```haskell
  LoweredDirectCall functionId operands
  ```

  into one result temporary, then return that operand. A tail-position call
  remains `LoweredDirectCall` plus `LoweredReturn`; never emit
  `LoweredDirectTailCall` in this child.

- [ ] **Step 5: Prove deterministic complete lowering twice**

  Parse, resolve, infer/finalize, lower, and validate every accepted fixture
  twice. Compare each complete production result and complete lowering result
  between runs, then against its explicit expectation.

  Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c \
    cabal test --project-dir=jazz-next \
      jazz-typed-core-expression-direct-call-spec \
      jazz-typed-core-contract-spec \
      jazz-lowered-ir-contract-spec \
      --jobs=1 --test-show-details=failures
  ```

  Expected: all 16 accepted cases produce exact validated typed core and IR;
  all 20 rejected cases produce exact ordered producer failures; permanent
  suites remain green.

- [ ] **Step 6: Commit direct-call lowering**

  ```bash
  git add jazz-next/src/JazzNext/Compiler/LoweredIR/Lower.hs \
    jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs \
    jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: lower typed direct calls"
  ```

### Task 6: Close manifest audits and compatibility evidence

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: production paths from prior tasks only when a behavioral test exposes
  a root-cause defect

**Interfaces:**

- Produces: fixed manifest closure and complete repeated pipeline evidence.
- Preserves: normal compile/run results, contract suites, and deterministic
  status ordering.

- [ ] **Step 1: Make manifest audits exhaustive**

  Assert exact ordered name lists, counts `16` and `20`, uniqueness, all eleven
  explicit widths, every admitted operator, every producer failure kind used
  by the manifest, and no unknown fixture category.

  Assert every accepted fixture reaches all of:

  ```text
  parse -> resolve -> analyze/infer -> finalize
        -> validate typed core -> lower -> validate lowered IR
  ```

  twice with complete value equality.

- [ ] **Step 2: Add lowerer-only negative values**

  Construct valid permanent `TypedProgram` values that the producer cannot
  emit in this profile—such as a managed scalar position or valid conditional—
  and assert exact `LoweredIRUnsupported` failures. Keep these separate from
  the fixed 20 rejected source fixtures because they test the lowerer's
  independent trust boundary.

- [ ] **Step 3: Run focused tests repeatedly**

  Run the focused command twice:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c \
    cabal test --project-dir=jazz-next \
      jazz-typed-core-expression-direct-call-spec \
      jazz-typed-core-contract-spec \
      jazz-lowered-ir-contract-spec \
      --jobs=1 --test-show-details=failures
  ```

  Expected both times: PASS with exact manifests, exact complete values, and
  no order-sensitive drift.

- [ ] **Step 4: Run the warning-clean development build**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c \
    cabal build --project-dir=jazz-next -fdevelopment all --jobs=1
  ```

  Expected: PASS with `-Wall -Werror`; no unused provisional fields, partial
  matches, incomplete record updates, or redundant constraints.

- [ ] **Step 5: Commit test closure or root-cause corrections**

  ```bash
  git add jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs \
    jazz-next/test/JazzNext/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs \
    jazz-next/src/JazzNext/Compiler/TypeInference.hs \
    jazz-next/src/JazzNext/Compiler/TypeInference/Diagnostics.hs \
    jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs \
    jazz-next/src/JazzNext/Compiler/TypeInference/Elaboration.hs \
    jazz-next/src/JazzNext/Compiler/LoweredIR/Lower.hs
  git commit -m "test: close typed-core direct-call profile"
  ```

  If no production correction was needed, stage only the two test files.

### Task 7: Run full verification and close the queue child

**Files:**

- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/done-archive.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md`
- Modify: `docs/superpowers/specs/2026-07-30-jazz-next-typed-core-expression-direct-call-design.md`
- Modify: `docs/superpowers/plans/2026-07-30-jazz-next-typed-core-expression-direct-call.md`
- Modify: `jazz-next/README.md`

**Interfaces:**

- Produces: verified documentation of the opt-in producer/lowerer boundary and
  archives this implementation child.
- Produces: closure/recursion as the next ordered design gate, without
  promoting it as implementation.

- [ ] **Step 1: Run the complete required matrix**

  Run exactly:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --jobs=1 --test-show-details=failures
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all --jobs=1
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --jobs=1 --test-show-details=failures
  nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: every command passes. Routine Cabal `all` remains serialized and
  does not opt into exhaustive parser-scale components.

- [ ] **Step 2: Review the implementation against the design**

  Confirm:

  - no second semantic traversal or sidecar typed annotation map exists;
  - inference-only entry points allocate no provisional trees/failures;
  - diagnostics and `InferenceResult` are unchanged;
  - statuses follow the fixed precedence;
  - permanent typed-core/lowered-IR constructors are unchanged;
  - accepted/rejected manifests are exactly 16/20;
  - identifiers and output order are deterministic;
  - direct tail-call terminators are absent;
  - all authored Jazz fixture functions use pattern-lambda bindings rather
    than function equations; and
  - `jazz-hs/` and `jazz2/` have no diff.

- [ ] **Step 3: Update status and archive the completed child**

  Set this plan to `status: done`, add `completed_on`, and update
  `last_verified`. Move its queue row to `done-archive.md` with concise
  behavior and verification evidence. Keep `queue.md` as a dispatcher.

  Update the bootstrap profile, blocker contract, design, and README to state
  that typed-core scalar/direct-call production and lowering are opt-in and
  complete while normal compile/run remains canonical-core/interpreter based.
  Name closure/recursion as the next design gate; do not promote closure,
  control flow, multi-module integration, LLVM, object/link, or native runtime
  without its own approved design and executor-ready child.

- [ ] **Step 4: Re-run coordination checks after closeout**

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: queue/archive/frontmatter parity and docs status checks pass.

- [ ] **Step 5: Commit closeout**

  ```bash
  git add docs/execution/blocker-contracts.md \
    docs/execution/done-archive.md \
    docs/execution/queue.md \
    docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md \
    docs/superpowers/specs/2026-07-30-jazz-next-typed-core-expression-direct-call-design.md \
    docs/superpowers/plans/2026-07-30-jazz-next-typed-core-expression-direct-call.md \
    jazz-next/README.md
  git commit -m "docs: close typed-core direct-call batch"
  ```

## Execution Handoff

Execute tasks in order. Task 1 owns the private inference plumbing; Tasks 2
and 3 extend its provisional result without changing the permanent typed-core
contract. Tasks 4 and 5 consume only validated permanent typed core and must
not import inference state. Task 6 closes behavioral evidence before Task 7
runs and records the full matrix.

If a permanent contract defect is discovered, stop with the exact invalid
fixture, validator result, and affected constructor. Do not widen target paths
or silently revise the mirrored contract.
