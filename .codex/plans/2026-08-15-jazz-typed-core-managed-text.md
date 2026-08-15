---
id: JN-BOOTSTRAP-TYPED-CORE-MANAGED-TEXT-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on: []
plan_section: "Task 2"
target_paths:
  - src/Jazz/Compiler/TypeInference/Elaboration.hs
  - src/Jazz/Compiler/LoweredIR/Lower.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  - jazz.cabal
  - docs/compiler/bootstrapping.md
  - docs/compiler/pipeline.md
  - docs/project/status.md
  - rfcs/README.md
  - rfcs/accepted/0014-typed-core-managed-text.md
  - .codex/execution/queue.md
  - .codex/execution/blocker-contracts.md
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  - nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  - bash scripts/check-execution-queue.sh
  - git diff --check
deliverable: "Produce and lower managed Text construction and transport plus strict equality, length, append, and append-char through exact backend-neutral runtime-service dependencies without changing ordinary compile/run."
last_verified: 2026-08-15
---

# Jazz Typed-Core Managed Text Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add one coherent managed-value vertical slice to the opt-in Haskell
Typed Core and backend-neutral Lowered IR path: Text construction and
transport, strict equality, length, append, and append-char.

**Architecture:** Inference remains the semantic owner and finalization reuses
the existing Typed Core Text nodes and managed recipe. Lowering maps every Text
recipe to one catalog-owned managed-reference layout, structurally collects
only required runtime services, and emits exact service calls without exposing
a host callback or native ABI. Ordinary compile/run remains on canonical core
and the reference interpreter.

**Tech Stack:** Haskell, Typed Core, Lowered IR v1, GHC 9.14.1, Cabal, Nix

## Global constraints

- Implement only RFC 0014 and
  `JN-BOOTSTRAP-TYPED-CORE-MANAGED-TEXT-001`.
- Preserve the existing Typed Core and Lowered IR schemas, versions, hosted
  Jazz validators, diagnostic ordering, binder identities, CFG identities,
  and generated-name grammar.
- Support Text only as a value: literal, binding, parameter, result, capture,
  argument, join operand, return, and tail-call operand.
- Keep pattern scrutinees inside the immediate-scalar profile. Do not add Text
  literal patterns, managed wildcard or variable scrutinees, or managed pattern
  lowering.
- Support only strict Text `==`, Text `!=`, and exactly saturated canonical
  kernel identities for `textLength`, `textAppend`, and `textAppendChar`.
- Resolve approved builtins through `BuiltinCatalog`. Do not identify them by
  raw source spelling after resolution.
- A bare approved builtin fails with
  `TypedCoreCallableValueUnsupported`; a partial approved call fails with
  `TypedCoreCallArityUnsupported`; an oversaturated call retains ordinary
  source-diagnostic precedence.
- Emit `LoweredLayoutId "jazz.layout.text.v1"` exactly once whenever any
  validated recipe requires Text, including nested closure recipes.
- Emit only referenced services, deduplicated in catalog order: equality,
  length, append, append-char. Text-only construction declares no services.
- Text runtime services are pure backend dependencies. Do not add
  `RuntimeHost`, Haskell callbacks, public symbols, native symbols, allocation
  policy, ownership policy, or target layout.
- Keep list, tuple, ADT, uncons, from-chars, concat, imports, Text I/O,
  multi-module execution, and normal compile/run cutover out of scope.
- Run Cabal only inside the checked-in Nix development shell with `--jobs=1`.
- Follow red-green-refactor: each production change starts with a focused exact
  expectation that fails for the missing behavior.
- Commit each green milestone with the commit message named below.

---

### Task 1: Promote the accepted managed-Text child

**Files:**

- Create: `.codex/plans/2026-08-15-jazz-typed-core-managed-text.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`
- Modify: `.codex/plans/2026-08-15-jazz-typed-core-managed-text-design.md`
- Modify: `rfcs/accepted/0014-typed-core-managed-text.md`

**Interfaces:**

- Source contracts:
  `.codex/plans/2026-08-15-jazz-typed-core-managed-text-design.md` and
  `rfcs/accepted/0014-typed-core-managed-text.md`.
- Promote one `P1`, size `L`, autonomous implementation row.
- Keep `Next Curation Target` empty while this row is executable.

- [x] **Step 1: Record final failure precedence.** State explicitly that bare
      and partial approved builtin uses fail at the producer boundary while
      oversaturation preserves ordinary source diagnostics.

- [x] **Step 2: Add this ready implementation plan.** Keep ordered frontmatter
      fields identical to the queue row. The future catalog creation is named
      in Task 3 rather than ready-row `target_paths`, because dispatcher
      validation requires every listed target to exist before execution.

- [x] **Step 3: Promote the queue row and bootstrap blocker contract.** Point
      the umbrella at RFC 0014 and name this child as the current smallest
      unblocker.

- [x] **Step 4: Validate curation metadata.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  git diff --check
  ```

  Expected: both commands exit zero and the queue row matches this plan.

- [x] **Step 5: Commit the curation milestone.** Run:

  ```bash
  git add .codex/plans/2026-08-15-jazz-typed-core-managed-text.md .codex/plans/2026-08-15-jazz-typed-core-managed-text-design.md .codex/execution/queue.md .codex/execution/blocker-contracts.md rfcs/accepted/0014-typed-core-managed-text.md
  git commit -m "docs: ready typed-core managed text"
  ```

### Task 2: Produce and validate managed Text transport

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`

**Interfaces:**

- Reuse these exact existing semantic nodes:

  ```haskell
  textInfo :: TypedNodeInfo
  textInfo =
    TypedNodeInfo
      { typedNodeType = TypedTextType
      , typedNodeRepresentation = TypedManagedTextRecipe
      , typedNodeConstraints = []
      , typedNodeEvidence = []
      }

  textExpr :: Text -> TypedExpr
  textExpr value = TypedLiteralExpr textInfo (TypedTextLiteral value)
  ```

- `scalarInfo TTextType` returns `textInfo`; the function name remains internal
  even though it now admits the one approved managed value.
- `typedLiteral (LText value, TypedTextType)` returns
  `TypedLiteralExpr textInfo (TypedTextLiteral value)`.
- Existing `valueInfo`, `valueTypeAndRecipe`, binder references, callable
  recipes, and closure-capture analysis transport the managed recipe unchanged.

- [x] **Step 1: Add exact producer fixtures.** Export
      `managedTextProducerFixtures` and `managedTextExpectedPrograms`. Cover:

  ```jazz
  "managed".
  ```

  ```jazz
  message = "managed".
  message.
  ```

  ```jazz
  identity :: Text -> Text.
  identity = \(item) -> item.
  identity "Jazz".
  ```

  ```jazz
  message = "managed".
  capture :: Bool -> Text.
  capture = \(ignored) -> message.
  capture True.
  ```

  Add a Bool-selected conditional returning Text and a Bool-scrutinee scalar
  case whose arms return Text. Expectations must spell the complete
  `TypedProgram`, binder references, callable schemes, and nested managed
  recipes; do not compare summaries.

- [x] **Step 2: Reclassify existing manifests precisely.** Change
      `managed-scalar-binding` from structured rejection to the exact accepted
      binding program. Keep primary fixture `text-value` rejected for its list
      expression, but remove the obsolete Text failure from its expected
      status. Keep `unsupported-managed-capture` rejected until its equality
      operation lands in Task 4; the new transport-only capture fixture proves
      capture independently.

- [x] **Step 3: Run the focused producer suite and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=direct --jobs=1
  ```

  Expected: the new exact fixtures receive
  `TypedCoreManagedValueUnsupported` or differ from their expected Text nodes.

- [x] **Step 4: Implement Text finalization only.** Add the two exact cases to
      `scalarInfo` and `typedLiteral`. Do not add builtin recognition or
      lowerer behavior in this task.

- [x] **Step 5: Run producer and contract verification.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec --test-show-details=direct --jobs=1
  ```

  Expected: all exact producer fixtures pass, malformed Typed Core remains
  rejected, and all earlier scalar/closure/currying/recursion/CFG fixtures stay
  unchanged.

- [x] **Step 6: Commit the producer milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/TypeInference/Elaboration.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: produce managed text values"
  ```

### Task 3: Lower Text layout, literals, and transport

**Files:**

- Create: `src/Jazz/Compiler/LoweredIR/RuntimeServiceCatalog.hs`
- Modify: `jazz.cabal`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`

**Interfaces:**

- The new catalog owns these exact values and ordering:

  ```haskell
  data RuntimeServiceKey
    = TextEqualService
    | TextLengthService
    | TextAppendService
    | TextAppendCharService
    deriving (Eq, Ord, Show)

  textLayoutId :: LoweredLayoutId
  textLayout :: LoweredLayout
  textRepresentation :: LoweredRepresentation
  runtimeServiceContract :: RuntimeServiceKey -> LoweredRuntimeService
  orderedRuntimeServices :: Set RuntimeServiceKey -> [LoweredRuntimeService]
  ```

  `textLayoutId` is `LoweredLayoutId "jazz.layout.text.v1"`;
  `textLayout` uses `LoweredTextLayout`; `textRepresentation` is
  `LoweredManagedReferenceRepresentation textLayoutId`.

- Add a validated-tree dependency summary in `Lower.hs`:

  ```haskell
  data RuntimeRequirements = RuntimeRequirements
    { runtimeRequiresTextLayout :: Bool
    , runtimeRequiredServices :: Set RuntimeServiceKey
    }

  collectRuntimeRequirements :: TypedModule -> RuntimeRequirements
  ```

- Requirement collection recursively visits node information, schemes,
  parameters, results, closure recipes, and expressions. In this task its
  service set remains empty.
- `requiredLayouts` emits the Text layout before deterministic closure layouts.
- `loweredRepresentation TypedManagedTextRecipe = Just textRepresentation`.
- General value contracts accept Text via `valueRepresentation`; the existing
  immediate-scalar `scalarRepresentation` remains the pattern-scrutinee gate.
- Text literals emit one `LoweredConstructText` instruction into a new
  block-local temporary.

- [ ] **Step 1: Add exact Lowered IR expectations.** Export
      `managedTextExpectedLoweredPrograms`. Cover literal construction, binding
      evaluated once and reused, direct parameter/result transport, closure
      capture environment fields, conditional and scalar-case value joins,
      and Text through direct and closure tail calls. Every expected program
      contains one Text layout and an empty runtime-service list.

- [ ] **Step 2: Add a lowerer-only negative artifact.** Hand-construct a valid
      Typed Core program that attempts a Text pattern scrutinee and assert that
      lowering fails with the existing managed/scrutinee profile failure and
      returns no partial Lowered IR program.

- [ ] **Step 3: Run the focused suite and verify RED.** Run the Task 2 focused
      suite. Expected failures name unsupported managed representation or Text
      literal lowering, and no new expected program validates yet.

- [ ] **Step 4: Create the runtime-service catalog foundation.** Add the module
      to `jazz.cabal` exposed modules. Implement all four exact service
      signatures even though this task emits none:

  ```haskell
  TextEqualService:
    (Text, Text) -> Bool
  TextLengthService:
    (Text) -> Int64
  TextAppendService:
    (Text, Text) -> Text
  TextAppendCharService:
    (Text, Char) -> Text
  ```

  Use the exact semantic IDs from RFC 0014 and return contracts in constructor
  order, not `Set` order.

- [ ] **Step 5: Implement requirement collection and Text construction.** Wire
      the Text representation into general values, module-scope binding
      contracts, parameters, results, capture layouts, edge operands, returns,
      and tail calls. Preserve `scalarRepresentation` for pattern checks.

- [ ] **Step 6: Run focused producer/lowerer/contracts verification.** Run the
      first frontmatter verification command. Expected: all exact transport
      programs validate; Text-only programs declare no service; managed
      scrutinees still fail closed.

- [ ] **Step 7: Commit the lowering milestone.** Run:

  ```bash
  git add jazz.cabal src/Jazz/Compiler/LoweredIR/RuntimeServiceCatalog.hs src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: lower managed text transport"
  ```

### Task 4: Produce exact Text operations

**Files:**

- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`

**Interfaces:**

- Resolve only compiler-owned kernel identities:

  ```haskell
  approvedTextRuntimeServiceBuiltin :: Name -> Maybe BuiltinSymbol
  approvedTextRuntimeServiceBuiltin name =
    case lookupBuiltinSymbolInMode ResolveKernelOnly (identifierText name) of
      Just symbol@BuiltinTextLength -> Just symbol
      Just symbol@BuiltinTextAppend -> Just symbol
      Just symbol@BuiltinTextAppendChar -> Just symbol
      _ -> Nothing
  ```

- The canonical callee is a `TypedVariableExpr` whose reference is
  `TypedBuiltinName (builtinSymbolName symbol)` and whose binder reference is
  absent.
- Use `builtinSymbolArity`; do not duplicate arity in elaboration.
- Exactly saturated calls use existing staged `TypedApplyExpr` nodes and the
  resolved callable type/recipe. No other builtin or non-local call is added to
  the profile.
- Existing `TypedBinaryExpr` production handles Text `==` and `!=` after both
  operands finalize with Text information; result information remains Bool.

- [ ] **Step 1: Add exact operation producer fixtures.** Cover:

  ```jazz
  "left" == "right".
  "left" != "right".
  __kernel_textLength "Jazz".
  __kernel_textAppend "Jazz" "!".
  __kernel_textAppendChar "Jazz" '!'.
  ```

  Add a combined fixture whose source order is equality, length, append, then
  append-char. Expected Typed Core must use canonical `TypedBuiltinName`
  identities and exact staged recipes.

- [ ] **Step 2: Add producer-boundary negatives.** Cover a bare approved
      builtin, a one-argument partial append, a one-argument partial
      append-char, and an oversaturated length call. Assert exact structured
      profile failures for bare/partial uses and ordinary diagnostic precedence
      for oversaturation. Keep kernel uncons, from-chars, and concat rejected
      by their existing boundaries.

- [ ] **Step 3: Run the focused suite and verify RED.** Expected: Text equality
      can now type but approved kernel calls still receive non-local call
      failures; exact operation expectations do not match.

- [ ] **Step 4: Implement canonical approved-builtin finalization.** Import
      `BuiltinSymbol(..)`, `BuiltinResolutionMode(ResolveKernelOnly)`,
      `builtinSymbolArity`, `builtinSymbolName`, and
      `lookupBuiltinSymbolInMode`. Branch before ordinary local-call handling,
      check exact arity, build the canonical callee, and reuse the existing
      staged application finalizer.

- [ ] **Step 5: Implement bare/partial profile failures without changing
      oversaturation.** A bare canonical approved builtin returns
      `TypedCoreCallableValueUnsupported`; an undersaturated application reports
      `TypedCoreCallArityUnsupported` with catalog expected/actual arity. Do not
      intercept a source expression that inference already rejected.

- [ ] **Step 6: Run focused producer and Typed Core contract suites.** Expected:
      exact positive nodes pass, all negative precedence assertions pass, and
      unrelated builtins remain rejected.

- [ ] **Step 7: Commit the operation-producer milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/TypeInference/Elaboration.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: produce managed text operations"
  ```

### Task 5: Lower Text service dependencies and calls

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/RuntimeServiceCatalog.hs`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`

**Interfaces:**

- Runtime requirements map semantic nodes only:

  ```haskell
  textOperationService :: BuiltinSymbol -> Maybe RuntimeServiceKey
  textOperationService BuiltinTextLength = Just TextLengthService
  textOperationService BuiltinTextAppend = Just TextAppendService
  textOperationService BuiltinTextAppendChar = Just TextAppendCharService
  textOperationService _ = Nothing
  ```

- A Text-typed `TypedBinaryExpr` with `==` or `!=` requires
  `TextEqualService`.
- `runtimeServiceContract` returns these exact signatures:

  ```text
  jazz.runtime.text.equal.v1       (Text, Text) -> Bool
  jazz.runtime.text.length.v1      (Text) -> Int64
  jazz.runtime.text.append.v1      (Text, Text) -> Text
  jazz.runtime.text.append-char.v1 (Text, Char) -> Text
  ```

- Text equality lowers both operands once, left to right, with existing carried
  operands. Emit `LoweredRuntimeCall` for equality; emit existing Boolean-not
  immediately afterward for `!=`.
- Approved applications are recognized from canonical `TypedBuiltinName` at
  the root of the exactly saturated staged application. Arguments lower once
  in source order and one `LoweredRuntimeCall` produces the final
  representation.
- Runtime calls remain instructions followed by `LoweredReturn`; they are not
  Lowered IR v1 tail terminators.

- [ ] **Step 1: Add exact service-lowering expectations.** For each operation,
      assert exact layout, exact referenced service list, argument order,
      instruction order, result representation, and return. Assert inequality
      uses equality plus Boolean-not and does not declare a second service.

- [ ] **Step 2: Add dependency-set expectations.** Prove literal-only transport
      has no services, duplicate equality uses declare equality once, and a
      combined program declares services in equality/length/append/append-char
      catalog order regardless of tree or `Set` ordering.

- [ ] **Step 3: Add nested CFG evaluation-order expectations.** Put Text calls
      around conditional or case operands and assert each argument is evaluated
      exactly once, left to right, with complete carried-operand edges.

- [ ] **Step 4: Run the focused suite and verify RED.** Expected: operation
      programs fail at unsupported binary/application lowering or contain no
      required services.

- [ ] **Step 5: Extend structural requirement collection.** Recognize only
      validated Text equality and canonical approved builtin spines. Deduplicate
      keys and materialize contracts with `orderedRuntimeServices`.

- [ ] **Step 6: Lower equality, inequality, and exact kernel spines.** Reuse the
      existing argument and carried-operand folds. Check each emitted operand
      and result against the catalog signature before producing an instruction;
      a mismatch returns the existing structured lowerer failure and no
      program.

- [ ] **Step 7: Run focused producer/lowerer/contracts verification.** Run the
      first frontmatter verification command. Expected: exact programs and both
      validator parity suites pass.

- [ ] **Step 8: Commit the runtime-service milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/LoweredIR/RuntimeServiceCatalog.hs src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "feat: lower managed text services"
  ```

### Task 6: Lock profile boundaries and close the child

**Files:**

- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/project/status.md`
- Modify: `rfcs/README.md`
- Modify: `rfcs/accepted/0014-typed-core-managed-text.md`
- Modify: `.codex/plans/2026-08-15-jazz-typed-core-managed-text.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Negative coverage remains exact for lists, tuples, ADTs, managed pattern
  scrutinees, Text literal patterns, uncons, from-chars, concat, imports,
  multi-module execution, Text I/O, and `RuntimeHost` operations.
- Public docs state that this is an opt-in backend profile, not a normal
  compile/run cutover and not a change to the already-shipped public Text
  semantics.
- RFC 0014 records implementation evidence. Queue closure removes the ready row
  and returns the bootstrap umbrella to a terminal-empty state without
  inventing a managed-collections successor.

- [ ] **Step 1: Complete negative exact fixtures.** Verify every exclusion
      above returns its existing ordered diagnostics/profile failures and no
      partial Typed Core or Lowered IR artifact. Include a hand-built malformed
      service signature/reference case for Lowered IR validation ownership.

- [ ] **Step 2: Run focused verification.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec --test-show-details=direct --jobs=1
  ```

  Expected: all positive, negative, and schema/validator parity cases pass.

- [ ] **Step 3: Commit the boundary tests.** Run:

  ```bash
  git add test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  git commit -m "test: lock managed text profile boundaries"
  ```

- [ ] **Step 4: Update implementation documentation and durable state.** Record
      the producer/lowerer boundary, exact layout/services, exclusions, tests,
      and ordinary compile/run non-cutover. Mark this plan complete, remove its
      ready row, and update the bootstrap blocker with completion evidence and
      no automatic successor.

- [ ] **Step 5: Run full repository verification.** Run, in order:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  nix --extra-experimental-features 'nix-command flakes' develop --command bash scripts/check-docs.sh
  bash scripts/check-execution-queue.sh
  git diff --check
  ```

  Expected: every command exits zero. Review the complete diff to confirm no
  hosted Jazz, schema, interpreter, public Text, native ABI, or unrelated queue
  change entered the batch.

- [ ] **Step 6: Commit the closeout milestone.** Run:

  ```bash
  git add docs/compiler/bootstrapping.md docs/compiler/pipeline.md docs/project/status.md rfcs/README.md rfcs/accepted/0014-typed-core-managed-text.md .codex/plans/2026-08-15-jazz-typed-core-managed-text.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "docs: close managed text lowering"
  ```

## Final review checklist

- [ ] Every RFC 0014 positive behavior has an exact Typed Core and exact
      Lowered IR expectation.
- [ ] Every explicit exclusion has a negative fixture at its owning boundary.
- [ ] Text construction/transport declares one layout and zero services.
- [ ] Equality, length, append, and append-char declare only referenced
      services in catalog order.
- [ ] Pattern scrutinees still use immediate-scalar validation.
- [ ] Bare, partial, and oversaturated approved builtin behavior matches the
      accepted failure precedence.
- [ ] No new schema node, version, hosted Jazz change, host callback, native
      symbol, or normal compile/run path exists.
- [ ] All frontmatter verification commands pass from a clean worktree.
