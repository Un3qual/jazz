---
id: JN-BOOTSTRAP-TYPED-CORE-DIRECT-RECURSION-001
status: complete
priority: P1
size: L
kind: impl
autonomous_ready: no
depends_on: []
plan_section: "Full closeout"
target_paths:
  - src/Jazz/Compiler/RecursiveBindings.hs
  - test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs
  - src/Jazz/Compiler/TypeInference/Elaboration.hs
  - src/Jazz/Compiler/TypedCore.hs
  - src/Jazz/Compiler/TypedCore/Validate.hs
  - jazz/compiler/TypedCoreTypes.jz
  - jazz/compiler/TypedCoreValidate.jz
  - src/Jazz/Compiler/Force.hs
  - src/Jazz/Compiler/LoweredIR/Lower.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs
  - test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  - test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs
  - test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs
  - test/Jazz/Compiler/ProfilingSpec.hs
  - benchmark/Jazz/Benchmark/StageInputs.hs
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop --command cabal test recursive-bindings-spec jazz-typed-core-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Transport ordered typed-core recursive groups, validate exact binder reachability in Haskell and hosted Jazz, and lower capture-free non-escaping self and mutual recursion through the existing direct-call representation."
last_verified: 2026-08-12
---

# Jazz Typed-Core Direct Recursion Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend the opt-in typed-core producer and backend-neutral lowerer with
capture-free, non-escaping self and mutual direct recursion.

**Architecture:** `Jazz.Compiler.RecursiveBindings` remains the only producer
of canonical recursive membership. Typed Core transports those ordered groups
as binder identities, Haskell and hosted-Jazz validators check the metadata
against binder-reference reachability, and the lowerer consumes the validated
groups without rebuilding an SCC graph. Accepted calls use the existing direct
function representation and Lowered IR instructions.

**Tech Stack:** Haskell, hosted Jazz, GHC 9.14.1, Cabal, backend-neutral Lowered
IR v1, Nix

## Global Constraints

- Implement only `JN-BOOTSTRAP-TYPED-CORE-DIRECT-RECURSION-001` from accepted
  RFC 0009.
- Keep production and lowering opt-in through
  `finalizeTypedCoreExpressionDirectCall` and
  `lowerTypedCoreExpressionDirectCall`; ordinary compile/run remains on
  canonical core and the reference interpreter.
- Accept only concrete monomorphic root function groups whose members are
  capture-free, direct-shaped, non-escaping leading lambdas.
- Resolve every self and peer reference by `TypedBinderId`; do not introduce a
  name-based lowering fallback or sidecar dependency map.
- Keep `Jazz.Compiler.RecursiveBindings` as the sole owner of canonical group
  discovery, declaration order, rebinding, outer-binding, and lexical-shadow
  semantics.
- Preserve canonical group order by earliest member and member order by source
  statement position; never derive output order from map or set iteration.
- Treat structurally valid closure-shaped groups as valid Typed Core but retain
  producer- and lowerer-profile rejection until the closure-recursion child.
- Retain structured rejection for recursive aliases, patterns, managed or
  unresolved values, capability evidence, imports, cross-module groups,
  closure-shaped members, captures, partial application, and escaping values.
- Add no Lowered IR constructor or version, closure environment, placeholder,
  mutable cell, adapter ABI, tail-call marker, runtime service, cache, or
  duplicate validator harness.
- Preserve failure precedence and exact statement/expression ordering; failed
  production, validation, or lowering returns no partial artifact.
- Run Cabal only inside the checked-in Nix development shell and use
  `--jobs=1`.
- Commit after each green milestone using the commit message named below.

---

### Task 1: Promote the executor-ready direct-recursion child

**Files:**

- Create: `.codex/plans/2026-08-12-jazz-typed-core-direct-recursion.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`
- Modify: `rfcs/accepted/0009-typed-core-closure-and-recursion.md`

**Interfaces:**

- Promote exactly one `P1`, size `L`, implementation row with this plan's
  frontmatter and RFC 0009's G4 gate.
- Add `src/Jazz/Compiler/Force.hs` and
  `test/Jazz/Compiler/ProfilingSpec.hs` to direct-recursion ownership because
  the `TypedModule` schema extension must update the strictness traversal and
  its regression fixture.
- Keep closure recursion unpromoted and ordinary compile/run unchanged.

- [x] **Step 1: Record the approved design.** Use
      `.codex/plans/2026-08-12-jazz-typed-core-direct-recursion-design.md` as
      the implementation contract alongside accepted RFC 0009.

- [x] **Step 2: Correct the live ownership matrix.** Add the two mechanical
      schema consumers above to the RFC row, blocker contract, queue row, and
      this plan without broadening semantic scope.

- [x] **Step 3: Promote the queue row.** Remove the candidate from `Next
Curation Target`, add the matching `Ready Now` row, and change the
      bootstrap blocker to execution of this validated plan.

- [x] **Step 4: Validate curation metadata.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: every command exits zero in the checked-in Nix environment where
  required tooling is provided.

- [x] **Step 5: Commit the curation milestone.** Run:

  ```bash
  git add .codex/plans/2026-08-12-jazz-typed-core-direct-recursion.md .codex/execution/queue.md .codex/execution/blocker-contracts.md rfcs/accepted/0009-typed-core-closure-and-recursion.md
  git commit -m "docs: ready typed-core direct recursion"
  ```

### Task 2: Add the recursive-group schema without semantic change

**Files:**

- Modify: `src/Jazz/Compiler/TypedCore.hs`
- Modify: `jazz/compiler/TypedCoreTypes.jz`
- Modify: `src/Jazz/Compiler/Force.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate.hs`
- Modify: `jazz/compiler/TypedCoreValidate.jz`
- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`
- Modify: `test/Jazz/Compiler/ProfilingSpec.hs`

**Interfaces:**

- Produce in Haskell:

  ```haskell
  newtype TypedRecursiveGroup = TypedRecursiveGroup [TypedBinderId]
    deriving (Eq, Ord, Show)

  data TypedModule
    = TypedModule
        [Text]
        TypedSourcePath
        [TypedResolvedImport]
        [TypedModuleExport]
        TypedModuleInterface
        [TypedRecursiveGroup]
        [TypedStatement]
        TypedNodeInfo
  ```

- Mirror `data TypedRecursiveGroup = TypedRecursiveGroup [TypedBinderId].` and
  the eight-field `TypedModule` constructor in hosted Jazz.
- Keep every existing fixture behavior unchanged by supplying `[]` groups until
  Task 3 introduces explicit contract fixtures.
- Extend the canonical Haskell-to-Jazz runtime encoder in place; do not create a
  second encoder.

- [x] **Step 1: Add the compile-time schema sentinel.** In
      `TypedCoreExpressionDirectCallFixtures.hs`, add an internal value that
      constructs:

  ```haskell
  TypedRecursiveGroup
    [TypedBinderId (modulePath, [1], resolvedName "loop")]
  ```

  Thread an explicit group list through a new helper with this exact type:

  ```haskell
  expectedFunctionProgramWithRecursiveGroups ::
    [[Text]] ->
    [Text] ->
    [ExpectedFunction] ->
    TypedExpr ->
    TypedProgram
  ```

  Keep `expectedFunctionProgram` as the `[]` group projection. Do not add a
  boolean or a parallel program builder.

- [x] **Step 2: Run the focused suites and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test profiling-spec jazz-typed-core-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: compilation fails because `TypedRecursiveGroup` and the new module
  field do not exist.

- [x] **Step 3: Add the Haskell and Jazz schema.** Add the constructor and
      module field exactly as specified above. Update all `TypedModule`
      construction and pattern matches in the listed files, using `[]` for
      unchanged programs and retaining the group list in transformations that
      rebuild a module.

- [x] **Step 4: Extend strictness and canonical encoding.** Add:

  ```haskell
  forceTypedRecursiveGroup :: Typed.TypedRecursiveGroup -> ()
  forceTypedRecursiveGroup (Typed.TypedRecursiveGroup members) =
    forceListWith forceTypedBinderId members
  ```

  Make `forceTypedModule` force groups before statements. In
  `CanonicalTypedCoreComparison.hs`, encode a group as
  `constructor "TypedRecursiveGroup" [listValue binderIdValue members]` and
  place the group list between the interface and statement fields.

- [x] **Step 5: Run the Step 2 command twice and verify GREEN.** Expected: all
      three suites pass twice with every pre-existing program carrying an empty
      group list and no changed validation result.

- [x] **Step 6: Commit the schema milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/TypedCore.hs jazz/compiler/TypedCoreTypes.jz src/Jazz/Compiler/Force.hs src/Jazz/Compiler/TypeInference/Elaboration.hs src/Jazz/Compiler/TypedCore/Validate.hs jazz/compiler/TypedCoreValidate.jz src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs test/Jazz/Compiler/ProfilingSpec.hs
  git commit -m "feat: add typed recursive group schema"
  ```

### Task 3: Validate declared groups against binder reachability

**Files:**

- Modify: `src/Jazz/Compiler/TypedCore.hs`
- Modify: `src/Jazz/Compiler/TypedCore/Validate.hs`
- Modify: `jazz/compiler/TypedCoreTypes.jz`
- Modify: `jazz/compiler/TypedCoreValidate.jz`
- Modify: `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`

**Interfaces:**

- Add exactly one validation kind in both schemas:

  ```haskell
  TypedRecursiveGroupMismatch
  ```

- Reuse `TypedUnknownBinder`, `TypedDuplicateBinder`, `TypedBinderDetail`, and
  `TypedIndexDetail` for precise existing categories.
- Add module-root helpers with these responsibilities:

  ```haskell
  rootRecursiveGroupFailures ::
    [Text] ->
    [TypedStatement] ->
    [TypedRecursiveGroup] ->
    [TypedCoreValidationFailure]

  rootRecursiveGroupsByStatement ::
    [TypedStatement] ->
    [TypedRecursiveGroup] ->
    Map Int [TypedStatement]

  rootCallableBinderDependencies ::
    Set TypedBinderId ->
    TypedExpr ->
    Set TypedBinderId
  ```

- Use declared metadata for module-root recursive visibility. Keep the existing
  recursive fact discovery only for nested block validation, which remains
  outside this lowerer profile.

- [x] **Step 1: Add literal valid and invalid contract programs.** Extend
      `JazzTypedCoreContractSpec.hs` with one valid direct self group, one valid
      direct mutual group, and these exact invalid cases:

  | Fixture                                      | Required failure                                                                             |
  | -------------------------------------------- | -------------------------------------------------------------------------------------------- |
  | `review-recursive-group-empty`               | module path, `TypedRecursiveGroupMismatch`, group index `0`                                  |
  | `review-recursive-group-unknown-member`      | module path, `TypedUnknownBinder`, unknown binder detail                                     |
  | `review-recursive-group-duplicate-member`    | repeated member's declaration statement path, `TypedDuplicateBinder`, repeated binder detail |
  | `review-recursive-group-multiple-membership` | repeated member's declaration statement path, `TypedDuplicateBinder`, repeated binder detail |
  | `review-recursive-group-member-order`        | module path, `TypedRecursiveGroupMismatch`, group index `0`                                  |
  | `review-recursive-group-order`               | module path, `TypedRecursiveGroupMismatch`, group index `1`                                  |
  | `review-recursive-group-missing-cycle`       | first cyclic member statement, `TypedRecursiveGroupMismatch`, member binder detail           |
  | `review-recursive-group-spurious-cycle`      | first declared member statement, `TypedRecursiveGroupMismatch`, member binder detail         |
  | `review-recursive-group-mixed-shapes`        | first declared member statement, `TypedRecursiveGroupMismatch`, member binder detail         |

  Run every valid and invalid program through Haskell validation twice and the
  existing hosted-Jazz runner twice. Store literal expected failure lists; do
  not derive expectations from either implementation.

- [x] **Step 2: Run the contract suite and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-contract-spec --test-show-details=failures --jobs=1
  ```

  Expected: empty/spurious/missing/order cases are accepted or fail with the
  wrong kind, and group-scoped forward peer references are invisible.

- [x] **Step 3: Implement root group facts in Haskell.** Build a source-indexed
      table of root callable `TypedLetStatement` binders and schemes. Validate
      group structure in group/member order, then traverse only binder
      references in each callable body to build the local dependency graph.
      Compare declared groups with cyclic SCCs; a singleton is cyclic only when
      its dependency set contains itself. Sort no emitted failures from a map.

- [x] **Step 4: Make module validation group-aware.** Pass
      `rootRecursiveGroupsByStatement` into the root statement-order traversal.
      For a group member, call `withBlockDeclarations` on the group's source-
      ordered declarations while validating that body. Preserve ordinary
      sequential visibility for every non-member and keep the current nested
      block recursion path separate.

- [x] **Step 5: Mirror the algorithm in hosted Jazz.** Reuse its existing list,
      membership, indexed-statement, and failure helpers. Compare exact binder
      identities and preserve input order; do not port the removed Haskell
      name-resolution SCC inference into a second new harness.

- [x] **Step 6: Run the Step 2 command twice and verify GREEN.** Expected: both
      validators accept the two valid groups and return identical literal
      failures, paths, details, and order for every malformed group on both
      repetitions.

- [x] **Step 7: Commit the validator milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/TypedCore.hs src/Jazz/Compiler/TypedCore/Validate.hs jazz/compiler/TypedCoreTypes.jz jazz/compiler/TypedCoreValidate.jz test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs
  git commit -m "feat: validate typed recursive groups"
  ```

### Task 4: Produce exact direct self and mutual groups

**Files:**

- Modify: `src/Jazz/Compiler/RecursiveBindings.hs`
- Modify: `test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`

**Interfaces:**

- Consume `provisionalCallableRecursiveGroupMembers :: Maybe [Int]` without
  rerunning recursive analysis.
- Export the exact producer manifest:

  ```haskell
  directRecursionExpectedPrograms :: [(Text, TypedProgram)]
  ```

  Back it with named `selfRecursiveExpectedProgram` and
  `mutuallyRecursiveExpectedProgram` constants so lowering reuses the same
  literal artifacts.

- Add a private producer helper:

  ```haskell
  orderedTypedRecursiveGroups ::
    [ProvisionalCallableDeclaration] ->
    [TypedRecursiveGroup]
  ```

- Deduplicate repeated per-member group evidence during a source-order fold;
  map every member statement index to its declaration binder, and retain the
  input member order exactly.
- Remove `TypedCoreRecursiveFunctionUnsupported` only for groups in which every
  member is a concrete monomorphic leading-lambda binding with
  `TypedDirectCallableShape` and no other owning-statement failure.

- [x] **Step 1: Lock canonical transport controls.** Extend
      `RecursiveBindingsSpec.hs` only if the existing self/mutual tests do not
      already prove that the exact resolved top-level sources produce `[self]`
      and `[left, right]` in declaration order. Add only the missing literal
      control; do not add another recursion algorithm.

- [x] **Step 2: Move the direct sources into the accepted manifest.** Move
      `self-recursive-function` and `mutually-recursive-functions` from
      `rejectedFixtures` to `acceptedFixtures`. Update literal manifest names
      and counts from `26/19` to `28/17`; keep the 36-row prior inventory and all
      other fixture order unchanged.

- [x] **Step 3: Add exact expected typed programs.** Add to the direct-recursion
      expected program table:

  ```haskell
  ( "self-recursive-function",
    expectedFunctionProgramWithLineOffsetAndRecursiveGroups
      1
      [["loop"]]
      []
      [ExpectedFunction "loop" [("item", intInfo)] intInfo TypedDirectCallableShape
        (directCall "loop" [intInfo] intInfo [variableExpr "item" intInfo])]
      (directCall "loop" [intInfo] intInfo [intExpr 1])
  )

  ( "mutually-recursive-functions",
    expectedFunctionProgramWithLineOffsetAndRecursiveGroups
      1
      [["left", "right"]]
      []
      [ ExpectedFunction "left" [("item", intInfo)] intInfo TypedDirectCallableShape
          (directCall "right" [intInfo] intInfo [variableExpr "item" intInfo]),
        ExpectedFunction "right" [("item", intInfo)] intInfo TypedDirectCallableShape
          (directCall "left" [intInfo] intInfo [variableExpr "item" intInfo])
      ]
      (directCall "left" [intInfo] intInfo [intExpr 1])
  )
  ```

  Both accepted sources are wrapped by `sourceFixtureNoExports`, so the exact
  expected programs use line offset `1` to preserve source spans and explicit
  empty-export semantics. The helper maps names to binding statement owners
  `[1]` and `[1,3]`; expected groups must contain those literal binders, not
  names.

- [x] **Step 4: Preserve the rejected boundary and failure order.** Keep
      closure-value self/mutual recursion, nested-lambda recursive escape,
      recursive capture, rebinding, aliases, conditional roots, patterns,
      imports, and combined descendant failures rejected. Update exact lists
      only where removing an earlier direct-recursion failure exposes an
      already-retained descendant failure.

- [x] **Step 5: Run G4 and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test recursive-bindings-spec jazz-typed-core-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: the two promoted source fixtures still return
  `TypedCoreRecursiveFunctionUnsupported` and no typed artifact.

- [x] **Step 6: Transport and admit all-direct groups.** Build the ordered group
      list once from declarations, attach it to `TypedModule`, and expose all
      group member schemes while finalizing each member body. Partition
      recursive binders into accepted all-direct members and still-unsupported
      members; preserve existing rebinding, shape, scheme, and descendant
      failures before constructing a program.

- [x] **Step 7: Run the Step 5 command twice and verify GREEN.** Expected: exact
      self/mutual programs and hosted-Jazz validation pass twice, while every
      closure-shaped or otherwise unsupported recursive fixture retains its
      documented rejection.

- [x] **Step 8: Commit the producer milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/RecursiveBindings.hs test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs src/Jazz/Compiler/TypeInference/Elaboration.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs
  git commit -m "feat: produce typed direct recursion"
  ```

### Task 5: Lower validated direct groups without rebuilding SCCs

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Modify: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`

**Interfaces:**

- Export the exact lowering manifest:

  ```haskell
  directRecursionExpectedLoweredPrograms ::
    [(Text, TypedProgram, LoweredProgram)]
  ```

  Reuse `selfRecursiveExpectedProgram` and
  `mutuallyRecursiveExpectedProgram`; do not reconstruct typed expectations in
  the lowering table.

- Index `TypedRecursiveGroup` membership alongside the existing
  `FunctionShape` maps before emitting any body.
- Replace `recursiveFunctionFailures` and its `stronglyConnComp` graph with a
  profile check over validated declared groups.
- Accept a group only when every member has an existing source-bound
  `FunctionShape` with `TypedDirectCallableShape`; if any member is closure-
  shaped, report `LoweredIRRecursiveFunctionUnsupported` for each member in
  source order.
- Reuse the existing exact-binder direct callee path and
  `LoweredCallSignature`; add no recursion-specific instruction.

- [x] **Step 1: Add exact expected Lowered IR.** Add
      `directRecursionExpectedLoweredPrograms` with:

  - `loop` taking `arg1`, emitting one direct call to `App::Main::loop` with
    `arg1`, and returning `t1`; entry directly calls `loop 1`.
  - `left` taking `arg1`, directly calling `App::Main::right`; `right` taking
    `arg1`, directly calling `App::Main::left`; entry directly calls `left 1`.

  Build the functions through the existing `expectedLocalFunction`,
  `expectedDirectCallInstruction`, `expectedCallableLoweredProgram`,
  `loweredParameter`, and `loweredTemporary` helpers. Use literal function
  order `[loop]` and `[left, right]`.

- [x] **Step 2: Update independent boundary manifests.** Move the valid direct
      self and mutual programs out of the lowerer rejection list and into the
      exact-success table. Add explicit recursive group metadata to all valid
      closure-shaped recursion boundary programs so typed-core validation still
      succeeds before the lowerer rejects them. Keep malformed mixed-shape and
      missing-group programs in the invalid typed-core manifest so they never
      reach lowering.

- [x] **Step 3: Run the expression suite and verify RED.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: self and mutual programs return the existing ordered
  `LoweredIRRecursiveFunctionUnsupported` failures instead of the literal
  Lowered Programs.

- [x] **Step 4: Consume validated group metadata.** Add the binder-indexed group
      table to `FunctionIndex`, implement the all-direct profile check, and
      remove `localFunctionDependencies`, the dependency return value from
      `validateStatementProfiles`, `recursiveFunctionFailures`, and the
      now-unused `Data.Graph` import. Do not keep the old graph as a fallback.

- [x] **Step 5: Run G4 twice and verify GREEN.** Run the Task 4 Step 5 command
      twice. Expected: both repetitions produce byte-for-byte equal direct
      recursive artifacts, exact lowerer boundary failures, and no regression
      in prior scalar, closure, capture, or currying fixtures.

- [x] **Step 6: Commit the lowering milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs
  git commit -m "feat: lower typed direct recursion"
  ```

### Task 6: Close the direct-recursion child and hand off closure recursion

**Files:**

- Modify: `.codex/plans/2026-08-12-jazz-typed-core-direct-recursion.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`
- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/project/status.md`
- Modify: `rfcs/accepted/0009-typed-core-closure-and-recursion.md`
- Modify: `scripts/check-docs.sh` to advance the pinned public implementation
  snapshot enforced by the documentation gate.
- Modify: `benchmark/Jazz/Benchmark/StageInputs.hs` only if the full suite
  exposes a recursive-group schema regression in its direct typed artifacts.

**Interfaces:**

- Mark this plan complete only after fresh G4, the full serialized compiler
  suite, docs, queue, and diff gates pass.
- Remove the completed row and add only
  `JN-BOOTSTRAP-TYPED-CORE-CLOSURE-RECURSION-001` as the next curation candidate
  using RFC 0009's exact G5 ownership and gate.
- Document capture-free direct self/mutual recursion as part of the opt-in
  profile; retain closure recursion and normal compile/run exclusions.

- [x] **Step 1: Run fresh G4 once after the final source edit.** Run the Task 4
      Step 5 command and read the complete exit status.

- [x] **Step 2: Run the full serialized compiler suite.** Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all --test-show-details=direct --jobs=1
  ```

  Expected: every registered suite passes without parallel timeout noise.

- [x] **Step 3: Update public compiler-boundary pages.** Add transported direct
      self/mutual recursive groups and direct recursive calls to the supported
      opt-in profile. Keep closure-shaped recursion and normal compile/run
      explicitly unchanged.

- [x] **Step 4: Synchronize plan, queue, blocker, and RFC state.** Check every
      task box, set frontmatter `status: complete` and `autonomous_ready: no`,
      use plan section `Full closeout`, remove the ready row, and place only the
      closure-recursion child in `Next Curation Target`.

- [x] **Step 5: Run closeout checks.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: every command exits zero; run the documentation check in the Nix
  development environment if host-only tools such as `lychee` are unavailable.

- [x] **Step 6: Perform the anti-slop review.** Enumerate every new type,
      helper, table, validator branch, failure kind, fixture builder, and
      fallback in the complete child diff. Remove anything without a concrete
      schema, ordered transport, invariant, direct-lowering, parity, strictness,
      or fixture responsibility. Confirm the lowerer contains no SCC rebuild or
      name-based recursion fallback.

- [x] **Step 7: Commit closeout.** Run:

  ```bash
  git add .codex/plans/2026-08-12-jazz-typed-core-direct-recursion.md .codex/execution/queue.md .codex/execution/blocker-contracts.md docs/compiler/bootstrapping.md docs/compiler/pipeline.md docs/project/status.md rfcs/accepted/0009-typed-core-closure-and-recursion.md
  git commit -m "docs: close typed-core direct recursion"
  ```

## Execution Handoff

Begin at Task 2. Execute Tasks 2-6 in order, use each task's named red/green
cycle and commit, and do not pre-promote or implement closure recursion.
