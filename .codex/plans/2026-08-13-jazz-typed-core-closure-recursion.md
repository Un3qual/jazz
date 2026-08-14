---
id: JN-BOOTSTRAP-TYPED-CORE-CLOSURE-RECURSION-001
status: complete
priority: P1
size: L
kind: impl
autonomous_ready: no
depends_on: []
plan_section: "Full closeout"
target_paths:
  - jazz.cabal
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
deliverable: "Produce and lower escaping or capturing recursive callable groups through one deterministic immutable shared external-capture environment while preserving structured rejection for later or interleaved captures."
last_verified: 2026-08-13
---

# Jazz Typed-Core Closure Recursion Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Complete RFC 0009 by admitting and lowering concrete monomorphic
escaping or capturing self and mutual recursion through one shared immutable
external-capture environment per recursive group.

**Architecture:** `Jazz.Compiler.RecursiveBindings` remains the sole owner of
ordered recursive membership. Typed-core production promotes every member of a
closure-shaped group together, rejects external captures that are not visible
before the first member, and retains the existing ordered `TypedRecursiveGroup`
binder contract. Lowering derives one group layout from validated member
shapes, constructs it once, reuses it for member closures, and reconstructs
self or peer closures inside member bodies without cyclic heap initialization.

**Tech Stack:** Haskell, hosted Jazz, GHC 9.14.1, Cabal, backend-neutral Lowered
IR v1, Nix

## Global Constraints

- Implement only `JN-BOOTSTRAP-TYPED-CORE-CLOSURE-RECURSION-001` from accepted
  RFC 0009.
- Keep production and lowering opt-in through
  `finalizeTypedCoreExpressionDirectCall` and
  `lowerTypedCoreExpressionDirectCall`; ordinary compile/run remains on
  canonical core and the reference interpreter.
- Accept only concrete monomorphic root function groups whose bodies fit the
  preceding closure profile.
- Resolve group members and external captures by `TypedBinderId`; introduce no
  textual-name fallback or second recursive analysis.
- If any member needs `TypedClosureCallableShape`, give every group member the
  closure shape and one shared environment containing only the ordered union of
  external captures.
- Order shared fields by group member source order and first canonical capture
  occurrence inside each member.
- Construct the shared environment once at the first member. Reject a member
  that captures a binder introduced or rebound at or after that first member.
- Construct each member closure at its own source statement. Inside a member
  body, reconstruct self and peer closures from the current shared environment.
- Preserve direct recursion unchanged for all-direct groups.
- Keep recursive aliases, patterns, non-closure managed data, capability
  evidence, imports, cross-module groups, and later/interleaved external
  captures as structured profile failures.
- Add no Lowered IR constructor or version, mutable cell, placeholder, cyclic
  product, runtime service, tail-call marker, bytecode, VM, or native ABI.
- Preserve RFC 0009 failure precedence and exact statement/expression order;
  failed production or lowering returns no partial artifact.
- Run heavy Cabal commands serially with `--jobs=1`.
- Commit after each green milestone using the commit messages below.

---

### Task 1: Promote the executor-ready closure-recursion child

**Files:**

- Create: `.codex/plans/2026-08-13-jazz-typed-core-closure-recursion.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`

**Interfaces:**

- Promote exactly one `P1`, size `L`, autonomous implementation row with RFC
  0009's ten required paths and G5 gate.
- Remove the matching curation candidate and point the bootstrap blocker at
  execution of this plan.
- Do not alter accepted RFC semantics or pre-seed a post-RFC child.

- [x] **Step 1: Validate live ownership.** Confirm each required and
      conditional G5 path still exists and no current schema change requires a
      typed-core mirror owner.

- [x] **Step 2: Promote the queue row.** Add this plan to `Ready Now`, empty
      `Next Curation Target`, and update the current executor and blocker text.

- [x] **Step 3: Validate metadata.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: all three commands exit zero.

- [x] **Step 4: Commit the curation milestone.** Run:

  ```bash
  git add .codex/plans/2026-08-13-jazz-typed-core-closure-recursion.md .codex/execution/queue.md .codex/execution/blocker-contracts.md
  git commit -m "docs: ready typed-core closure recursion"
  ```

### Task 2: Admit complete closure-shaped recursive groups

**Files:**

- Modify: `src/Jazz/Compiler/RecursiveBindings.hs` only if an ordered
  definition-time visibility fact is missing from the canonical owner
- Test: `test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs` only with that
  owner change
- Modify: `src/Jazz/Compiler/TypeInference/Elaboration.hs`
- Test: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Test: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Test: `test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs`
- Test: `test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs`

**Interfaces:**

- Replace the direct-only recursive admission partition with a complete group
  classifier that returns accepted all-direct members, accepted all-closure
  members, and rejected members while retaining one transported group list.
- Promote a whole group to `TypedClosureCallableShape` when any member is used
  as a value, captures a supported prior scalar/closure binder, is partially
  applied, or is otherwise closure-shaped by the preceding profile.
- The production tests move `closure-value-self-recursion` and
  `closure-value-mutual-recursion` into the accepted manifest and add literal
  accepted prior-capture self/mutual sources plus literal rejected later and
  interleaved capture sources.

- [x] **Step 1: Write producer RED fixtures.** Add accepted fixtures equivalent
      to:

  ```jazz
  seed = 1.
  loop :: Int -> Int.
  loop = \(item) -> loop (item + seed).
  loop 1.
  ```

  and a mutual pair whose members both use the same prior `seed`. Add a later
  capture control where the captured binder is declared after the first member
  and an interleaved control where it is rebound between members.

- [x] **Step 2: Add literal typed expectations.** Assert exact member binder
      order, closure callable shapes, binder-resolved self/peer references,
      existing nested unary closure recipes, and source-order failure paths.

- [x] **Step 3: Run G5 and verify RED.** Run:

  ```bash
  cabal test recursive-bindings-spec jazz-typed-core-contract-spec jazz-lowered-ir-contract-spec jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: promoted closure-recursive fixtures still report
  `TypedCoreRecursiveFunctionUnsupported`; new visibility controls fail their
  expected artifact/failure assertions.

- [x] **Step 4: Implement group-wide shape and visibility admission.** Reuse
      transported group membership and declaration-order scalar binder state.
      Admit a complete closure group only when every required external binder
      is visible before the group's first statement. Retain existing owning
      failures and descendant ordering.

- [x] **Step 5: Run G5 twice and verify GREEN.** Expected: repeated typed
      artifacts are equal; Haskell and hosted-Jazz validation agree; all prior
      direct-recursion fixtures remain unchanged.

- [x] **Step 6: Commit the producer milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/RecursiveBindings.hs test/Jazz/Compiler/Semantics/RecursiveBindingsSpec.hs src/Jazz/Compiler/TypeInference/Elaboration.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/JazzTypedCoreContractSpec.hs test/Jazz/Compiler/Bootstrap/CanonicalTypedCoreComparison.hs
  git commit -m "feat: produce typed closure recursion"
  ```

### Task 3: Lower one immutable recursive environment per group

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR/Lower.hs`
- Test: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs`
- Test: `test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs`
- Test: `test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs`
- Test: `test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs`

**Interfaces:**

- Add one internal recursive-group lowering shape indexed by member binder:

  ```haskell
  data RecursiveClosureGroupShape = RecursiveClosureGroupShape
    { recursiveClosureGroupMembers :: [TypedBinderId],
      recursiveClosureGroupLayout :: LoweredLayoutId,
      recursiveClosureGroupCaptures :: [CaptureShape],
      recursiveClosureGroupFirstStatement :: Int
    }
  ```

- Generate the shared layout with RFC 0009's `recursive-env` identity grammar
  from the first member binder path and literal name `group`.
- Replace each member's ordinary per-function capture layout with the shared
  layout. The layout fields are the ordered union of external captures and
  exclude every group member.
- During entry lowering, construct the environment once at the first member,
  retain its operand in lowering state, and construct member closures at their
  source statements from that operand.
- During member-body lowering, project shared external captures and reconstruct
  referenced self/peer closures from the environment parameter.

- [x] **Step 1: Write exact lowered RED fixtures.** Assert one shared layout,
      one environment construction, source-order member closure construction,
      self/peer reconstruction inside bodies, ordered capture projections, and
      unchanged direct recursive artifacts.

- [x] **Step 2: Run the expression suite and verify RED.** Run:

  ```bash
  cabal test jazz-typed-core-expression-direct-call-spec --test-show-details=failures --jobs=1
  ```

  Expected: valid closure-shaped groups report the existing ordered
  `LoweredIRRecursiveFunctionUnsupported` failures.

- [x] **Step 3: Build deterministic group shapes.** Fold validated transported
      groups in source order, compute member-source-order capture unions, and
      reject invalid or unavailable captures before instruction emission.

- [x] **Step 4: Reuse the shared environment.** Extend lowering state with the
      constructed group environment operand, specialize closure construction
      for group members, and reconstruct member closures from the current
      environment parameter inside a group body.

- [x] **Step 5: Preserve validator parity.** Add only fixture pressure needed
      to prove existing Lowered IR constructors validate identical shared
      layout/product/closure artifacts in Haskell and Jazz; do not change the
      schema or version.

- [x] **Step 6: Run G5 twice and verify GREEN.** Expected: byte-for-byte stable
      Lowered Programs and no prior closure, capture, currying, or direct
      recursion regression.

- [x] **Step 7: Commit the lowering milestone.** Run:

  ```bash
  git add src/Jazz/Compiler/LoweredIR/Lower.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallSpec.hs test/Jazz/Compiler/Bootstrap/TypedCoreExpressionDirectCallFixtures.hs test/Jazz/Compiler/Bootstrap/JazzLoweredIRContractSpec.hs test/Jazz/Compiler/Bootstrap/CanonicalLoweredIRComparison.hs
  git commit -m "feat: lower typed closure recursion"
  ```

### Task 4: Complete the RFC 0009 boundary and close the queue child

**Files:**

- Modify: `.codex/plans/2026-08-13-jazz-typed-core-closure-recursion.md`
- Modify: `.codex/execution/queue.md`
- Modify: `.codex/execution/blocker-contracts.md`
- Modify: `docs/compiler/bootstrapping.md`
- Modify: `docs/compiler/pipeline.md`
- Modify: `docs/project/status.md`
- Modify: `rfcs/accepted/0009-typed-core-closure-and-recursion.md`
- Modify: `scripts/check-docs.sh` only to advance a pinned implementation
  snapshot required by the documentation gate

**Interfaces:**

- Mark the child complete only after fresh G5, full compiler tests, docs,
  queue, and diff gates pass.
- Empty `Ready Now` and `Next Curation Target`; state explicitly that RFC 0009
  is complete and no later child is source-backed yet.
- Document shared immutable closure recursion only in the opt-in profile and
  retain normal compile/run, tail-call, native, and multi-module exclusions.

- [x] **Step 1: Run fresh G5.** Run the Task 2 Step 3 command and read the full
      exit status.

- [x] **Step 2: Run the full serialized compiler suite.** Run:

  ```bash
  cabal test all --test-show-details=direct --jobs=1
  ```

  Expected: every registered suite passes.

- [x] **Step 3: Update public compiler boundaries.** Describe escaping and
      capturing self/mutual recursion through one immutable shared environment;
      keep ordinary compile/run unchanged.

- [x] **Step 4: Synchronize durable state.** Check all plan boxes, mark the
      frontmatter complete and non-autonomous with the full-closeout section,
      remove the ready row, and record no named successor.

- [x] **Step 5: Run closeout checks.** Run:

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

- [x] **Step 6: Perform an anti-slop review.** Enumerate each new helper, map,
      state field, failure branch, and fixture. Remove anything without a
      concrete group-classification, shared-layout, capture-order, environment
      reuse, peer-reconstruction, validation, or fixture responsibility.

- [x] **Step 7: Commit closeout.** Run:

  ```bash
  git add .codex/plans/2026-08-13-jazz-typed-core-closure-recursion.md .codex/execution/queue.md .codex/execution/blocker-contracts.md docs/compiler/bootstrapping.md docs/compiler/pipeline.md docs/project/status.md rfcs/accepted/0009-typed-core-closure-and-recursion.md scripts/check-docs.sh
  git commit -m "docs: close typed-core closure recursion"
  ```

## Execution Handoff

Execute Tasks 2-4 in order with each named red/green cycle. Do not add a
post-RFC queue child without a separate source-backed design contract.
