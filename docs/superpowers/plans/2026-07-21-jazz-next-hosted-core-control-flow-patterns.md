---
id: JN-BOOTSTRAP-JAZZ-CORE-CONTROL-FLOW-PATTERNS-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-JAZZ-CORE-EXPRESSION-FOUNDATION-001
last_verified: 2026-07-21
plan_section: "Implementation Batch: Hosted Core Control Flow and Patterns"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-21-jazz-next-hosted-canonical-core-design.md
  - docs/superpowers/specs/2026-07-21-jazz-next-hosted-core-control-flow-patterns-design.md
  - docs/superpowers/plans/2026-07-21-jazz-next-hosted-core-control-flow-patterns.md
  - jazz-next/README.md
  - jazz-next/jazz/compiler/CoreLower.jz
  - jazz-next/jazz/compiler/CoreTypes.jz
  - jazz-next/jazz-next.cabal
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreControlFlowPatternsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreParity.hs
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-core-control-flow-patterns-spec jazz-core-expression-foundation-spec canonical-core-comparison-spec canonical-parser-comparison-spec jazz-parser-control-flow-patterns-spec repository-audit-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Extend the internal Jazz-authored canonical-core lowerer through every pattern, guarded case, conditional, nested control-flow, and multi-parameter or pattern-lambda rule with exact one-based generated names, repeated direct and hosted-parser-composed stage-0 parity, and all-or-nothing rejection of later-child forms."
---

# Jazz-Next Hosted Core Control Flow and Patterns Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox syntax for tracking. This plan intentionally specifies interfaces,
> observable behavior, tests, commands, and commit boundaries without embedding
> implementation bodies.

**Goal:** Extend the internal hosted core lowerer through patterns, cases,
conditionals, and lambdas while preserving the child-1 foundation boundary and
all later-child deferrals.

**Architecture:** `CoreLower.jz` gains one private profile-driven recursive
kernel. The existing foundation wrapper keeps its exact behavior; a new
control-flow wrapper enables child-2 forms without duplicating foundational
lowering. The differential harness continues to derive expectations from the
stage-0 lowerer and compares fixed direct and hosted-parser-composed families.

**Tech Stack:** GHC 9.14.1, Haskell 2010 with existing package extensions,
ordinary Jazz `.jz` modules, the hosted parser surface ADTs, canonical runtime
values, the stack-safe Jazz interpreter, Cabal test components, and the
Nix-pinned development environment.

**Design checkpoint:**
[`2026-07-21-jazz-next-hosted-core-control-flow-patterns-design.md`](../specs/2026-07-21-jazz-next-hosted-core-control-flow-patterns-design.md)

## Global Constraints

- Modify compiler behavior only under `jazz-next/`; `jazz-hs/` and `jazz2/`
  remain read-only references.
- Keep the production stage-0 parser/lowerer and every downstream compiler
  phase unchanged.
- Preserve `lowerFoundationExpression :: SurfaceExpr -> Maybe CoreExpr` and its
  exact child-1 rejection boundary.
- Add no public production facade, host callback, lowering intrinsic, bytecode,
  VM, type inference, analysis, runtime, lowered-IR, LLVM, object/link, or
  native-runtime behavior.
- Keep explicit type application, `$`, signatures, declarations,
  operator-storage names, modules, and imports deferred at every tree depth.
- Compare complete canonical values, never rendered Haskell structures,
  source-text heuristics, or implementation-file contents.
- Refactor existing Jazz code only when it removes duplication or clarifies a
  touched ownership boundary; do not perform syntax-adoption churn.
- Keep checked-in `.jz` indentation at exactly two spaces.
- Write and run each failing behavior test before its production change.
- Run compiler and test commands through the Nix-pinned environment.
- Do not run any exhaustive parser scale component. Routine Cabal `all` runs
  only the bounded default scale suite.
- Commit each independently reviewable green milestone.

## File and Responsibility Map

| File | Responsibility in this child |
| --- | --- |
| `jazz-next/jazz/compiler/CoreLower.jz` | Shared profile-driven expression and statement lowering; structural patterns; child-2 cases, conditionals, and lambdas. |
| `jazz-next/jazz/compiler/CoreTypes.jz` | Existing canonical child-2 schema baseline; no schema redesign or production change is expected. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreParity.hs` | Shared stage-0 expected-value construction and generated Jazz batch execution for both lowering profiles. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreControlFlowPatternsSpec.hs` | Fixed 18-case direct family, 14-source composed family, 12-case unsupported family, repetition, and exact generated-name assertions. |
| `jazz-next/jazz-next.cabal` | Register the focused child-2 suite and its shared test dependencies. |
| Coordination and status paths in frontmatter | Promote, document, close, archive, and hand off this child without promoting declarations, modules, or backend work. |

## Stable Interfaces

| Owner | Interface |
| --- | --- |
| `CoreLower` | Preserve `lowerFoundationExpression :: SurfaceExpr -> Maybe CoreExpr`; add `lowerControlFlowPatternsExpression :: SurfaceExpr -> Maybe CoreExpr`. |
| `CoreLower` private kernel | Use `FoundationProfile` and `ControlFlowPatternsProfile` to select constructor ownership while sharing recursive foundational logic. |
| `JazzCoreParity` | Preserve foundation runners; add direct and source-batch wrappers for `lowerControlFlowPatternsExpression` that reuse one private generated-module runner. |
| Stage-0 oracle | Continue using `lowerSurfaceExpr` followed by `canonicalCoreExprRuntimeValue`; the harness never recreates a lowering rule. |

## Implementation Batch: Hosted Core Control Flow and Patterns

### Task 0: Promote the reviewed child

**Files:** this plan, `docs/execution/queue.md`, and
`docs/execution/blocker-contracts.md`.

**Produces:** One exact P1/L `Ready Now` row matching the approved plan
frontmatter, with the parent bootstrap blocker naming this child as active.

- [x] Change plan status to `ready` and `autonomous_ready` to `yes` after user
  approval.
- [x] Move the named candidate from `Next Curation Target` to `Ready Now` with
  exact dependency, plan section, target paths, deliverable, verification, and
  date parity.
- [x] Update the blocker contract to record the approved child while preserving
  child 3, child 4, and every backend stage as unpromoted.
- [x] Run `bash scripts/check-execution-queue.sh`,
  `bash scripts/check-docs.sh`, and `git diff --check`.
- [x] Commit as `docs: promote hosted core control flow`.

### Task 1: Lower patterns, cases, and conditionals through a shared profile

**Files:** `CoreLower.jz`, `JazzCoreParity.hs`,
`JazzCoreControlFlowPatternsSpec.hs`, and `jazz-next.cabal`.

**Consumes:** Existing `SurfaceExpr`, `SurfacePattern`, `CoreExpr`,
`CorePattern`, and canonical-adapter contracts.

**Produces:** The new exported child-2 wrapper, exact pattern conversion, and
profile-aware recursive lowering without changing the foundation wrapper.

- [x] Register `jazz-core-control-flow-patterns-spec` and add the first nine
  direct fixtures from the approved design: four conditional cases and five
  case/pattern cases, including the complete pattern inventory and guarded,
  empty-arm, nested-scrutinee, and nested-body behavior.
- [x] Add harness wrappers that request the new entry point while continuing to
  compute expected values through stage-0 `lowerSurfaceExpr` and the checked
  canonical adapter.
- [x] Run the new focused suite and confirm it fails because
  `lowerControlFlowPatternsExpression` is not exported; keep the existing
  foundation suite green.
- [x] Refactor foundational expression, expression-list, statement, and
  statement-list recursion behind the two private profiles. Do not duplicate
  the child-1 constructor logic.
- [x] Add structural lowering for every approved pattern constructor, optional
  case guards, source-ordered case arms, conditionals, and recursive child-2
  subtrees.
- [x] Confirm `FoundationProfile` still rejects all control-flow constructors
  and `ControlFlowPatternsProfile` rejects every later-child constructor.
- [x] Run the new focused suite, foundation suite, canonical-core suite, and
  canonical-parser suite; require complete exact values and no compile/runtime
  errors.
- [x] Commit as `feat: lower hosted core control flow`.

### Task 2: Lower identifier and pattern lambdas with exact generated indices

**Files:** `CoreLower.jz` and
`JazzCoreControlFlowPatternsSpec.hs`.

**Consumes:** The green profile kernel from Task 1 and structured
`CoreLambdaPatternArgument Int` names from `CoreTypes`.

**Produces:** Exact nested unary lambda lowering for direct fixtures 10-18,
including mixed identifier/pattern parameter lists.

- [x] Add the remaining nine positive direct fixtures from the approved
  design. Assert complete canonical output for identifier lambdas, multiple
  parameters, every pattern-lambda shape, nested control flow, and control flow
  inside ordinary blocks.
- [x] Include explicit assertions showing that mixed parameters use generated
  index 2 and that two pattern parameters use indices 1 and 2 in their original
  source positions.
- [x] Run the focused suite and confirm the lambda cases fail with `Nothing`
  while the Task-1 control-flow cases remain green.
- [x] Lower parameters from the final body toward the first source parameter.
  Use source names for identifiers and one structured generated name for both
  the binder and one-arm pattern-case scrutinee of each pattern parameter.
- [x] Thread the full child-2 profile through lambda bodies so nested cases,
  conditions, lambdas, collections, applications, and ordinary blocks lower
  recursively, while one deferred subtree rejects the whole lambda.
- [x] Run the focused suite twice and require identical complete canonical
  values. Run the foundation suite to prove its lambda rejection is unchanged.
- [x] Commit as `feat: lower hosted core pattern lambdas`.

### Task 3: Prove parser composition and all-or-nothing ownership

**Files:** `JazzCoreParity.hs` and
`JazzCoreControlFlowPatternsSpec.hs`.

**Consumes:** The complete direct child-2 lowerer and the existing hosted parser
source facade.

**Produces:** Exact repeated parity for the approved 14-source family and exact
`Nothing` results for the approved 12-case deferred family.

- [x] Add the 14 composed sources verbatim from the approved design, retaining
  their stable order and covering nested conditions, blocks, every parser-owned
  pattern shape, guards, nested cases, identifier/pattern/mixed lambdas, and the
  recursive block example.
- [x] Add the 12 named direct unsupported cases from the approved design across
  roots, conditions, case scrutinees, guards, bodies, lambdas, and block
  statements.
- [x] Run the focused suite and confirm the composed assertions fail because
  the child-2 source-batch wrapper is not present; keep direct child-2 and
  foundation evidence green.
- [x] Generalize only the private harness plumbing needed for foundation and
  child-2 source wrappers to share generated module construction and checked-in
  source lookup. Preserve all existing public foundation helpers.
- [x] Execute direct positive, composed positive, and unsupported families
  twice. Require exact stage-0 values for positive cases, only `Nothing` for
  unsupported cases, no compile/runtime errors, and identical repeated output.
- [x] Run `jazz-core-expression-foundation-spec`,
  `canonical-core-comparison-spec`, `canonical-parser-comparison-spec`,
  `jazz-parser-control-flow-patterns-spec`, and `repository-audit-spec`.
- [x] Commit as `test: prove hosted core control flow parity`.

### Task 4: Verify and close the child

**Files:** all coordination and status paths listed in frontmatter.

**Produces:** Complete routine evidence, durable closure documentation, and
`JN-BOOTSTRAP-JAZZ-CORE-SIGNATURES-DECLARATIONS-OPERATORS-001` as the sole next
curation candidate.

- [ ] Run the exact focused verification command from frontmatter.
- [ ] Run the warning-clean development build, routine non-exhaustive Cabal
  `all`, and `cabal check`. Confirm no opt-in exhaustive parser scale component
  is enabled or invoked.
- [ ] Update the hosted canonical-core design, bootstrap profile, and
  `jazz-next/README.md` with the landed child-2 entry point, exact generated-name
  rule, parity evidence, and remaining child-3/child-4 boundary.
- [ ] Mark this plan done, archive the child with concrete verification
  evidence, empty `Ready Now`, and seed only
  `JN-BOOTSTRAP-JAZZ-CORE-SIGNATURES-DECLARATIONS-OPERATORS-001` in
  `Next Curation Target` without promoting it.
- [ ] Update the parent blocker contract to make the child-3 implementation
  plan the next approval gate while leaving modules and all backend stages
  unpromoted.
- [ ] Run queue/docs validators and `git diff --check`.
- [ ] Commit as `docs: close hosted core control flow`.

## Done Criteria

- The foundation and child-2 wrappers share one recursive implementation while
  preserving distinct ownership boundaries.
- Every canonical pattern, guarded case, conditional, nested control-flow, and
  approved lambda rule matches stage 0 exactly.
- Pattern-lambda generated names use the original one-based parameter position
  for both binder and scrutinee.
- All 18 direct positive, 14 composed positive, and 12 unsupported fixtures run
  twice with deterministic complete results.
- Later-child forms reject the entire tree at every tested depth without fake
  core output or temporary diagnostics.
- Existing hosted core/parser suites remain green and production lowering is
  unchanged.
- Focused tests, warning-clean build, routine Cabal matrix, package check,
  queue/docs validation, and whitespace checks pass.
- No exhaustive parser scale component is run.
