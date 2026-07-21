---
id: JN-BOOTSTRAP-JAZZ-CORE-SIGNATURES-DECLARATIONS-OPERATORS-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-JAZZ-CORE-CONTROL-FLOW-PATTERNS-001
last_verified: 2026-07-21
plan_section: "Implementation Batch: Hosted Core Signatures, Declarations, and Operators"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-21-jazz-next-hosted-canonical-core-design.md
  - docs/superpowers/specs/2026-07-21-jazz-next-hosted-core-signatures-declarations-operators-design.md
  - docs/superpowers/plans/2026-07-21-jazz-next-hosted-core-signatures-declarations-operators.md
  - jazz-next/README.md
  - jazz-next/jazz/compiler/CoreLower.jz
  - jazz-next/jazz/compiler/CoreTypes.jz
  - jazz-next/jazz-next.cabal
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreParity.hs
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-core-signatures-declarations-operators-spec jazz-core-control-flow-patterns-spec jazz-core-expression-foundation-spec canonical-core-comparison-spec canonical-parser-comparison-spec jazz-parser-types-declarations-modules-spec jazz-parser-operators-full-parity-spec repository-audit-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Extend the internal Jazz-authored canonical-core lowerer through every signature type and payload, explicit type application, `$` application, data/class/impl declaration, and exact hidden operator-storage name with repeated direct and hosted-parser-composed stage-0 parity and all-or-nothing module/import deferral."
---

# Jazz-Next Hosted Core Signatures, Declarations, and Operators Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox syntax for tracking. This plan intentionally specifies interfaces,
> observable behavior, tests, commands, and commit boundaries without embedding
> implementation bodies.

**Goal:** Make hosted expression lowering complete except for module/import
extraction by adding signatures, declarations, explicit type application, `$`
desugaring, and exact operator-storage names.

**Architecture:** `CoreLower.jz` extends its ordered private profile and shared
recursive kernel. Total helpers lower signature and declaration payloads, while
the new child wrapper retains the existing `Maybe` boundary only for module and
import statements. The Haskell harness continues to derive all expected values
from stage 0 before structural canonical adaptation.

**Tech Stack:** GHC 9.14.1, Haskell 2010 with existing package extensions,
ordinary Jazz `.jz` modules, hosted parser surface ADTs, canonical runtime
values, the stack-safe Jazz interpreter, Cabal test components, and the
Nix-pinned development environment.

**Design checkpoint:**
[`2026-07-21-jazz-next-hosted-core-signatures-declarations-operators-design.md`](../specs/2026-07-21-jazz-next-hosted-core-signatures-declarations-operators-design.md)

## Global Constraints

- Modify compiler behavior only under `jazz-next/`; `jazz-hs/` and `jazz2/`
  remain read-only references.
- Keep production stage-0 lowering and all downstream compiler phases
  unchanged.
- Preserve the foundation and control-flow wrapper interfaces and their exact
  earlier-child rejection boundaries.
- Preserve the already-complete `CoreTypes.jz` schema unless parity reveals a
  genuine representation mismatch.
- Return `Nothing` for any tree containing a module or import statement; never
  drop the statement or emit a partial core value.
- Keep module extraction, export metadata, path validation, source-path span
  qualification, diagnostics, and the composed production facade deferred.
- Add no parser behavior, analysis, resolution, type inference, evaluation,
  runtime, lowered IR, LLVM, object/link, or native-runtime work.
- Compare complete canonical values, not Haskell `Show`, source heuristics, or
  implementation-file contents.
- Refactor existing Jazz code only where it removes duplication or clarifies
  the touched lowering boundary; do not perform syntax-adoption churn.
- Keep checked-in `.jz` indentation at exactly two spaces.
- Write and run each failing behavior test before its production change.
- Run compiler and test commands through the Nix-pinned environment.
- Do not run opt-in exhaustive parser scale components. Routine Cabal `all`
  may run only the bounded default scale suite.
- Commit each independently reviewable green milestone.

## File and Responsibility Map

| File | Responsibility in this child |
| --- | --- |
| `jazz-next/jazz/compiler/CoreLower.jz` | Ordered third profile; shared expression/statement traversal; total signature, declaration, `$`, type-application, and operator-name lowering. |
| `jazz-next/jazz/compiler/CoreTypes.jz` | Existing complete canonical schema; expected to remain unchanged. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreParity.hs` | Stage-0 expected-value construction and generated direct/source execution for the third wrapper. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreSignaturesDeclarationsOperatorsSpec.hs` | Fixed 20-case direct, 16-source composed, and 8-case deferred families with repeated exact assertions. |
| `jazz-next/jazz-next.cabal` | Focused child-suite registration and shared test dependencies. |
| Coordination/status paths in frontmatter | Promote, document, close, archive, and hand off the child without promoting modules or backend work. |

## Stable Interfaces

| Owner | Interface |
| --- | --- |
| `CoreLower` | Preserve `lowerFoundationExpression :: SurfaceExpr -> Maybe CoreExpr` and `lowerControlFlowPatternsExpression :: SurfaceExpr -> Maybe CoreExpr`; add `lowerSignaturesDeclarationsOperatorsExpression :: SurfaceExpr -> Maybe CoreExpr`. |
| `CoreLower` private profile | Add `SignaturesDeclarationsOperatorsProfile` after the two landed profiles and use ordered capability checks in the one recursive kernel. |
| `CoreLower` signature-name helper | Convert exactly one non-empty `qualifier::member` pair to `CoreQualifiedName`; retain every other spelling as `CoreSourceName`. |
| `CoreLower` binding-name helper | Convert any `$operator:`-prefixed parsed name to `CoreGeneratedName (CoreOperatorBinding storedName)` without decoding or re-encoding it. |
| `JazzCoreParity` | Preserve existing runners; add direct and source wrappers for the third entry point through the same private batch machinery. |
| Stage-0 oracle | Continue using `lowerSurfaceExpr` followed by `canonicalCoreExprRuntimeValue`; never recreate lowering decisions in the harness. |

## Implementation Batch: Hosted Core Signatures, Declarations, and Operators

### Task 0: Promote the reviewed child

**Files:** this plan, `docs/execution/queue.md`, and
`docs/execution/blocker-contracts.md`.

**Produces:** One exact P1/L `Ready Now` row matching approved plan metadata,
with the parent bootstrap blocker naming this child as active.

- [x] Change plan status to `ready` and `autonomous_ready` to `yes`.
- [x] Move the sole candidate from `Next Curation Target` to `Ready Now` with
  exact dependency, section, existing target paths, deliverable, verification,
  and date parity. Omit the not-yet-created test path from both ready metadata
  sets; Task 1 adds it atomically when the file exists.
- [x] Update the blocker contract to record child 3 as active while preserving
  modules/corpus closure and every backend stage as unpromoted.
- [x] Run `bash scripts/check-execution-queue.sh`,
  `bash scripts/check-docs.sh`, and `git diff --check`.
- [x] Commit as `docs: promote hosted core declarations`.

### Task 1: Lower signature and operator expression forms

**Files:** `CoreLower.jz`, `JazzCoreParity.hs`,
`JazzCoreSignaturesDeclarationsOperatorsSpec.hs`, and `jazz-next.cabal`.

**Consumes:** The shared child-2 profile kernel, all surface/core signature
ADTs, the canonical stage-0 adapter, and the approved direct fixtures 1-13.

**Produces:** The exported child-3 wrapper, total signature helpers, explicit
type application, `$` application, and shared ordinary/operator binding names.

- [ ] Register `jazz-core-signatures-declarations-operators-spec`; add direct
  fixtures 1-13 in approved order, including every signature type, numeric
  width, constraint, unsupported token, and exact operator name.
- [ ] Add the direct harness wrapper for the new entry point while continuing
  to compute expected values through stage-0 `lowerSurfaceExpr` and the checked
  adapter. Add the now-existing test path to plan and queue metadata.
- [ ] Run the focused suite and confirm it fails because
  `lowerSignaturesDeclarationsOperatorsExpression` is not exported. Keep the
  landed foundation and control-flow suites green.
- [ ] Add the ordered third profile and total recursive helpers for signature
  names, types, constraints, payloads, and tokens.
- [ ] Lower explicit type applications, binary `$`, ordinary lets/signatures,
  and `$operator:` bindings/signatures with exact spans and stored names.
- [ ] Prove the two earlier wrappers still reject every child-3 form and the
  new wrapper still rejects module/import statements.
- [ ] Run the focused suite twice plus both earlier hosted-core suites and
  require exact deterministic canonical values with no compile/runtime errors.
- [ ] Commit as `feat: lower hosted core signatures and operators`.

### Task 2: Lower data, class, and impl payloads

**Files:** `CoreLower.jz` and
`JazzCoreSignaturesDeclarationsOperatorsSpec.hs`.

**Consumes:** The green signature helpers and direct fixtures 14-20.

**Produces:** Structural declaration lowering with recursive impl method bodies
and unchanged source order/spans.

- [ ] Add direct fixtures 14-20 in approved order, covering empty and populated
  data/class/impl declarations, named and opaque constructor arguments, method
  signatures, recursive method bodies, and one mixed block.
- [ ] Run the focused suite and confirm declaration cases return `Nothing`
  while all Task-1 cases remain green.
- [ ] Add total helpers for data constructors, class method signatures, and
  lists of declaration payloads; reuse signature and name helpers.
- [ ] Lower impl method bodies through the full child-3 profile so nested type
  applications, `$`, control flow, lambdas, and ordinary blocks are admitted,
  while one module/import subtree rejects the full declaration.
- [ ] Run the focused suite twice and both earlier hosted-core suites; require
  exact values and unchanged earlier-child boundaries.
- [ ] Commit as `feat: lower hosted core declarations`.

### Task 3: Prove hosted-parser composition and module deferral

**Files:** `JazzCoreParity.hs` and
`JazzCoreSignaturesDeclarationsOperatorsSpec.hs`.

**Consumes:** The complete direct child-3 lowerer, hosted parser source facade,
and approved 16-source and 8-case families.

**Produces:** Repeated source-to-core parity and all-or-nothing child-4
deferral at representative recursive owners.

- [ ] Add all 16 composed sources verbatim in approved order, covering explicit
  type application, right-associated `$`, signatures, data/class/impl forms,
  operator signature/binding storage, and mixed declarations/control flow.
- [ ] Add all 8 named deferred direct fixtures, placing module/import
  statements at roots and inside conditions, cases, lambdas, lets, impl
  methods, and hidden operator bindings.
- [ ] Run the focused suite and confirm composed assertions fail until the
  child-3 source wrapper is wired; retain direct and earlier-child green
  evidence.
- [ ] Generalize only the private harness plumbing needed by the new source
  wrapper. Preserve every existing public helper and stage-0 oracle path.
- [ ] Execute all positive and deferred families twice. Require exact stage-0
  values for positives, only `Nothing` for deferrals, no compile/runtime
  errors, and byte-identical repeated rendering.
- [ ] Run the focused regression command from frontmatter.
- [ ] Commit as `test: prove hosted core declaration parity`.

### Task 4: Verify and close the child

**Files:** all coordination and status paths listed in frontmatter.

**Produces:** Complete routine evidence, durable child-3 closure, and modules /
corpus closure as the sole next curation target.

- [ ] Run the exact focused verification command from frontmatter.
- [ ] Run the warning-clean development build, routine non-exhaustive Cabal
  `all`, and `cabal check`. Confirm no opt-in exhaustive parser scale component
  is enabled or invoked.
- [ ] Update the hosted canonical-core design, bootstrap profile, and
  `jazz-next/README.md` with the child-3 entry point, exact transformations,
  fixture evidence, and remaining module boundary.
- [ ] Mark this plan done, archive the child with concrete evidence, empty
  `Ready Now`, and seed only the child-4 modules/corpus candidate in
  `Next Curation Target` without promoting it.
- [ ] Update the parent blocker contract to make the child-4 design/plan the
  next approval gate while leaving all backend stages unpromoted.
- [ ] Run queue/docs validators and `git diff --check`.
- [ ] Commit as `docs: close hosted core declarations`.

## Done Criteria

- The new wrapper shares one recursive implementation with both earlier
  wrappers while preserving all three observable ownership boundaries.
- Every signature type/payload/token, declaration payload, explicit type
  application, and `$` transformation matches stage 0 exactly.
- Operator bindings and adjacent signatures use identical exact stored names.
- All 20 direct positives, 16 composed positives, and 8 deferred fixtures run
  twice with deterministic complete results.
- Any module/import statement rejects the entire tree without partial output.
- Existing hosted core/parser suites remain green and production behavior is
  unchanged.
- Focused tests, warning-clean build, routine Cabal matrix, package check,
  queue/docs validation, and whitespace checks pass.
- No opt-in exhaustive parser scale component is run.
