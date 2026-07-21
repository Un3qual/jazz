---
id: JN-BOOTSTRAP-JAZZ-CORE-MODULES-CORPUS-CLOSURE-001
status: proposed
priority: P1
size: L
kind: impl
autonomous_ready: no
depends_on:
  - JN-BOOTSTRAP-JAZZ-CORE-SIGNATURES-DECLARATIONS-OPERATORS-001
last_verified: 2026-07-21
plan_section: "Implementation Batch: Hosted Core Modules and Corpus Closure"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-21-jazz-next-hosted-canonical-core-design.md
  - docs/superpowers/specs/2026-07-21-jazz-next-hosted-core-modules-corpus-closure-design.md
  - docs/superpowers/plans/2026-07-21-jazz-next-hosted-core-modules-corpus-closure.md
  - jazz-next/README.md
  - jazz-next/jazz/compiler/CoreLower.jz
  - jazz-next/jazz/compiler/CoreTypes.jz
  - jazz-next/jazz-next.cabal
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalCoreComparison.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreParity.hs
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-core-modules-corpus-closure-spec jazz-core-signatures-declarations-operators-spec jazz-core-control-flow-patterns-spec jazz-core-expression-foundation-spec canonical-core-comparison-spec canonical-parser-comparison-spec jazz-parser-types-declarations-modules-spec jazz-parser-control-flow-patterns-spec jazz-parser-operators-full-parity-spec jazz-parser-parity-spec repository-audit-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Complete the private Jazz-authored canonical-core boundary with total expression lowering, exact module/import metadata, expected-path validation, structured `E4005`/`E4006` results, recursive source-path qualification, a composed source facade, and repeated exact parity for all 196 accepted fixtures in the fixed parser corpus."
---

# Jazz-Next Hosted Core Modules and Corpus Closure Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox syntax for tracking. Per maintainer direction, this plan records
> exact responsibilities, interfaces, observable tests, commands, and commit
> boundaries without embedding the implementation bodies.

**Goal:** Complete hosted canonical-core parity through modules, failures,
source qualification, and every accepted parser-corpus fixture.

**Architecture:** Refactor the shared lowering kernel to return a canonical
value with its minimum required profile, preserving the three landed `Maybe`
wrappers while making the fourth expression entry total. `CoreLower.jz` then
owns stage-0-equivalent module collection and qualification; a small `Core.jz`
facade composes the existing hosted parser exactly once.

**Tech Stack:** GHC 9.14.1, Haskell 2010 with existing package extensions,
ordinary Jazz `.jz` modules, hosted lexer/parser surface ADTs, canonical runtime
values, the stack-safe Jazz interpreter, Cabal test components, and the
Nix-pinned development environment.

**Design checkpoint:**
[`2026-07-21-jazz-next-hosted-core-modules-corpus-closure-design.md`](../specs/2026-07-21-jazz-next-hosted-core-modules-corpus-closure-design.md)

## Global Constraints

- Modify compiler behavior only under `jazz-next/`; `jazz-hs/` and `jazz2/`
  remain read-only references.
- Keep the production Haskell parser/lowerer and all downstream compiler
  phases unchanged.
- Preserve all three landed wrapper signatures and exact earlier-child
  deferral boundaries.
- Use one canonical recursive transformation; do not create a parallel module
  expression lowerer or reproduce lowering rules in the Haskell harness.
- Module lowering may fail only with the structured `E4005` or `E4006`
  counterpart already represented in `CoreTypes.jz`.
- Keep lexical, parser, module-lowering, and successful facade outcomes
  structurally distinct; do not render failures to text.
- Extract only top-level module/import statements. Preserve nested imports in
  the executable core body exactly as stage 0 does.
- Preserve omitted versus explicit-empty exports, source order, exact path
  segments, aliases, symbols, namespaces, and constructor selectors.
- Qualify every retained span and no span-free value with the supplied
  canonical source path.
- Compare complete canonical values, never Haskell `Show`, source heuristics,
  or implementation-file contents.
- Keep checked-in `.jz` indentation at exactly two spaces.
- Write and run each failing behavior test before its production change.
- Run compiler and test commands through the Nix-pinned environment.
- Do not run opt-in exhaustive parser scale components. Routine Cabal `all`
  may run only the bounded `jazz-parser-scale-spec` component.
- Add no parser syntax, public compiler API, name/module resolution, analysis,
  warnings, type inference, evaluation, host callback, intrinsic, lowered IR,
  bytecode, VM, LLVM, object/link, or native-runtime work.
- Commit each independently reviewable green milestone.

## File and Responsibility Map

| File | Responsibility in this child |
| --- | --- |
| `jazz-next/jazz/compiler/CoreTypes.jz` | Add only the composed source-result ADT; retain the existing canonical module and structured failure schema. |
| `jazz-next/jazz/compiler/CoreLower.jz` | Minimum-profile kernel, total expression entry, export/import conversion, module validation, metadata extraction, and recursive span qualification. |
| `jazz-next/jazz/compiler/Core.jz` | Thin single-call composition of `Parser.parseSource` and `CoreLower.lowerModule`. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalCoreComparison.hs` | Structurally adapt stage-0 module results and composed phase results without making lowering decisions. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreParity.hs` | Generate direct complete-expression, direct module, facade, and corpus executions with stage-0 expected values. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreModulesCorpusClosureSpec.hs` | Own the 17 direct fixtures, 13 facade sources, 196-entry accepted-corpus manifest, audits, repetition, and bounded smoke assertion. |
| `jazz-next/jazz-next.cabal` | Register `Core.jz` as checked-in source and the focused child suite with shared test modules. |
| Coordination/status paths in frontmatter | Promote, document, close, archive, and expose the backend-planning gate without promoting backend implementation. |

## Stable Interfaces

| Owner | Interface |
| --- | --- |
| `CoreLower` existing wrappers | Preserve `lowerFoundationExpression`, `lowerControlFlowPatternsExpression`, and `lowerSignaturesDeclarationsOperatorsExpression`, each as `SurfaceExpr -> Maybe CoreExpr`. |
| `CoreLower` complete expression | Add `lowerCanonicalExpression :: SurfaceExpr -> CoreExpr`. |
| `CoreLower` module boundary | Add `lowerModule :: CanonicalSourcePath -> [Text] -> SurfaceExpr -> CoreModuleLoweringResult`. |
| `CoreTypes` composed boundary | Add `CanonicalCoreSourceResult = CanonicalCoreSourceLexicalFailure CanonicalSourcePath CanonicalLexError | CanonicalCoreSourceParserFailure CanonicalSourcePath ParserFailure | CanonicalCoreSourceModuleResult CoreModuleLoweringResult`. |
| `Core` facade | Add `lowerCoreSource :: CanonicalSourcePath -> [Text] -> Text -> CanonicalCoreSourceResult`. |
| Private lowering kernel | Produce a `CoreExpr` together with the maximum minimum-profile requirement of the root and all recursive children; wrapper admission compares that requirement with its requested profile. |
| `JazzCoreParity` | Continue deriving expected values from `lowerSurfaceExpr` or `lowerSurfaceModuleDetailed` followed by the checked canonical adapter. |
| Corpus manifest | Store each accepted parser fixture name and its fixed expected module path in parser-corpus order; never derive an expected path adaptively during comparison. |

## Implementation Batch: Hosted Core Modules and Corpus Closure

### Task 0: Promote the reviewed child

**Files:** this plan, `docs/execution/queue.md`, and
`docs/execution/blocker-contracts.md`.

**Produces:** One exact P1/L `Ready Now` row matching approved plan metadata,
with the parent bootstrap blocker naming child 4 as active.

- [ ] Change plan status to `ready` and `autonomous_ready` to `yes` after user
  approval.
- [ ] Move the sole candidate from `Next Curation Target` to `Ready Now` with
  exact dependency, section, existing target paths, deliverable, verification,
  and date parity. Keep not-yet-created `Core.jz` and focused-spec paths out of
  both metadata sets until their creating tasks add them atomically.
- [ ] Update the blocker contract to name modules/corpus closure as active
  while preserving every backend stage as unpromoted.
- [ ] Run `bash scripts/check-execution-queue.sh`,
  `bash scripts/check-docs.sh`, and `git diff --check`.
- [ ] Commit as `docs: promote hosted core modules closure`.

### Task 1: Make the shared expression transformation complete

**Files:** `CoreLower.jz`, `JazzCoreParity.hs`,
`JazzCoreModulesCorpusClosureSpec.hs`, `jazz-next.cabal`, this plan, and
`docs/execution/queue.md`.

**Consumes:** The three landed profile wrappers and every current
surface/core expression and statement constructor.

**Produces:** `lowerCanonicalExpression`, structural module/import statement
lowering, and unchanged earlier wrapper behavior through one recursive kernel.

- [ ] Create and register `jazz-core-modules-corpus-closure-spec`, add its path
  to plan/queue metadata, and add direct complete-expression assertions for
  module and import statements nested through representative recursive owners.
- [ ] Run the new suite and confirm it fails because
  `lowerCanonicalExpression` is absent; keep all three earlier core suites
  green.
- [ ] Refactor the kernel to compute the lowered value and minimum required
  profile in one traversal. Requirement aggregation must include statements,
  impl bodies, guards, lambdas, applications, and collections.
- [ ] Add the fourth profile plus total module/import statement transforms.
  Preserve the stage-0 rule that expression-level module statements omit their
  export payload.
- [ ] Route the three existing wrappers through profile admission and expose
  the total wrapper without an impossible `Nothing` case.
- [ ] Run the focused suite twice plus all three earlier core suites. Require
  exact stage-0 expression values, deterministic output, and unchanged
  deferrals.
- [ ] Commit as `feat: complete hosted core expression lowering`.

### Task 2: Lower modules, metadata, failures, and qualified spans

**Files:** `CoreLower.jz`, `CanonicalCoreComparison.hs`, `JazzCoreParity.hs`,
and `JazzCoreModulesCorpusClosureSpec.hs`.

**Consumes:** The total expression entry, existing canonical module/failure
schema, stage-0 `lowerSurfaceModuleDetailed`, and the approved 17 direct
fixtures.

**Produces:** `lowerModule` with exact extraction, validation, selector
conversion, structured failures, and complete qualification.

- [ ] Add the 17 direct fixtures in the approved order and compare their
  complete expected module results through the stage-0 detailed lowerer and
  canonical adapter. Include explicit assertions for omitted/empty exports,
  metadata order, nested import retention, all span owners, and both failures.
- [ ] Run the focused suite and confirm module-result cases fail because
  `lowerModule` is absent while Task-1 expression cases remain green.
- [ ] Add pure conversion helpers for namespaces, located names, type
  constructor selectors, export selectors, declarations, and resolved imports.
- [ ] Collect and remove only top-level metadata in source order; validate
  declaration count before path equality; preserve the exact declaration list
  in `CoreMultipleModuleDeclarationsFailure`.
- [ ] Add one recursive qualification pass for every span-bearing core owner
  plus metadata/failure declaration spans. Keep line and column unchanged.
- [ ] Run the focused suite twice and the three earlier core suites; require
  exact structural results and unchanged direct-expression spans.
- [ ] Commit as `feat: lower hosted core modules`.

### Task 3: Compose source parsing and module lowering

**Files:** `CoreTypes.jz`, new `Core.jz`, `CanonicalCoreComparison.hs`,
`JazzCoreParity.hs`, `JazzCoreModulesCorpusClosureSpec.hs`, `jazz-next.cabal`,
this plan, and `docs/execution/queue.md`.

**Consumes:** `Parser.parseSource`, `CoreLower.lowerModule`, and the approved
13-source facade family.

**Produces:** `CanonicalCoreSourceResult` and `lowerCoreSource` with strict
phase ownership.

- [ ] Add all 13 composed sources in approved order and stage-0 expected-result
  support for module success/failure, lexical failure, and parser failure.
- [ ] Run the focused suite and confirm source cases fail because `Core.jz` and
  `CanonicalCoreSourceResult` do not exist; retain direct module tests green.
- [ ] Add the composed result ADT without changing the existing parser or
  module result types.
- [ ] Create `Core.jz`, call `Parser.parseSource` exactly once, forward lexical
  and parser values unchanged, and call `lowerModule` only after parse success.
- [ ] Register `Core.jz` as checked-in compiler source and add its path to both
  plan and queue metadata in the same change.
- [ ] Run all direct and 13 composed fixtures twice. Require complete exact
  results, phase-distinct failures, deterministic output, and no compiler or
  runtime errors.
- [ ] Commit as `feat: compose hosted source to canonical core`.

### Task 4: Close the accepted parser corpus

**Files:** `JazzCoreParity.hs` and
`JazzCoreModulesCorpusClosureSpec.hs`.

**Consumes:** The fixed 365-case `parserFixtureCorpus`, its 196 accepted
fixtures, the complete facade, and stage-0 detailed module lowering.

**Produces:** An explicit ordered 196-entry lowering manifest plus repeated
end-to-end parity for every accepted parser fixture.

- [ ] Add manifest validation tests for duplicate names, missing names, unknown
  names, rejected inclusions, accepted omissions, order drift, and the exact
  196/169/365 counts. Run them before adding the complete manifest and observe
  the expected omissions.
- [ ] Add every accepted fixture name and its reviewed expected module path in
  parser-corpus order. Do not alter `FixtureCorpus.hs` or infer expected paths
  at comparison time.
- [ ] Add shared expected/actual corpus runners that independently process
  stage 0 and hosted `Core.lowerCoreSource` results without reproducing parser
  or lowering decisions.
- [ ] Execute the full 196-result list twice. Require exact complete parity,
  byte-identical repeated rendering, and no host callback or intrinsic.
- [ ] Run the fixed `mixed-full-surface` bounded smoke assertion. Do not add or
  invoke an exhaustive scale component.
- [ ] Run the exact focused regression command from frontmatter.
- [ ] Commit as `test: prove hosted core corpus closure`.

### Task 5: Verify and close the hosted canonical-core milestone

**Files:** all coordination and status paths listed in frontmatter.

**Produces:** Complete routine evidence, durable child-4 closure, and no
unpromoted canonical-core implementation child.

- [ ] Run the exact focused verification command from frontmatter.
- [ ] Run the warning-clean development build, routine non-exhaustive Cabal
  `all`, and `cabal check`. Confirm only bounded `jazz-parser-scale-spec` runs;
  do not enable any `jazz-parser-scale-full-*` component.
- [ ] Update the hosted canonical-core design, bootstrap profile, and
  `jazz-next/README.md` with final entry points, failure ownership, exact
  17/13/196 fixture evidence, and the continuing production-Haskell boundary.
- [ ] Mark this plan and design done, archive the child with concrete evidence,
  empty `Ready Now`, and remove the completed child from `Next Curation Target`.
- [ ] Update the parent blocker contract and live queue to state that hosted
  canonical core is complete and that backend-neutral lowered IR planning is
  the next approval gate, without promoting backend implementation.
- [ ] Run queue/docs validators and `git diff --check`.
- [ ] Commit as `docs: close hosted canonical core`.

## Done Criteria

- `lowerCanonicalExpression` is total for every fixed `SurfaceExpr`
  constructor while all three earlier wrappers preserve their exact boundaries.
- Module lowering matches stage 0 for extraction, order, metadata,
  omitted/empty distinctions, validation, failures, executable body, and every
  qualified span.
- `Core.lowerCoreSource` keeps lexical, parser, lowering, and success outcomes
  distinct and contains no duplicated parser/lowering logic.
- All 17 direct fixtures and 13 composed sources run twice with exact complete
  deterministic results.
- The explicit ordered manifest contains all and only the 196 accepted fixtures
  from the fixed 365-case parser corpus, and every one matches stage 0 twice.
- Existing hosted core/parser suites remain green and production behavior is
  unchanged.
- Focused tests, warning-clean build, routine Cabal matrix, package check,
  queue/docs validation, and whitespace checks pass.
- No opt-in exhaustive parser scale component runs.
