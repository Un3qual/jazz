---
id: JN-BOOTSTRAP-JAZZ-PARSER-TYPES-DECLARATIONS-MODULES-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-JAZZ-PARSER-EXPRESSION-FOUNDATION-001
last_verified: 2026-07-17
plan_section: "Implementation Batch: Types, Declarations, and Modules"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-12-jazz-next-bootstrap-jazz-parser-design.md
  - docs/superpowers/specs/2026-07-17-jazz-next-bootstrap-parser-types-declarations-modules-design.md
  - docs/superpowers/plans/2026-07-17-jazz-next-bootstrap-parser-types-declarations-modules.md
  - jazz-next/README.md
  - jazz-next/jazz/compiler/ParserToken.jz
  - jazz-next/jazz/compiler/ParserExpression.jz
  - jazz-next/jazz/compiler/ParserProgram.jz
  - jazz-next/jazz/compiler/Parser.jz
  - jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParity.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParitySpec.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScale.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleSpec.hs
  - jazz-next/jazz-next.cabal
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-types-declarations-modules-spec jazz-parser-component-spec jazz-parser-parity-spec canonical-parser-comparison-spec parser-core-spec token-parser-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec repository-audit-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-scale-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Extend the Jazz-authored parser with shared signature types, explicit type application, immutable grammar context, declarations, modules, imports, and exports while matching stage 0 exactly over a fixed 101-case family and a deterministic 513-statement mixed-grammar scale profile."
---

# Jazz-Next Bootstrap Parser Types, Declarations, and Modules Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` or `superpowers:subagent-driven-development` to
> implement this plan task-by-task. This plan is intentionally
> outcome-oriented: it fixes behavior, interfaces, tests, commands, review
> boundaries, and milestone commits without prescribing the final code.

**Goal:** Extend the landed Jazz-authored parser through the next accepted
stage-0 grammar slice: signatures and types, explicit type application,
data/class/impl declarations, and module/import/export forms.

**Architecture:** Split declaration parsing out of `ParserProgram` along the
same stage-0 domain boundaries already accepted in the design. `ParserSignature`
owns the one shared type grammar, `ParserContext` owns immutable statement
scope, `ParserDeclaration` owns statement classification and declaration-level
grammar, `ParserProgram` owns recursive sequencing, and `ParserExpression`
remains the expression owner while consuming explicit context. The public
façade and fixed surface schema do not change.

**Tech Stack:** GHC 9.14.1, Haskell 2010 plus the existing test extensions,
ordinary Jazz `.jz` modules, the compiler-local `ParserCore` and `ParserToken`
layers, the canonical lexer/parser comparison adapters, runtime observation
statistics, Cabal test components, and the Nix-pinned development environment.

**Design checkpoint:**
[`2026-07-17-jazz-next-bootstrap-parser-types-declarations-modules-design.md`](../specs/2026-07-17-jazz-next-bootstrap-parser-types-declarations-modules-design.md)

## Global Constraints

- Modify compiler implementation only under `jazz-next/`; `jazz-hs/` and
  `jazz2/` remain read-only references.
- Deliver one pull request with reviewable milestone commits. Do not collapse
  the entire child into one implementation commit.
- Keep `ParserCore.jz` grammar-neutral and unchanged. Add the read-only
  remaining-token snapshot at the `ParserToken` specialization layer.
- Keep `ParserTypes.jz` unchanged. If implementation exposes a genuinely
  missing schema constructor, stop and seek a separate schema review rather
  than changing the accepted contract for convenience.
- Keep `Parser.jz` as the same two-entry façade. Token input remains primary;
  source input continues to distinguish lexical failure from parser failure.
- Preserve fail-fast behavior, complete structured failure payloads, retained
  spans, lossless unsupported-signature tokens, and deterministic source order.
- Use explicit immutable context. Do not put aliases or statement mode in
  `ParserCore`, mutable state, evaluator effects, or a test-only host bridge.
- Keep operator metadata and operator grammar deferred. Do not pre-add a
  context field merely for the later operators child.
- Add no control flow, patterns, lambdas, recovery, lowering, canonical core,
  backend, native runtime, or full-corpus closure work.
- Reuse the shared signature type parser for explicit type application. Do not
  create a second expression-local type grammar.
- Prefer behavior tests over source-spelling assertions. New module ownership
  must be proved by compiling and executing through the real Jazz module graph.
- Keep all checked-in `.jz` files at exactly two spaces per indentation level.
- The current GHC 9.14 test-gate migration deleted the old warning and stdlib
  shell wrappers. Their responsibilities now belong to the warning-clean Cabal
  build and `repository-audit-spec`; do not reference or recreate the deleted
  scripts.
- No shell script is an expected target. If execution discovers that a script
  must change, treat that as a scope deviation and add syntax validation only
  after review.
- Commit after each independently reviewable milestone.

## File and Responsibility Map

| File | Responsibility in this child |
| --- | --- |
| `jazz-next/jazz/compiler/ParserToken.jz` | Add a non-consuming remaining-token snapshot without changing generic parser semantics. |
| `jazz-next/jazz/compiler/ParserSignature.jz` | New owner for supported signature payloads, signature types, lossless unsupported fallback, and the type-prefix parser shared with expressions. |
| `jazz-next/jazz/compiler/ParserContext.jz` | New owner for statement mode and the immutable visible-import-alias set. |
| `jazz-next/jazz/compiler/ParserDeclaration.jz` | New owner for statement classification, signatures, data/class/impl, modules/imports/exports, alias scans, scope rules, and duplicate validation. |
| `jazz-next/jazz/compiler/ParserExpression.jz` | Accept explicit grammar context and add adjacent explicit type application through `ParserSignature`. |
| `jazz-next/jazz/compiler/ParserProgram.jz` | Orchestrate pre-scan, recursive block/module parsing, context threading, module-first state, and flattened stage-0 output. |
| `jazz-next/jazz/compiler/Parser.jz` | Keep the existing public token/source façade wired to the expanded program parser. |
| `jazz-next/jazz/compiler/ParserCore.jz` | Locked dependency; no changes in this child. |
| `jazz-next/jazz/compiler/ParserTypes.jz` | Locked surface/result/failure schema; no changes in this child. |
| `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs` | Add the exact ordered 101-case family and three focused fixtures; preserve the 52-case expression family. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserTypesDeclarationsModulesSpec.hs` | New focused real-module-graph suite for signatures, context, declarations, modules, and explicit type application. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs` | Preserve the complete shared-corpus name order and stage-0 parser classification audit after the three focused additions. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs` | Lock the new corpus total, family manifest, and stage-0 classification boundaries. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParity.hs` | Load either named family and construct exact token/source expectations through the canonical adapters. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParitySpec.hs` | Run both entries twice and require exact deterministic parity for both named families. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScale.hs` | Generate and execute the additive mixed declaration/module scale program. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleSpec.hs` | Lock exact output, deterministic observations, zero host operations, and fixed work/stack ceilings for both scale profiles. |
| `jazz-next/jazz-next.cabal` | Register the new focused suite and its ordinary test dependencies. |
| Active docs named in Task 8 | Promotion, landed evidence, archive, and next-child curation. |

The new Jazz and Haskell files are listed here and in their owning tasks. The
frontmatter `target_paths` lists the concrete files that exist at review time,
matching the queue convention used by earlier parser children.

## Stable Interfaces and Invariants

The implementation may choose internal helper names, but these cross-module
contracts must remain stable after their owning task lands:

| Owner | Required interface or invariant |
| --- | --- |
| `ParserToken` | `tokenRemaining :: TokenParser([CanonicalToken])` returns the current suffix without consumption or cursor change. |
| `ParserSignature` | `parseSignaturePayload :: [CanonicalToken] -> SurfaceSignaturePayload` interprets an already-delimited payload; unsupported syntax becomes `UnsupportedSignature` with tokens preserved in order. |
| `ParserSignature` | `parseSignatureType :: TokenParser(SurfaceSignatureType)` parses one supported type prefix for reuse by explicit type application. |
| `ParserContext` | Own `StatementContext` values for top level, module body, and nested block plus initial/module/nested transformations and visible-alias membership/registration. |
| `ParserDeclaration` | Accept context-aware expression and statement-list callbacks, classify exactly one statement, and return its flattened surface statements plus the next immutable context. |
| `ParserDeclaration` | Expose depth-aware `collectImportAliasesUntilEnd :: [CanonicalToken] -> Set(Text)` and `collectImportAliasesUntilBrace :: [CanonicalToken] -> Set(Text)` scans over the current statement-list scope. |
| `ParserProgram` | Keep the top-level “prior form seen” bit as sequencing state, not name-visibility context; flatten a module header and module body into the stage-0 statement sequence. |
| `ParserExpression` | Accept `ParserContext`; after an expression, parse only adjacent `@Type` tails and delegate the type prefix to `ParserSignature`. |
| `Parser` | Preserve `parseTokens` and `parseSource` signatures and phase-separated results. |

The callback shape exists to break recursive module dependencies; it is not a
license to duplicate expression or statement grammar. Within a task, prefer
the smallest private API that satisfies these contracts and its tests.

## Fixed Evidence Contracts

### Exact parity family

Add `TypesDeclarationsModules` to `ParserFixtureFamily`. Its manifest is the
exact ordered list of 98 existing fixtures enumerated in the approved design,
followed by these three focused additions:

| New fixture | Boundary locked by the fixture |
| --- | --- |
| `types-declarations-modules-unsupported-forall-signature` | A signature payload containing `forall` is parser-accepted as `UnsupportedSignature`; its token list is complete and ordered. |
| `types-declarations-modules-foundational-impl-method` | A minimal impl method body uses only the landed expression grammar and proves impl callback integration. |
| `types-declarations-modules-applied-explicit-type-application` | An explicit type application accepts an applied named type through the shared signature parser. |

The family size is exactly 101. Adding those fixtures increases the shared
corpus from 359 to 362. The existing `ExpressionFoundation` family remains
exactly 52 cases and keeps its current order and expected values.

For every 101-case member, the final gate compares complete rendered values:
the stage-0 result, hosted token entry twice, and hosted source entry twice.
Success/failure classification alone is not sufficient.

### Additive mixed-grammar scale profile

Generate one module with exactly 512 body forms:

- 128 signature/binding pairs whose bindings use aliases imported later in
  the same module body, producing 256 forms;
- 128 generic data declarations with unique names; and
- 128 trailing imports with unique aliases.

Including the module header, the returned flattened block contains exactly
513 statements. Two runs must both output `513`, report identical runtime
observations, perform zero host operations, and stay within all fixed ceilings:

| Observation | Maximum |
| --- | ---: |
| Evaluator transitions | 80,000,000 |
| Applications | 10,000,000 |
| List cells constructed | 500,000 |
| Maximum continuation depth | 1,100 |

The existing 512-binding expression scale profile remains unchanged and
continues to enforce its tighter ceilings.

## Implementation Batch: Types, Declarations, and Modules

### Task 0: Promote the reviewed child into the live queue

**Files:**

- Modify: `docs/superpowers/plans/2026-07-17-jazz-next-bootstrap-parser-types-declarations-modules.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/blocker-contracts.md`

**Produces:** One executor-safe `Ready Now` row aligned with the reviewed
design and this plan. No implementation file changes in this milestone.

- [x] **Step 1: Mark the approved plan executable.**

  Change only `status` from `proposed` to `ready` and `autonomous_ready` from
  `no` to `yes`. Keep the child ID, dependency, deliverable, target paths,
  verification, and plan section unchanged.

- [x] **Step 2: Promote only this child.**

  Move the current candidate from `Next Curation Target` to `Ready Now` using
  the plan frontmatter verbatim. Update the bootstrap blocker and executor
  status to name this child as active. Keep control-flow/patterns and
  operators/full parity unpromoted.

- [x] **Step 3: Validate and commit the promotion.**

  Run:

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Review the diff for queue/frontmatter equality, then commit:

  ```bash
  git add docs/execution/queue.md docs/execution/blocker-contracts.md docs/superpowers/plans/2026-07-17-jazz-next-bootstrap-parser-types-declarations-modules.md
  git commit -m "docs: promote parser declarations batch"
  ```

### Task 1: Establish the exact 101-case contract

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs`

**Produces:** A fixed, reviewable family manifest and expected stage-0
evidence before hosted support is added. The committed milestone remains green;
Task 6 activates the hosted whole-family gate after all grammar owners land.

- [x] **Step 1: Add the failing contract assertions first.**

  Assert family size `101`, shared corpus size `362`, unique family members,
  unique corpus names, complete lookup, and the intended accepted/failure
  classification of each new case before adding the new family data. Run
  `canonical-parser-comparison-spec` and confirm it fails on those missing
  contract facts rather than an unrelated parser regression.

- [x] **Step 2: Add the exact family and focused fixtures.**

  Add the `TypesDeclarationsModules` family, copy the 98 existing fixture names
  exactly from the accepted design, append the three focused fixtures in the
  specified order, and add their sources. Keep all expected values derived
  through the existing stage-0 parser and canonical renderers; do not add a
  second AST or failure serialization path.

- [x] **Step 3: Prove the corpus and stage-0 contract are green.**

  Run:

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next canonical-parser-comparison-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec jazz-parser-parity-spec --test-show-details=failures
  ```

  Expected: all three suites pass with a 362-case corpus and a valid ordered
  101-case family. Also run the existing `jazz-parser-parity-spec` unchanged to
  confirm the 52-case expression family remains green. Do not activate the new
  whole-family hosted assertion in this milestone.

- [x] **Step 4: Commit the contract.**

  ```bash
  git add jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs docs/execution/queue.md docs/superpowers/plans/2026-07-17-jazz-next-bootstrap-parser-types-declarations-modules.md
  git commit -m "test: define parser declarations parity family"
  ```

### Task 2: Isolate context and declaration ownership without changing behavior

**Files:**

- Create: `jazz-next/jazz/compiler/ParserContext.jz`
- Create: `jazz-next/jazz/compiler/ParserDeclaration.jz`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserTypesDeclarationsModulesSpec.hs`
- Modify: `jazz-next/jazz/compiler/ParserToken.jz`
- Modify: `jazz-next/jazz/compiler/ParserExpression.jz`
- Modify: `jazz-next/jazz/compiler/ParserProgram.jz`
- Modify: `jazz-next/jazz/compiler/Parser.jz`
- Modify: `jazz-next/jazz-next.cabal`

**Produces:** The accepted ownership boundaries and recursive callback seam,
with all landed expression behavior preserved and no new grammar yet.

- [x] **Step 1: Register focused tests for the new module seams.**

  Add the `jazz-parser-types-declarations-modules-spec` component. Its first
  tests must compile through the real module graph and cover:

  - `tokenRemaining` returns the untouched current suffix;
  - initial, module-body, and nested-block context transitions have the
    accepted alias inheritance/isolation behavior;
  - statement dispatch still parses landed bindings and expression statements;
  - the 52-case expression family and 512-binding scale behavior are unchanged.

  Run the new suite and record the expected failure caused by the missing
  modules/interfaces.

- [x] **Step 2: Add the token snapshot and immutable context owner.**

  Implement only the stable contracts in the interface table. Keep the token
  snapshot non-consuming. Keep context limited to statement mode and visible
  aliases; do not add operator state.

- [x] **Step 3: Move, do not duplicate, landed statement dispatch.**

  Transfer binding/expression/signature-boundary classification from
  `ParserProgram` into `ParserDeclaration`. Thread context through
  `ParserExpression`, `ParserProgram`, and the recursive callbacks while
  preserving current output and failure values. Remove the old parallel
  classifiers after their new owner is green.

- [x] **Step 4: Run refactor regression gates.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-types-declarations-modules-spec jazz-parser-component-spec jazz-parser-parity-spec jazz-parser-scale-spec parser-core-spec token-parser-spec --test-show-details=failures
  ```

  Expected: the new ownership tests, existing 52-case expression family,
  generic parser kernel, token layer, and existing scale profile pass. The
  101-case hosted whole-family gate remains intentionally inactive until Task
  6; new behavior is driven by the focused suite in the meantime.

- [x] **Step 5: Commit the behavior-preserving split.**

  ```bash
  git add jazz-next/jazz/compiler/ParserToken.jz jazz-next/jazz/compiler/ParserContext.jz jazz-next/jazz/compiler/ParserDeclaration.jz jazz-next/jazz/compiler/ParserExpression.jz jazz-next/jazz/compiler/ParserProgram.jz jazz-next/jazz/compiler/Parser.jz jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserTypesDeclarationsModulesSpec.hs jazz-next/jazz-next.cabal
  git commit -m "refactor: isolate hosted parser declaration context"
  ```

### Task 3: Add the shared signature grammar and explicit type application

**Files:**

- Create: `jazz-next/jazz/compiler/ParserSignature.jz`
- Modify: `jazz-next/jazz/compiler/ParserDeclaration.jz`
- Modify: `jazz-next/jazz/compiler/ParserExpression.jz`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserTypesDeclarationsModulesSpec.hs`

**Produces:** One type grammar shared by signature statements and expression
type application, including lossless unsupported-signature fallback.

- [ ] **Step 1: Add focused failing behavior tests.**

  Cover primitive and numeric-width types, variables and named types, adjacent
  named application, `List(a)` normalization, list/tuple/unit types,
  right-associative functions, qualified/empty/non-empty constraints, and
  retained spans. Cover unsupported `forall` fallback with every token in
  order, plus malformed supported syntax at statement boundaries.

  For explicit type application, cover primitive and applied types, chained
  application tails, adjacency, intervening whitespace, incomplete types, and
  the exact stage-0 structured failures.

- [ ] **Step 2: Implement the shared signature owner.**

  Delimit a signature payload in `ParserDeclaration`, interpret it in
  `ParserSignature`, and preserve unsupported tokens rather than rejecting the
  statement. Expose only the supported type-prefix parser to
  `ParserExpression`. Keep matching-binding checks and terminators in the
  declaration owner.

- [ ] **Step 3: Verify focused behavior and landed regressions.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-types-declarations-modules-spec jazz-parser-component-spec jazz-parser-parity-spec --test-show-details=failures
  ```

  Expected: the focused signature and explicit-type-application cases match
  stage 0; existing hosted expression parity remains green. Declaration and
  module behavior stays assigned to Tasks 4 and 5.

- [ ] **Step 4: Commit the grammar slice.**

  ```bash
  git add jazz-next/jazz/compiler/ParserSignature.jz jazz-next/jazz/compiler/ParserDeclaration.jz jazz-next/jazz/compiler/ParserExpression.jz jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserTypesDeclarationsModulesSpec.hs
  git commit -m "feat: parse hosted signatures and type applications"
  ```

### Task 4: Add data, class, and impl declarations

**Files:**

- Modify: `jazz-next/jazz/compiler/ParserDeclaration.jz`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserTypesDeclarationsModulesSpec.hs`

**Produces:** Complete stage-0 surface values and structured failures for this
child’s type/abstraction declarations, without adding later expression forms.

- [ ] **Step 1: Add failing declaration-owner tests.**

  Derive cases from the design’s 9 data, 11 class, and 5 impl family members.
  Cover success values and retained spans as well as parameter validation,
  constructor/method naming, duplicate parameters/constructors/methods,
  delimiter failures, illegal scope, impl target validation, and impl method
  expressions through the injected callback.

- [ ] **Step 2: Implement one declaration family at a time.**

  Add data, then class, then impl parsing under `ParserDeclaration`, running
  the focused suite after each family. Build only existing `ParserTypes`
  constructors. The foundational impl fixture must stay within landed
  expression grammar; do not pull control flow, patterns, or operators forward.

- [ ] **Step 3: Verify the owner and affected parity subset.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-types-declarations-modules-spec jazz-parser-parity-spec --test-show-details=failures
  ```

  Expected: the focused signature/type/data/class/impl cases match stage 0.
  Module/import/export behavior remains assigned to Task 5.

- [ ] **Step 4: Commit the declaration milestone.**

  ```bash
  git add jazz-next/jazz/compiler/ParserDeclaration.jz jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserTypesDeclarationsModulesSpec.hs
  git commit -m "feat: parse hosted type declarations"
  ```

### Task 5: Add modules, imports, exports, and scoped forward aliases

**Files:**

- Modify: `jazz-next/jazz/compiler/ParserContext.jz`
- Modify: `jazz-next/jazz/compiler/ParserDeclaration.jz`
- Modify: `jazz-next/jazz/compiler/ParserProgram.jz`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserTypesDeclarationsModulesSpec.hs`

**Produces:** Stage-0-compatible module placement, flattened module bodies,
imports/exports, and depth-aware alias visibility with no cross-scope leakage.

- [ ] **Step 1: Add failing scope and module tests.**

  Cover the design’s 22 import/alias and 27 module/export members, including:

  - module header first-position enforcement and nested-module rejection;
  - flattened header/body statement order;
  - module paths, empty/explicit export lists, namespace selectors, constructor
    selectors, and duplicate exports;
  - imports with optional aliases and explicit symbol lists;
  - alias/name conflicts, duplicate aliases, idempotent registration, and
    alias-qualified references;
  - forward alias visibility within one statement-list scope; and
  - fresh module-body aliases plus inherited nested-block aliases, with no
    leakage between sibling or enclosing scopes.

- [ ] **Step 2: Implement depth-aware alias pre-collection.**

  Scan only the current statement-list depth, stopping at end-of-input or the
  matching right brace as appropriate. Collect `import ... as alias` without
  consuming parser input. Seed the current immutable context once before
  sequential parsing and preserve source-order output.

- [ ] **Step 3: Implement module/import/export parsing and orchestration.**

  Keep syntax, duplicate validation, and scope checks in `ParserDeclaration`.
  Keep sequencing, module-first state, recursive context construction, and
  flattened output in `ParserProgram`. Remove any temporary duplicate logic
  once the owner-specific tests pass.

- [ ] **Step 4: Verify focused and family behavior.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-types-declarations-modules-spec jazz-parser-parity-spec jazz-parser-component-spec --test-show-details=failures
  ```

  Expected: the focused module/import/export cases match stage 0 through both
  entry paths and the grammar slice is complete. Task 6 activates the exact
  whole-family proof. Do not relax a failure payload or exclude a case to make
  the focused gate green.

- [ ] **Step 5: Commit the module milestone.**

  ```bash
  git add jazz-next/jazz/compiler/ParserContext.jz jazz-next/jazz/compiler/ParserDeclaration.jz jazz-next/jazz/compiler/ParserProgram.jz jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserTypesDeclarationsModulesSpec.hs
  git commit -m "feat: parse hosted modules and imports"
  ```

### Task 6: Close exact hosted parity over both named families

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParity.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParitySpec.hs`
- Potentially modify, only for a demonstrated mismatch: `jazz-next/jazz/compiler/ParserSignature.jz`
- Potentially modify, only for a demonstrated mismatch: `jazz-next/jazz/compiler/ParserContext.jz`
- Potentially modify, only for a demonstrated mismatch: `jazz-next/jazz/compiler/ParserDeclaration.jz`
- Potentially modify, only for a demonstrated mismatch: `jazz-next/jazz/compiler/ParserExpression.jz`
- Potentially modify, only for a demonstrated mismatch: `jazz-next/jazz/compiler/ParserProgram.jz`
- Potentially modify, only for a demonstrated mismatch: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserTypesDeclarationsModulesSpec.hs`

**Produces:** Exact, deterministic token/source parity for the 101-case child
family while retaining the existing 52-case regression family unchanged.

- [ ] **Step 1: Make repetition and entry-point coverage explicit.**

  For each family, obtain the stage-0 expected rendering once, then run hosted
  token entry twice and hosted source entry twice. Assert equality within each
  entry point and exact equality with stage 0, including complete AST/failure
  payloads and spans.

- [ ] **Step 2: Classify every mismatch by owner.**

  For any discrepancy, add the smallest reproducer to the focused suite,
  correct only the owning module, and rerun the affected family. Do not add
  fixture exceptions, rendering normalization, or acceptance-only comparison.

- [ ] **Step 3: Run the parity closure gate.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-types-declarations-modules-spec jazz-parser-component-spec jazz-parser-parity-spec canonical-parser-comparison-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec --test-show-details=failures
  ```

  Expected: 101 declaration/module fixtures and 52 expression fixtures pass
  both entries twice with exact deterministic values; the 362-case shared
  corpus and lexer gates remain green.

- [ ] **Step 4: Commit parity closure.**

  Stage the parity files, the focused regression test, and only parser owner
  files actually corrected, then commit:

  ```bash
  git commit -m "test: prove parser declarations parity"
  ```

### Task 7: Prove the additive mixed-grammar scale profile

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScale.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleSpec.hs`

**Produces:** Deterministic semantic work/stack evidence for forward aliases,
signatures, bindings, data declarations, imports, and flattened module output.

- [ ] **Step 1: Add the failing 513-statement scale contract.**

  Add a separate generated-case entry point and assertions for the exact shape,
  output, repetition, host-operation count, and all four ceilings. Keep the
  existing 512-binding generator and assertions untouched.

- [ ] **Step 2: Implement the generator and ordinary runtime path.**

  Generate only grammar owned by this child and run through the same ordinary
  Jazz compiler/module graph and runtime observation mechanism as the landed
  scale case. The generator must make names unique and make forward-alias use
  observable in the parsed structure.

- [ ] **Step 3: Run scale and warning-clean gates.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-scale-spec --test-show-details=failures
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  ```

  Expected: both scale profiles pass twice with deterministic observations and
  zero host operations. Do not raise a ceiling without profiling evidence and
  a design review.

- [ ] **Step 4: Commit scale evidence.**

  ```bash
  git add jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScale.hs jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleSpec.hs
  git commit -m "test: prove parser declarations scale"
  ```

### Task 8: Run full verification and close the child

**Files:**

- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/done-archive.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md`
- Modify: `docs/superpowers/specs/2026-07-12-jazz-next-bootstrap-jazz-parser-design.md`
- Modify: `docs/superpowers/specs/2026-07-17-jazz-next-bootstrap-parser-types-declarations-modules-design.md`
- Modify: `docs/superpowers/plans/2026-07-17-jazz-next-bootstrap-parser-types-declarations-modules.md`
- Modify: `jazz-next/README.md`

**Produces:** Full current-repository verification evidence, archived child
status, and one unpromoted control-flow/patterns curation target.

- [ ] **Step 1: Run focused verification from a clean milestone.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-types-declarations-modules-spec jazz-parser-component-spec jazz-parser-parity-spec canonical-parser-comparison-spec parser-core-spec token-parser-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec repository-audit-spec --test-show-details=failures
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-scale-spec --test-show-details=failures
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  ```

- [ ] **Step 2: Run full package and metadata verification.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
  nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
  ```

  The warning-clean build and `repository-audit-spec` are the current owners of
  the responsibilities formerly covered by deleted compatibility scripts.

- [ ] **Step 3: Confirm scope and cleanliness before documentation claims.**

  ```bash
  git diff --check
  git status --short
  git diff --name-only HEAD~7..HEAD
  ```

  Review the file list directly. Confirm no file under `jazz-hs/`, `jazz2/`,
  `ParserCore.jz`, or `ParserTypes.jz` changed. If milestone count differs
  because adjacent commits were safely combined, compare against the actual
  pre-implementation promotion commit rather than assuming `HEAD~7`.

- [ ] **Step 4: Record exact landed evidence and curate the next child.**

  Mark this plan and design done with actual verification and scale statistics.
  Remove its `Ready Now` row and archive it in `done-archive.md`. Update the
  bootstrap blocker, parent designs, and README with only landed facts. Make
  control-flow/patterns the sole `Next Curation Target`; keep operators/full
  parity ordered behind it and unpromoted. Do not claim full parser parity or
  stage-1 bootstrap readiness.

- [ ] **Step 5: Validate documentation and commit closeout.**

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Then stage the closeout docs and commit:

  ```bash
  git commit -m "docs: close parser declarations batch"
  ```

## Completion Gate

This child is complete only when all of the following are true:

- The accepted stage-0 ownership split is present and tested through the real
  Jazz module graph.
- `ParserCore.jz` and `ParserTypes.jz` are unchanged.
- Unsupported signatures preserve every payload token in order and remain
  parser successes.
- Explicit type application and signature statements use one shared type
  grammar.
- Alias pre-collection is depth-aware, forward-visible within scope, and does
  not leak across module or block boundaries.
- Data, class, impl, module, import, and export forms preserve complete surface
  values and exact structured failures.
- All 101 `TypesDeclarationsModules` fixtures match stage 0 through token and
  source entry twice; all 52 `ExpressionFoundation` fixtures remain unchanged
  and green.
- The 513-statement mixed profile and existing 512-binding profile are
  deterministic, host-operation-free, and below every fixed ceiling.
- The development build, focused suites, full Cabal suite, Cabal metadata,
  repository audit, queue/docs checks, and whitespace check pass.
- No control-flow, pattern, operator, recovery, full-corpus, lowering, backend,
  native-runtime, or legacy-reference change entered the child.
- The child is archived and control-flow/patterns is curated but not promoted.
