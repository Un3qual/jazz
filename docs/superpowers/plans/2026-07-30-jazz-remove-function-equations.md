# Jazz Function-Equation Removal Implementation Plan

> **Status:** Completed for named Haskell-style equation removal. The
> instruction below to rewrite different-body alternatives as explicit `case`
> was superseded on `2026-07-30` by
> [`2026-07-30-jazz-pattern-lambda-clauses.md`](2026-07-30-jazz-pattern-lambda-clauses.md).
> Named equations remain removed; ordered multi-body dispatch now uses the
> expression-level `\|` clause form.
> **For agentic workers:** Execute each task with red-green TDD and commit
> coherent milestones. Do not modify `jazz-hs/` or `jazz2/`.

**Goal:** Remove Haskell-style function equations while retaining Jazz's
pre-existing pattern-lambda function heads, including multiple parameters and
top-level or-pattern alternatives.

**Architecture:** Ordinary `SSLet` bindings whose values are lambda
expressions remain the only function-definition surface. Pattern lambda
parameters continue to lower through the existing pattern-case path.
Equation-only AST, parser, lowering, generated names, and bootstrap-mirror
types are deleted. This plan originally used explicit `case` as the only
different-body replacement; the superseding clause-lambda plan adds
expression-level `SEPatternLambda` without restoring named equations.

**Tech Stack:** Haskell 2010, Jazz-authored bootstrap frontend modules, Cabal
test suites, repository source/feature audits.

## Constraints

- Preserve `f = \(pattern1, pattern2) -> body.`.
- Preserve `f = \(Just x | Also x, fallback) -> body.`.
- Preserve ordinary `case` patterns and semantics.
- Reject `f pattern = body.` rather than silently interpreting it differently.
- Migrate every active `.jz` source to retained syntax.
- Remove dead equation representations from both frontend implementations.
- Keep historical documents, but mark their equation decisions superseded.

### Task 1: Lock the Language Boundary with Failing Tests

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Parser/DeclarationParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`

- [ ] Add a declaration-parser test that requires `length [] = 0.` and a
  multi-clause equation group to be rejected.
- [ ] Add or strengthen parser assertions for several pattern-lambda
  parameters and a top-level lambda or-pattern followed by another parameter.
- [ ] Add a runtime test whose ordinary lambda uses an explicit multi-arm
  `case`, proving the retained replacement handles different bodies.
- [ ] Run `declaration-parser-spec`, `lambda-parser-spec`, and
  `lambda-semantics-spec`.
- [ ] Confirm the new rejection assertion fails because equation syntax is
  still accepted, while the preservation tests pass.

### Task 2: Remove Hosted Equation Parsing and Lowering

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Pattern.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Failure.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Name.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Force.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference/Scope.hs`
- Delete: `jazz-next/test/JazzNext/Compiler/Parser/FunctionEquationParserSpec.hs`
- Delete: `jazz-next/test/JazzNext/Compiler/Semantics/FunctionEquationSemanticsSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

- [ ] Delete `SurfaceFunctionClause` and `SSFunction`.
- [ ] Delete equation lookahead, contiguous grouping, clause parsing, and the
  equation-specific head-pattern parser.
- [ ] Delete equation lowering and `FunctionEquationArgument`.
- [ ] Remove equation-only traversal, resolution, recursive-scope, diagnostic,
  and forcing branches.
- [ ] Remove the dedicated equation test suites and Cabal registrations.
- [ ] Run the Task 1 tests and all directly affected parser/type/runtime suites.
- [ ] Commit the hosted removal once the rejection/preservation boundary is
  green.

### Task 3: Remove the Jazz-Authored Bootstrap Mirror

**Files:**

- Modify: `jazz-next/jazz/compiler/ParserTypes.jz`
- Modify: `jazz-next/jazz/compiler/ParserPattern.jz`
- Modify: `jazz-next/jazz/compiler/ParserDeclaration.jz`
- Modify: `jazz-next/jazz/compiler/CoreTypes.jz`
- Modify: `jazz-next/jazz/compiler/CoreLower.jz`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparison.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalCoreComparison.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalCoreComparisonSpec.hs`

- [ ] Delete the Jazz-authored function-clause schema, lookahead/group parser,
  equation head parser, lowering branch, and generated-name variant.
- [ ] Delete equation-only hosted/Jazz canonical comparison adapters and cases.
- [ ] Keep the lambda-parameter parser and its pattern/or-pattern branches
  unchanged except for necessary import/export cleanup.
- [ ] Run the canonical parser/core comparison suites and Jazz parser scale
  suites.
- [ ] Commit hosted/Jazz parity cleanup.

### Task 4: Migrate Every Authored Jazz Source

**Files:**

- Modify: equation-using files under `jazz-next/jazz/`,
  `jazz-next/programs/`, `jazz-next/test/fixtures/`, and editor fixtures.
- Modify: embedded Jazz sources under `jazz-next/test/`.
- Modify: `jazz-next/test/JazzNext/Repository/FeatureInventory.hs`

- [ ] Inventory equation nodes before deleting the parser, plus textual and
  embedded-source uses.
- [ ] Convert single-head functions to ordinary pattern lambdas.
- [ ] Convert same-body compatible alternatives to the retained lambda
  or-pattern syntax where it improves the source.
- [ ] Superseded: convert different-body alternatives to ordered `\|` clause
  lambdas when the complete function head is being matched; retain explicit
  `case` for computed or nested scrutinees.
- [ ] Preserve existing `$`, sections, patterns, aliases, annotations, and
  other implemented features where they remain clear and useful.
- [ ] Replace equation feature-inventory entries with separate coverage for
  multiple pattern-lambda parameters, lambda or-patterns, and explicit
  multi-arm case dispatch.
- [ ] Run the authored-source, source-format, feature-inventory, program-corpus,
  standard-library, and editor-fixture suites.
- [ ] Commit the source migration.

### Task 5: Reconcile Current Documentation

**Files:**

- Modify: `docs/execution/queue.md`
- Modify: `docs/superpowers/specs/2026-07-28-jazz-pre-bootstrap-language-quality-design.md`
- Modify: equation-related current implementation plans as needed.
- Modify: current language, benchmark, and readiness documentation found by
  repository search.

- [ ] Remove function equations from active language and readiness claims.
- [ ] Mark the earlier equation decision and implementation plan superseded by
  the 2026-07-30 removal design without rewriting history.
- [ ] Document ordinary pattern lambdas, ordered `\|` clause lambdas, and
  intentional explicit `case` as the canonical forms.
- [ ] Search active source and documentation for stale equation claims.
- [ ] Commit the documentation reconciliation.

### Task 6: Full Verification and Anti-Slop Review

- [ ] Run `git diff --check`.
- [ ] Search production code for all removed equation identifiers.
- [ ] Search active `.jz` and embedded sources for equation syntax.
- [ ] Run the full Jazz test matrix with `cabal test --project-dir=jazz-next
  all --test-show-details=failures` in the repository's development shell.
- [ ] Review the complete diff for dead compatibility branches, accidental
  pattern-lambda removal, unrelated churn, and inconsistent Jazz formatting.
- [ ] If cleanup changes are needed, rerun the affected focused suites and the
  full matrix.
- [ ] Commit the final verified cleanup.
