---
id: JN-BOOTSTRAP-JAZZ-PARSER-EXPRESSION-FOUNDATION-001
status: done
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-JAZZ-PARSER-FOUNDATION-001
last_verified: 2026-07-16
completed_on: 2026-07-16
plan_section: "Implementation Batch: Expression Foundation"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-12-jazz-next-bootstrap-jazz-parser-design.md
  - docs/superpowers/specs/2026-07-16-jazz-next-bootstrap-parser-expression-foundation-design.md
  - docs/superpowers/plans/2026-07-16-jazz-next-bootstrap-parser-expression-foundation.md
  - jazz-next/README.md
  - jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs
  - jazz-next/jazz-next.cabal
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-component-spec jazz-parser-parity-spec canonical-parser-comparison-spec parser-core-spec token-parser-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec repository-audit-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-scale-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Implement the first Jazz-authored grammar slice over canonical tokens, covering foundational expressions, ordinary statements, recursive blocks, complete programs, and separate token/source facades with exact structured stage-0 parity over a stable named fixture family."
---

# Jazz-Next Bootstrap Parser Expression Foundation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` to implement this plan task-by-task. This plan
> is intentionally outcome-oriented: it fixes behavior, interfaces, tests,
> commands, and review boundaries without pasting the final implementation.

**Goal:** Implement the first Jazz-authored grammar slice with exact stage-0
parity for foundational expressions, ordinary statements, complete programs,
and distinct token/source façades.

**Architecture:** A compiler-local `ParserToken` layer specializes the generic
`ParserCore` for canonical Jazz tokens and structured grammar failures.
`ParserExpression` owns primary expressions and application, `ParserProgram`
owns statements and recursive block/program sequencing, and a small `Parser`
façade returns the fixed `ParserTypes` results. A named fixture family and
reusable Haskell harness compare both entry points through the real Jazz module
graph.

**Tech Stack:** GHC 9.14.1, Haskell 2010 plus project-local extensions,
`MultilineStrings`, ordinary Jazz `.jz` modules, the existing Jazz evaluator
and module graph, runtime observation statistics, the canonical lexer/parser
comparison adapters, Cabal test components, and the Nix-pinned development
environment.

**Design checkpoint:**
[`2026-07-16-jazz-next-bootstrap-parser-expression-foundation-design.md`](../specs/2026-07-16-jazz-next-bootstrap-parser-expression-foundation-design.md)

## Global Constraints

- Modify compiler implementation only under `jazz-next/`; `jazz-hs/` and
  `jazz2/` remain read-only references.
- Keep `ParserCore.jz` grammar-neutral and `ParserTypes.jz` as the accepted
  surface/result/failure schema. The implemented child made one narrow
  foundation-boundary correction after explicit imports exposed a genuine
  abstraction defect: `ParserCore` now exports the existing `Parser`
  constructor plus typed failure offset/problem accessors. Its semantics and
  the `ParserTypes` schema remain unchanged.
- Keep the parser fail-fast. Add no recovery, partial AST, multiple-error
  accumulation, synchronization token, or presentation-string comparison.
- Consume canonical lexer tokens at the primary boundary. The source façade
  must preserve success, lexical failure, and parser failure as distinct ADTs.
- Add no parser-specific builtin, Haskell callback, mutable token buffer,
  private host bridge, custom serializer, or public stdlib parser API.
- Cover only literals, ordinary and qualified names, grouping, unit, tuples,
  lists, application, blocks, ordinary bindings, expression statements, and
  complete program sequencing.
- Keep signatures, explicit type application, declarations, modules/imports,
  lambdas, control flow, patterns, operators, lowering, canonical core, and
  backend work outside this child.
- Preserve normalized integer text and source-exact fractional components.
  Validate Float64 magnitude with ordinary Jazz `Text` and `Char` services;
  do not add a numeric parsing primitive.
- Keep lists, tuples, applications, statements, and blocks in source order
  while accumulating internally in a traversal-safe direction.
- Use explicit Jazz import lists at the parser-module boundaries. Do not import
  both `ParserCore.ParserFailure` and `ParserTypes.ParserFailure` into one
  module.
- Use `MultilineStrings` for fixed Jazz programs embedded in Haskell. Dynamic
  fixture values may be inserted through one shared renderer/template;
  explicit escaped strings are limited to whitespace, adjacency, span, and
  line-ending cases.
- Prefer behavior tests over source-text assertions. Architectural tests must
  compile and execute through the real module graph.
- Treat runtime semantic counts as deterministic regression evidence. Treat
  wall-clock time and physical allocation as review evidence, not portable
  CI thresholds.
- Keep all checked-in `.jz` modules at exactly two spaces per indentation
  level and commit after each independently reviewable task.

---

## File and Responsibility Map

| File | Responsibility |
| --- | --- |
| `jazz-next/jazz/compiler/ParserToken.jz` | Canonical-token specialization of `ParserCore`, grammar failure envelope, token/span/adjacency helpers, and complete-run conversion. |
| `jazz-next/jazz/compiler/ParserExpression.jz` | Foundational primary expressions, source-exact numerics, lists, tuples, grouping, qualification, and left-associated application. |
| `jazz-next/jazz/compiler/ParserProgram.jz` | Ordinary binding/expression statements, block termination, top-level sequencing, and the recursive block/expression connection. |
| `jazz-next/jazz/compiler/Parser.jz` | Public compiler façade for complete token and source parsing; no grammar alternatives. |
| `jazz-next/jazz/compiler/ParserCore.jz` | Generic consumption, choice, progress, and cursor contract; narrowly exposes the existing parser wrapper and typed failure inspection needed by explicit imports. |
| `jazz-next/jazz/compiler/ParserTypes.jz` | Read-only dependency in this child; fixed surface AST and canonical parser/source results. |
| `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs` | Fixed 350-case corpus, named parser-family metadata, explicit expression-family membership, and manifest validation. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs` | Test-only canonical token runtime encoding reused by token-entry parity. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs` | Shared-corpus ordering and stage-0 classification assertions for the added semantic fixtures. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs` | Family-manifest validation and fixed-corpus regression coverage. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserComponentSpec.hs` | Real-module-graph tests for the token, expression, and program owners before full façade parity. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParity.hs` | Reusable stage-0 expected-value construction plus hosted token/source batch execution. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParitySpec.hs` | Exact family parity, façade phase separation, and repeated determinism. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScale.hs` | Generated hosted-parser scale program execution through runtime observation. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleSpec.hs` | Isolated 512-binding deterministic work/stack regression evidence. |
| `jazz-next/jazz-next.cabal` | Registers the parser component, parity, and scale suites and their test-only support modules. |
| Active docs named in Task 6 | Promotion, status, completion evidence, and next-child curation. |

The implementation split the expensive scale harness from fast parity support;
it did not add another production parser owner. `ParserTypes.jz` remained
unchanged. The narrow `ParserCore.jz` export/accessor correction is recorded as
the focused foundation defect discovered by explicit-import implementation.

## Stable Interfaces Between Tasks

- `ParserToken` owns an internal grammar-failure value containing the optional
  selected span and a `ParserFailureReason`. It also owns a complete-run result
  that distinguishes success from ordinary grammar failure and kernel invariant
  failure. These names remain compiler-private.
- `ParserToken` is the only new module that imports the kernel failure/reply
  constructors. It does not import the public `ParserTypes.ParserFailure`
  constructor.
- `ParserExpression` exposes one foundational expression parser parameterized
  by a block parser. The parameter breaks the expression/program import cycle;
  no test-only expression grammar is added.
- `ParserProgram` ties the recursive parser knot and exposes one complete
  program parser producing `BlockExpression [SurfaceStatement]`.
- `Parser` exposes `parseTokens` and `parseSource`. It imports the complete-run
  result from `ParserToken` and the public result constructors from
  `ParserTypes`, but it does not import `ParserCore`.
- `parseTokens` accepts `CanonicalSourcePath` plus `[CanonicalToken]` and
  returns `CanonicalParserResult` after requiring complete consumption.
- `parseSource` accepts `CanonicalSourcePath` plus `Text`, calls `Lexer.lexSource`,
  and returns `CanonicalSourceResult` without flattening either failure phase.
- The test manifest exposes an `ExpressionFoundation` family, its ordered
  member names, a validated lookup, and explicit validation violations for
  duplicate corpus names, duplicate family members, and missing members.
- `JazzParserParity` renders expected stage-0 values only through the existing
  canonical adapters and `renderRuntimeValue`; it does not duplicate AST or
  failure serialization.

## Fixed Expression-Foundation Fixture Family

The family contains these existing stable fixtures:

| Coverage | Existing fixture names |
| --- | --- |
| Source normalization and spans | `lexer-leading-zero-integer`, `lexer-crlf-spans`, `lexer-unicode-and-escape-values`, `lexer-all-supported-escapes`, `parser-corpus-0001`, `parser-corpus-0234` |
| Source façade lexical failure | `lexer-unexpected-character` |
| Unit, tuple, list/application, and qualification | `parser-corpus-0024`, `parser-corpus-0028`, `parser-corpus-0036`, `parser-corpus-0193`, `parser-corpus-0194`, `parser-corpus-0206`, `parser-corpus-0310` |
| Ordinary identifier/binding behavior | `parser-corpus-0051`, `parser-corpus-0182`, `parser-corpus-0214`, `parser-corpus-0233`, `parser-corpus-0236` |
| Blocks and sequencing | `parser-corpus-0237`, `parser-corpus-0308`, `parser-corpus-0312` |
| Numeric fidelity and boundaries | `parser-corpus-0032`, `parser-corpus-0240`, `parser-corpus-0241`, `parser-corpus-0309` |

Add exactly these semantically named fixtures to fill uncovered boundaries:

| New stable fixture name | Required behavior |
| --- | --- |
| `expression-foundation-empty-program` | Accept an empty token stream as an empty top-level block. |
| `expression-foundation-empty-block` | Accept an empty block expression statement. |
| `expression-foundation-grouped-name` | Preserve grouping without adding an AST wrapper. |
| `expression-foundation-empty-list` | Produce an empty `ListExpression`. |
| `expression-foundation-list-literals` | Preserve a heterogeneous literal list in source order. |
| `expression-foundation-parenthesized-application` | Apply a grouped function expression to a following argument. |
| `expression-foundation-list-missing-close` | Match stage 0 when a list reaches a statement boundary without `]`. |
| `expression-foundation-list-trailing-comma` | Match stage 0 when a comma has no following list element. |
| `expression-foundation-tuple-missing-close` | Match stage 0 when a tuple reaches a statement boundary without `)`. |
| `expression-foundation-tuple-trailing-comma` | Match stage 0 when a comma has no following tuple element. |
| `expression-foundation-binding-missing-rhs` | Remain committed to a binding after `=` and report the missing expression. |
| `expression-foundation-binding-missing-dot` | Report end of input while requiring the binding terminator. |
| `expression-foundation-expression-missing-dot` | Report end of input while requiring the expression-statement terminator. |
| `expression-foundation-qualified-missing-member` | Report the exact missing-member failure immediately after `::`. |
| `expression-foundation-qualified-whitespace` | Preserve the immediate-adjacency rule and fail at the spaced qualifier boundary. |
| `expression-foundation-dot-without-expression` | Report the exact unexpected-dot expression failure. |
| `expression-foundation-max-float64` | Accept the exact maximum finite Float64 decimal boundary paired with the existing one-unit-over rejection. |

The 26 existing members plus 17 new fixtures produce a 43-case
`ExpressionFoundation` family and increase the shared corpus from 333 to 350
cases. Historical completion evidence that truthfully records the earlier
333-case milestone remains unchanged.

## Implementation Batch: Expression Foundation

### Task 0: Promote the reviewed plan into the live queue

**Files:**

- Modify: `docs/superpowers/plans/2026-07-16-jazz-next-bootstrap-parser-expression-foundation.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/blocker-contracts.md`

**Produces:** One executor-safe `Ready Now` row whose frontmatter, target
paths, deliverable, and verification exactly match this reviewed plan.

- [x] **Step 1: Add ready frontmatter after written plan approval.**

  Use id `JN-BOOTSTRAP-JAZZ-PARSER-EXPRESSION-FOUNDATION-001`, priority `P1`,
  size `L`, kind `impl`, `autonomous_ready: yes`, dependency
  `JN-BOOTSTRAP-JAZZ-PARSER-FOUNDATION-001`, and plan section
  `Implementation Batch: Expression Foundation`. Per the execution-queue
  contract, frontmatter names only concrete files that already exist and this
  plan modifies. The new files remain fixed in the responsibility map and task
  file lists above; keep the locked foundation files out of target paths.

- [x] **Step 2: Promote only the expression child.**

  Move it from `Next Curation Target` to `Ready Now`. Update the executor status
  and bootstrap blocker to say the reviewed expression slice is active.
  Types/declarations/modules, control flow/patterns, and operators/full parity
  remain unpromoted.

- [x] **Step 3: Validate and commit the promotion.**

  Run:

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  ```

  Expected: all checks pass. Commit as
  `docs: promote parser expression foundation`.

### Task 1: Lock the named fixture family and manifest validation

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs`

**Produces:** A validated, explicitly ordered 43-case family backed by the
fixed 350-case shared corpus, without positional selection or fixture renames.

- [x] **Step 1: Add RED manifest behavior tests.**

  Cover duplicate global fixture names, duplicate family membership, unknown
  member names, stable declared order, the exact 43-member family, and the
  presence of accepted, parser-rejected, and lexically rejected source
  outcomes. Keep validation generic enough for later parser families.

- [x] **Step 2: Run the canonical parser suite RED.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next canonical-parser-comparison-spec --test-show-details=failures
  ```

  Expected: failure because family metadata, validation, and the 17 semantic
  fixtures do not exist.

- [x] **Step 3: Add the family manifest and fixtures.**

  Preserve all existing fixture names and order. Append the 17 semantic cases,
  add explicit family membership using the names in this plan, return all
  validation violations in deterministic order, and update the fixed corpus
  count to 350. Do not infer family membership from source text, expectation,
  or numeric prefixes.

- [x] **Step 4: Run corpus and lexer/parser comparison GREEN.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next canonical-parser-comparison-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec --test-show-details=failures
  ```

  Expected: all three suites pass over the expanded corpus; the Jazz lexer
  remains byte-identical for all 350 cases.

- [x] **Step 5: Commit the stable family.**

  Stage only the fixture manifest and canonical comparison spec. Commit as
  `test: define parser expression fixture family`.

### Task 2: Add the canonical-token parser boundary

**Files:**

- Create: `jazz-next/jazz/compiler/ParserToken.jz`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserComponentSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Consumes:** `ParserCore` consumption/progress semantics, `LexerTypes`
canonical tokens, and selected `ParserTypes` reason/encountered/span types.

**Produces:** Reusable token primitives, an internal grammar-failure envelope,
and complete-run conversion that later grammar owners can use without importing
both `ParserFailure` constructors.

- [x] **Step 1: Register RED token-boundary component tests.**

  Through the real module graph, cover successful predicate/token consumption,
  identifier and punctuation matching, current-token and end-of-input failures,
  raw lexeme/span preservation, immediate versus spaced adjacency, committed
  consumption, trailing-token rejection, and nonrecoverable zero-progress
  conversion. Add an import-boundary case that would expose ambiguous
  `ParserFailure` ownership.

- [x] **Step 2: Run the component and repository suites RED.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-component-spec repository-audit-spec --test-show-details=failures
  ```

  Expected: failure because `ParserToken.jz` and its exported boundary do not
  exist.

- [x] **Step 3: Implement the specialized token boundary.**

  Build solely from `ParserCore` combinators and canonical token values. Keep
  the internal failure envelope separate from public `ParserTypes.ParserFailure`.
  Use explicit imports so the token layer sees kernel reply/failure constructors
  but not the public failure constructor. Map zero progress to
  `InternalParserFailure TokenStreamParseFailure` without making it recoverable.

- [x] **Step 4: Run token-boundary GREEN.**

  Re-run the Step 2 command and also run `parser-core-spec`. Expected: all
  component, kernel, and layering tests pass with `ParserCore.jz` unchanged.

- [x] **Step 5: Commit the token boundary.**

  Stage `ParserToken.jz`, the component spec, and Cabal registration. Commit as
  `feat: add Jazz parser token boundary`.

### Task 3: Implement foundational expressions

**Files:**

- Create: `jazz-next/jazz/compiler/ParserExpression.jz`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparison.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserComponentSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Consumes:** `ParserToken` primitives and internal failure contract, the fixed
surface literal/expression types, and an injected block parser.

**Produces:** A block-parameterized expression parser for every expression form
accepted by this child.

- [x] **Step 1: Add RED expression component tests.**

  Compare ordinary Jazz expression results against the existing Haskell
  canonical adapter for integers, the exact maximum Float64 decimal and the
  one-unit-over failure, suffixes, booleans, characters, text, ordinary and
  qualified names, grouping, unit, lists, tuples, and multi-argument
  application. Include missing/trailing delimiters and qualifier adjacency.
  Inject a failing block parser for non-block component tests rather than
  adding a test-only production entry point.

- [x] **Step 2: Run expression component tests RED.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-component-spec canonical-parser-comparison-spec --test-show-details=failures
  ```

  Expected: new expression cases fail because `ParserExpression.jz` is absent.

- [x] **Step 3: Implement scalar and name expressions.**

  Preserve normalized integer text, exact fractional whole/fractional text,
  supported suffixes, and existing boolean/character/text values. Implement
  Float64 boundary comparison with ordinary text length, character comparison,
  and traversal. Preserve the immediate `name::member` rule and exact
  structured failures.

- [x] **Step 4: Implement composite primaries and application.**

  Add grouping, unit, list, tuple, and injected block handling. Reject trailing
  commas and missing delimiters exactly like stage 0. Build left-associated
  application iteratively/tail-recursively and preserve collection element
  order without repeated append of growing prefixes.

- [x] **Step 5: Run expression and boundary suites GREEN.**

  Re-run the Step 2 command plus `parser-core-spec`, `token-parser-spec`, and
  `repository-audit-spec`. Expected: all pass with no new builtin or foundation
  schema change.

- [x] **Step 6: Commit foundational expressions.**

  Stage the expression module, canonical token test adapter export, component
  tests, and Cabal metadata. Commit as
  `feat: parse foundational Jazz expressions`.

### Task 4: Add program sequencing and both complete façades

**Files:**

- Create: `jazz-next/jazz/compiler/ParserProgram.jz`
- Create: `jazz-next/jazz/compiler/Parser.jz`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParity.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParitySpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserComponentSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Consumes:** The block-parameterized expression parser, token complete-run
contract, Jazz lexer, fixed parser/source results, the 43-case family, and the
canonical Haskell adapters.

**Produces:** Complete `parseTokens` and `parseSource` behavior plus reusable
token/source family parity infrastructure.

- [x] **Step 1: Add RED program and façade tests.**

  Extend component coverage for empty/populated programs, ordinary bindings,
  expression statements, nested/empty blocks, block application, missing
  right-hand sides, missing dots, end of input inside a block, and the binding
  commit point. Register the parity suite and require separate token/source
  batch results for the named family.

- [x] **Step 2: Run component and parity suites RED.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-component-spec jazz-parser-parity-spec --test-show-details=failures
  ```

  Expected: failure because program recursion and the public façades do not
  exist.

- [x] **Step 3: Implement statements and recursive blocks.**

  Tie the expression/block recursion in `ParserProgram`. Recognize a binding
  only from the ordinary identifier-plus-equals shape, commit after `=`, and
  require statement terminators. Accumulate statements in reverse and restore
  source order once at each completed block/program boundary.

- [x] **Step 4: Implement the token and source façades.**

  Keep `Parser.jz` free of grammar alternatives and `ParserCore` imports. Map
  the token layer's internal complete result to `CanonicalParserResult`, add
  error code `E0001` at that public boundary, and compose `Lexer.lexSource` into
  the three-way `CanonicalSourceResult` without invoking parsing after lexical
  failure.

- [x] **Step 5: Implement reusable exact parity batches.**

  For lexically valid fixtures, inject the exact canonical stage-0 token list
  and compare `CanonicalParserResult`. For every family fixture, compare the
  complete source result. Compare full structured runtime rendering, preserve
  manifest order, and run each batch twice. Do not compare only the
  `ParserFixtureExpectation` summary.

- [x] **Step 6: Run complete focused parity GREEN.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-component-spec jazz-parser-parity-spec canonical-parser-comparison-spec parser-core-spec token-parser-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec repository-audit-spec --test-show-details=failures
  ```

  Expected: exact stage-0 parity for both entry points; byte-identical repeated
  batches; all foundation and layering suites pass.

- [x] **Step 7: Commit complete expression-foundation parsing.**

  Stage the program/façade modules, reusable parity harness/spec, component
  updates, and Cabal metadata. Commit as
  `feat: complete Jazz parser expression foundation`.

### Task 5: Prove traversal scale and deterministic runtime work

**Files:**

- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScale.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`
- Modify only if the new evidence demonstrates an owning defect:
  `jazz-next/jazz/compiler/ParserExpression.jz`
- Modify only if the new evidence demonstrates an owning defect:
  `jazz-next/jazz/compiler/ParserProgram.jz`

**Produces:** Regression evidence that the grammar does not introduce prefix
append, remainder copying, nondeterminism, or host-stack growth.

- [x] **Step 1: Add the large generated program case.**

  Generate 512 sequential ordinary bindings whose right-hand sides combine an
  application, list, tuple, integer, boolean, and text literal, followed by one
  expression statement. Parse through the source façade and inspect the
  structured block length rather than relying on a rendered megabyte-scale AST
  as the only success signal.

- [x] **Step 2: Add deterministic observation assertions.**

  Run the same module graph twice with `RuntimeObservationStatistics`. Require
  identical output, successful termination, identical statistics, no host
  operations, and reviewed explicit ceilings for evaluator transitions,
  applications, list-cell construction, and maximum continuation depth.

  Establish the checked-in ceilings from two identical completed runs, round
  them upward once to a documented review margin, and record the observed
  values in this plan's completion evidence. A later ceiling change requires a
  performance explanation rather than an automatic baseline rewrite.

- [x] **Step 3: Run the scale case RED or expose missing behavior.**

  Run `jazz-parser-scale-spec` alone. Expected before final traversal fixes:
  the new case either fails its success/stack condition or exposes an
  unexplained deterministic budget overrun. If it already passes, retain the
  test and record that no production correction was necessary.

- [x] **Step 4: Correct only demonstrated traversal defects.**

  Keep cursor representation and `ParserCore` unchanged. Fix accumulation or
  recursion in the owning expression/program module; do not add a host shortcut
  or weaken exact parity.

- [x] **Step 5: Run scale and development-warning GREEN.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-scale-spec --test-show-details=failures
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  ```

  Expected: deterministic observations and large traversal pass; the complete
  development build is warning-free.

- [x] **Step 6: Capture optional physical review evidence and commit.**

  Run the focused test executable with RTS allocation statistics when the
  local profiling/build mode supports it. Record elapsed time, allocation, and
  residency as environment-specific evidence without adding a threshold.
  Commit the deterministic scale coverage as
  `test: prove hosted parser traversal scale`.

### Task 6: Verify and close the expression child

**Files:**

- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/done-archive.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md`
- Modify: `docs/superpowers/specs/2026-07-12-jazz-next-bootstrap-jazz-parser-design.md`
- Modify: `docs/superpowers/specs/2026-07-16-jazz-next-bootstrap-parser-expression-foundation-design.md`
- Modify: `docs/superpowers/plans/2026-07-16-jazz-next-bootstrap-parser-expression-foundation.md`
- Modify: `jazz-next/README.md` only for directly affected hosted-parser/test-layout documentation

**Produces:** Clean completion evidence, an archived expression child, and one
unpromoted next curation target for types/declarations/modules.

- [x] **Step 1: Run the final focused gate.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-component-spec jazz-parser-parity-spec canonical-parser-comparison-spec parser-core-spec token-parser-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec repository-audit-spec --test-show-details=failures
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-scale-spec --test-show-details=failures
  ```

  Expected: every focused suite passes from the implementation head.

- [x] **Step 2: Run the development build and full suite.**

  ```bash
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
  ```

  Expected: warning-free build and all Cabal test suites pass.

- [x] **Step 3: Record completion and curate only the next child.**

  Mark this plan and design implemented, move the ready row to the done archive,
  update the bootstrap blocker/parent designs with exact landed evidence, and
  make `JN-BOOTSTRAP-JAZZ-PARSER-TYPES-DECLARATIONS-MODULES-001` the sole
  `Next Curation Target`. Do not promote it. Update `jazz-next/README.md` to
  name the hosted compiler parser stack and its `Bootstrap` test ownership
  without claiming complete parser or stage-1 support. Leave
  `docs/feature-status.md` unchanged because no top-level end-to-end status
  changes in this child.

- [x] **Step 4: Run documentation and cleanliness gates.**

  ```bash
  bash scripts/check-execution-queue.sh
  bash scripts/check-docs.sh
  git diff --check
  git status --short
  ```

  Expected: validators pass, no whitespace errors exist, no generated
  benchmark/profile artifacts are tracked, and only intended closeout files are
  pending.

- [x] **Step 5: Commit the closeout.**

  Commit active docs, this plan, and any directly affected README line as
  `docs: close parser expression foundation`.

## Completion Evidence

Completed on `2026-07-16`.

- The final focused parser component/parity, canonical parser, parser core,
  token parser, canonical lexer, Jazz lexer parity, and repository audit gate
  passed. The warning-clean `-fdevelopment` build and complete Cabal suite also
  passed.
- The explicit `ExpressionFoundation` family contains 43 stable cases in the
  fixed 350-case corpus. Token and source entry batches match complete stage-0
  values exactly and produce byte-identical repeated output; the Jazz lexer
  remains exact over all 350 cases.
- The isolated 512-binding scale fixture plus terminal expression parsed as a
  513-statement block twice with identical output, successful termination,
  identical runtime statistics, and zero host operations.
- Observed deterministic work was 21,751,223 evaluator transitions, 2,630,524
  applications, 110,804 list cells, and maximum continuation depth 1,060.
  Checked-in ceilings are 22,000,000, 2,700,000, 115,000, and 1,100.
- `ParserTypes.jz` did not change. `ParserCore.jz` received the narrow generic
  boundary fix described above: export of the existing `Parser` constructor and
  typed failure offset/problem accessors, with kernel behavior preserved by its
  focused suite.
- No optional RTS physical evidence was recorded; wall-clock time and physical
  allocation remain non-gating review data.
- Queue/docs validators and `git diff --check` passed at closeout.
- `JN-BOOTSTRAP-JAZZ-PARSER-TYPES-DECLARATIONS-MODULES-001` is the sole next
  curation target and remains unpromoted.

## Acceptance Checklist

- The named family has 43 explicit stable members and no positional selection.
- The shared 350-case corpus remains deterministic and lexer-compatible.
- `ParserToken` is the sole canonical-token/kernel boundary and preserves
  consumed, unconsumed, farthest-failure, and zero-progress semantics.
- No module imports both conflicting `ParserFailure` constructors.
- Foundational expression ASTs and failures match stage 0 exactly.
- Binding/expression disambiguation commits only after `=` and never hides a
  malformed binding behind fallback.
- Token and source façades compare exact complete structured values.
- Lexical failure never invokes grammar parsing or becomes parser failure.
- Repeated family batches and large-program observation are deterministic.
- Large traversal remains stack-safe and avoids repeated growing-prefix append.
- No parser-specific builtin, host callback, stdlib parser API, `ParserTypes`
  schema change, later grammar, lowering, backend, or legacy-reference change
  enters the child. The generic `ParserCore` boundary correction is limited to
  existing representation/access and does not change kernel semantics.
- Final focused, development, full-suite, queue/docs, and diff gates pass.
- Closeout archives only this child and curates, but does not promote, the
  types/declarations/modules child.
