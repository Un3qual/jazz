---
id: JN-BOOTSTRAP-JAZZ-PARSER-CONTROL-FLOW-PATTERNS-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-JAZZ-PARSER-TYPES-DECLARATIONS-MODULES-001
last_verified: 2026-07-20
plan_section: "Implementation Batch: Control Flow and Patterns"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-12-jazz-next-bootstrap-jazz-parser-design.md
  - docs/superpowers/specs/2026-07-20-jazz-next-bootstrap-parser-control-flow-patterns-design.md
  - docs/superpowers/plans/2026-07-20-jazz-next-bootstrap-parser-control-flow-patterns.md
  - jazz-next/README.md
  - jazz-next/jazz/compiler/ParserExpression.jz
  - jazz-next/jazz/compiler/ParserProgram.jz
  - jazz-next/jazz/compiler/ParserDeclaration.jz
  - jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParity.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParitySpec.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScale.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleSpec.hs
  - jazz-next/jazz-next.cabal
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-control-flow-patterns-spec jazz-parser-component-spec jazz-parser-parity-spec canonical-parser-comparison-spec parser-core-spec token-parser-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec if-expression-parser-spec lambda-parser-spec pattern-parser-spec adt-pattern-parser-spec expression-parser-spec declaration-parser-spec module-import-parser-spec repository-audit-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-scale-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Extend the Jazz-authored parser with lambdas, conditionals, cases, single case-arm guards, and the complete accepted pattern surface while matching stage 0 exactly over a fixed 75-case family and a deterministic 513-statement control-flow scale profile."
---

# Jazz-Next Bootstrap Parser Control Flow and Patterns Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox syntax for tracking. This plan intentionally specifies outcomes,
> interfaces, tests, commands, and review boundaries without embedding the
> implementation bodies.

**Goal:** Add the fourth hosted-parser grammar slice: lambdas, conditionals,
cases, single case-arm guards, and every accepted stage-0 pattern form.

**Architecture:** Add one independent `ParserPattern` module for pattern and
lambda-parameter grammar. Keep recursive control-flow ownership in
`ParserExpression` behind a private stop-aware seam, preserving the existing
program, declaration, context, surface-schema, parser-core, and façade
contracts.

**Tech Stack:** GHC 9.14.1, ordinary Jazz `.jz` modules, the compiler-local
`ParserCore`/`ParserToken` stack, canonical lexer/parser comparison adapters,
runtime observation statistics, Cabal test components, and the Nix-pinned
development environment.

**Design checkpoint:**
[`2026-07-20-jazz-next-bootstrap-parser-control-flow-patterns-design.md`](../specs/2026-07-20-jazz-next-bootstrap-parser-control-flow-patterns-design.md)

## Global Constraints

- Modify compiler behavior only under `jazz-next/`; `jazz-hs/` and `jazz2/`
  remain read-only references.
- Deliver reviewable milestone commits. Do not collapse the child into one
  implementation commit.
- Keep `ParserTypes.jz`, `ParserCore.jz`, `ParserContext.jz`, `ParserToken.jz`,
  and `Parser.jz` unchanged.
- Preserve the public expression and parser façade entry points. Stop-aware
  parsing remains a private `ParserExpression` concern.
- Preserve complete structured failures, retained spans, source order, and the
  lexical-failure versus parser-failure distinction.
- Keep operator grammar and mixed operator/control-flow fixtures deferred to
  Child 5. Do not add operator metadata or a partial operator parser here.
- Add no recovery, partial ASTs, pattern forms, lowering, canonical core,
  backend, native runtime, or full-corpus closure.
- Use behavior tests and complete canonical values. Acceptance-only or
  source-spelling assertions are insufficient.
- Keep checked-in `.jz` indentation at exactly two spaces.
- Run all compiler and test commands through the Nix-pinned environment.
- Commit after each independently reviewable milestone.

## File and Responsibility Map

| File | Responsibility in this child |
| --- | --- |
| `jazz-next/jazz/compiler/ParserPattern.jz` | New independent owner for case patterns, case-arm alternatives, and lambda parameters. |
| `jazz-next/jazz/compiler/ParserExpression.jz` | Add the private stop-aware recursive seam and own lambda, conditional, case, guard, and arm-body parsing. |
| `jazz-next/jazz/compiler/ParserProgram.jz` | Retain sequencing and nested-block callbacks; change only if the expanded expression seam requires narrow wiring. |
| `jazz-next/jazz/compiler/ParserDeclaration.jz` | Retain statement/declaration ownership; change only if the expanded expression seam requires narrow wiring. |
| `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs` | Add the exact 75-case `ControlFlowPatterns` family and three reviewed focused fixtures. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserControlFlowPatternsSpec.hs` | New focused real-module-graph component and exact stage-0 boundary suite. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParity.hs` | Load the third named family through the existing generic parity harness. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParitySpec.hs` | Run all three families through token and source entries twice. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs` | Preserve complete corpus ordering and classification after three additions. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs` | Lock the new corpus total and family-manifest invariants. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScale.hs` | Generate and execute the additive 513-statement control-flow profile. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleSpec.hs` | Lock output, deterministic observations, zero host operations, and ceilings for all three profiles. |
| `jazz-next/jazz-next.cabal` | Register the new focused Haskell test suite; the existing `jazz/compiler/*.jz` source glob already includes `ParserPattern`. |
| Coordination docs named in Task 7 | Promote the child, record evidence, archive it, and curate Child 5 without promoting it. |

## Stable Interfaces and Invariants

| Owner | Required interface or invariant |
| --- | --- |
| `ParserPattern` | `parseCasePattern :: TokenParser(SurfacePattern)` parses one non-alternative case pattern. |
| `ParserPattern` | `parseCaseArmPattern :: TokenParser(SurfacePattern)` collects only top-level case-arm alternatives. |
| `ParserPattern` | `parseLambdaParameter :: TokenParser(SurfaceLambdaParameter)` preserves identifier versus pattern parameter constructors and lambda-specific restrictions. |
| `ParserExpression` | Preserve `parseFoundationalExpression` and `parseFoundationalExpressionWithContext`; both expand to the complete non-operator expression slice. |
| `ParserExpression` | The private stop seam checks delimiters before primary dispatch and application continuation without consuming the delimiter. |
| `ParserExpression` | `if`, `case`, and lambda dispatch precede ordinary identifier fallback only at the relevant expression position. |
| `ParserProgram` | Nested blocks still parse through `NestedBlockContext`; no control-flow form mutates grammar context. |
| `ParserDeclaration` | Bindings, expression statements, declarations, modules, and callback ownership remain unchanged. |
| `JazzParserParity` | Add `loadControlFlowPatternsFixtures :: IO [ParserFixture]`; the token/source batch runners remain generic. |
| `JazzParserScale` | Add `runJazzParserControlFlowScale :: RuntimeObservationRequest -> IO RunResult`; retain both landed runners unchanged. |

Private helper names may differ when that reduces duplication, but the
cross-module interfaces and invariants above must remain recognizable and
covered by focused tests.

## Fixed Evidence Contract

- `ControlFlowPatterns` contains exactly the 72 existing stable names and
  three focused additions enumerated in the approved design, in that order.
- The three focused fixtures retain their reviewed source and expected
  classification; do not replace them with looser equivalents.
- Every family member compares the complete stage-0 token result, hosted token
  result twice, stage-0 source result, and hosted source result twice.
- `ExpressionFoundation` remains exactly 52 cases.
- `TypesDeclarationsModules` remains exactly 101 cases.
- Mixed operator/control-flow fixtures listed in the design remain outside the
  new family.
- The control-flow scale program returns exactly 513 statements twice, reports
  identical runtime statistics, performs zero host operations, and stays
  within the approved initial ceilings.
- The expression and declarations scale profiles retain their existing source,
  output, statistics, and ceilings.

## Implementation Batch: Control Flow and Patterns

### Task 0: Promote the reviewed child

**Files:**

- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: this plan frontmatter

**Produces:** One exact `Ready Now` row whose metadata matches this plan and
whose former curation row is removed.

- [x] Change plan status from `planned` to `ready` and keep
  `autonomous_ready: yes`.
- [x] Add the P1/L implementation row with the exact dependency, plan section,
  target paths, deliverable, verification commands, and `2026-07-20`
  verification date from this plan.
- [x] Update executor and blocker text to name this child as active while
  keeping operators/full parity unpromoted.
- [x] Run `bash scripts/check-execution-queue.sh`, `bash scripts/check-docs.sh`,
  and `git diff --check`; expect all three to pass.
- [x] Commit as `docs: promote parser control flow batch`.

### Task 1: Lock the 75-case contract before grammar implementation

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParity.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParitySpec.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserControlFlowPatternsSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

**Consumes:** The exact family names, focused sources, and deferred mixed cases
from the approved design.

**Produces:** A registered focused suite and deterministic family contract that
fail because hosted control-flow/pattern grammar is not implemented yet.

- [x] Add the `ControlFlowPatterns` family, exact 72-name list, and exact three
  focused fixtures; extend manifest tests for the new total and family size.
- [x] Add the generic family loader and extend repeated parity orchestration to
  all three families without copying batch-runner logic.
- [x] Add focused assertions for direct pattern forms, lambda boundaries,
  delimiter ownership, nested control flow, exact failure payloads, and both
  parser façades. Each assertion compares complete stage-0 and hosted results.
- [x] Register `jazz-parser-control-flow-patterns-spec` with the same ordinary
  comparison/test-source dependencies as the prior focused suite.
- [x] Run the new focused suite and canonical manifest suites. Expect the
  manifest checks to pass and hosted grammar/parity assertions to fail on the
  first unsupported lambda/control-flow fixture.
- [x] Commit as `test: define parser control flow parity family`.

Verification command:

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-control-flow-patterns-spec canonical-parser-comparison-spec canonical-lexer-comparison-spec jazz-parser-parity-spec --test-show-details=failures
```

### Task 2: Implement independent pattern ownership

**Files:**

- Create: `jazz-next/jazz/compiler/ParserPattern.jz`
- Modify: `jazz-next/jazz-next.cabal`
- Test: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserControlFlowPatternsSpec.hs`

**Consumes:** Existing `SurfacePattern`, `SurfaceLambdaParameter`, pattern
failure constructors, canonical tokens, and `ParserCore` combinators.

**Produces:** The three stable `ParserPattern` entry points without expression,
program, declaration, or context dependencies.

- [ ] Add failing direct component cases for literals, wildcard, variables,
  constructors, exact/cons lists, tuple/unit, as-patterns, top-level
  alternatives, lambda identifiers/patterns, and every reviewed malformed
  boundary.
- [ ] Implement the smallest pattern module that makes those direct cases match
  stage 0 exactly, including fractional and cons-list structured failures.
- [ ] Confirm nested/grouped alternatives and lambda guards remain rejected and
  that pattern parsing stops without consuming arm/guard/body delimiters.
- [ ] Run the focused suite plus active stage-0 pattern suites; expect direct
  pattern cases to pass while full hosted program cases still fail at lambda,
  `if`, or `case` dispatch.
- [ ] Commit as `feat: add hosted pattern parser`.

Verification command:

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-control-flow-patterns-spec pattern-parser-spec adt-pattern-parser-spec --test-show-details=failures
```

### Task 3: Add stop-aware recursion and lambdas

**Files:**

- Modify: `jazz-next/jazz/compiler/ParserExpression.jz`
- Test: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserControlFlowPatternsSpec.hs`

**Consumes:** `ParserPattern.parseLambdaParameter` and the existing recursive
expression/block callbacks.

**Produces:** Private delimiter-aware expression recursion and exact lambda
parity without changing public expression signatures.

- [ ] Add focused failing cases proving stop checks happen before primary
  dispatch and application continuation and do not consume the delimiter.
- [ ] Add failing lambda cases for multiple parameters, every accepted pattern
  parameter, recursive/nested bodies, missing arrows, trailing commas, bare
  parameters, guarded alternatives, and retained failure spans.
- [ ] Refactor expression recursion behind the private stop seam while keeping
  existing literal, application, type-application, list, tuple, qualified-name,
  and block behavior byte-identical.
- [ ] Implement lambda dispatch and parameter/body parsing through
  `ParserPattern`.
- [ ] Run focused lambda, expression, declarations, and landed parity suites;
  expect lambda cases and both landed families to pass while conditional/case
  cases remain the only new-family failures.
- [ ] Commit as `feat: parse hosted lambdas`.

Verification command:

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-control-flow-patterns-spec jazz-parser-types-declarations-modules-spec jazz-parser-component-spec jazz-parser-parity-spec lambda-parser-spec expression-parser-spec declaration-parser-spec --test-show-details=failures
```

### Task 4: Add exact conditional parsing

**Files:**

- Modify: `jazz-next/jazz/compiler/ParserExpression.jz`
- Test: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserControlFlowPatternsSpec.hs`

**Produces:** Stage-0-exact `if`/`then`/`else` parsing through the private stop
seam, including nearest-`if` association and exact malformed boundaries.

- [ ] Add or enable failing conditionals for nesting, block branches, missing
  condition/`then`/true branch/`else`/false branch, extra `else`, reserved
  binding use, and inherited outer delimiters.
- [ ] Implement conditional dispatch before ordinary identifier fallback only
  at expression start, leaving `then`/`else` ordinary outside the matching stop
  site wherever stage 0 allows them.
- [ ] Match complete stage-0 surfaces and failures for every operator-independent
  conditional fixture; do not pull comparison-expression fixtures forward.
- [ ] Run the focused suite, active conditional suite, and landed parity suites;
  expect conditional cases to pass while case/guard cases remain outstanding.
- [ ] Commit as `feat: parse hosted conditionals`.

Verification command:

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-control-flow-patterns-spec jazz-parser-parity-spec if-expression-parser-spec expression-parser-spec --test-show-details=failures
```

### Task 5: Add cases, guards, and exact 75-case parity

**Files:**

- Modify: `jazz-next/jazz/compiler/ParserExpression.jz`
- Modify only if required: `jazz-next/jazz/compiler/ParserProgram.jz`
- Modify only if required: `jazz-next/jazz/compiler/ParserDeclaration.jz`
- Test: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserControlFlowPatternsSpec.hs`
- Test: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParitySpec.hs`

**Consumes:** Complete `ParserPattern`, stop-aware expression recursion,
conditional parsing, and existing nested-block/context callbacks.

**Produces:** Exact token/source parity for all 75 control-flow/pattern fixtures
twice while preserving both landed families.

- [ ] Add or enable focused failures for case scrutinee braces, required first
  pipe, empty arms, pattern alternatives, one simple guard, missing guard
  expression, second guards, arm arrows, arm-body stops, nested cases/lambdas/
  conditionals/blocks, and closing braces.
- [ ] Implement case and arm recursion so only top-level pipes and braces stop
  an arm body; nested delimiters remain owned by their inner parser.
- [ ] Preserve unusual accepted stage-0 arm boundaries exactly. Do not clean up
  or reclassify a fixture to satisfy the family gate.
- [ ] Run the focused suite and all three parity families twice. Require exact
  complete-value equality and deterministic repeated output.
- [ ] Run parser-core, token-parser, schema/comparison, lexer parity, active
  expression/declaration/module, and repository audit regressions.
- [ ] Commit as `feat: parse hosted cases and guards`.

Verification command:

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-control-flow-patterns-spec jazz-parser-component-spec jazz-parser-parity-spec canonical-parser-comparison-spec parser-core-spec token-parser-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec if-expression-parser-spec lambda-parser-spec pattern-parser-spec adt-pattern-parser-spec expression-parser-spec declaration-parser-spec module-import-parser-spec repository-audit-spec --test-show-details=failures
```

### Task 6: Add deterministic control-flow scale evidence

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScale.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleSpec.hs`

**Produces:** A third additive scale profile with exact output, deterministic
statistics, zero host operations, and fixed ceilings.

- [ ] Add the reviewed rotating 512-binding generator plus one terminal
  expression and assert the parsed block contains exactly 513 statements.
- [ ] Run it twice through the ordinary module graph and require identical
  output and runtime observation statistics.
- [ ] Lock zero host operations and the approved initial ceilings, record actual
  observations, and tighten any ceiling that is excessively loose without
  changing the generated source.
- [ ] Re-run the expression and declarations profiles unchanged and confirm
  their existing outputs and ceilings still pass.
- [ ] Commit as `test: prove parser control flow scale`.

Verification command:

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-scale-spec --test-show-details=failures
```

### Task 7: Verify and close the child

**Files:**

- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/done-archive.md`
- Modify: `docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md`
- Modify: `docs/superpowers/specs/2026-07-12-jazz-next-bootstrap-jazz-parser-design.md`
- Modify: `docs/superpowers/specs/2026-07-20-jazz-next-bootstrap-parser-control-flow-patterns-design.md`
- Modify: this plan
- Modify: `jazz-next/README.md`

**Produces:** Warning-clean verified implementation, recorded evidence, archived
queue row, and operators/full parity as the sole unpromoted next curation
target.

- [ ] Run the focused parser/lexer/kernel/repository command from frontmatter;
  require all named suites to pass.
- [ ] Run the scale suite twice if the first closeout run changes any generated
  source or ceiling; require all three profiles to pass.
- [ ] Run the warning-clean development build, all registered Cabal suites, and
  `cabal check`; require success.
- [ ] Compare the final diff with the promotion commit and confirm no change to
  `jazz-hs/`, `jazz2/`, `ParserTypes.jz`, `ParserCore.jz`, `ParserContext.jz`,
  `ParserToken.jz`, or `Parser.jz`.
- [ ] Record 75-case parity, all three scale observations, final suite evidence,
  and the exact completion date in the design, plan, parent parser design,
  interpreter profile, and README.
- [ ] Mark this plan `done` with `completed_on`, remove the `Ready Now` row,
  archive it, and replace the curation target with the separately bounded
  operators/full-parity child. Do not promote that child.
- [ ] Run queue/docs validators and `git diff --check` after every stale
  control-flow-as-future reference is removed.
- [ ] Commit as `docs: close parser control flow batch`.

Full closeout commands:

```sh
nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-control-flow-patterns-spec jazz-parser-component-spec jazz-parser-parity-spec canonical-parser-comparison-spec parser-core-spec token-parser-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec if-expression-parser-spec lambda-parser-spec pattern-parser-spec adt-pattern-parser-spec expression-parser-spec declaration-parser-spec module-import-parser-spec repository-audit-spec --test-show-details=failures
nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-scale-spec --test-show-details=failures
nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

## Completion Gate

- The queue, plan, design, blocker contract, and archive agree.
- `ParserPattern` has no expression/program/declaration/context dependency.
- Public parser and expression signatures are unchanged.
- All 75 new-family fixtures match exact token/source results twice.
- Both landed parity families remain exact and deterministic.
- All three scale profiles pass with deterministic observations and zero host
  operations.
- The warning-clean build, focused suites, all Cabal suites, `cabal check`,
  queue/docs checks, and diff check pass.
- No locked or legacy-reference path changed.
- Operators/full parity is the sole next curation target and remains
  unpromoted.
