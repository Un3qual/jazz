---
id: JN-BOOTSTRAP-JAZZ-PARSER-OPERATORS-FULL-PARITY-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-JAZZ-PARSER-CONTROL-FLOW-PATTERNS-001
last_verified: 2026-07-20
plan_section: "Implementation Batch: Operators and Full Parity"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-12-jazz-next-bootstrap-jazz-parser-design.md
  - docs/superpowers/specs/2026-07-20-jazz-next-bootstrap-parser-operators-full-parity-design.md
  - docs/superpowers/plans/2026-07-20-jazz-next-bootstrap-parser-operators-full-parity.md
  - jazz-next/README.md
  - jazz-next/jazz/compiler/ParserContext.jz
  - jazz-next/jazz/compiler/ParserExpression.jz
  - jazz-next/jazz/compiler/ParserDeclaration.jz
  - jazz-next/jazz/compiler/ParserProgram.jz
  - jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParity.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParitySpec.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScale.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleSpec.hs
  - jazz-next/jazz-next.cabal
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-operators-full-parity-spec jazz-parser-component-spec jazz-parser-parity-spec canonical-parser-comparison-spec parser-core-spec token-parser-spec canonical-lexer-comparison-spec jazz-lexer-parity-spec jazz-parser-control-flow-patterns-spec jazz-parser-types-declarations-modules-spec operator-fixity-spec operator-invalid-syntax-spec operator-section-spec if-expression-parser-spec lambda-parser-spec pattern-parser-spec adt-pattern-parser-spec expression-parser-spec declaration-parser-spec module-import-parser-spec repository-audit-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next jazz-parser-scale-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Complete the Jazz-authored hosted parser with fixed and source-local declared operators, precedence, associativity, values, sections, bindings, signatures, exact mixed control-flow composition, exclusive 365-fixture assignment, repeated complete parity, and deterministic operator scale evidence."
---

# Jazz-Next Bootstrap Parser Operators and Full Parity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox syntax for tracking. This plan intentionally specifies outcomes,
> interfaces, tests, commands, and review boundaries without embedding the
> implementation bodies.

**Goal:** Complete the fifth hosted-parser grammar child with exact operator
behavior and complete 365-fixture stage-0 parity.

**Architecture:** Add a grammar-neutral `ParserOperator` metadata owner, carry
declared operators through immutable `ParserContext`, thread context in
`ParserProgram`, keep declarations and statement forms in `ParserDeclaration`,
and integrate a private stop-aware precedence climber into `ParserExpression`.
Preserve the fixed parser schema, kernel, pattern owner, and public façades.

**Tech Stack:** GHC 9.14.1, ordinary Jazz `.jz` modules, compiler-local
`ParserCore`/`ParserToken`, canonical differential adapters, runtime
observation statistics, Cabal test components, and the Nix-pinned environment.

**Design checkpoint:**
[`2026-07-20-jazz-next-bootstrap-parser-operators-full-parity-design.md`](../specs/2026-07-20-jazz-next-bootstrap-parser-operators-full-parity-design.md)

## Global Constraints

- Modify compiler behavior only under `jazz-next/`; `jazz-hs/` and `jazz2/`
  remain read-only references.
- Keep `ParserTypes.jz`, `ParserCore.jz`, `ParserToken.jz`, `ParserPattern.jz`,
  and `Parser.jz` unchanged.
- Preserve the public expression and parser entry points; precedence and stop
  parameters remain private.
- Preserve complete canonical ASTs, structured failures, retained spans,
  source order, and lexical/parser phase separation.
- Do not add operator imports/exports, new built-ins, runtime dispatch,
  recovery, lowering, core, backend, native-runtime, host-callback, or public
  parser-library work.
- Use behavior and differential tests, not source-string assertions.
- Write and run each failing test before its production change.
- Keep checked-in `.jz` indentation at exactly two spaces.
- Run compiler and test commands through the Nix-pinned environment.
- Commit each independently reviewable milestone.

## File and Responsibility Map

| File | Responsibility in this child |
| --- | --- |
| `jazz-next/jazz/compiler/ParserOperator.jz` | New single owner for associativity, metadata, frozen built-ins, tier/precedence mapping, lookup, and symbol policy. |
| `jazz-next/jazz/compiler/ParserContext.jz` | Carry immutable source-order declared operator metadata and exact scope transitions. |
| `jazz-next/jazz/compiler/ParserDeclaration.jz` | Parse declarations, signatures, and bindings; return context updates and exact structured failures. |
| `jazz-next/jazz/compiler/ParserProgram.jz` | Thread statement results and operator context without pre-scanning. |
| `jazz-next/jazz/compiler/ParserExpression.jz` | Add stop-aware precedence climbing, operator values, sections, and mixed control-flow composition. |
| `jazz-next/test/JazzNext/Compiler/Parser/FixtureCorpus.hs` | Add three fixed families and exclusive whole-corpus manifest validation. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserOperatorsFullParitySpec.hs` | Exercise metadata, context, declaration, expression, and mixed boundaries through the real Jazz module graph. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParity.hs` | Load all new families plus the validated complete corpus through generic batch runners. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserParitySpec.hs` | Compare focused families and the complete corpus through token/source entries twice. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalLexerComparisonSpec.hs` | Preserve complete corpus order and source classifications. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalParserComparisonSpec.hs` | Lock the 365-case total, six family sizes, and exclusive assignment failures. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScale.hs` | Generate and run the additive operator/control-flow profile. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzParserScaleSpec.hs` | Require deterministic output/statistics, zero host operations, and bounded cost for all four profiles. |
| `jazz-next/jazz-next.cabal` | Register the focused suite; existing Jazz source discovery includes the new compiler module. |
| Coordination docs in frontmatter | Promote, record evidence, archive, and move the parent milestone without absorbing later stages. |

## Stable Interfaces and Invariants

| Owner | Required interface or invariant |
| --- | --- |
| `ParserOperator` | Expose abstract `OperatorInfo`, `OperatorAssociativity`, selectors, declared-before-built-in lookup, tier/precedence constructors, associativity replacement, and symbol predicates. |
| `ParserContext` | Expose declared-operator lookup/registration while preserving alias APIs; module bodies reset operators and nested blocks inherit them read-only. |
| `ParserDeclaration` | A declaration returns no surface statement and the next context; bindings/signatures remain existing `LetStatement`/`SignatureStatement` values. |
| `ParserProgram` | Every ordinary sequencing path consumes `([SurfaceStatement], ParserContext)` and threads the returned context to only following statements. |
| `ParserExpression` | Preserve both public entry points; private recursion takes stop and minimum-precedence state. Application binds tighter than infix syntax. |
| `FixtureCorpus` | Six explicit families contain 365 unique assignments; validator reports duplicate corpus names, duplicate family members, missing names, cross-family duplicates, and unassigned fixtures deterministically. |
| `JazzParserParity` | Add loaders for `Operators`, `MixedOperatorControlFlow`, `CorpusClosure`, and the validated complete corpus without changing generic batch runners. |
| `JazzParserScale` | Add `runJazzParserOperatorScale :: RuntimeObservationRequest -> IO RunResult`; retain all three landed runners. |

Private helper names may change to reduce duplication, but these cross-module
contracts and behaviors must remain reviewable and covered.

## Fixed Evidence Contract

- Preserve the landed family sizes 52, 101, and 75.
- Add the exact 55-, 26-, and 56-case families enumerated in the approved
  design, yielding 365 unique assignments.
- Compare complete stage-0 and hosted values, not acceptance alone.
- Run every hosted focused and complete batch twice and require identical
  output.
- Preserve lexical failures as source-façade lexical failures.
- The operator profile produces exactly 513 surface statements twice, reports
  identical statistics, performs zero host operations, and finishes under
  tightened measured ceilings.
- All three prior profiles retain their sources, output, observations, and
  ceilings.

## Implementation Batch: Operators and Full Parity

### Task 0: Promote the reviewed child

**Files:** queue, blocker contract, design, and this plan.

**Produces:** One exact `Ready Now` row matching this frontmatter and no stale
operators/full-parity curation row.

- [x] Mark the plan `ready` and preserve `autonomous_ready: yes`.
- [x] Add the P1/L queue row with the exact dependency, plan section, target
  paths, deliverable, verification, and date from frontmatter.
- [x] Update executor and parent blocker text to name this child as active and
  keep canonical core/backend work separate.
- [x] Run queue/docs validators and `git diff --check`.
- [x] Commit as `docs: promote parser operators batch`.

### Task 1: Lock complete manifest ownership and red parity evidence

**Files:** fixture corpus, canonical comparison specs, parity loader/spec,
focused operator spec, and Cabal registration.

**Consumes:** The exact 55/26/56 stable-name lists and manifest rules in the
approved design.

**Produces:** A registered test contract that fails because hosted operator
grammar is absent, while proving all 365 fixtures are assigned exactly once.

- [ ] Add the three family constructors and exact ordered name lists.
- [ ] Add deterministic cross-family duplicate and unassigned violations.
- [ ] Lock corpus total 365 and family sizes 52/101/75/55/26/56.
- [ ] Add family and complete-corpus loaders and repeated token/source parity
  cases.
- [ ] Register the focused suite and add behavior cases for built-ins,
  declarations, visibility, sections, scope, and mixed delimiters.
- [ ] Run the comparison and parity suites; verify manifest tests pass and the
  operator behavior/parity cases fail for missing hosted grammar.
- [ ] Commit as `test: lock hosted operator parity`.

Verification command:

```bash
nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next canonical-parser-comparison-spec canonical-lexer-comparison-spec jazz-parser-parity-spec jazz-parser-operators-full-parity-spec --test-show-details=failures
```

### Task 2: Add operator metadata and immutable context

**Files:** `ParserOperator.jz`, `ParserContext.jz`, focused spec.

**Consumes:** Fixed operator/fixity tables and source-order transitions from the
design.

**Produces:** One metadata owner and an immutable context capable of exact
lookup, registration, reset, and inheritance; no token grammar yet.

- [ ] Run focused metadata/context cases and confirm they fail for the missing
  module/interfaces.
- [ ] Add associativity and abstract operator metadata with selectors.
- [ ] Add the frozen built-in table, tier and precedence mapping, optional
  associativity replacement, declared-first lookup, and exact symbol policy.
- [ ] Extend context so top level starts empty, module bodies reset, nested
  blocks inherit, and registration returns a new value.
- [ ] Run the focused suite plus landed component, control-flow, and
  declarations suites; require metadata/context cases and regressions to pass.
- [ ] Commit as `feat: add hosted operator context`.

### Task 3: Parse declarations, signatures, bindings, and source order

**Files:** `ParserDeclaration.jz`, `ParserProgram.jz`, focused spec.

**Consumes:** `OperatorInfo` creation/lookup and immutable context transitions.

**Produces:** Exact declaration validation and statement sequencing where only
following statements see new metadata.

- [ ] Add failing behavior cases for valid tier/precedence/associativity,
  invalid symbols/ranges/words, duplicates, forward use, module isolation,
  nested rejection, and binding/signature requirements.
- [ ] Change ordinary statement sequencing to return and thread statement lists
  plus next context, allowing declarations to emit an empty list.
- [ ] Implement declaration parsing and exact validation/failure ownership.
- [ ] Implement parenthesized user-operator bindings and signatures through the
  existing surface constructors and hidden binding-name convention.
- [ ] Run focused and landed declaration/module/component suites; require the
  new source-order cases and prior alias/module behavior to pass.
- [ ] Commit as `feat: parse hosted operator declarations`.

### Task 4: Parse precedence, associativity, values, and sections

**Files:** `ParserExpression.jz`, focused spec.

**Consumes:** Context lookup and the existing stop-aware primary/application
grammar.

**Produces:** Complete non-control-flow operator expression behavior through
existing AST/failure constructors.

- [ ] Add failing cases for all frozen precedence tiers, left/right grouping,
  declared precedence, non-associative rejection, application binding power,
  undeclared use, operator values, both sections, and malformed operands.
- [ ] Introduce the private stop-aware minimum-precedence recursion without
  changing public entry points.
- [ ] Resolve visible metadata before consuming infix operators and preserve
  caller-owned delimiters.
- [ ] Add exact bare-value and section recognition in parenthesized expressions.
- [ ] Run focused, stage-0 operator, expression, component, and landed parity
  suites; require non-control operator cases to pass.
- [ ] Commit as `feat: parse hosted operator expressions`.

### Task 5: Compose operators with control flow and close focused parity

**Files:** `ParserExpression.jz`, focused spec, parity spec.

**Consumes:** The general precedence climber and landed stop-aware lambda,
conditional, case, guard, body, list, tuple, and block grammar.

**Produces:** Exact mixed operator/control-flow parsing, including pipe/arm
ownership and the fixed 55- and 26-case family parity.

- [ ] Run the 26-case mixed family and confirm remaining failures are delimiter
  or precedence integration failures rather than manifest errors.
- [ ] Route every recursive expression site through the same private climber.
- [ ] Preserve `then`, `else`, arrow, next-arm pipe, brace, comma, bracket,
  parenthesis, and dot ownership while permitting visible operators inside the
  bounded expression.
- [ ] Match complete stage-0 token/source results for both new operator families
  twice; keep all three landed families exact.
- [ ] Run focused/parser/operator/control-flow/parity suites and commit as
  `feat: compose hosted operators with control flow`.

### Task 6: Prove complete corpus parity and operator scale

**Files:** corpus closure/parity tests, scale runner/spec, focused spec if a
behavioral regression needs a smaller reproduction.

**Consumes:** All hosted grammar and the exclusive six-family manifest.

**Produces:** Repeated exact 365-case parity and deterministic 513-statement
operator scale evidence without changing earlier profile behavior.

- [ ] Run the `CorpusClosure` and complete-corpus batches; reproduce every
  mismatch in the focused suite before changing production grammar.
- [ ] Fix only root grammar/context ownership mismatches and rerun the smallest
  red/green test plus all family parity after each correction.
- [ ] Add the generated operator/control-flow profile and its exact output,
  termination, deterministic statistics, and zero-host-operation checks.
- [ ] Measure the profile twice, record stable observations, and tighten all
  four ceilings above measured values without weakening prior ceilings.
- [ ] Require all six families, complete corpus, and all four scale profiles to
  pass twice.
- [ ] Commit as `test: prove hosted parser full parity`.

### Task 7: Verify and close the final parser child

**Files:** parent designs, child design/plan, queue, blocker contract, archive,
README, and only directly affected evidence docs.

**Produces:** Warning-clean verified implementation, recorded observations,
archived child, empty `Ready Now`, and no automatic promotion of canonical
core or backend work.

- [ ] Run the complete focused command from frontmatter.
- [ ] Run the scale suite twice and record the exact operator observation.
- [ ] Run the warning-clean development build, all Cabal suites, and
  `cabal check`.
- [ ] Confirm no diff under legacy or fixed parser paths listed in Global
  Constraints.
- [ ] Record six family sizes, complete 365-case parity, all four observations,
  and exact verification evidence in child/parent docs and README.
- [ ] Mark this plan `done` with `completed_on`, remove and archive the queue
  row, and update the umbrella blocker without promoting later stages.
- [ ] Run queue/docs validators and `git diff --check`.
- [ ] Commit as `docs: close hosted parser parity batch`.

## Completion Gate

- Queue, plan, design, blocker contract, archive, and README agree.
- Metadata has one owner and context preserves exact source order/isolation.
- Declarations, bindings, signatures, precedence, associativity, values,
  sections, adjacency, and mixed delimiters match stage 0.
- Fixed parser schema/kernel/pattern/token/façade and legacy paths are unchanged.
- Six manifest families assign all 365 fixtures exactly once.
- Focused and complete token/source parity is byte-identical and deterministic.
- All four scale profiles pass twice with recorded bounded observations and
  zero host operations.
- Focused suites, warning-clean build, all Cabal suites, `cabal check`, and
  repository validators pass.
- No canonical-core, lowering, backend, runtime, host-callback, or public-parser
  work entered the child.
