---
id: JN-BOOTSTRAP-JAZZ-CORE-EXPRESSION-FOUNDATION-001
status: done
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-JAZZ-PARSER-OPERATORS-FULL-PARITY-001
last_verified: 2026-07-21
completed_on: 2026-07-21
plan_section: "Implementation Batch: Core Contract and Expression Foundation"
target_paths:
  - docs/execution/blocker-contracts.md
  - docs/execution/done-archive.md
  - docs/execution/queue.md
  - docs/superpowers/specs/2026-07-10-jazz-next-bootstrap-interpreter-profile-design.md
  - docs/superpowers/specs/2026-07-21-jazz-next-hosted-canonical-core-design.md
  - docs/superpowers/plans/2026-07-21-jazz-next-hosted-core-expression-foundation.md
  - jazz-next/README.md
  - jazz-next/jazz/compiler/CoreLower.jz
  - jazz-next/jazz/compiler/CoreTypes.jz
  - jazz-next/jazz-next.cabal
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalCoreComparison.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalCoreComparisonSpec.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreExpressionFoundationSpec.hs
  - jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreParity.hs
verification:
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next canonical-core-comparison-spec jazz-core-expression-foundation-spec canonical-parser-comparison-spec jazz-parser-component-spec repository-audit-spec --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal build --project-dir=jazz-next -fdevelopment all
  - nix --extra-experimental-features 'nix-command flakes' develop -c cabal test --project-dir=jazz-next all --test-show-details=failures
  - nix --extra-experimental-features 'nix-command flakes' develop -c bash -lc 'cd jazz-next && cabal check'
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Define the complete hosted canonical-core comparison contract and total stage-0 adapter, then lower the fixed foundational expression subset in ordinary Jazz with exact direct and composed parser parity, deterministic repetition, and explicit rejection of not-yet-owned surface forms."
---

# Jazz-Next Hosted Core Expression Foundation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox syntax for tracking. This plan intentionally specifies interfaces,
> observable behavior, tests, commands, and commit boundaries without embedding
> implementation bodies.

**Goal:** Establish the hosted canonical-core data and comparison boundary, then
implement exact Jazz-authored lowering for a fixed foundational expression
subset.

**Architecture:** `CoreTypes.jz` defines the complete canonical comparison
schema once. A test-only Haskell adapter translates already-lowered stage-0
values into that schema. `CoreLower.jz` initially exposes a deliberately scoped
`Maybe`-returning foundation function, so later surface forms cannot produce
placeholder core nodes or temporary compiler errors. Direct and parser-composed
tests compare the hosted value with stage 0.

**Tech Stack:** GHC 9.14.1, Haskell 2010 with existing package extensions,
ordinary Jazz `.jz` modules, the hosted parser surface ADTs, the stack-safe Jazz
interpreter, canonical runtime-value rendering, Cabal test components, and the
Nix-pinned development environment.

**Design checkpoint:**
[`2026-07-21-jazz-next-hosted-canonical-core-design.md`](../specs/2026-07-21-jazz-next-hosted-canonical-core-design.md)

## Global Constraints

- Modify compiler behavior only under `jazz-next/`; `jazz-hs/` and `jazz2/`
  remain read-only references.
- Keep the existing production `lowerSurfaceModule` diagnostic behavior and all
  downstream compiler phases unchanged. A typed detailed result may expose the
  same validation outcome to the structural comparison adapter.
- Do not add type inference, name resolution, analysis, evaluation,
  backend-neutral IR, LLVM, object/link, or native-runtime behavior.
- Do not add host lowerer intrinsics, Haskell callbacks, bytecode, or a VM.
- Compare canonical behavior, not Haskell `Show` output, source strings, or
  implementation file contents.
- Preserve arbitrary-precision integers and exact fractional source parts;
  never use rounded host formatting as parity evidence.
- Keep checked-in `.jz` indentation at exactly two spaces.
- Write and run each failing behavior test before its production change.
- Run compiler and test commands through the Nix-pinned environment.
- Do not run any exhaustive parser scale component. The routine `all` matrix
  includes only the small default parser smoke suite.
- Commit each independently reviewable green milestone.

## File and Responsibility Map

| File | Responsibility in this child |
| --- | --- |
| `jazz-next/jazz/compiler/CoreTypes.jz` | Complete hosted canonical names, spans, literals, patterns, signatures, statements, expressions, modules, and module-lowering results. |
| `jazz-next/jazz/compiler/CoreLower.jz` | Pure foundational surface-expression lowering with an explicit unsupported boundary. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalCoreComparison.hs` | Total structural adapter from already-lowered stage-0 values to the canonical runtime schema. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/CanonicalCoreComparisonSpec.hs` | Constructor-inventory, exact primitive representation, generated-name, span, module, and adapter-totality coverage. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreParity.hs` | Shared direct/composed runner and stage-0 expected-value construction for hosted core tests. |
| `jazz-next/test/JazzNext/Compiler/Bootstrap/JazzCoreExpressionFoundationSpec.hs` | Fixed foundation cases, unsupported-boundary cases, parser composition, and repeated deterministic parity. |
| `jazz-next/jazz-next.cabal` | Register both focused suites and their shared test modules. |
| Coordination/docs paths in frontmatter | Promote, document, close, archive, and hand off the child without promoting later compiler stages. |

## Stable Interfaces

| Owner | Interface |
| --- | --- |
| `CoreTypes` | Export the complete canonical core ADTs described by the design, including optional-path one-based spans and only source, qualified, and lowering-generated names. |
| `CoreLower` | Export `lowerFoundationExpression :: SurfaceExpr -> Maybe CoreExpr`. Return `Just` only when the entire tree belongs to this child; return `Nothing` without partial output for every other form. |
| `CanonicalCoreComparison` | Export checked `Either Text RuntimeValue` conversions for `Expr`, `CoreModule`, and the typed detailed module-lowering result; reject post-lowering names and invalid canonical paths without partial functions, while unrelated diagnostics remain excluded by the result type. |
| `JazzCoreParity` | Run a supplied Jazz lowering expression through the existing compiler module graph and compare it with a stage-0 value produced before canonical adaptation. |

`lowerFoundationExpression` is an internal milestone interface, not the final
lowering facade. The next control-flow child replaces this boundary with a total
expression lowerer only after every `SurfaceExpr` constructor can be handled
without placeholders.

## Fixed Foundation Contract

The successful subset includes:

- integer, fractional, Boolean, character, and text literals;
- source variables and two-segment qualified variables;
- operator values;
- empty and populated lists and tuples, including unit;
- left-associated ordinary application;
- non-`$` binary nodes and both section nodes; and
- blocks containing only ordinary non-operator bindings and expression
  statements whose nested expressions are all in this subset.

The child explicitly returns `Nothing` for lambdas, pattern cases,
conditionals, explicit type applications, `$` desugaring, signatures, data,
class, impl, module, import, hidden operator bindings, and any supported node
that contains one of those forms recursively.

The canonical schema includes later core constructors so the contract and
stage-0 adapter do not churn between children. Those constructors are covered
by adapter inventory tests but are not emitted by the foundation lowerer.

## Implementation Batch: Core Contract and Expression Foundation

### Task 0: Promote the reviewed child

**Files:** this plan, `docs/execution/queue.md`, and
`docs/execution/blocker-contracts.md`.

**Produces:** One exact P1/L `Ready Now` row matching the approved frontmatter,
with the parent bootstrap blocker naming this child as active.

- [x] Change plan status to `ready` and `autonomous_ready` to `yes` after user
  approval.
- [x] Move the named candidate from `Next Curation Target` to `Ready Now` with
  exact dependency, plan section, target paths, deliverable, verification, and
  date parity.
- [x] Update the parent blocker contract to identify the active child while
  preserving control-flow, declarations, modules, and every backend stage as
  unpromoted.
- [x] Run queue/docs validators and `git diff --check`.
- [x] Commit as `docs: promote hosted core foundation`.

### Task 1: Establish the canonical schema and stage-0 adapter

**Files:** `CoreTypes.jz`, `CanonicalCoreComparison.hs`,
`CanonicalCoreComparisonSpec.hs`, and `jazz-next.cabal`.

**Produces:** One stable data contract plus a total adapter that only translates
already-lowered stage-0 values.

- [x] Register `canonical-core-comparison-spec` and write failing inventory
  cases for every core expression, statement, pattern, literal, signature,
  name, span, module, import, export, and permitted module-failure form.
- [x] Verify the suite fails because the hosted core module and adapter are
  absent.
- [x] Add the complete `CoreTypes` schema. Represent fractional literals with
  exact normalized whole/fractional source parts and optional width; do not
  derive canonical data from rounded `Double` rendering.
- [x] Add structural Haskell conversions for `Expr` and `CoreModule`. Preserve
  the semantic inputs of the typed `E4005`/`E4006` module failures; keep the
  production diagnostic wrapper stable and exclude unrelated diagnostics from
  the adapter by type.
- [x] Require exact optional-path span preservation and exact source,
  qualified, lambda-pattern, and operator-storage name representations.
- [x] Run `canonical-core-comparison-spec` and the existing canonical parser
  comparison suite; require both to pass.
- [x] Commit as `feat: define hosted canonical core contract`.

### Task 2: Lock red foundation parity evidence

**Files:** `JazzCoreParity.hs`, `JazzCoreExpressionFoundationSpec.hs`, and
`jazz-next.cabal`.

**Consumes:** `CoreTypes` and the stage-0 adapter from Task 1.

**Produces:** A registered differential suite that fails only because
`CoreLower.lowerFoundationExpression` does not exist yet.

- [x] Add direct surface-AST cases for every successful foundation form,
  including arbitrary-precision integers, exact fractional source parts and
  widths, nested collections/application, spans, and ordinary blocks.
- [x] Add explicit unsupported-boundary cases for every deferred top-level
  form and for a supported outer node containing a deferred nested form.
- [x] Add the shared runner that computes expected values through stage-0
  `lowerSurfaceExpr`, then independently executes the Jazz function through
  the existing module graph.
- [x] Register `jazz-core-expression-foundation-spec` and verify it fails for
  the missing `CoreLower` module/function while the canonical contract suite
  remains green.
- [x] Keep the red evidence uncommitted and proceed directly to Task 3.

### Task 3: Implement pure foundational lowering

**Files:** `CoreLower.jz` and `JazzCoreExpressionFoundationSpec.hs`.

**Consumes:** hosted parser surface ADTs and the `CoreTypes` constructors.

**Produces:** Exact direct parity for the complete fixed foundation subset and
all-or-nothing rejection outside it.

- [x] Implement exact literal and name conversion, preserving canonical text,
  purity spelling, exact fractional source parts, and optional widths.
- [x] Implement recursive collection, tuple, application, operator-value,
  non-`$` binary, and section lowering.
- [x] Implement ordinary block lowering only when every statement and nested
  expression belongs to the child; reject hidden operator binding names.
- [x] Ensure recursive unsupported input returns `Nothing` for the whole tree,
  with no partial canonical result and no compiler diagnostic.
- [x] Run the focused direct suite twice and require identical complete values.
- [x] Run the canonical comparison, parser component, and repository audit
  regressions.
- [x] Commit as `feat: lower hosted core foundation`.

### Task 4: Prove parser composition and deterministic boundaries

**Files:** `JazzCoreParity.hs` and
`JazzCoreExpressionFoundationSpec.hs`.

**Consumes:** the existing hosted parser source facade and the green direct
foundation lowerer.

**Produces:** Exact source-to-surface-to-foundation parity without adding a
production `Core.jz` facade early.

- [x] Add a fixed composed-source family covering scalar bindings, nested
  lists/tuples, qualified application, operator values, generic binary nodes,
  both sections, multiple statements, and empty/unit forms.
- [x] Compute expected results through independent stage-0 lexing, parsing,
  and `lowerSurfaceExpr`; do not adapt hosted surface output into the expected
  value.
- [x] Execute every direct and composed case twice and require exact identical
  canonical output.
- [x] Assert parser-rejected input remains parser-owned and deferred valid
  surface forms return `Nothing`, not a lowering diagnostic.
- [x] Run the focused suites plus all landed hosted parser parity suites; do not
  invoke manual full-scale components.
- [x] Commit as `test: prove hosted core foundation parity`.

### Task 5: Verify and close the child

**Files:** all coordination and status paths listed in frontmatter.

**Produces:** A clean routine matrix, durable closure evidence, and the single
named `JN-BOOTSTRAP-JAZZ-CORE-CONTROL-FLOW-PATTERNS-001` curation target rather
than premature promotion.

- [x] Run the exact focused verification command from frontmatter.
- [x] Run the warning-clean development build, routine `all` matrix, and
  `cabal check`. Confirm the exhaustive parser scale components were neither
  built with `full-parser-scale` nor run directly.
- [x] Update the hosted canonical-core design and bootstrap profile with the
  landed foundation boundary and explicit remaining children.
- [x] Update `jazz-next/README.md` with the hosted core contract, internal
  foundation entry point, parity evidence, and unsupported boundary.
- [x] Mark this plan done, archive closure evidence, empty `Ready Now`, and
  validate and record only `JN-BOOTSTRAP-JAZZ-CORE-CONTROL-FLOW-PATTERNS-001`
  in `Next Curation Target`. Its target paths must name existing `CoreTypes.jz`,
  `CoreLower.jz`, `JazzCoreParity.hs`, and `jazz-next.cabal` plus its planned
  focused test; its verification must include focused parity, all prior core
  suites, the routine matrix, and repository validators.
- [x] Run queue/docs validators and `git diff --check`.
- [x] Commit as `docs: close hosted core foundation`.

## Done Criteria

- The complete hosted canonical-core schema exists in ordinary Jazz.
- The test-only Haskell adapter is structural, total over its stated input, and
  independently inventory-tested.
- Every fixed foundation case matches stage 0 directly and through hosted
  parser composition twice.
- Every deferred form returns `Nothing` for the whole tree without fake core
  output or temporary diagnostics.
- Production lowering and downstream compiler behavior remain unchanged.
- Focused tests, warning-clean build, routine all-suite matrix, Cabal package
  check, queue/docs validation, and whitespace checks pass.
- No exhaustive parser scale component is run.
