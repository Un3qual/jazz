# If Expression Surface and Semantics Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Resolve the control-flow inconsistency where `if`/`else` exist in AST/runtime branches but are not part of parser/typechecker behavior.

**Architecture:** Decide first whether `if` is a first-class surface construct or intentionally unsupported in this phase. Then either implement full parse->analyze->runtime support or remove dead scaffolding.

**Tech Stack:** Markdown spec docs, validation scripts, and `jazz-next` parser/analyzer/runtime implementation work.

Execution note:
- `jazz-hs/` references in this plan are legacy evidence only.
- All net-new implementation and tests for this item must land in `jazz-next/`.
- Start by bootstrapping lexer/parser support in `jazz-next`; do not continue analyzer-only fixtures as the primary implementation path for this item.
- If parser behavior reveals syntax ambiguity while implementing `if`, update the matching spec clarification text in the same change.

---

## Progress

- [x] Control-flow drift evidence captured
- [x] Gate decision recorded (`if` included)
- [x] Selected execution track chosen (Track A)
- [x] Canonical `if` spec published (Track A0)
- [x] Tests and docs aligned
- [ ] Clarification closed

## Decision Lock (Approved 2026-03-03)

- [x] `if` is included as a language surface construct.
- [x] Initial implementation should desugar `if` to `case`.
- [x] Exact parser/lowering form lock: parse to `EIf`, then lower in a later explicit desugaring phase.

## Verification Evidence (Current Drift)

- `jazz-hs/src/AST.hs`: `EIf` node exists.
- `jazz-hs/src/CodeGen/Javascript.hs`: `EIf` has lowering branch.
- `jazz-hs/src/Parser/Lib.hs`: `if` and `else` are reserved words.
- `jazz-hs/src/Parser/Lang.hs`: no parser production constructs `EIf`.
- `jazz-hs/src/Analyzer/TypeInference.hs`: no `EIf` inference branch; unsupported expressions fall through to runtime `error`.

## Scope Guardrails

In scope:

1. decision on `if` surface inclusion,
1. syntax and typing contract (if included),
1. cleanup of dead AST/runtime branches (if excluded).

Out of scope:

1. pattern-match guard syntax,
1. advanced control-flow sugar (`unless`, `cond`, guards) unless explicitly added after gate approval.

## Decision Gate

1. [x] Gate A: Is `if` a canonical language construct in current phase?
   1. [x] Option A1 (selected): Include now (recommended if interpreter-first roadmap needs direct branching syntax).
   1. [ ] Option A2: Exclude now (rely on `case` only and remove scaffolding).

## Track A: Include `if` in Surface Language

### Phase A0: Spec Freeze

- [x] Specify grammar and precedence for `if` expression form.
- [x] Specify canonical desugaring:
  - `if cond thenExpr elseExpr` desugars to
  - `case cond { | True -> thenExpr | False -> elseExpr }`.
- [x] Specify typing rules:
  - condition must typecheck to `Bool`,
  - branch type unification policy,
  - diagnostics for branch mismatch.
- [x] Specify evaluation order and short-circuit behavior.

Create:
- `docs/spec/control-flow/if-expressions.md`

### Phase A1: Tests First

- [x] Add parser tests for valid and invalid `if` syntax.
- [x] Add type inference tests for condition and branch-type constraints.

Modify:
- `jazz-next/test/IfExpressionParserSpec.hs`
- `jazz-next/test/IfExpressionTypeSpec.hs`

### Phase A2: Parser + Analyzer Implementation

- [x] Add `if` parser production in `Parser` (`Parser.hs`) that constructs `EIf`.
- [ ] Add an explicit desugaring phase that rewrites `EIf` to `ECase` before analysis.
- [ ] Keep analyzer canonical on post-desugared forms; avoid permanent duplicate typing logic if `EIf` is always lowered pre-analysis.
- [x] Add error messages for non-`Bool` condition and branch mismatch.

Modify:
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Diagnostics.hs` (if needed)

### Phase A3: Runtime Path Alignment

- [ ] Keep runtime branch semantics aligned with spec (JS path or interpreter path, depending on active backend).
- [ ] Add pipeline test showing parser->analyzer->runtime roundtrip for `if`.

Modify:
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/CLI/Main.hs`

### Commit Checkpoints (Track A)

```bash
git add docs/spec/control-flow/if-expressions.md \
  docs/plans/spec-clarification/2026-03-03/control-flow/14-if-expression-surface-and-semantics.md
git commit -m "docs(spec): define if-expression grammar and typing contract"

git add jazz-next/test/IfExpressionParserSpec.hs jazz-next/test/IfExpressionTypeSpec.hs
git commit -m "test(control-flow): add if-expression parser and type tests"

git add jazz-next/src/JazzNext/Compiler/Parser.hs \
  jazz-next/src/JazzNext/Compiler/Parser/AST.hs \
  jazz-next/src/JazzNext/Compiler/Parser/Lower.hs \
  jazz-next/src/JazzNext/Compiler/AST.hs \
  jazz-next/src/JazzNext/Compiler/TypeInference.hs \
  jazz-next/src/JazzNext/Compiler/Diagnostics.hs
git commit -m "feat(control-flow): implement if-expression parsing and typing"
```

## Track B: Exclude `if` for Current Phase

Not selected by maintainer (2026-03-03). Keep this track only as historical fallback.

### Phase B0: Decision Doc

- [ ] Record that `if` is intentionally excluded and `case` is the canonical branching form.

Create:
- `docs/spec/control-flow/branching-policy.md`

### Phase B1: Remove Scaffolding

- [ ] Remove `EIf` from active AST and runtime branches, or mark as explicitly future-only behind TODO guards.
- [ ] Ensure parser reserved words and docs do not imply unsupported `if` surface.

Modify:
- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `docs/jazz-language-state.md`

### Commit Checkpoints (Track B)

```bash
git add docs/spec/control-flow/branching-policy.md \
  docs/plans/spec-clarification/2026-03-03/control-flow/14-if-expression-surface-and-semantics.md
git commit -m "docs(spec): lock branching policy and if exclusion"

git add jazz-next/src/JazzNext/Compiler/AST.hs \
  jazz-next/src/JazzNext/Compiler/Driver.hs \
  jazz-next/src/JazzNext/Compiler/Parser.hs \
  docs/jazz-language-state.md
git commit -m "refactor(control-flow): remove unsupported if-expression scaffolding"
```

## Verification Commands

```bash
bash jazz-next/scripts/test-warning-config.sh
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/IfExpressionParserSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/IfExpressionTypeSpec.hs
```

## Definition of Done

1. [x] `if` inclusion/exclusion is explicit and documented.
1. [ ] No AST/parser/analyzer/runtime drift remains for control-flow surface.
1. [x] Tests enforce the chosen control-flow contract.

## Implementation Status Verification (2026-03-03, Batch 2)

- [x] Verified `if` remained unimplemented in `jazz-next` parser/AST/lowering/type checks before starting this batch.
- [x] Added parser coverage for valid/invalid `if` syntax and nearest-`else` association.
- [x] Added type-contract coverage for non-`Bool` conditions and branch type mismatches.
- [x] Implemented parser/lowering/core-AST support for `if` and `Bool` literals in `jazz-next`.
- [x] Added type-check diagnostics (`E2001`/`E2002`) for `if` condition and branch constraints.
- [x] Ran `bash jazz-next/scripts/test-warning-config.sh`, `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/IfExpressionParserSpec.hs`, and `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/IfExpressionTypeSpec.hs`.
