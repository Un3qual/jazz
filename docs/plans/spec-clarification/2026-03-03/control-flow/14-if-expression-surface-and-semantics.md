# If Expression Surface and Semantics Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Resolve the control-flow inconsistency where `if`/`else` exist in AST/runtime branches but are not part of parser/typechecker behavior.

**Architecture:** Decide first whether `if` is a first-class surface construct or intentionally unsupported in this phase. Then either implement full parse->analyze->runtime support or remove dead scaffolding.

**Tech Stack:** Haskell (`jazz-hs` parser/analyzer/codegen or interpreter path), Markdown spec docs, `stack` tests.

---

## Progress

- [x] Control-flow drift evidence captured
- [x] Gate decision recorded (`if` included)
- [x] Selected execution track chosen (Track A)
- [ ] Tests and docs aligned
- [ ] Clarification closed

## Decision Lock (Approved 2026-03-03)

- [x] `if` is included as a language surface construct.
- [x] Initial implementation should desugar `if` to `case`.
- [ ] Exact parser/lowering form lock (direct parse to `ECase` vs parse `EIf` then immediate lowering) still required.

## Verification Evidence (Current Drift)

- `jazz-hs/src/AST.hs`: `EIf` node exists.
- `jazz-hs/src/CodeGen/Javascript.hs`: `EIf` has lowering branch.
- `jazz-hs/src/Parser/Lib.hs`: `if` and `else` are reserved words.
- `jazz-hs/src/Parser/Lang.hs`: no parser production constructs `EIf`.
- `jazz-hs/src/Analyzer/TypeInference.hs`: no `EIf` inference branch; unsupported expressions fall through to runtime `error`.

## Scope Guardrails

In scope:
- decision on `if` surface inclusion,
- syntax and typing contract (if included),
- cleanup of dead AST/runtime branches (if excluded).

Out of scope:
- pattern-match guard syntax,
- advanced control-flow sugar (`unless`, `cond`, guards) unless explicitly added after gate approval.

## Decision Gate

- [x] Gate A: Is `if` a canonical language construct in current phase?
  - [x] Option A1 (selected): Include now (recommended if interpreter-first roadmap needs direct branching syntax).
  - [ ] Option A2: Exclude now (rely on `case` only and remove scaffolding).

## Track A: Include `if` in Surface Language

### Phase A0: Spec Freeze

- [ ] Specify grammar and precedence for `if` expression form.
- [ ] Specify canonical desugaring:
  - `if cond thenExpr elseExpr` desugars to
  - `case cond { | True -> thenExpr | False -> elseExpr }`.
- [ ] Specify typing rules:
  - condition must typecheck to `Bool`,
  - branch type unification policy,
  - diagnostics for branch mismatch.
- [ ] Specify evaluation order and short-circuit behavior.

Create:
- `docs/spec/control-flow/if-expressions.md`

### Phase A1: Tests First

- [ ] Add parser tests for valid and invalid `if` syntax.
- [ ] Add type inference tests for condition and branch-type constraints.

Modify:
- `jazz-hs/test/ParserSpec.hs`
- `jazz-hs/test/Analyzer/TypeInferenceSpec.hs`

### Phase A2: Parser + Analyzer Implementation

- [ ] Add `if` parser production in `Parser.Lang` and lower to `case` in the same phase.
- [ ] Keep analyzer canonical on `ECase`; only add `EIf` inference branch if an intermediate `EIf` node remains after parsing.
- [ ] Add error messages for non-`Bool` condition and branch mismatch.

Modify:
- `jazz-hs/src/Parser/Lang.hs`
- `jazz-hs/src/Analyzer/TypeInference.hs`
- `jazz-hs/src/Errors.hs` (if needed)

### Phase A3: Runtime Path Alignment

- [ ] Keep runtime branch semantics aligned with spec (JS path or interpreter path, depending on active backend).
- [ ] Add pipeline test showing parser->analyzer->runtime roundtrip for `if`.

Modify:
- `jazz-hs/src/CodeGen/Javascript.hs` (if still exercised)
- `jazz-hs/src/Interpreter.hs` (if interpreter path is active)

### Commit Checkpoints (Track A)

```bash
git add docs/spec/control-flow/if-expressions.md \
  docs/plans/spec-clarification/2026-03-03/control-flow/14-if-expression-surface-and-semantics.md
git commit -m "docs(spec): define if-expression grammar and typing contract"

git add jazz-hs/test/ParserSpec.hs jazz-hs/test/Analyzer/TypeInferenceSpec.hs
git commit -m "test(control-flow): add if-expression parser and type tests"

git add jazz-hs/src/Parser/Lang.hs jazz-hs/src/Analyzer/TypeInference.hs jazz-hs/src/Errors.hs
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
- `jazz-hs/src/AST.hs`
- `jazz-hs/src/CodeGen/Javascript.hs`
- `jazz-hs/src/Parser/Lib.hs`
- `docs/jazz-language-state.md`

### Commit Checkpoints (Track B)

```bash
git add docs/spec/control-flow/branching-policy.md \
  docs/plans/spec-clarification/2026-03-03/control-flow/14-if-expression-surface-and-semantics.md
git commit -m "docs(spec): lock branching policy and if exclusion"

git add jazz-hs/src/AST.hs jazz-hs/src/CodeGen/Javascript.hs jazz-hs/src/Parser/Lib.hs docs/jazz-language-state.md
git commit -m "refactor(control-flow): remove unsupported if-expression scaffolding"
```

## Verification Commands

```bash
cd jazz-hs
stack test --ta '--match "if"'
stack test --ta '--match "case"'
stack test
```

## Definition of Done

- [ ] `if` inclusion/exclusion is explicit and documented.
- [ ] No AST/parser/analyzer/runtime drift remains for control-flow surface.
- [ ] Tests enforce the chosen control-flow contract.
