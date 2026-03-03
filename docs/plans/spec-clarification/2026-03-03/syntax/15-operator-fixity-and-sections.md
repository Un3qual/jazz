# Operator Fixity and Sections Policy Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Define a canonical operator model (fixity, precedence, associativity, and sections) that is explicit, testable, and maintainable.

**Architecture:** Publish a normative operator contract first, then align parser and tests to that contract. Preserve room for future extensibility but prevent implicit parser hacks from becoming de facto language rules.

**Tech Stack:** Haskell parser (`Megaparsec` in `jazz-hs`), parser tests, Markdown spec docs.

---

## Progress

- [x] Operator drift evidence captured
- [ ] Fixity model decision locked
- [x] Section semantics decision locked
- [ ] Parser/tests aligned with canonical contract
- [x] User-defined operator roadmap direction decided (staged)

## Decision Lock (Approved 2026-03-03)

- [x] User-defined operators will be delivered in staged phases.
- [x] Stage architecture must preserve easy completion in later phases (no parser rewrite trap).
- [ ] Builtin fixity table lock (Gate A) still required.
- [x] Section representation lock (Gate B): explicit AST node for operator sections.

## Verification Evidence (Current Drift)

- `jazz-hs/src/Parser/Operator.hs`:
  - hardcoded `operatorTable` and `builtinInfixOps`,
  - TODO for user-defined operators.
- `jazz-hs/src/Parser/Lang.hs`:
  - right-partial sections use synthetic lambda parameter `__partialInfixLambdaParam0`.
- `jazz-hs/test/ParserSpec.hs`:
  - tests encode current partial-section behavior, including the synthetic lambda shape.

## Scope Guardrails

In scope:
- builtin operator fixity contract,
- partial section semantics,
- explicit stance on user-defined operators (defer vs implement).

Out of scope:
- full operator overloading semantics,
- custom precedence declarations unless gate explicitly enables them.

## Decision Gates

- [ ] Gate A: builtin operator set and fixity are frozen for v1.
- [x] Gate B: section semantics policy.
  - [ ] Option B1: keep lambda-desugaring semantics but hide synthetic names from external AST contract.
  - [x] Option B2 (selected): introduce explicit AST node for operator sections.
  - [ ] Option B3: limit section support to one side for now (documented restriction).
- [x] Gate C: user-defined operators.
  - [ ] Option C1: defer with explicit non-goal.
  - [x] Option C2 (selected): staged support with restricted characters and fixed precedence tiers.
  - [ ] Option C3: full declarations now (highest complexity).

## Phase 0: Normative Operator Spec

- [ ] Write EBNF-like operator grammar and fixity table.
- [ ] Record precedence and associativity examples, including `$`.
- [ ] Record section desugaring examples and invalid cases.
- [ ] Define section AST contract and lowering rules:
  - Add explicit section nodes (for example `ESectionLeft`/`ESectionRight` or equivalent).
  - Keep parser output stable and independent from synthetic lambda variable names.
  - Lower section nodes in desugaring, not in parser ad hoc branches.
- [ ] Define stage boundaries and compatibility contract:
  - Stage 1: frozen builtin operators + parser/runtime cleanup.
  - Stage 2: controlled user-defined operator declarations with fixed precedence tiers.
  - Stage 3: optional custom precedence declarations (only if needed).
- [ ] Specify extensibility mechanism that keeps later phases low-risk:
  - parser consumes a centralized operator metadata table,
  - tests assert both current behavior and forward-compatible table format.

Create:
- `docs/spec/syntax/operators.md`

### Commit Checkpoint (Phase 0)

```bash
git add docs/spec/syntax/operators.md \
  docs/plans/spec-clarification/2026-03-03/syntax/15-operator-fixity-and-sections.md
git commit -m "docs(spec): define canonical operator fixity and section rules"
```

## Phase 1: Tests-First Alignment

- [ ] Add parser tests that assert canonical fixity behavior and section AST contract.
- [ ] Add explicit invalid-case tests for ambiguous or unsupported operator forms.

Modify:
- `jazz-hs/test/ParserSpec.hs`

### Commit Checkpoint (Phase 1)

```bash
git add jazz-hs/test/ParserSpec.hs
git commit -m "test(parser): codify operator fixity and section contract"
```

## Phase 2: Parser Refactor

- [ ] Align `Parser.Operator` with the published fixity table.
- [ ] Replace synthetic-name leakage in section semantics (either AST refactor or internal hygiene pass).
- [ ] If Gate C enables user-defined operators, implement only approved scope.

Modify:
- `jazz-hs/src/Parser/Operator.hs`
- `jazz-hs/src/Parser/Lang.hs`
- `jazz-hs/src/AST.hs` (required for explicit section nodes)
- `jazz-hs/src/Desugar.hs` or `jazz-hs/src/Desugar/Operators.hs` (section-node lowering)

### Commit Checkpoint (Phase 2)

```bash
git add jazz-hs/src/Parser/Operator.hs jazz-hs/src/Parser/Lang.hs jazz-hs/src/AST.hs
git commit -m "feat(parser): align operator parsing with canonical fixity policy"
```

(If `AST.hs` is unchanged, omit it.)

## Phase 3: Documentation and Drift Prevention

- [ ] Update language-state/operator references to canonical wording.
- [ ] Add one maintenance checklist section so future operator changes require spec+test updates together.

Modify:
- `docs/jazz-language-state.md`
- `docs/plans/spec-clarification/2026-03-03/README.md`

### Commit Checkpoint (Phase 3)

```bash
git add docs/jazz-language-state.md docs/plans/spec-clarification/2026-03-03/README.md
git commit -m "docs(spec): close operator fixity clarification and add anti-drift checks"
```

## Verification Commands

```bash
cd jazz-hs
stack test --ta '--match "infix"'
stack test --ta '--match "partial"'
stack test --ta '--match "operator"'
stack test
```

## Definition of Done

- [ ] Operator precedence/associativity is explicitly specified.
- [ ] Section behavior is explicit and test-protected.
- [ ] User-defined operator status is no longer implicit/TODO-only.
