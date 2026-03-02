# Spec Clarification Item #11: ADT and Pattern-Matching Positioning (Locked: CORE)

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to execute this plan phase-by-phase.

**Goal:** Treat ADTs and pattern matching as core language commitments and define a concrete, testable path to full parser/analyzer/interpreter support.

**Architecture:** Keep parser coverage, fill analyzer gaps first, then implement runtime behavior in a Haskell interpreter path. Remove ambiguity from docs/tests by making ADT/pattern support release-critical.

**Tech Stack:** Haskell (`jazz-hs` parser/analyzer/interpreter/tests), Markdown specs, Nix-based reproducible command flow.

---

## Progress Tracker

- [x] Ambiguity evidence gathered from old code/specs
- [x] Positioning decision locked by maintainer: `CORE` (2026-03-02)
- [ ] Core semantics spec finalized
- [ ] Analyzer support for core ADT/pattern forms implemented and verified
- [ ] Interpreter support for core ADT/pattern forms implemented and verified
- [ ] Documentation and status tracking aligned

## Decision Lock (Approved 2026-03-02)

- [x] ADTs are `CORE` language functionality.
- [x] Pattern matching is `CORE` language functionality.
- [x] Testing priority for ADT/pattern behavior is `P0` (release-blocking once implemented in interpreter pipeline).

## Verification Evidence (Why This Needed Clarification)

- `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md:8` and `:15` claim ADTs/pattern matching as features.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md:398` still listed ADT/pattern positioning as unresolved.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs:173-191` parses `data` and `case`.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs:179-181` still errors for unsupported expressions.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/ScopeAnalyzer.hs:103` has `EData` as `undefined`.

## Core Commitment Contract

1. `data` declarations must have defined typing/runtime behavior.
2. Constructor usage must be typechecked and executable.
3. `case` evaluation semantics must be explicit and executable.
4. Pattern forms (`literal`, `wildcard`, `tuple`, `list`, `constructor`) must have deterministic match rules.
5. Pattern-matching failures and non-exhaustive behavior must have explicit diagnostics.
6. Docs cannot advertise partial support as complete support.

## Phase 0: Core Semantics Spec Freeze

- [ ] Create core semantics docs with explicit evaluation and typing rules.
- [ ] Record non-goals (advanced exhaustiveness analysis, GADT-like semantics, effect typing interactions).

Create:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/adt-pattern-semantics.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/pattern-matching-semantics.md`

Commit checkpoint:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/adt-pattern-semantics.md /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/pattern-matching-semantics.md /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/semantics/11-adt-and-pattern-matching-positioning.md
git commit -m "docs(spec): lock core semantics contract for ADT and pattern matching"
```

## Phase 1: Analyzer Completion (TDD)

- [ ] Add failing tests for `EData`, `ECase`, constructor patterns, and lambda pattern parameters.
- [ ] Implement missing analyzer logic in minimal slices.
- [ ] Remove `undefined`/fallback errors for committed ADT/pattern forms.

Modify:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Analyzer/TypeInferenceSpec.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/ScopeAnalyzer.hs`

Commit checkpoint:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Analyzer/TypeInferenceSpec.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/ScopeAnalyzer.hs
git commit -m "feat(analyzer): implement core ADT and pattern matching analysis"
```

## Phase 2: Interpreter Semantics (No JS Backend Dependency)

- [ ] Add failing interpreter tests for constructor creation, case matching, and pattern-parameter lambdas.
- [ ] Implement runtime value representation and pattern matching evaluation.
- [ ] Ensure interpreter diagnostics align with core semantics docs.

Create/Modify:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Interpreter.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/InterpreterSpec.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Spec.hs`

Commit checkpoint:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Interpreter.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/InterpreterSpec.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Spec.hs
git commit -m "feat(interpreter): add core ADT and pattern matching runtime execution"
```

## Phase 3: Parser and Example Conformance

- [ ] Re-enable and expand parser tests for `case` and constructor patterns.
- [ ] Add example programs that exercise ADT/pattern core behavior.

Modify:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/ExamplePrograms/ComplexProgram.jz`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/ExamplePrograms/MediumProgram.jz`

Commit checkpoint:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/ExamplePrograms/ComplexProgram.jz /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/ExamplePrograms/MediumProgram.jz
git commit -m "test(parser): enforce core ADT and pattern syntax expectations"
```

## Phase 4: Documentation and Status Closure

- [ ] Update language-state and README to mark this area as core and implemented (only when verification passes).
- [ ] Link implementation evidence and tests.

Modify:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02/README.md`

Commit checkpoint:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md /Users/admin/.codex/worktrees/8c77/jazz-main/README.md /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-cleanup/2026-03-02/README.md
git commit -m "docs: close ADT and pattern-matching positioning as core"
```

## Nix Verification Commands

```bash
export NIXPKGS_REF='github:NixOS/nixpkgs/68cc97d306d3187c142cfb2378852f28d47bc098'
nix --extra-experimental-features 'nix-command flakes' shell \
  "$NIXPKGS_REF#stack" \
  "$NIXPKGS_REF#ghc" \
  "$NIXPKGS_REF#ripgrep" \
  -c bash -lc '
    set -euo pipefail
    cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs
    stack test --test-arguments "--match case"
    stack test --test-arguments "--match constructor"
    stack test --test-arguments "--match Interpreter"
    stack test
  '
```

## Definition of Done

- [ ] ADT/pattern core semantics are documented and linked.
- [ ] Analyzer + interpreter support core ADT/pattern forms without `undefined`/fallback gaps.
- [ ] Parser + interpreter tests cover the committed behavior.
- [ ] Docs no longer describe this area as unresolved.
