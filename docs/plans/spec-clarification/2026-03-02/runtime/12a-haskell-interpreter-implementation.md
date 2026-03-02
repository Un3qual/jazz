# Haskell Interpreter Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement a working Haskell interpreter as the primary Jazz execution backend, replacing JS codegen as the active runtime path.

**Architecture:** Deliver interpreter functionality in vertical slices: runtime values -> evaluator core -> builtins/environment -> control/data features -> CLI integration. Keep analyzer checks in place and use tests-first progression.

**Tech Stack:** Haskell (`jazz-hs`), `stack` tests, Nix shell reproducibility, existing AST/parser/analyzer modules.

---

## Progress

- [x] Runtime strategy precondition set (`interpreter-only`) in item #12
- [ ] Phase 0 baseline and interpreter scope freeze
- [ ] Phase 1 runtime value model and minimal evaluator loop
- [ ] Phase 2 function application, lexical scope, and builtins
- [ ] Phase 3 control/data features (case, data constructors, patterns)
- [ ] Phase 4 CLI/runtime integration and docs alignment
- [ ] Phase 5 verification and closure

## Scope Guardrails

In scope:
- interpreter execution for core language surface,
- deterministic runtime errors,
- interpreter-first CLI flow.

Out of scope:
- JS codegen parity work,
- LLVM backend work,
- advanced optimization passes.

## Phase 0: Baseline + Scope Freeze

- [ ] Confirm parser/analyzer baseline test status before interpreter changes.
- [ ] Freeze first runtime feature slice with explicit included/excluded constructs.
- [ ] Create interpreter test module scaffold.

Create/Modify:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/InterpreterSpec.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Spec.hs`

Commit checkpoint:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/InterpreterSpec.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Spec.hs /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md
git commit -m "test(interpreter): add interpreter test scaffold and scope freeze"
```

## Phase 1: Runtime Values + Minimal Evaluator

- [ ] Define runtime value ADT (ints, floats, bools, strings, lists, functions, constructors).
- [ ] Implement minimal expression evaluator for literals/variables/let/block.
- [ ] Add failing tests then pass them.

Modify:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Interpreter.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/InterpreterSpec.hs`

Commit checkpoint:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Interpreter.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/InterpreterSpec.hs
git commit -m "feat(interpreter): implement runtime values and core expression eval"
```

## Phase 2: Functions, Closures, and Builtins

- [ ] Implement lambda evaluation and closure capture.
- [ ] Implement function application semantics.
- [ ] Wire builtin runtime environment (`+`, `-`, `*`, `/`, `==`, `print!`, `map`, `hd`, `tl`) with purity checks from analyzer respected upstream.
- [ ] Add interpreter tests for currying and partial application.

Modify:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Interpreter.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs` (only if shared builtin metadata extraction is needed)
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/InterpreterSpec.hs`

Commit checkpoint:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Interpreter.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Types.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/InterpreterSpec.hs
git commit -m "feat(interpreter): add closures, application, and builtin runtime"
```

(If `Types.hs` is unchanged, omit it.)

## Phase 3: ADT/Pattern/Case Runtime Support

- [ ] Implement constructor value representation and matching semantics.
- [ ] Implement `case` evaluation.
- [ ] Implement lambda pattern-parameter behavior consistent with item #11 core contract.
- [ ] Add exhaustive tests for success/failure pattern-match paths.

Modify:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Interpreter.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/InterpreterSpec.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Analyzer/TypeInferenceSpec.hs` (for analyzer/runtime contract alignment)

Commit checkpoint:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Interpreter.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/InterpreterSpec.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Analyzer/TypeInferenceSpec.hs
git commit -m "feat(interpreter): add case and pattern matching runtime semantics"
```

## Phase 4: CLI Integration and Pipeline Switch

- [ ] Add interpreter entrypoint in library layer.
- [ ] Update CLI to run interpreter path by default.
- [ ] Keep any legacy codegen entrypoint clearly non-default/deprecated until removed.

Modify:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Lib.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/app/Main.hs`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/run.sh`

Commit checkpoint:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Lib.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/app/Main.hs /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/run.sh
git commit -m "feat(runtime): switch cli execution to haskell interpreter"
```

## Phase 5: Docs + Verification + Closure

- [ ] Update runtime docs and examples for interpreter-first usage.
- [ ] Verify full tests and example program execution.
- [ ] Record closure in planning trackers.

Modify:
- `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/README.md`

Commit checkpoint:

```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/README.md /Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/README.md
git commit -m "docs(runtime): document interpreter-first execution and close plan"
```

## Nix Verification Commands

```bash
export NIXPKGS_REF='github:NixOS/nixpkgs/68cc97d306d3187c142cfb2378852f28d47bc098'
nix --extra-experimental-features 'nix-command flakes' shell \
  "$NIXPKGS_REF#stack" \
  "$NIXPKGS_REF#ghc" \
  -c bash -lc '
    set -euo pipefail
    cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs
    stack test --test-arguments "--match Interpreter"
    stack test
  '
```

## Definition of Done

- [ ] Interpreter executes core language programs through CLI.
- [ ] Interpreter tests cover literals, functions, builtins, ADT/case/pattern behavior.
- [ ] JS/LLVM are not active runtime dependencies.
- [ ] Docs and plans consistently describe interpreter-only strategy.
