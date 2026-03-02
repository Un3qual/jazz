# Spec-Cleanup Item #6 Plan: Resolve Parse-Only Features in `jazz-hs`

> Item #6 from `docs/jazz-language-state.md`: "Either remove parse-only features from jazz-hs or finish typechecking/codegen for them."

## Goal

Make `jazz-hs` internally consistent so no language feature is parser-accepted but missing in type inference or JS codegen.

## Success Criteria

- Every parser-accepted feature is explicitly classified as either:
  - retained and implemented through parse -> analyze -> JS codegen, or
  - removed from parser/AST/tests/docs.
- Test coverage exists for each retained feature at parser and pipeline level.
- README/spec docs no longer imply support for features that fail end-to-end.
- Work is reproducible in a Nix environment with scripted matrix commands.

## Verification Evidence (Item Is Still Unfinished)

- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`
  Observation: The cleanup list still contains item #6 verbatim, indicating it has not been resolved.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs`
  Observation: Parser accepts `import`, `module`, `class`, `impl`, `data`, `case`, tuple literals, and lambda pattern params (`FPPattern`).
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs`
  Observation: `inferExpr` only handles a subset (`ELet`, `ELiteral`, `EVar`, `EApply`, `ELambda`, `ETypeSignature`, `EBlock`) and falls back to `error "Inference not implemented for this expression"` for other AST nodes.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs`
  Observation: `inferParam` explicitly errors for `FPPattern` (`"Inference for pattern matching not implemented yet."`).
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Javascript.hs`
  Observation: Tuple literal codegen fails (`"JS generation not implemented for tuples."`), pattern params fail (`"Generation for pattern matching not implemented yet."`), and unsupported expressions hit a catch-all `error`.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs`
  Observation: Parser tests exist for `import/module/class/impl/data` and constructor patterns, but these features are not covered end-to-end; `case` parser tests are present but commented out.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lib.hs` + `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs`
  Observation: `if`/`else` are reserved words and `EIf` exists in AST/codegen, but no parser path constructs `EIf`; this is additional scaffolding drift to clean up while resolving item #6.

## Progress Tracker

- [ ] Phase 0: Resolve strategy and write per-feature decision matrix
- [ ] Phase 1: Add Nix-based reproducible dev/test environment and baseline matrix
- [ ] Phase 2: Implement chosen feature-resolution work (remove vs complete, per feature)
- [ ] Phase 3: Align tests, fixtures, and docs to final feature surface
- [ ] Phase 4: Final verification matrix and closure of spec-cleanup item #6

## Phase 0: Strategy Resolution (Decision Gate)

- [ ] Create a feature-resolution matrix at:
  - `docs/plans/spec-cleanup/2026-03-02/compiler/06-feature-resolution-matrix.md`
- [ ] Enumerate each currently parse-accepted feature family and assign one action:
  - `REMOVE` (delete parser/AST surface)
  - `IMPLEMENT` (finish type inference + codegen)
- [ ] Use objective decision criteria per feature:
  - implementation complexity and risk
  - compatibility with working subset
  - testability in current architecture
  - impact on top-level docs claims
- [ ] Create execution sub-plan docs (to split large work):
  - `docs/plans/spec-cleanup/2026-03-02/compiler/06a-remove-parse-only-track.md`
  - `docs/plans/spec-cleanup/2026-03-02/compiler/06b-complete-pipeline-track.md`
- [ ] Freeze a final per-feature decision table before code changes.

### Commit Checkpoint (Phase 0)

Suggested message:
`plan(spec-cleanup-6): define per-feature remove-vs-implement strategy`

Exact `git add` targets:
```bash
git add docs/plans/spec-cleanup/2026-03-02/compiler/06-feature-resolution-matrix.md \
  docs/plans/spec-cleanup/2026-03-02/compiler/06a-remove-parse-only-track.md \
  docs/plans/spec-cleanup/2026-03-02/compiler/06b-complete-pipeline-track.md
```

## Phase 1: Reproducible Environment + Baseline Matrix (Nix Required)

- [ ] Add a pinned Nix dev shell for `jazz-hs`.
  - Preferred files:
    - `jazz-hs/flake.nix`
    - `jazz-hs/flake.lock`
- [ ] Ensure the dev shell contains at minimum: `stack`, compatible `ghc`, `nodejs`, `bash`, `git`.
- [ ] Add matrix runner scripts:
  - `jazz-hs/scripts/spec-cleanup-06/run-matrix.sh`
  - `jazz-hs/scripts/spec-cleanup-06/run-matrix-remove-track.sh`
  - `jazz-hs/scripts/spec-cleanup-06/run-matrix-implement-track.sh`
- [ ] Persist command output to reproducible artifacts directory:
  - `docs/plans/spec-cleanup/2026-03-02/compiler/artifacts/06-baseline/`

### Reproducible Baseline Commands (from repo root)

```bash
nix develop ./jazz-hs -c stack --version
nix develop ./jazz-hs -c ghc --version
nix develop ./jazz-hs -c node --version
nix develop ./jazz-hs -c stack test
nix develop ./jazz-hs -c stack test --test-arguments "--match \"Statement Tests\""
nix develop ./jazz-hs -c stack test --test-arguments "--match \"Simple Expression Type Inference Tests\""
nix develop ./jazz-hs -c stack run -- ExamplePrograms/ComplexProgram.jz > /tmp/jazz-spec6-complex.js
nix develop ./jazz-hs -c node /tmp/jazz-spec6-complex.js
```

### Commit Checkpoint (Phase 1)

Suggested message:
`build(spec-cleanup-6): add nix devshell and reproducible test matrix scripts`

Exact `git add` targets:
```bash
git add jazz-hs/flake.nix \
  jazz-hs/flake.lock \
  jazz-hs/scripts/spec-cleanup-06/run-matrix.sh \
  jazz-hs/scripts/spec-cleanup-06/run-matrix-remove-track.sh \
  jazz-hs/scripts/spec-cleanup-06/run-matrix-implement-track.sh \
  docs/plans/spec-cleanup/2026-03-02/compiler/artifacts/06-baseline
```

## Phase 2: Execute Per-Feature Resolution Decisions

For each feature family below, execute only one of `REMOVE` or `IMPLEMENT` from the frozen matrix:

- data declarations (`EData`)
- class declarations (`EClass`)
- impl declarations (`EClassImpl`)
- module/import declarations (`EModule`, `EImport`)
- case expressions (`ECase`)
- tuple runtime/codegen (`LTuple`)
- lambda pattern parameters (`FPPattern`)
- type signatures in codegen path (`ETypeSignature`)

- [ ] Apply feature-by-feature changes in small slices with tests in the same commit.
- [ ] Do not leave mixed intermediate states where parser accepts syntax that analysis/codegen rejects.
- [ ] Keep each slice independently reversible.

## Phase 3A (If Matrix Chooses REMOVE for a Feature)

- [ ] Remove AST constructors and unspanned forms for that feature from:
  - `jazz-hs/src/AST.hs`
- [ ] Remove parser entry points/branches and keyword handling from:
  - `jazz-hs/src/Parser/Lang.hs`
  - `jazz-hs/src/Parser/Lib.hs` (only where keywords are no longer language surface)
- [ ] Update parser tests to assert parse failure (not parser success) in:
  - `jazz-hs/test/ParserSpec.hs`
- [ ] Remove or simplify now-unreachable analysis/codegen branches in:
  - `jazz-hs/src/Analyzer/TypeInference.hs`
  - `jazz-hs/src/CodeGen/Javascript.hs`
- [ ] Update docs to stop claiming removed syntax as current capability.

### Commit Checkpoints (Remove Track)

Checkpoint A1 message:
`refactor(parser): remove unsupported parse-only constructs from surface grammar`

Exact `git add` targets:
```bash
git add jazz-hs/src/AST.hs \
  jazz-hs/src/Parser/Lang.hs \
  jazz-hs/src/Parser/Lib.hs \
  jazz-hs/test/ParserSpec.hs
```

Checkpoint A2 message:
`refactor(compiler): delete unreachable analyzer/codegen branches after syntax removal`

Exact `git add` targets:
```bash
git add jazz-hs/src/Analyzer/TypeInference.hs \
  jazz-hs/src/CodeGen/Javascript.hs \
  jazz-hs/test/Analyzer/TypeInferenceSpec.hs
```

## Phase 3B (If Matrix Chooses IMPLEMENT for a Feature)

- [ ] Define feature semantics before coding in sub-plan docs:
  - `docs/plans/spec-cleanup/2026-03-02/compiler/06b1-patterns-tuples-case.md`
  - `docs/plans/spec-cleanup/2026-03-02/compiler/06b2-declarations-modules-classes.md`
- [ ] Implement analyzer/type-inference support first (with failing tests written first).
- [ ] Implement JS codegen for the same feature slice immediately after inference support.
- [ ] Add end-to-end tests that parse + analyze + codegen for each implemented feature.
- [ ] Avoid catch-all runtime `error` branches for features declared as implemented.

### Commit Checkpoints (Implement Track)

Checkpoint B1 message:
`feat(type-inference): support selected formerly parse-only feature slice`

Exact `git add` targets:
```bash
git add jazz-hs/src/Analyzer/TypeInference.hs \
  jazz-hs/test/Analyzer/TypeInferenceSpec.hs
```

Checkpoint B2 message:
`feat(codegen): add js lowering for selected formerly parse-only feature slice`

Exact `git add` targets:
```bash
git add jazz-hs/src/CodeGen/Javascript.hs \
  jazz-hs/test/CodeGenSpec.hs \
  jazz-hs/test/Spec.hs
```

Checkpoint B3 message:
`feat(parser-pipeline): complete end-to-end support for selected feature slice`

Exact `git add` targets:
```bash
git add jazz-hs/src/AST.hs \
  jazz-hs/src/Parser/Lang.hs \
  jazz-hs/src/Analyzer/TypeInference.hs \
  jazz-hs/src/CodeGen/Javascript.hs \
  jazz-hs/test/ParserSpec.hs \
  jazz-hs/test/Analyzer/TypeInferenceSpec.hs \
  jazz-hs/test/CodeGenSpec.hs \
  jazz-hs/test/Spec.hs
```

## Phase 4: Documentation Alignment + Closure

- [ ] Update user-facing language claims and implemented subset docs:
  - `README.md`
  - `docs/jazz-language-state.md`
  - `jazz-hs/README.md` (if used as canonical local compiler doc)
- [ ] Replace ambiguous claims with explicit implemented/deferred sections.
- [ ] Close item #6 in cleanup tracking docs by referencing merged commits.

### Commit Checkpoint (Phase 4)

Suggested message:
`docs(spec-cleanup-6): align language claims with implemented compiler surface`

Exact `git add` targets:
```bash
git add README.md \
  docs/jazz-language-state.md \
  jazz-hs/README.md \
  docs/plans/spec-cleanup/2026-03-02/compiler
```

## Final Verification Matrix (Must Pass Before Declaring Done)

Run from repo root in Nix shell:

```bash
nix develop ./jazz-hs -c stack test
nix develop ./jazz-hs -c stack test --test-arguments "--match \"Statement Tests\""
nix develop ./jazz-hs -c stack test --test-arguments "--match \"Simple Expression Type Inference Tests\""
nix develop ./jazz-hs -c stack test --test-arguments "--match \"constructor\""
nix develop ./jazz-hs -c stack run -- ExamplePrograms/ComplexProgram.jz > /tmp/jazz-spec6-final.js
nix develop ./jazz-hs -c node /tmp/jazz-spec6-final.js
```

Track-specific assertions:

- Remove track:
  - [ ] Removed features now fail parser tests with explicit diagnostics.
  - [ ] No analyzer/codegen paths mention removed AST nodes.
- Implement track:
  - [ ] Retained features have parser + inference + codegen coverage and no catch-all `error` path for those forms.

## Exit Criteria Checklist

- [ ] Per-feature matrix has no `UNDECIDED` rows.
- [ ] No parser-accepted feature is missing type inference/codegen support.
- [ ] Docs no longer over-claim unsupported language features.
- [ ] Nix-based matrix commands are documented and reproducible.
- [ ] Item #6 is explicitly marked resolved with commit references.
