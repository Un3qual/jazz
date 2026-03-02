# Spec Clarification Item #09: Module Loader and Import Resolution Semantics Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to execute this plan task-by-task.

**Goal:** Clarify module/import semantics beyond syntax so Jazz has an explicit, testable contract for file layout, module resolution, loader behavior, qualified imports, and migration constraints.

**Architecture:** Use a docs-first clarification flow: capture unresolved behavior from current code/specs, publish normative semantics docs, then encode the semantics in parser/analyzer/loader verification tests before implementation changes.

**Tech Stack:** Markdown specs and plans, `rg`/`sed`/`git`, Haskell (`stack`, `hspec`, `tasty`) in `jazz-hs`, Nix (`flake.nix`, `nix develop`) for reproducible checks.

---

## Progress

- [x] Collected unresolved semantics evidence from current specs and compiler code.
- [x] Mapped module/import behavior gaps across parser, analyzer, codegen, CLI loader path, and tests.
- [x] Defined phased clarification plan with commit checkpoints and reproducible command matrix.
- [ ] Execute clarification phases and publish normative module/import specs.
- [ ] Implement/verify final semantics in compiler/runtime code after clarification approval.

## Verification Evidence (Unresolved Semantics)

- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md:255`
  Observation: `module` and `import` are explicitly listed as "features that exist mostly as scaffolding".
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md:261`
  Observation: "true module loading" is listed as missing, confirming unresolved loader/runtime semantics.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md:396`
  Observation: spec still asks whether modules/imports are only syntax or part of a real loader.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/README.md:114`
  Observation: README shows module syntax examples but does not define file-to-module mapping, package roots, or loader order.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Parser/Lang.hs:131`
  Observation: parser accepts global/child module paths plus optional `as` and symbol-list qualifiers, but only as AST shape.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/AST.hs:141`
  Observation: AST models `ModuleName` and `ImportQualifier`, but no package/source-root/file metadata exists for resolution.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer/TypeInference.hs:179`
  Observation: non-handled expressions hit a generic error path; `EImport`/`EModule` are not implemented in type inference.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Analyzer.hs:11`
  Observation: analysis pipeline only invokes inference; there is no module graph or import resolution stage.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/CodeGen/Javascript.hs:43`
  Observation: unsupported expressions fail with a catch-all error; no import/module lowering behavior is defined.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/src/Lib.hs:63`
  Observation: compilation path parses/analyzes one provided text blob, with no dependency loading or multi-file module traversal.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/app/Main.hs:12`
  Observation: CLI reads one source file and compiles directly; there is no module resolver/loader entrypoint.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs:555`
  Observation: import/module tests validate parse output (`TEImport`/`TEModule`) only; no analyzer/loader semantics are verified.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz2/src/Jazz/AST.hs:76`
  Observation: old `jazz2` module/require declarations are mostly commented out, showing historical but unresolved module-system redesign.
- `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/static/Prelude.jz:1`
  Observation: nested module-heavy prelude exists, but there is no active loader wiring that imports/links it into compilation.

**Conclusion:** module/import syntax exists, but resolution and loader semantics are still undefined and non-executable end-to-end.

## Scope and Clarification Targets

- [ ] File layout contract (module name to file path/package root mapping).
- [ ] Module resolution algorithm (search order, ambiguity rules, cycle/error model).
- [ ] Loader behavior (entrypoint, dependency graph build, caching, determinism, diagnostics).
- [ ] Qualified import behavior (`as`, symbol-list imports, collisions, shadowing).
- [ ] Migration constraints from current parser-first/parse-only state to enforceable semantics.

## Proposed Sub-Plans (If Decomposition Is Needed)

- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/modules/subplans/09a-file-layout-and-package-roots.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/modules/subplans/09b-resolution-order-and-cycle-rules.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/modules/subplans/09c-loader-pipeline-caching-and-errors.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/modules/subplans/09d-qualified-imports-and-name-binding.md`
- `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/modules/subplans/09e-migration-and-compatibility-constraints.md`

## Phase 0: Baseline Clarification Matrix and Decision Inputs

- [ ] Create a baseline matrix documenting unresolved semantics and candidate options:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/00-module-clarification-matrix.md`
- [ ] For each semantic area (layout, resolution, loader, qualified imports), record:
  - current behavior (if any)
  - known gaps
  - invariants to preserve
  - open decision questions
- [ ] Add evidence links to current code/specs for every matrix row.
- [ ] Explicitly tag which rows require maintainer product decisions vs executor research decisions.

### Commit Checkpoint (Phase 0)

Suggested message:
`docs(spec-modules): add baseline clarification matrix for module/import semantics`

Exact `git add` targets:
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/00-module-clarification-matrix.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/modules/subplans/09a-file-layout-and-package-roots.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/modules/subplans/09b-resolution-order-and-cycle-rules.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/modules/subplans/09c-loader-pipeline-caching-and-errors.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/modules/subplans/09d-qualified-imports-and-name-binding.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/plans/spec-clarification/2026-03-02/modules/subplans/09e-migration-and-compatibility-constraints.md
```

## Phase 1: File Layout and Package-Root Contract

- [ ] Define canonical mapping rules from module names to files.
- [ ] Clarify root semantics for:
  - workspace-local modules
  - optional package roots
  - relative module references (if retained)
- [ ] Decide case-sensitivity, separator normalization, and extension rules (`.jz`, future alternatives).
- [ ] Publish normative file-layout spec:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/01-file-layout-and-package-roots.md`
- [ ] Add migration notes for old patterns (`module A::B {}` in nested blocks vs one-file-per-module expectations).

Research latitude for executor:
- [ ] May evaluate two strategies before final recommendation:
  - strict one-file-per-module canonicalization
  - hybrid explicit-module-block compatibility mode

### Commit Checkpoint (Phase 1)

Suggested message:
`docs(spec-modules): define module-to-file layout and package root contract`

Exact `git add` targets:
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/01-file-layout-and-package-roots.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/00-module-clarification-matrix.md
```

## Phase 2: Resolution Algorithm and Dependency Graph Semantics

- [ ] Specify import resolution order deterministically:
  - local module scope
  - explicit package roots
  - standard library root
- [ ] Define ambiguity policy when multiple modules match.
- [ ] Define unresolved import diagnostics (error categories, required context in messages).
- [ ] Define cycle detection and reporting behavior (including minimal cycle trace requirement).
- [ ] Publish normative resolver spec:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/02-resolution-algorithm-and-cycles.md`
- [ ] Attach pseudocode and truth-table examples for ambiguous/cyclic cases.

Research latitude for executor:
- [ ] Can choose DFS or BFS graph traversal as long as deterministic ordering and diagnostics requirements are met.

### Commit Checkpoint (Phase 2)

Suggested message:
`docs(spec-modules): define deterministic import resolution and cycle semantics`

Exact `git add` targets:
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/02-resolution-algorithm-and-cycles.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/00-module-clarification-matrix.md
```

## Phase 3: Loader Pipeline Behavior (Parse, Analyze, Cache, Emit)

- [ ] Define loader entrypoint contract (single-file compile vs module-graph compile).
- [ ] Define when parsing, resolution, analysis, and codegen occur in the pipeline.
- [ ] Define cache semantics and invalidation invariants (if cache is introduced).
- [ ] Define stable diagnostic contract including:
  - source location requirements
  - import chain context requirements
  - reproducibility expectations
- [ ] Publish loader behavior spec:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/03-loader-behavior-and-diagnostics.md`
- [ ] Document minimum behavior required to retire "parse-only import/module" status.

Research latitude for executor:
- [ ] Can recommend no-cache V1 loader if explicitly justified with complexity and correctness tradeoffs.

### Commit Checkpoint (Phase 3)

Suggested message:
`docs(spec-modules): define loader pipeline behavior and diagnostics`

Exact `git add` targets:
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/03-loader-behavior-and-diagnostics.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/00-module-clarification-matrix.md
```

## Phase 4: Qualified Import and Name-Binding Semantics

- [ ] Clarify exact semantics for:
  - `import Foo::Bar as B`
  - `import Std::List (map, filter)`
  - coexistence of `as` alias and symbol-list import (allowed/disallowed)
- [ ] Define namespace collision/shadowing rules:
  - local bindings vs imported names
  - alias conflicts
  - duplicate symbol imports
- [ ] Define whether qualification is required or optional after `as`.
- [ ] Publish qualified-import spec:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/04-qualified-imports-and-binding.md`
- [ ] Add executable examples and explicit invalid-case examples.

Research latitude for executor:
- [ ] May propose additional syntax restrictions if they reduce ambiguity and are migration-safe.

### Commit Checkpoint (Phase 4)

Suggested message:
`docs(spec-modules): define qualified import and name binding behavior`

Exact `git add` targets:
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/04-qualified-imports-and-binding.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/00-module-clarification-matrix.md
```

## Phase 5: Migration Constraints and Compatibility Windows

- [ ] Define migration constraints from current behavior:
  - parser accepts module/import forms today
  - analyzer/codegen do not implement them
  - single-file CLI contract currently exists
- [ ] Define compatibility policy:
  - accepted legacy forms
  - deprecation warnings timeline
  - removal gates
- [ ] Define migration safety checks:
  - no silent behavior changes for existing single-file programs
  - deterministic failure modes for newly enforced import semantics
- [ ] Publish migration spec:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/05-migration-and-compatibility.md`
- [ ] Update tracking docs to reference clarified module semantics:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md`

### Commit Checkpoint (Phase 5)

Suggested message:
`docs(spec-modules): add migration and compatibility policy for loader rollout`

Exact `git add` targets:
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/05-migration-and-compatibility.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/jazz-language-state.md \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules/00-module-clarification-matrix.md
```

## Phase 6: Verification Harness and Closure

- [ ] Add parser/analyzer/loader verification tests aligned to clarified semantics:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs`
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Analyzer/TypeInferenceSpec.hs`
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ModuleResolutionSpec.hs` (new)
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/LoaderSpec.hs` (new)
- [ ] Add docs consistency checks for module semantics references:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/scripts/check-module-docs.sh`
- [ ] Ensure docs/spec/test contract stays synchronized with reproducible commands.
- [ ] Close clarification item status once all checks pass and docs are linked.

### Commit Checkpoint (Phase 6)

Suggested message:
`test(spec-modules): add module resolution and loader verification coverage`

Exact `git add` targets:
```bash
git add /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ParserSpec.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/Analyzer/TypeInferenceSpec.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/ModuleResolutionSpec.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/test/LoaderSpec.hs \
  /Users/admin/.codex/worktrees/8c77/jazz-main/scripts/check-module-docs.sh \
  /Users/admin/.codex/worktrees/8c77/jazz-main/docs/spec/modules
```

## Nix Environment and Reproducible Command Matrix

### Nix Environment (Required)

- [ ] Add/maintain a pinned dev shell for `jazz-hs`:
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/flake.nix`
  - `/Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs/flake.lock`
- [ ] Ensure shell includes: `stack`, `ghc`, `nodejs`, `bash`, `git`, `ripgrep`.

### Environment Sanity Commands

```bash
nix --extra-experimental-features "nix-command flakes" develop /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs -c stack --version
nix --extra-experimental-features "nix-command flakes" develop /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs -c ghc --version
nix --extra-experimental-features "nix-command flakes" develop /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs -c node --version
```

### Parser Checks (Reproducible)

```bash
nix --extra-experimental-features "nix-command flakes" develop /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs -c bash -lc 'cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs && stack test --ta "--match \"when given an import statement|when given a module definition\""' 
nix --extra-experimental-features "nix-command flakes" develop /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs -c bash -lc 'cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs && stack test --ta "--match \"Should handle a simple program with imports\""' 
```

### Analyzer/Loader Checks (Reproducible)

```bash
nix --extra-experimental-features "nix-command flakes" develop /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs -c bash -lc 'cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs && stack test --ta "--match \"Type Inference\""' 
nix --extra-experimental-features "nix-command flakes" develop /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs -c bash -lc 'cd /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs && stack test --ta "--match \"ModuleResolutionSpec|LoaderSpec\""' 
```

### Documentation Consistency Checks (Reproducible)

```bash
nix --extra-experimental-features "nix-command flakes" develop /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs -c bash -lc 'cd /Users/admin/.codex/worktrees/8c77/jazz-main && rg -n "module|import|resolution|loader|qualified" docs/spec/modules docs/jazz-language-state.md README.md'
nix --extra-experimental-features "nix-command flakes" develop /Users/admin/.codex/worktrees/8c77/jazz-main/jazz-hs -c bash -lc 'cd /Users/admin/.codex/worktrees/8c77/jazz-main && bash scripts/check-module-docs.sh'
```

## Final Exit Criteria

- [ ] File-layout rules are normative and unambiguous.
- [ ] Resolution order, ambiguity handling, and cycle semantics are specified.
- [ ] Loader behavior is documented with deterministic diagnostics requirements.
- [ ] Qualified import/name-binding rules are explicit and testable.
- [ ] Migration constraints preserve current users while enabling loader rollout.
- [ ] Nix-based parser/analyzer/doc checks are reproducible and documented.

## Short Checkbox Summary

- [x] Evidence of unresolved module/import semantics recorded with exact paths.
- [x] Phased clarification plan with commit checkpoints and exact `git add` targets.
- [x] Nix environment and reproducible parser/analyzer/doc command matrix included.
- [x] Sub-plan paths proposed for deeper research-driven breakdown.
