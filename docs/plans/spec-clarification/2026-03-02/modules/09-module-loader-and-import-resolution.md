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
- [x] Executed `jazz-next` parser/lowering bootstrap batch for module/import declarations (syntax-only; resolver/loader still pending).
- [x] Executed `jazz-next` resolver/loader integration batches with deterministic module-graph diagnostics and CLI `--entry-module`/`--module-root` flow.
- [x] Executed `jazz-next` module declaration contract batch (`E4005`/`E4006`) with resolver/loader/CLI regression coverage.
- [x] Executed `jazz-next` qualified-import parser hardening batch for duplicate symbol-list imports.
- [x] Executed `jazz-next` qualified-import binding batch with resolver-enforced symbol export validation and alias/symbol collision diagnostics (`E4007`/`E4008`/`E4009`).
- [ ] Execute clarification phases and publish normative module/import specs.
- [ ] Implement/verify final semantics in compiler/runtime code after clarification approval.

## Verification Evidence (Unresolved Semantics)

- `docs/jazz-language-state.md:255`
  Observation: `module` and `import` are explicitly listed as "features that exist mostly as scaffolding".
- `docs/jazz-language-state.md:261`
  Observation: "true module loading" is listed as missing, confirming unresolved loader/runtime semantics.
- `docs/jazz-language-state.md:396`
  Observation: spec still asks whether modules/imports are only syntax or part of a real loader.
- `README.md:114`
  Observation: README shows module syntax examples but does not define file-to-module mapping, package roots, or loader order.
- `jazz-hs/src/Parser/Lang.hs:131`
  Observation: parser accepts global/child module paths plus optional `as` and symbol-list qualifiers, but only as AST shape.
- `jazz-hs/src/AST.hs:141`
  Observation: AST models `ModuleName` and `ImportQualifier`, but no package/source-root/file metadata exists for resolution.
- `jazz-hs/src/Analyzer/TypeInference.hs:179`
  Observation: non-handled expressions hit a generic error path; `EImport`/`EModule` are not implemented in type inference.
- `jazz-hs/src/Analyzer.hs:11`
  Observation: analysis pipeline only invokes inference; there is no module graph or import resolution stage.
- `jazz-hs/src/CodeGen/Javascript.hs:43`
  Observation: unsupported expressions fail with a catch-all error; no import/module lowering behavior is defined.
- `jazz-hs/src/Lib.hs:63`
  Observation: compilation path parses/analyzes one provided text blob, with no dependency loading or multi-file module traversal.
- `jazz-hs/app/Main.hs:12`
  Observation: CLI reads one source file and compiles directly; there is no module resolver/loader entrypoint.
- `jazz-hs/test/ParserSpec.hs:555`
  Observation: import/module tests validate parse output (`TEImport`/`TEModule`) only; no analyzer/loader semantics are verified.
- `jazz2/src/Jazz/AST.hs:76`
  Observation: old `jazz2` module/require declarations are mostly commented out, showing historical but unresolved module-system redesign.
- `jazz-hs/static/Prelude.jz:1`
  Observation: nested module-heavy prelude exists, but there is no active loader wiring that imports/links it into compilation.

**Conclusion:** legacy `jazz-hs` behavior remains parse-only, while active `jazz-next` now has executable module-graph resolution/loader behavior plus resolver-enforced qualified-import binding checks; normative docs are still pending.

## Scope and Clarification Targets

- [ ] File layout contract (module name to file path/package root mapping).
- [ ] Module resolution algorithm (search order, ambiguity rules, cycle/error model).
- [ ] Loader behavior (entrypoint, dependency graph build, caching, determinism, diagnostics).
- [ ] Qualified import behavior (`as`, symbol-list imports, collisions, shadowing).
- [ ] Migration constraints from current parser-first/parse-only state to enforceable semantics.

## Proposed Sub-Plans (If Decomposition Is Needed)

- `docs/plans/spec-clarification/2026-03-02/modules/subplans/09a-file-layout-and-package-roots.md`
- `docs/plans/spec-clarification/2026-03-02/modules/subplans/09b-resolution-order-and-cycle-rules.md`
- `docs/plans/spec-clarification/2026-03-02/modules/subplans/09c-loader-pipeline-caching-and-errors.md`
- `docs/plans/spec-clarification/2026-03-02/modules/subplans/09d-qualified-imports-and-name-binding.md`
- `docs/plans/spec-clarification/2026-03-02/modules/subplans/09e-migration-and-compatibility-constraints.md`

## Phase 0: Baseline Clarification Matrix and Decision Inputs

- [ ] Create a baseline matrix documenting unresolved semantics and candidate options:
  - `docs/spec/modules/00-module-clarification-matrix.md`
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
git add docs/spec/modules/00-module-clarification-matrix.md \
  docs/plans/spec-clarification/2026-03-02/modules/subplans/09a-file-layout-and-package-roots.md \
  docs/plans/spec-clarification/2026-03-02/modules/subplans/09b-resolution-order-and-cycle-rules.md \
  docs/plans/spec-clarification/2026-03-02/modules/subplans/09c-loader-pipeline-caching-and-errors.md \
  docs/plans/spec-clarification/2026-03-02/modules/subplans/09d-qualified-imports-and-name-binding.md \
  docs/plans/spec-clarification/2026-03-02/modules/subplans/09e-migration-and-compatibility-constraints.md
```

## Phase 1: File Layout and Package-Root Contract

- [ ] Define canonical mapping rules from module names to files.
- [ ] Clarify root semantics for:
  - workspace-local modules
  - optional package roots
  - relative module references (if retained)
- [ ] Decide case-sensitivity, separator normalization, and extension rules (`.jz`, future alternatives).
- [ ] Publish normative file-layout spec:
  - `docs/spec/modules/01-file-layout-and-package-roots.md`
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
git add docs/spec/modules/01-file-layout-and-package-roots.md \
  docs/spec/modules/00-module-clarification-matrix.md
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
  - `docs/spec/modules/02-resolution-algorithm-and-cycles.md`
- [ ] Attach pseudocode and truth-table examples for ambiguous/cyclic cases.

Research latitude for executor:
- [ ] Can choose DFS or BFS graph traversal as long as deterministic ordering and diagnostics requirements are met.

### Commit Checkpoint (Phase 2)

Suggested message:
`docs(spec-modules): define deterministic import resolution and cycle semantics`

Exact `git add` targets:
```bash
git add docs/spec/modules/02-resolution-algorithm-and-cycles.md \
  docs/spec/modules/00-module-clarification-matrix.md
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
  - `docs/spec/modules/03-loader-behavior-and-diagnostics.md`
- [ ] Document minimum behavior required to retire "parse-only import/module" status.

Research latitude for executor:
- [ ] Can recommend no-cache V1 loader if explicitly justified with complexity and correctness tradeoffs.

### Commit Checkpoint (Phase 3)

Suggested message:
`docs(spec-modules): define loader pipeline behavior and diagnostics`

Exact `git add` targets:
```bash
git add docs/spec/modules/03-loader-behavior-and-diagnostics.md \
  docs/spec/modules/00-module-clarification-matrix.md
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
  - `docs/spec/modules/04-qualified-imports-and-binding.md`
- [ ] Add executable examples and explicit invalid-case examples.

Research latitude for executor:
- [ ] May propose additional syntax restrictions if they reduce ambiguity and are migration-safe.

### Commit Checkpoint (Phase 4)

Suggested message:
`docs(spec-modules): define qualified import and name binding behavior`

Exact `git add` targets:
```bash
git add docs/spec/modules/04-qualified-imports-and-binding.md \
  docs/spec/modules/00-module-clarification-matrix.md
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
  - `docs/spec/modules/05-migration-and-compatibility.md`
- [ ] Update tracking docs to reference clarified module semantics:
  - `docs/jazz-language-state.md`

### Commit Checkpoint (Phase 5)

Suggested message:
`docs(spec-modules): add migration and compatibility policy for loader rollout`

Exact `git add` targets:
```bash
git add docs/spec/modules/05-migration-and-compatibility.md \
  docs/jazz-language-state.md \
  docs/spec/modules/00-module-clarification-matrix.md
```

## Phase 6: Verification Harness and Closure

- [ ] Add parser/analyzer/loader verification tests aligned to clarified semantics:
  - `jazz-hs/test/ParserSpec.hs`
  - `jazz-hs/test/Analyzer/TypeInferenceSpec.hs`
  - `jazz-hs/test/ModuleResolutionSpec.hs` (new)
  - `jazz-hs/test/LoaderSpec.hs` (new)
- [ ] Add docs consistency checks for module semantics references:
  - `scripts/check-module-docs.sh`
- [ ] Ensure docs/spec/test contract stays synchronized with reproducible commands.
- [ ] Close clarification item status once all checks pass and docs are linked.

### Commit Checkpoint (Phase 6)

Suggested message:
`test(spec-modules): add module resolution and loader verification coverage`

Exact `git add` targets:
```bash
git add jazz-hs/test/ParserSpec.hs \
  jazz-hs/test/Analyzer/TypeInferenceSpec.hs \
  jazz-hs/test/ModuleResolutionSpec.hs \
  jazz-hs/test/LoaderSpec.hs \
  scripts/check-module-docs.sh \
  docs/spec/modules
```

## Nix Environment and Reproducible Command Matrix

### Nix Environment (Required)

- [ ] Add/maintain a pinned dev shell in the repository root:
  - `flake.nix`
  - `flake.lock`
- [ ] Ensure shell includes: `stack`, `ghc`, `nodejs`, `bash`, `git`, `ripgrep`.

### Environment Sanity Commands

```bash
nix --extra-experimental-features "nix-command flakes" develop -c stack --version
nix --extra-experimental-features "nix-command flakes" develop -c ghc --version
nix --extra-experimental-features "nix-command flakes" develop -c node --version
```

### Parser Checks (Reproducible)

```bash
nix --extra-experimental-features "nix-command flakes" develop -c bash -lc 'cd jazz-hs && stack test --ta "--match \"when given an import statement|when given a module definition\""'
nix --extra-experimental-features "nix-command flakes" develop -c bash -lc 'cd jazz-hs && stack test --ta "--match \"Should handle a simple program with imports\""'
```

### Analyzer/Loader Checks (Reproducible)

```bash
nix --extra-experimental-features "nix-command flakes" develop -c bash -lc 'cd jazz-hs && stack test --ta "--match \"Type Inference\""'
nix --extra-experimental-features "nix-command flakes" develop -c bash -lc 'cd jazz-hs && stack test --ta "--match \"ModuleResolutionSpec|LoaderSpec\""'
```

### Documentation Consistency Checks (Reproducible)

```bash
nix --extra-experimental-features "nix-command flakes" develop -c bash -lc 'cd . && rg -n "module|import|resolution|loader|qualified" docs/spec/modules docs/jazz-language-state.md README.md'
nix --extra-experimental-features "nix-command flakes" develop -c bash -lc 'cd . && bash scripts/check-module-docs.sh'
```

## Final Exit Criteria

- [ ] File-layout rules are normative and unambiguous.
- [ ] Resolution order, ambiguity handling, and cycle semantics are specified.
- [ ] Loader behavior is documented with deterministic diagnostics requirements.
- [ ] Qualified import/name-binding rules are explicit and testable.
- [ ] Migration constraints preserve current users while enabling loader rollout.
- [ ] Nix-based parser/analyzer/doc checks are reproducible and documented.

## Implementation Status Verification (2026-03-04, Batch 1, `jazz-next`)

- [x] Re-verified candidate unchecked steps before implementation and confirmed `jazz-next` still lacked module/import lexer+parser+AST+l lowering support.
- [x] Added parser-surface module/import statement variants in `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`.
- [x] Added core lowered statement variants in `jazz-next/src/JazzNext/Compiler/AST.hs` and lowering pass support in `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`.
- [x] Added lexer keywords (`module`, `import`, `as`) and parser productions for:
  - `module A::B.`
  - `import A::B.`
  - `import A::B as X.`
  - `import A::B (x, y).`
- [x] Added no-op handling in analyzer/type-inference/runtime for module/import declarations to preserve current compile/run behavior while resolver work remains out of scope.
- [x] Added focused parser/lowering coverage in `jazz-next/test/ModuleImportParserSpec.hs` including invalid-case diagnostics.
- [x] Added the new suite to the default verification runner: `jazz-next/scripts/test-warning-config.sh`.

## Implementation Status Verification (2026-03-05, Batch 2, `jazz-next`)

- [x] Re-verified unchecked candidate steps and confirmed qualified-import parser behavior plus executable parser examples were already implemented in `jazz-next` before this batch.
- [x] Added deterministic module path mapping + graph resolution foundation in `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`.
- [x] Added deterministic resolver diagnostics for unresolved imports (`E4001`), ambiguous matches (`E4002`), import cycles (`E4003`), and module parse failures (`E4004`).
- [x] Added `jazz-next/test/ModuleResolutionSpec.hs` covering path mapping, deterministic dependency ordering, unresolved import context, ambiguity handling, cycle traces, and imported-module parse failures.
- [x] Added `ModuleResolutionSpec` to the default verification runner and re-ran full verification via `bash jazz-next/scripts/test-warning-config.sh`.
- [x] Kept parser/analyzer/type/runtime statement semantics unchanged in this batch; resolver/loader integration into file-entry CLI flow remains pending follow-on work.

## Implementation Status Verification (2026-03-05, Batch 3, `jazz-next`)

- [x] Added module-graph resolver lookup integration in `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs` (`resolveModuleGraphWithLookup`) so resolver behavior can be exercised through file-path lookups in CLI/driver flows.
- [x] Added driver module-graph entrypoints in `jazz-next/src/JazzNext/Compiler/Driver.hs`:
  - `compileModuleGraphWithPrelude`
  - `runModuleGraphWithPrelude`
- [x] Added deterministic module-source replay after resolution so module-graph compile/run paths preserve dependency-first ordering.
- [x] Added loader integration coverage in `jazz-next/test/LoaderSpec.hs` (compile success, run success, unresolved import diagnostics, cycle diagnostics, deterministic read order).
- [x] Added CLI module-graph file-entry mode in `jazz-next/src/JazzNext/CLI/Main.hs` with:
  - `--entry-module <A::B>` argument parsing + validation,
  - repeatable `--module-root <path>` support,
  - compile/run routing through module-graph driver entrypoints when `--entry-module` is present.
- [x] Added CLI coverage in `jazz-next/test/CLISpec.hs` for module-graph parse/options behavior, resolver error surfacing, and stdin bypass in module mode.
- [x] Added `LoaderSpec` to the default verification runner and re-ran full verification via `bash jazz-next/scripts/test-warning-config.sh`.

## Implementation Status Verification (2026-03-05, Batch 4, `jazz-next`)

- [x] Re-verified candidate Phase-1/2 loader-contract steps and confirmed module-path mapping/graph traversal were implemented, but module declaration vs resolved-file contract checks were still missing.
- [x] Added resolver contract diagnostics in `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`:
  - `E4005` for duplicate module declarations in one source file.
  - `E4006` for module declaration mismatch against the resolved module path.
- [x] Added resolver coverage in `jazz-next/test/ModuleResolutionSpec.hs` for matching declarations, duplicate declarations, and mismatch diagnostics.
- [x] Added loader/CLI propagation coverage in:
  - `jazz-next/test/LoaderSpec.hs`
  - `jazz-next/test/CLISpec.hs`
- [x] Re-ran focused verification:
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/ModuleResolutionSpec.hs`
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/LoaderSpec.hs`
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/CLISpec.hs`

## Implementation Status Verification (2026-03-05, Batch 5, `jazz-next`)

- [x] Re-verified Phase-4 parser candidates and confirmed alias/symbol-list mixing checks existed, but duplicate symbol-list imports were still accepted.
- [x] Added duplicate import symbol-list diagnostics in `jazz-next/src/JazzNext/Compiler/Parser.hs` (rejecting repeated names in `import A::B (x, ..., x)`).
- [x] Added parser regression coverage in `jazz-next/test/ModuleImportParserSpec.hs`.
- [x] Re-ran focused parser verification:
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/ModuleImportParserSpec.hs`
- [x] Re-ran full `jazz-next` verification:
  - `bash jazz-next/scripts/test-warning-config.sh`

## Implementation Status Verification (2026-03-05, Batch 6, `jazz-next`)

- [x] Re-verified candidate Phase-4 qualified-import steps and confirmed parser syntax checks existed, but resolver-level symbol export validation and cross-import alias/symbol collision checks were still missing.
- [x] Added resolver-level qualified-import binding validation in `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`:
  - `E4007` for symbol-list imports that request symbols not exported by the imported module.
  - `E4008` for symbol collisions across symbol-list imports within the same importing module.
  - `E4009` for alias collisions across `import ... as ...` declarations within the same importing module.
- [x] Added module export inventory capture (`SSLet` names) during module parse so symbol-list validation is deterministic and independent of parse-order accidents.
- [x] Added resolver regression coverage in `jazz-next/test/ModuleResolutionSpec.hs` for:
  - valid symbol-list import acceptance,
  - missing imported symbol diagnostics,
  - symbol collision diagnostics,
  - alias collision diagnostics.
- [x] Added loader/CLI propagation coverage for missing-symbol diagnostics in:
  - `jazz-next/test/LoaderSpec.hs`
  - `jazz-next/test/CLISpec.hs`
- [x] Re-ran focused verification:
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/ModuleResolutionSpec.hs`
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/LoaderSpec.hs`
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/CLISpec.hs`
- [x] Re-ran full `jazz-next` verification:
  - `bash jazz-next/scripts/test-warning-config.sh`

## Short Checkbox Summary

- [x] Evidence of unresolved module/import semantics recorded with exact paths.
- [x] Phased clarification plan with commit checkpoints and exact `git add` targets.
- [x] Nix environment and reproducible parser/analyzer/doc command matrix included.
- [x] Sub-plan paths proposed for deeper research-driven breakdown.
