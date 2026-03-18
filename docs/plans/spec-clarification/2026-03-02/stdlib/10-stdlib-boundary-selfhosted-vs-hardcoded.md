# Spec Clarification #10 Plan: Standard Library Boundary (`self-hosted .jz` Prelude vs Hardcoded Compiler/Runtime Builtins)

> **For Executor:** REQUIRED SUB-SKILL: Use `executing-plans` to execute this plan phase-by-phase with checkpoints.

**Goal:** Clarify and ratify the stdlib ownership boundary, then migrate from today’s hardcoded builtin surface toward a self-hosted `.jz` prelude without breaking `jazz-hs` behavior.

**Architecture:** Keep a minimal hardcoded intrinsic kernel (compiler/runtime boundary), move user-visible standard library APIs into a loadable `.jz` prelude, and run a compatibility bridge until parity is verified.

**Tech Stack:** Haskell (`jazz-hs` parser/analyzer/codegen), Jazz `.jz` prelude sources, JS backend helpers, Markdown specs/plans, Stack, Node, Nix shell tooling.

---

## Plan Progress

- [x] Baseline evidence gathered from old code and current specs
- [x] Contradictions captured with exact paths
- [x] Verified candidate-step implementation status against current `jazz-next` state before selecting next batch
- [x] Executed `jazz-next` intrinsic-boundary hardening batch (shared builtin catalog + conformance checks)
- [x] Closed Gate B naming/export contract baseline for active `jazz-next` path (bundled prelude path + bridge export contract documented)
- [x] Executed `jazz-next` bundled-default prelude bootstrap + ownership-metadata scaffolding batch
- [x] Executed `jazz-next` prelude-owned-default migration batch (strict kernel-only prelude mode + explicit no-prelude compatibility fallback)
- [x] Executed `jazz-next` phase-4 public-builtin migration batch (public names now flow through the prelude; raw compiler/runtime fallback is restricted to `__kernel_*` bridge symbols)
- [x] Executed `jazz-next` explicit no-prelude kernel-only batch (driver/CLI/runtime low-level paths now reject canonical public aliases and accept only `__kernel_*` bridge names)
- [x] Migration phases executed with compatibility gates
- [x] Active `jazz-next` direct builtin access reduced to the approved `__kernel_*` bridge surface
- [x] Hardcoded builtin surface reduced to approved kernel
- [x] Reproducibility and closure checks completed

## Verification Evidence (Legacy Contradictions + Active-Path Resolutions)

- [x] Historical evidence: prior `docs/jazz-language-state.md` tracker state explicitly flagged the undecided question: self-hosted `.jz` stdlib vs hardcoded compiler/runtime builtins. Active-path resolution: `docs/spec/stdlib-boundary.md` now serves as the authoritative ownership contract, and this closure batch removes the lingering "in progress" tracker state.
- [x] Evidence: `jazz-hs/src/Types.hs:114` defines `builtinFuncs` in compiler code (`+`, `-`, `*`, `/`, `==`, `print!`, `map`, `hd`, `tl`). Active-path resolution: `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs` limits the current boundary catalog to the approved `map`/`filter`/`hd`/`tl`/`print!` bridge surface, while public names are supplied by `jazz-next/stdlib/Prelude.jz`.
- [x] Evidence: `jazz-hs/src/Types.hs:105` defines `traitsTable` in compiler code. Active-path resolution: the current `jazz-next` stdlib boundary does not rely on trait-table hardcoding; trait-surface follow-up remains outside domain `10`.
- [x] Evidence: `jazz-hs/src/Analyzer/ScopeAnalyzer.hs:45` seeds scope from `builtinFuncs`; `jazz-hs/src/Analyzer/TypeInference.hs:78` seeds typing env from `builtinFuncs`. Active-path resolution: `jazz-next/src/JazzNext/Compiler/Analyzer.hs`, `jazz-next/src/JazzNext/Compiler/TypeInference.hs`, and `jazz-next/src/JazzNext/Compiler/Runtime.hs` all resolve builtin visibility through `BuiltinCatalog`.
- [x] Evidence: `jazz-hs/src/CodeGen/Javascript.hs:11` injects a hardcoded JS `stdLib` string and maps builtins via fixed lowering rules. Active-path resolution: `jazz-next` no longer routes stdlib ownership through a JS stdlib string; the active runtime subset uses `BuiltinCatalog`, `Runtime.hs`, and prelude injection instead.
- [x] Evidence: `jazz-hs/src/Lib.hs:61` compiles only user source (`parse -> analyze -> optimize -> generate`) with no prelude load path. Active-path resolution: `jazz-next/src/JazzNext/Compiler/Driver.hs` now composes bundled or explicit prelude source ahead of user programs.
- [x] Evidence: `jazz-hs/src/Parser/Lang.hs:160` parses `class`; `jazz-hs/static/Prelude.jz:24` uses `trait`. Active-path resolution: `jazz-next/stdlib/Prelude.jz` no longer depends on the legacy trait syntax and instead contains only kernel self-bridges plus public aliases.
- [x] Evidence: `jazz-hs/src/Parser/Lang.hs:167` requires `impl` with `@{...}:` constraints; `jazz-hs/static/Prelude.jz:79` uses `impl Num(Int)` style. Active-path resolution: the active bundled prelude avoids legacy impl syntax entirely, so the boundary contract no longer depends on this mismatch.
- [x] Evidence: `jazz-hs/static/Prelude.jz` references primitives (`$intAdd`, `$floatAdd`, etc.) while `jazz-hs/src` and `jazz-hs/static/runtime.c` contain no definitions for these names (`rg` over those exact paths returns no matches). Active-path resolution: `jazz-next/src/JazzNext/Compiler/PreludeContract.hs` validates the supported `__kernel_*` bridge contract instead of the legacy `$intAdd` primitive naming scheme.
- [x] Evidence: `docs/plans/spec-cleanup/2026-03-02/decisions/01-authoritative-syntax.md`, `02-map-filter-order.md`, `03-purity-bang-semantics.md`, and `04-trait-vs-class-keyword.md` lock adjacent language decisions but do not define stdlib boundary ownership. Active-path resolution: `docs/spec/stdlib-boundary.md` now closes that ownership gap for the current runtime subset.

## Boundary Contract To Clarify (Decision Gates)

- [x] **Gate A: Minimal hardcoded intrinsic kernel**
- [x] Decide exact primitives that must remain compiler/runtime-owned (for example host I/O and backend-native numeric/FFI intrinsics only).
- [x] Explicitly exclude user-facing collection/utility APIs from kernel unless proven impossible to self-host.
- [x] **Gate B: Prelude ownership**
- [x] Decide canonical source of truth for user-visible stdlib APIs (`.jz` prelude modules, versioned in-repo).
- [x] Define naming/export contract for prelude symbols consumed by user programs.
- [x] **Gate C: Compiler/runtime boundary**
- [x] Define intrinsic call boundary shape (name scheme, arity contract, typing authority, codegen mapping ownership).
- [x] Define whether intrinsic typing lives in compiler tables, prelude declarations, or dual-checked contract files during migration.
- [x] **Gate D: Compatibility window**
- [x] Decide temporary dual-source policy (`hardcoded builtins + prelude`) and explicit removal criteria/date.

## Phase 0: Boundary Inventory Freeze and Decision Rubric

- [ ] Reconfirm baseline files and freeze references used by this plan.
- [ ] Build an explicit symbol inventory matrix with columns:
- [ ] Symbol name
- [ ] Current owner (`Types.hs`, `CodeGen/Javascript.hs`, `Prelude.jz`, runtime)
- [ ] Target owner (`kernel` vs `prelude`)
- [ ] Migration strategy (`alias`, `rewrite`, `drop`)
- [ ] Decision status (`LOCKED` or `UNRESOLVED`)
- [ ] Add a risk tag per symbol (`LOW`, `MEDIUM`, `HIGH`) for sequencing.

### Commit checkpoint (Phase 0)

Suggested commit message: `docs(spec): add stdlib boundary inventory and decision rubric`

```bash
git add docs/plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md
git commit -m "docs(spec): add stdlib boundary inventory and decision rubric"
```

## Phase 1: Ratify Boundary Contract (No Behavioral Changes Yet)

- [x] Create canonical boundary spec doc with explicit ownership rules.
- [x] Lock kernel list (hardcoded forever or until backend swap).
- [x] Lock prelude-owned API list (must be loaded from `.jz`, not hardcoded).
- [x] Lock compatibility policy (hard switch vs staged dual support).
- [x] Cross-reference existing locked decisions (`class`, function-first map/filter, purity stub) so prelude syntax/API choices do not regress them.

### Expected file touch-set

- `docs/spec/stdlib-boundary.md` (create)
- `docs/jazz-language-state.md` (update unresolved-item status/links)
- `docs/plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md` (update progress)

### Commit checkpoint (Phase 1)

Suggested commit message: `docs(spec): ratify stdlib ownership boundary contract`

```bash
git add docs/spec/stdlib-boundary.md docs/jazz-language-state.md docs/plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md
git commit -m "docs(spec): ratify stdlib ownership boundary contract"
```

## Phase 2: Add Prelude Loading Path Behind Compatibility Guard

- [x] Add a deterministic prelude load strategy in compilation pipeline (prepend/parse-combine/import bootstrap) without removing hardcoded builtins yet.
- [x] Keep compatibility mode enabled so existing programs still compile while prelude is integrated.
- [x] Add tests that prove prelude definitions are visible at parse/scope/type phases.
- [x] Ensure failure modes are explicit when prelude file is missing or parse-invalid.

### Expected file touch-set

- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/CLI/Main.hs`
- `jazz-next/test/CLISpec.hs`
- `jazz-next/test/PreludeLoadingSpec.hs`
- `jazz-next/scripts/test-warning-config.sh`

Executed `jazz-next` touch-set (2026-03-04 Batch 2):

- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/CLI/Main.hs`
- `jazz-next/test/CLISpec.hs`
- `jazz-next/test/PreludeLoadingSpec.hs`
- `jazz-next/scripts/test-warning-config.sh`

### Commit checkpoint (Phase 2)

Suggested commit message: `feat(compiler): load self-hosted prelude with compatibility guard`

```bash
git add jazz-next/src/JazzNext/Compiler/Driver.hs jazz-next/src/JazzNext/CLI/Main.hs jazz-next/test/CLISpec.hs jazz-next/test/PreludeLoadingSpec.hs jazz-next/scripts/test-warning-config.sh
git commit -m "feat(compiler): load self-hosted prelude with compatibility guard"
```

## Phase 3: Introduce Explicit Intrinsic Bridge Contract

- [x] Define intrinsic symbol naming and mapping table used by codegen/runtime boundary.
- [x] Separate intrinsic typing contract from user-facing stdlib APIs.
- [x] Add compile-time checks that prelude intrinsic references map to known kernel entries.
- [x] Keep fallback path for current hardcoded helpers until parity is proven.

### Expected file touch-set

- `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
- `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/test/BuiltinCatalogSpec.hs`
- `jazz-next/test/PreludeLoadingSpec.hs`
- `jazz-next/test/CLISpec.hs`

### Commit checkpoint (Phase 3)

Suggested commit message: `feat(runtime): add explicit intrinsic bridge for prelude boundary`

```bash
git add jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs jazz-next/src/JazzNext/Compiler/PreludeContract.hs jazz-next/src/JazzNext/Compiler/Driver.hs jazz-next/test/BuiltinCatalogSpec.hs jazz-next/test/PreludeLoadingSpec.hs jazz-next/test/CLISpec.hs
git commit -m "feat(runtime): add explicit intrinsic bridge for prelude boundary"
```

## Phase 4: Migrate User-Facing Builtins to Prelude Ownership

- [x] Migrate public APIs (`map`, `hd`, `tl`, and other non-kernel helpers) from hardcoded builtin tables to prelude exports.
- [x] Keep temporary aliases only where needed; emit deprecation guidance for compiler-hardcoded access paths.
- [x] Update parser/analyzer/codegen tests to assert canonical prelude-owned call paths.
- [x] Ensure docs/examples stop implying compiler-owned stdlib APIs.

### Expected file touch-set

- `jazz-next/stdlib/Prelude.jz`
- `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
- `jazz-next/src/JazzNext/CLI/Main.hs`
- `jazz-next/test/BuiltinCatalogSpec.hs`
- `jazz-next/test/CLISpec.hs`
- `README.md`
- `docs/jazz-language-state.md`

### Commit checkpoint (Phase 4)

Suggested commit message: `feat(stdlib): migrate user-facing builtins to self-hosted prelude`

```bash
git add jazz-next/stdlib/Prelude.jz jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs jazz-next/src/JazzNext/CLI/Main.hs jazz-next/test/BuiltinCatalogSpec.hs jazz-next/test/CLISpec.hs README.md docs/jazz-language-state.md
git commit -m "feat(stdlib): migrate user-facing builtins to self-hosted prelude"
```

Executed `jazz-next` touch-set (2026-03-16 Batch 3):

- `jazz-next/stdlib/Prelude.jz`
- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
- `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/test/BuiltinCatalogSpec.hs`
- `jazz-next/test/CLISpec.hs`
- `jazz-next/test/PreludeLoadingSpec.hs`
- `jazz-next/test/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/PuritySemanticsSpec.hs`
- `jazz-next/test/RuntimeSemanticsSpec.hs`
- `README.md`
- `docs/jazz-language-state.md`
- `docs/spec/stdlib-boundary.md`

## Phase 5: Shrink Hardcoded Surface to Kernel-Only and Close Contradictions

- [x] Switch explicit no-prelude compile/run paths and low-level helper defaults in `jazz-next` to kernel-only resolution.
- [x] Remove deprecated hardcoded stdlib entries not in approved kernel.
- [x] Keep only intrinsic/kernel entries with explicit contract docs.
- [x] Update unresolved contradiction list in this plan and mark each resolved for the active `jazz-next` path.
- [x] Mark stdlib-boundary item resolved in language-state cleanup tracking.

### Expected file touch-set

- `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/BuiltinCatalogSpec.hs`
- `jazz-next/test/PreludeLoadingSpec.hs`
- `jazz-next/test/RuntimeSemanticsSpec.hs`
- `jazz-next/test/CLISpec.hs`
- `docs/spec/stdlib-boundary.md`
- `docs/jazz-language-state.md`
- `docs/plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md`

### Commit checkpoint (Phase 5)

Suggested commit message: `refactor(stdlib): reduce compiler hardcoded surface to intrinsic kernel`

```bash
git add jazz-next/src/JazzNext/Compiler/Analyzer.hs jazz-next/src/JazzNext/Compiler/TypeInference.hs jazz-next/src/JazzNext/Compiler/Driver.hs jazz-next/src/JazzNext/Compiler/Runtime.hs jazz-next/test/BuiltinCatalogSpec.hs jazz-next/test/PreludeLoadingSpec.hs jazz-next/test/RuntimeSemanticsSpec.hs jazz-next/test/CLISpec.hs docs/spec/stdlib-boundary.md docs/jazz-language-state.md docs/plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md
git commit -m "refactor(stdlib): reduce compiler hardcoded surface to intrinsic kernel"
```

## Nix Reproducibility Commands

Use the repo-level flake as the canonical reproducibility entrypoint.

### Baseline capture (before migration)

```bash
nix flake check
nix develop . -c bash -lc '
  set -euo pipefail
  cd jazz-hs
  stack --version
  ghc --version
  node --version
  stack test
  ./run.sh ExamplePrograms/ComplexProgram.jz >/tmp/jazz-stdlib-boundary-baseline.js
'
```

### Phase-gate verification (run after each phase touching compiler behavior)

```bash
nix develop . -c bash -lc '
  set -euo pipefail
  cd jazz-hs
  stack test
  ./run.sh ExamplePrograms/ComplexProgram.jz >/tmp/jazz-stdlib-boundary-current.js
  node /tmp/jazz-stdlib-boundary-current.js || true
  cd ..
  rg -n "builtinFuncs|traitsTable|stdLib = \\[s\\|" jazz-hs/src
  rg -n "\\$intAdd|\\$floatAdd|\\$intEq|\\$floatEq" jazz-hs/static/Prelude.jz jazz-hs/src jazz-hs/static/runtime.c -S || true
'
```

## Additional Plan Splits (Create Only If Scope Splits Across Owners)

- [ ] `docs/plans/spec-clarification/2026-03-02/stdlib/10a-stdlib-boundary-contract-matrix.md` (decision matrix + owner sign-off)
- [ ] `docs/plans/spec-clarification/2026-03-02/stdlib/10b-stdlib-prelude-loader-migration.md` (compiler pipeline integration track)
- [ ] `docs/plans/spec-clarification/2026-03-02/stdlib/10c-stdlib-intrinsic-kernel-hardening.md` (runtime/kernel isolation track)

## Risks and Rollback Notes

- [ ] Risk: prelude syntax still mismatches parser decisions (`class`/`impl` forms), causing migration deadlock.
- [ ] Risk: intrinsic naming churn breaks codegen or analyzer assumptions mid-migration.
- [ ] Risk: dual-source compatibility window drifts into permanent ambiguity.
- [ ] Rollback strategy: revert in reverse phase order; never remove compatibility mode before parity tests and docs are both green.

## Definition of Done

- [x] Boundary contract is explicit, versioned, and linked from language-state docs.
- [x] User-visible stdlib APIs are prelude-owned by default.
- [x] Compiler/runtime hardcoded layer is reduced to agreed intrinsic kernel only.
- [x] Contradictions listed in `Verification Evidence` are all marked resolved for the active `jazz-next` path.
- [x] Active-path verification from `docs/execution/queue.md` (`bash jazz-next/scripts/test-warning-config.sh`) passes, while the legacy Nix/`jazz-hs` commands above remain preserved as reference-only baseline capture.

## Implementation Status Verification (2026-03-04, Batch 1, `jazz-next`)

- [x] Re-verified candidate steps before implementation and confirmed phase-3 intrinsic bridge work was partially present in code but drift-prone due to duplicated builtin tables.

## Implementation Status Verification (2026-03-16, Batch 5, `jazz-next`)

- [x] Re-verified that module/import work remains partly code-complete but docs-first for normative semantics, so stdlib phase 4 was the next safe executable code batch.
- [x] Re-verified that public builtin names were still directly available without prelude in analyzer/type/runtime before this batch.
- [x] Re-verified after the batch that raw compiler/runtime fallback now accepts only `__kernel_*` bridge names, while CLI/default-prelude and explicit-prelude flows keep public builtin APIs working.
- [x] Added canonical boundary spec doc for active `jazz-next` runtime subset: `docs/spec/stdlib-boundary.md`.
- [x] Added shared builtin catalog module: `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`.
- [x] Refactored analyzer/type-inference/runtime to consume the shared catalog:
  - `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
  - `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
  - `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- [x] Added catalog conformance coverage: `jazz-next/test/BuiltinCatalogSpec.hs`.
- [x] Added catalog suite to default verification runner: `jazz-next/scripts/test-warning-config.sh`.
- [x] Ran targeted verification:
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PrimitiveSemanticsSpec.hs`
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/RuntimeSemanticsSpec.hs`
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/CLISpec.hs`
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/BuiltinCatalogSpec.hs`

## Implementation Status Verification (2026-03-04, Batch 2, `jazz-next`)

- [x] Re-verified candidate unchecked phase items and confirmed prelude loading path plus explicit diagnostics were still missing in active `jazz-next` compile/run entrypoints.
- [x] Added prelude-aware driver entrypoints in `jazz-next/src/JazzNext/Compiler/Driver.hs` (`compileSourceWithPrelude`, `runSourceWithPrelude`) with deterministic source composition and prelude parse diagnostics (`E0002`).
- [x] Added CLI compatibility-guard controls in `jazz-next/src/JazzNext/CLI/Main.hs` (`--prelude`, `--no-prelude`, `JAZZ_PRELUDE`) plus explicit missing-prelude diagnostics (`E0003`).
- [x] Added CLI behavior coverage for prelude success/failure and override cases in `jazz-next/test/CLISpec.hs`.
- [x] Added dedicated pipeline tests in `jazz-next/test/PreludeLoadingSpec.hs` for prelude binding visibility and parse-failure handling.
- [x] Added the new suite to the default verification runner: `jazz-next/scripts/test-warning-config.sh`.

## Implementation Status Verification (2026-03-04, Batch 3, `jazz-next`)

- [x] Re-verified phase-3 candidate items and confirmed explicit code-level prelude/kernel symbol-conformance checks were still missing.
- [x] Added bridge contract surface in `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs` (`kernelBridgeBindingPrefix`, `kernelBridgeTargetName`).
- [x] Added conformance validator in `jazz-next/src/JazzNext/Compiler/PreludeContract.hs` and wired deterministic diagnostics (`E0004` unknown bridge target, `E0005` malformed bridge declaration) in `jazz-next/src/JazzNext/Compiler/Driver.hs`.
- [x] Added conformance coverage updates in:
  - `jazz-next/test/BuiltinCatalogSpec.hs`
  - `jazz-next/test/PreludeLoadingSpec.hs`
  - `jazz-next/test/CLISpec.hs`
- [x] Preserved compatibility fallback path for valid programs and bridge-free preludes.

## Implementation Status Verification (2026-03-05, Batch 4, `jazz-next`)

- [x] Re-verified candidate Phase-4 unchecked steps and confirmed bundled-default prelude loading plus ownership metadata were still missing from active `jazz-next` CLI/catalog behavior.
- [x] Added bundled prelude source at `jazz-next/stdlib/Prelude.jz` with explicit kernel bridge declarations and prelude-owned aliases for `map`/`filter`/`hd`/`tl`/`print!`.
- [x] Updated CLI prelude resolution order in `jazz-next/src/JazzNext/CLI/Main.hs` to:
  - explicit `--prelude` path,
  - then `JAZZ_PRELUDE`,
  - then bundled default `jazz-next/stdlib/Prelude.jz`,
  while preserving `--no-prelude` override.
- [x] Kept compatibility behavior stable by treating missing bundled-default prelude as optional fallback (`Right Nothing`), while still surfacing `E0003` for missing explicit/env prelude paths.
- [x] Added ownership metadata scaffolding in `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs` via `BuiltinOwnership` + `builtinSymbolOwnership` (current runtime subset tagged `PreludeTarget`).
- [x] Added regression coverage updates in:
  - `jazz-next/test/CLISpec.hs` (bundled default prelude load + `--no-prelude` override with bundled path),
  - `jazz-next/test/BuiltinCatalogSpec.hs` (ownership contract assertions).
- [x] Ran focused verification:
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/CLISpec.hs`
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/BuiltinCatalogSpec.hs`
- [x] Re-ran full `jazz-next` verification:
  - `bash jazz-next/scripts/test-warning-config.sh`

## Implementation Status Verification (2026-03-17, Batch 7, `jazz-next`)

- [x] Re-verified Phase-5 candidate behavior before implementation and confirmed explicit no-prelude paths still accepted canonical public aliases while rejecting `__kernel_*` bridge names.
- [x] Switched explicit no-prelude compile/run paths and low-level helper defaults to kernel-only resolution in:
  - `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
  - `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
  - `jazz-next/src/JazzNext/Compiler/Driver.hs`
  - `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- [x] Updated no-prelude contract coverage in:
  - `jazz-next/test/BuiltinCatalogSpec.hs`
  - `jazz-next/test/PreludeLoadingSpec.hs`
  - `jazz-next/test/RuntimeSemanticsSpec.hs`
  - `jazz-next/test/CLISpec.hs`
- [x] Updated boundary/status docs to reflect current `jazz-next` defaults without falsely closing broader Phase-5 cleanup:
  - `docs/spec/stdlib-boundary.md`
  - `docs/jazz-language-state.md`
  - `docs/plans/spec-clarification/2026-03-03/README.md`
  - `docs/plans/2026-03-03-jazz-interpreter-roadmap.md`
  - `docs/plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md`
- [x] Ran full active-path verification:
  - `bash jazz-next/scripts/test-warning-config.sh`

## Implementation Status Verification (2026-03-18, Closure Batch, `jazz-next`)

- [x] Re-verified the only eligible `Ready Now` queue item (`JN-STDLIB-CLOSE-001`) against its linked Phase-5 plan and confirmed the remaining work was tracker closure, not missing `jazz-next` behavior.
- [x] Re-verified the approved kernel boundary in active-path files:
  - `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
  - `jazz-next/src/JazzNext/Compiler/Driver.hs`
  - `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
  - `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
  - `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- [x] Re-verified coverage that bundled/explicit prelude paths expose public aliases while no-prelude helpers accept only `__kernel_*` bridge names:
  - `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`
  - `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
  - `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`
  - `jazz-next/test/JazzNext/CLI/CLISpec.hs`
- [x] Ran full active-path verification:
  - `bash jazz-next/scripts/test-warning-config.sh`
- [x] Closed queue/status metadata in:
  - `docs/execution/queue.md`
  - `docs/spec/stdlib-boundary.md`
  - `docs/jazz-language-state.md`
  - `docs/plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md`

## Implementation Status Verification (2026-03-05, Batch 5, `jazz-next`)

- [x] Re-verified all unchecked Phase-4 candidate steps before implementation and confirmed they were still incomplete (not tracker drift): direct canonical builtin fallback remained unconditional in analyzer/type/runtime, bridge validation enforced legacy `__kernel_x = x` shape, and tests/docs still implied compiler-owned APIs.
- [x] Added builtins-resolution modes in `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`:
  - strict prelude-owned mode (`ResolveKernelOnly`) for prelude-enabled compile/run paths,
  - explicit compatibility mode (`ResolveCompatibility`) for no-prelude fallback paths.
- [x] Refactored analyzer/type/runtime to consume mode-aware builtin resolution:
  - `jazz-next/src/JazzNext/Compiler/Analyzer.hs`
  - `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
  - `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- [x] Updated driver execution policy in `jazz-next/src/JazzNext/Compiler/Driver.hs`:
  - source compile/run now attempts bundled prelude by default,
  - prelude-enabled paths compile/run with strict kernel-only builtin resolution,
  - explicit no-prelude paths retain compatibility aliases by design.
- [x] Updated bridge contract + bundled prelude ownership shape:
  - `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`
  - `jazz-next/stdlib/Prelude.jz` (`__kernel_x = __kernel_x` self-bridges + public aliases `x = __kernel_x`).
- [x] Rebaselined boundary conformance tests and fixtures:
  - `jazz-next/test/BuiltinCatalogSpec.hs`
  - `jazz-next/test/PreludeLoadingSpec.hs`
  - `jazz-next/test/CLISpec.hs`
- [x] Updated boundary contract docs to reflect prelude-owned default + explicit no-prelude compatibility deprecation guidance:
  - `docs/spec/stdlib-boundary.md`
- [x] Ran focused verification:
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/BuiltinCatalogSpec.hs`
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PreludeLoadingSpec.hs`
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/CLISpec.hs`
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PuritySemanticsSpec.hs`
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/PrimitiveSemanticsSpec.hs`
  - `runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/RuntimeSemanticsSpec.hs`

## Short Checkbox Summary

- [x] Evidence-backed contradiction map included
- [x] Detailed phased migration + commit checkpoints included
- [x] Nix reproducibility commands included
- [x] Execution and closure completed
