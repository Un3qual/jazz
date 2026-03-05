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
- [ ] Boundary contract approval pending (Gate B naming/export contract unresolved)
- [ ] Migration phases executed with compatibility gates
- [ ] Hardcoded builtin surface reduced to approved kernel
- [ ] Reproducibility and closure checks completed

## Verification Evidence (Exact Paths + Unresolved Contradictions)

- [x] Evidence: `docs/jazz-language-state.md:397` explicitly flags the undecided question: self-hosted `.jz` stdlib vs hardcoded compiler/runtime builtins. Unresolved contradiction: no authoritative boundary contract exists yet.
- [x] Evidence: `jazz-hs/src/Types.hs:114` defines `builtinFuncs` in compiler code (`+`, `-`, `*`, `/`, `==`, `print!`, `map`, `hd`, `tl`). Unresolved contradiction: language-level library surface is compiler-hardcoded instead of prelude-defined.
- [x] Evidence: `jazz-hs/src/Types.hs:105` defines `traitsTable` in compiler code. Unresolved contradiction: trait universe defaults are hardcoded while `Prelude.jz` also models abstraction/typeclass structures.
- [x] Evidence: `jazz-hs/src/Analyzer/ScopeAnalyzer.hs:45` seeds scope from `builtinFuncs`; `jazz-hs/src/Analyzer/TypeInference.hs:78` seeds typing env from `builtinFuncs`. Unresolved contradiction: analyzer bootstraps bypass prelude ownership entirely.
- [x] Evidence: `jazz-hs/src/CodeGen/Javascript.hs:11` injects a hardcoded JS `stdLib` string and maps builtins via fixed lowering rules. Unresolved contradiction: runtime lowering assumes compiler-owned stdlib definitions.
- [x] Evidence: `jazz-hs/src/Lib.hs:61` compiles only user source (`parse -> analyze -> optimize -> generate`) with no prelude load path. Unresolved contradiction: `jazz-hs/static/Prelude.jz` is present but not in pipeline.
- [x] Evidence: `jazz-hs/src/Parser/Lang.hs:160` parses `class`; `jazz-hs/static/Prelude.jz:24` uses `trait`. Unresolved contradiction: prelude dialect and parser authority diverge.
- [x] Evidence: `jazz-hs/src/Parser/Lang.hs:167` requires `impl` with `@{...}:` constraints; `jazz-hs/static/Prelude.jz:79` uses `impl Num(Int)` style. Unresolved contradiction: prelude impl syntax is not parser-aligned.
- [x] Evidence: `jazz-hs/static/Prelude.jz` references primitives (`$intAdd`, `$floatAdd`, etc.) while `jazz-hs/src` and `jazz-hs/static/runtime.c` contain no definitions for these names (`rg` over those exact paths returns no matches). Unresolved contradiction: prelude primitive contract is undocumented and unimplemented in active backend.
- [x] Evidence: `docs/plans/spec-cleanup/2026-03-02/decisions/01-authoritative-syntax.md`, `02-map-filter-order.md`, `03-purity-bang-semantics.md`, and `04-trait-vs-class-keyword.md` lock adjacent language decisions but do not define stdlib boundary ownership. Unresolved contradiction: key dependency decisions exist, but stdlib ownership contract is still missing.

## Boundary Contract To Clarify (Decision Gates)

- [x] **Gate A: Minimal hardcoded intrinsic kernel**
- [x] Decide exact primitives that must remain compiler/runtime-owned (for example host I/O and backend-native numeric/FFI intrinsics only).
- [x] Explicitly exclude user-facing collection/utility APIs from kernel unless proven impossible to self-host.
- [ ] **Gate B: Prelude ownership**
- [x] Decide canonical source of truth for user-visible stdlib APIs (`.jz` prelude modules, versioned in-repo).
- [ ] Define naming/export contract for prelude symbols consumed by user programs.
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
- [ ] Cross-reference existing locked decisions (`class`, function-first map/filter, purity stub) so prelude syntax/API choices do not regress them.

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

- [ ] Migrate public APIs (`map`, `hd`, `tl`, and other non-kernel helpers) from hardcoded builtin tables to prelude exports.
- [ ] Keep temporary aliases only where needed; emit deprecation guidance for compiler-hardcoded access paths.
- [ ] Update parser/analyzer/codegen tests to assert canonical prelude-owned call paths.
- [ ] Ensure docs/examples stop implying compiler-owned stdlib APIs.

### Expected file touch-set

- `jazz-hs/src/Types.hs`
- `jazz-hs/src/Analyzer/ScopeAnalyzer.hs`
- `jazz-hs/src/Analyzer/TypeInference.hs`
- `jazz-hs/src/CodeGen/Javascript.hs`
- `jazz-hs/static/Prelude.jz`
- `jazz-hs/ExamplePrograms/ComplexProgram.jz`
- `README.md`
- `docs/jazz-language-state.md`

### Commit checkpoint (Phase 4)

Suggested commit message: `feat(stdlib): migrate user-facing builtins to self-hosted prelude`

```bash
git add jazz-hs/src/Types.hs jazz-hs/src/Analyzer/ScopeAnalyzer.hs jazz-hs/src/Analyzer/TypeInference.hs jazz-hs/src/CodeGen/Javascript.hs jazz-hs/static/Prelude.jz jazz-hs/ExamplePrograms/ComplexProgram.jz README.md docs/jazz-language-state.md
git commit -m "feat(stdlib): migrate user-facing builtins to self-hosted prelude"
```

## Phase 5: Shrink Hardcoded Surface to Kernel-Only and Close Contradictions

- [ ] Remove deprecated hardcoded stdlib entries not in approved kernel.
- [ ] Keep only intrinsic/kernel entries with explicit contract docs.
- [ ] Update unresolved contradiction list in this plan and mark each resolved with commit references.
- [ ] Mark stdlib-boundary item resolved in language-state cleanup tracking.

### Expected file touch-set

- `jazz-hs/src/Types.hs`
- `jazz-hs/src/Analyzer/ScopeAnalyzer.hs`
- `jazz-hs/src/Analyzer/TypeInference.hs`
- `docs/spec/stdlib-boundary.md`
- `docs/jazz-language-state.md`
- `docs/plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md`

### Commit checkpoint (Phase 5)

Suggested commit message: `refactor(stdlib): reduce compiler hardcoded surface to intrinsic kernel`

```bash
git add jazz-hs/src/Types.hs jazz-hs/src/Analyzer/ScopeAnalyzer.hs jazz-hs/src/Analyzer/TypeInference.hs docs/spec/stdlib-boundary.md docs/jazz-language-state.md docs/plans/spec-clarification/2026-03-02/stdlib/10-stdlib-boundary-selfhosted-vs-hardcoded.md
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

- [ ] Boundary contract is explicit, versioned, and linked from language-state docs.
- [ ] User-visible stdlib APIs are prelude-owned by default.
- [ ] Compiler/runtime hardcoded layer is reduced to agreed intrinsic kernel only.
- [ ] Contradictions listed in `Verification Evidence` are all marked resolved with commit references.
- [ ] Nix-based reproducibility commands run successfully for baseline and final state.

## Implementation Status Verification (2026-03-04, Batch 1, `jazz-next`)

- [x] Re-verified candidate steps before implementation and confirmed phase-3 intrinsic bridge work was partially present in code but drift-prone due to duplicated builtin tables.
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

## Short Checkbox Summary

- [x] Evidence-backed contradiction map included
- [x] Detailed phased migration + commit checkpoints included
- [x] Nix reproducibility commands included
- [ ] Execution and closure pending
