# Jazz Spec-Cleanup Item #2: `map`/`filter` Argument Order Implementation Plan

> **For Executor:** REQUIRED SUB-SKILL: Use `executing-plans` to execute this plan task-by-task with checkpoints.

**Goal:** Decide and apply one canonical argument order for `map`/`filter` across spec/docs, parser expectations, type environment, code generation, and tests.

**Architecture:** Use `jazz-hs` as behavioral authority, settle a canonical API order early, then execute one implementation track (function-first or collection-first). Keep migration and compatibility explicit so docs and runtime do not drift again.

**Tech Stack:** Haskell (`stack` in `jazz-hs`), JS codegen prelude, Markdown docs, Nix dev shell tooling.

---

## Verification Evidence

- `docs/plans/spec-cleanup/2026-03-02/decisions/02-map-filter-order.md`: file did not exist before this planning task (`MISSING` check result), so this item had no decision/implementation plan yet.
- `docs/jazz-language-state.md:390`: explicitly lists collection API order as unresolved (`map f xs` vs `map xs f`).
- `docs/jazz-language-state.md:423`: lists this exact cleanup item as still pending (`Decide whether map/filter are function-first or collection-first`).
- `README.md:57` and `README.md:58`: examples use collection-first (`filter myArr ...`, `map myArr ...`).
- `jazz-hs/src/Types.hs:122`: builtin `map` type is function-first (`(a -> b) -> [a] -> [b]`).
- `jazz-hs/src/CodeGen/Javascript.hs:17`: JS lowering defines `map` as function-first (`f => xs => xs.map(f)`).
- `jazz-hs/test/ParserSpec.hs:228`: parser test usage is function-first (`map (2+) myList`).
- `jazz-hs/src/Types.hs` and `jazz-hs/src/CodeGen/Javascript.hs`: no builtin `filter` entry exists, while docs mention `filter`.

## Plan Progress

- [ ] Phase 0: Baseline lock and decision rubric
- [x] Phase 1: Canonical order decision + compatibility mode (order locked, compatibility mode pending)
- [ ] Phase 2: Implement chosen order in compiler/runtime/tests
- [ ] Phase 3: Migration and docs convergence
- [ ] Phase 4: Nix environment + command wiring
- [ ] Phase 5: Verification and closure
- [x] 2026-03-05 `jazz-next` implementation batch: verified candidate steps and added `filter` runtime/type support with tests.

## Decision Lock (Approved 2026-03-02)

- [x] Canonical order for collection combinators is function-first.
- [x] `map` canonical call form: `map f xs`.
- [x] `filter` canonical call form: `filter p xs`.
- [ ] Compatibility policy still to choose (hard switch vs temporary alias window).

## Phase 0: Baseline lock and decision rubric

- [ ] Re-check current state of these files before coding: `README.md`, `docs/jazz-language-state.md`, `jazz-hs/src/Types.hs`, `jazz-hs/src/CodeGen/Javascript.hs`, `jazz-hs/test/ParserSpec.hs`.
- [ ] Build an explicit option matrix in this decision doc:
- [ ] Option A: function-first (`map f xs`, `filter p xs`).
- [ ] Option B: collection-first (`map xs f`, `filter xs p`).
- [ ] Score each option against: existing parser/test shape, currying ergonomics, codegen simplicity, migration cost, compatibility impact.
- [ ] Confirm whether `filter` is in-scope for the same change (recommended: yes, to avoid another order mismatch immediately after this decision).

### Commit checkpoint (Phase 0)

Suggested commit message: `docs(spec): capture map/filter decision rubric`

```bash
git -C "$(git rev-parse --show-toplevel)" add docs/plans/spec-cleanup/2026-03-02/decisions/02-map-filter-order.md
git commit -m "docs(spec): capture map/filter decision rubric"
```

## Phase 1: Canonical order decision + compatibility mode

- [x] Decision Gate 1: canonical order for both `map` and `filter` is function-first.
- [ ] Decision Gate 2: choose compatibility strategy.
- [ ] Mode 1: hard switch (only canonical order accepted immediately).
- [ ] Mode 2: temporary compatibility window (accept both forms; non-canonical form emits deprecation warning).
- [ ] Mode 3: permanent dual-form support (only if maintainers explicitly accept ongoing complexity cost).
- [ ] Decision Gate 3: if Mode 2 is selected, set exact sunset milestone/date for non-canonical form.
- [ ] Add acceptance criteria in this decision doc before implementation begins.

### Commit checkpoint (Phase 1)

Suggested commit message: `docs(spec): decide canonical map/filter order and compatibility mode`

```bash
git -C "$(git rev-parse --show-toplevel)" add docs/plans/spec-cleanup/2026-03-02/decisions/02-map-filter-order.md docs/jazz-language-state.md
git commit -m "docs(spec): decide canonical map/filter order and compatibility mode"
```

## Phase 2: Implement chosen order in compiler/runtime/tests

### Track A (execute if canonical order is function-first)

- [ ] Keep `map` as function-first in `jazz-hs/src/Types.hs` and `jazz-hs/src/CodeGen/Javascript.hs`.
- [ ] Add `filter` builtin with function-first type and JS lowering.
- [ ] Add parser/analyzer tests for `filter predicate xs` and partial application (`filter predicate`).
- [ ] If compatibility mode allows collection-first temporarily, implement adapter/normalization plus warning behavior and tests.

### Track B (execute if canonical order is collection-first)

- [ ] Flip `map` and add `filter` to collection-first signatures.
- [ ] Update JS lowering path to preserve semantics with chosen order.
- [ ] Update parser expectations and tests to canonical collection-first call shape.
- [ ] Migrate function-first assumptions in examples/tests.
- [ ] If compatibility mode allows function-first temporarily, implement adapter/normalization plus warning behavior and tests.

### Likely file touch-set (choose only what changes)

- `jazz-hs/src/Types.hs`
- `jazz-hs/src/CodeGen/Javascript.hs`
- `jazz-hs/test/ParserSpec.hs`
- `jazz-hs/test/AnalyzerSpec.hs`
- `jazz-hs/test/Spec.hs`
- `jazz-hs/ExamplePrograms/MediumProgram.jz`
- `jazz-hs/ExamplePrograms/LongProgram.jz`

### Commit checkpoint (Phase 2)

Suggested commit message: `feat(lang): align map/filter behavior with canonical argument order`

```bash
git -C "$(git rev-parse --show-toplevel)" add jazz-hs/src/Types.hs jazz-hs/src/CodeGen/Javascript.hs jazz-hs/test/ParserSpec.hs jazz-hs/test/AnalyzerSpec.hs jazz-hs/test/Spec.hs jazz-hs/ExamplePrograms/MediumProgram.jz jazz-hs/ExamplePrograms/LongProgram.jz
git commit -m "feat(lang): align map/filter behavior with canonical argument order"
```

### 2026-03-05 execution batch (`jazz-next`)

- [x] Re-verified unchecked candidate implementation steps in this plan against active `jazz-next` code before changes.
- [x] Confirmed `map` already used canonical function-first order in active `jazz-next` type/runtime behavior.
- [x] Confirmed `filter` was still missing from active `jazz-next` builtin catalog and runtime/type pipeline.
- [x] Added `filter` builtin catalog support (`BuiltinFilter`, name lookup, arity).
- [x] Added `filter` type-instantiation contract in `jazz-next` type inference: `(a -> Bool) -> [a] -> [a]`.
- [x] Added runtime `filter` semantics and diagnostics (`E3017`, `E3018`, `E3019`) in `jazz-next`.
- [x] Added regression coverage in:
  - `jazz-next/test/BuiltinCatalogSpec.hs`
  - `jazz-next/test/PrimitiveSemanticsSpec.hs`
  - `jazz-next/test/RuntimeSemanticsSpec.hs`
- [x] Verified red/green cycle for added tests plus full suite via `bash jazz-next/scripts/test-warning-config.sh`.

## Phase 3: Migration and docs convergence

- [ ] Publish the final migration rule with before/after syntax examples for both `map` and `filter`.
- [ ] If hard switch was chosen, document as a breaking change and include mandatory source edits.
- [ ] If compatibility window was chosen, document warning text, deprecation timeline, and removal milestone.
- [ ] Update `README.md` and `docs/jazz-language-state.md` so examples/spec/runtime all agree.
- [ ] Add one concise “current behavior” statement and one “historical note” to prevent future drift.

### Commit checkpoint (Phase 3)

Suggested commit message: `docs: migrate map/filter examples and compatibility guidance`

```bash
git -C "$(git rev-parse --show-toplevel)" add README.md docs/jazz-language-state.md docs/plans/spec-cleanup/2026-03-02/decisions/02-map-filter-order.md
git commit -m "docs: migrate map/filter examples and compatibility guidance"
```

## Phase 4: Nix environment + command wiring

- [ ] Baseline confirmation: no repo-root `flake.nix`/`shell.nix` currently exists in `.`.
- [ ] Decision Gate 4: choose Nix entrypoint style.
- [ ] Preferred: add `flake.nix` with `devShells.default`.
- [ ] Fallback: add `shell.nix` if flake use is blocked.
- [ ] Include tools needed for this item: Haskell toolchain (`ghc`, `stack`, optionally `cabal-install`), Node runtime (for generated JS checks), formatter/linter chosen by maintainers (`ormolu` or `fourmolu`, optional `hlint`).
- [ ] Wire standardized commands for this decision area (choose one wiring style and document):
- [ ] `test:jazz-hs` (full tests)
- [ ] `test:parser` (targeted parser suite)
- [ ] `format:haskell`
- [ ] `verify:map-filter-order` (aggregates relevant checks)
- [ ] Ensure all wired commands can run as `nix develop -c <command>`.

### Commit checkpoint (Phase 4)

Suggested commit message: `chore(dev): add nix shell and test/format command wiring`

```bash
git -C "$(git rev-parse --show-toplevel)" add flake.nix shell.nix justfile Makefile scripts/dev/test-jazz-hs.sh scripts/dev/format-hs.sh README.md
git commit -m "chore(dev): add nix shell and test/format command wiring"
```

## Phase 5: Verification and closure

- [ ] Run formatter through Nix shell and ensure no unexpected diffs remain.
- [ ] Run targeted parser tests and full `jazz-hs` tests through Nix shell.
- [ ] Run at least one end-to-end program using both canonical `map` and canonical `filter` forms.
- [ ] Search docs/source for contradictory argument-order examples and resolve remaining mismatches.
- [ ] Mark this decision doc complete with closure date and selected compatibility outcome.

### Verification command checklist (executor adapts to chosen wiring)

```bash
nix develop -c <format-command>
nix develop -c <targeted-parser-test-command>
nix develop -c <full-test-command>
rg -n "map myArr|filter myArr|map ::|filter ::" "$(git rev-parse --show-toplevel)/README.md" "$(git rev-parse --show-toplevel)/docs/jazz-language-state.md" "$(git rev-parse --show-toplevel)/jazz-hs"
```

### Commit checkpoint (Phase 5)

Suggested commit message: `chore(spec-cleanup): close item #2 map/filter order`

```bash
git -C "$(git rev-parse --show-toplevel)" add README.md docs/jazz-language-state.md docs/plans/spec-cleanup/2026-03-02/decisions/02-map-filter-order.md jazz-hs/src/Types.hs jazz-hs/src/CodeGen/Javascript.hs jazz-hs/test/ParserSpec.hs jazz-hs/test/AnalyzerSpec.hs flake.nix shell.nix justfile Makefile scripts/dev/test-jazz-hs.sh scripts/dev/format-hs.sh
git commit -m "chore(spec-cleanup): close item #2 map/filter order"
```

## Migration strategy and compatibility decision points summary

- [ ] Select one canonical order and apply it uniformly to both `map` and `filter`.
- [ ] Choose hard-switch vs temporary compatibility window vs permanent dual support.
- [ ] If temporary compatibility is chosen, define sunset milestone/date up front.
- [ ] Publish migration guidance with concrete before/after examples and warning behavior.
- [ ] Close item only after parser/type/codegen/tests/docs all reflect the same rule.

## Exit criteria

- [ ] Canonical argument order is explicitly documented in repo sources.
- [ ] Runtime type env, JS lowering, and tests agree with that order.
- [ ] `filter` order and support status are no longer ambiguous.
- [ ] Nix-based reproducible test/format workflow exists for this item.
