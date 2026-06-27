# Spec Clarification Item #12: Backend Target Strategy (Legacy Closure)

> **Legacy closure note (2026-06-24):** this plan is now reference-only. The
> original March 2026 strategy work targeted `jazz-hs` runtime, CLI, and build
> files, but `jazz-hs/` is a read-only legacy reference under the current
> workspace policy. Active runtime work belongs in `jazz-next/` and is tracked
> by
> `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`.

**Closed decision:** the active product path is the `jazz-next`
interpreter-backed compile/run/help baseline. No backend implementation, legacy
runtime edit, generated-artifact policy, or alternative codegen target should
be promoted from this legacy plan.

**Current authority:** runtime product work is governed by the active
`jazz-next` runtime architecture plan and the queue blocker
`JN-RUNTIME-PRODUCTIZE-CLOSURE-PLAN-001`. Any future runtime product delta must
name concrete `jazz-next` target paths and focused verification before it can
return to `Ready Now`.

**Historical tech stack:** Haskell (`jazz-hs` parser/analyzer/interpreter/tests),
Markdown specs/ADRs, Stack, and Nix shell validation commands. These remain
historical evidence only.

---

## Progress Tracker

- [x] Contradiction inventory captured (JS pipeline + LLVM aspirations)
- [x] Strategy lock approved by maintainer (2026-03-02)
- [x] Legacy backend-target execution path closed as reference-only
  (2026-06-24)
- [x] Active runtime product authority moved to the `jazz-next` runtime
  architecture plan
- [x] Later legacy `jazz-hs` runtime/CLI/build phases superseded and no longer
  executor-actionable

## Decision Lock (Approved 2026-03-02)

- [x] No JavaScript backend in active scope.
- [x] No LLVM backend in active scope.
- [x] Backend strategy is Haskell interpreter only for now.
- [x] Active runtime planning now uses `jazz-next`; this legacy strategy plan is
  not an implementation source.

## Historical Evidence (Why Clarification Was Needed)

- `README.md:10` still references future LLVM IR.
- `jazz-hs/src/Lib.hs:19` historically imported the legacy JavaScript codegen
  module.
- `jazz-hs/app/Main.hs:18` historically emitted legacy JavaScript output.
- `docs/jazz-language-state.md:399` had backend target as unresolved.
- `jazz-hs/src/CodeGen/Builtins.hs:3` and cabal/stack metadata retain legacy LLVM/QBE traces.

## Scope Guardrails

Active closure scope:

- preserve the historical interpreter-only strategy decision,
- mark the legacy plan reference-only,
- keep real runtime product work routed through active `jazz-next` plans.

Out of scope (for this item):

- editing `jazz-hs/` or `jazz2/`,
- adding backend implementation,
- reopening alternate backend or codegen policy,
- changing active compile/run/help semantics.

## Superseded Legacy Execution Phases

The phase plan below is preserved as historical context only. Do not execute
these phases as active work. Any future runtime product change must start from
the active `jazz-next` runtime architecture plan and a queue-promoted concrete
delta.

## Phase 0: ADR and Policy Freeze

- [ ] Create ADR documenting interpreter-only backend policy.
- [ ] Mark JS/LLVM as explicitly deferred/archived targets, not active roadmap targets.
- [ ] Define conditions that would justify revisiting backend targets later.

Create:
- `docs/decisions/adr-0012-interpreter-only-runtime.md`

Modify:
- `docs/jazz-language-state.md`
- `docs/plans/spec-clarification/2026-03-02/runtime/12-backend-target-strategy.md`

Commit checkpoint:

```bash
git add docs/decisions/adr-0012-interpreter-only-runtime.md docs/jazz-language-state.md docs/plans/spec-clarification/2026-03-02/runtime/12-backend-target-strategy.md
git commit -m "docs(decision): lock backend policy to haskell interpreter only"
```

## Phase 1: Runtime Contract and CLI Alignment Plan

- [ ] Define canonical compile/execute contract: parse -> analyze -> interpret.
- [ ] Define CLI behavior for running programs through interpreter.
- [ ] Define error-reporting and exit-code behavior for interpreter mode.

Create:
- `docs/spec/runtime/interpreter-runtime-contract.md`

Modify:
- `jazz-hs/app/Main.hs`
- `jazz-hs/src/Lib.hs`

Commit checkpoint:

```bash
git add docs/spec/runtime/interpreter-runtime-contract.md jazz-hs/app/Main.hs jazz-hs/src/Lib.hs
git commit -m "feat(runtime): define interpreter runtime contract and cli path"
```

## Phase 2: Interpreter-Focused Test Matrix

- [ ] Add interpreter test coverage as primary runtime verification.
- [ ] Move or replace backend tests that assume JS codegen output.
- [ ] Add smoke tests for example programs through interpreter path.

Create/Modify:
- `jazz-hs/test/InterpreterSpec.hs`
- `jazz-hs/test/Spec.hs`
- `jazz-hs/ExamplePrograms/ComplexProgram.jz`

Commit checkpoint:

```bash
git add jazz-hs/test/InterpreterSpec.hs jazz-hs/test/Spec.hs jazz-hs/ExamplePrograms/ComplexProgram.jz
git commit -m "test(runtime): prioritize interpreter execution coverage"
```

## Phase 3: JS/LLVM De-scope Cleanup Plan

- [ ] Update docs to remove active-target language for JS/LLVM.
- [ ] Mark legacy codegen modules as deprecated/internal-only until removed.
- [ ] Track removal/deprecation of unused backend dependencies in cabal/stack files.

Modify:
- `README.md`
- `jazz-hs/jazz.cabal`
- `jazz-hs/stack.yaml`
- `docs/spec/feature-status.md` (if created by item #5 execution)

Commit checkpoint:

```bash
git add README.md jazz-hs/jazz.cabal jazz-hs/stack.yaml docs/spec/feature-status.md
git commit -m "docs(build): remove active JS/LLVM target commitments"
```

(If `docs/spec/feature-status.md` does not exist yet, omit it.)

## Follow-On Plan (Interpreter Implementation)

- [x] Dedicated implementation plan path is defined:
- `docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md`

## Nix Reproducibility Commands

```bash
export NIXPKGS_REF='github:NixOS/nixpkgs/68cc97d306d3187c142cfb2378852f28d47bc098'
nix --extra-experimental-features 'nix-command flakes' shell \
  "$NIXPKGS_REF#stack" \
  "$NIXPKGS_REF#ghc" \
  "$NIXPKGS_REF#ripgrep" \
  -c bash -lc '
    set -euo pipefail
    cd .
    rg -n "JavaScript|LLVM|backend" README.md docs/jazz-language-state.md docs/plans/spec-clarification/2026-03-02/runtime/12-backend-target-strategy.md
    cd jazz-hs
    stack test
  '
```

## Definition of Done

- [ ] Backend policy is documented as interpreter-only in ADR + language-state + README.
- [ ] Interpreter runtime path is the canonical execution path in docs and CLI contract.
- [ ] JS/LLVM are no longer described as active targets.
- [ ] Follow-on interpreter implementation plan is in place and linked.
