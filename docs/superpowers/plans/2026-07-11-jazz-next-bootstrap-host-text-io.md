---
id: JN-BOOTSTRAP-HOST-TEXT-IO-001
status: ready
priority: P1
size: L
kind: impl
autonomous_ready: yes
depends_on:
  - JN-BOOTSTRAP-TEXT-TRAVERSAL-001
last_verified: 2026-07-11
plan_section: "Implementation Batch: Bootstrap Host Text I/O"
target_paths:
  - docs/feature-status.md
  - docs/jazz-language-state.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
  - docs/execution/done-archive.md
  - docs/superpowers/specs/2026-07-11-jazz-next-bootstrap-host-text-io-design.md
  - jazz-next/jazz-next.cabal
  - jazz-next/src/JazzNext/CLI/Main.hs
  - jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs
  - jazz-next/src/JazzNext/Compiler/Driver.hs
  - jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs
  - jazz-next/src/JazzNext/Compiler/Runtime.hs
  - jazz-next/src/JazzNext/Compiler/RuntimeHost.hs
  - jazz-next/src/JazzNext/Compiler/TypeInference.hs
  - jazz-next/stdlib/Prelude.jz
  - jazz-next/test/JazzNext/CLI/CLISpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/Runtime/HostIOTests.hs
  - jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
verification:
  - cabal test builtin-catalog-spec primitive-semantics-spec purity-semantics-spec runtime-semantics-spec loader-spec prelude-loading-spec cli-spec --test-show-details=failures
  - bash jazz-next/scripts/check-stdlib-format.sh
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Add ordinary explicit-import IO and IOError modules backed by private backend-neutral host intrinsics, a typed monadic RuntimeHost seam, strict UTF-8 production file and stream operations, process arguments, and exit while preserving pure wrappers and future native-runtime portability."
---

# Jazz-Next Bootstrap Host Text I/O Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> `superpowers:executing-plans` to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give Jazz-authored bootstrap tools recoverable UTF-8 text I/O,
standard streams, process arguments, and explicit process termination without
coupling the Jazz API to the stage-0 Haskell interpreter or future LLVM code
generation.

**Architecture:** Add seven private `KernelIntrinsic` operations and implement
their public semantics in ordinary `IOError.jz` and `IO.jz` modules. Convert
the evaluator's effect-carrying path from `Either Diagnostic` to a small
`RuntimeHost m`-parameterized monadic layer while keeping existing pure entry
points as disabled-host wrappers. Module and driver execution gain explicit
host variants; only CLI run mode installs the production strict-UTF-8 host.

**Tech Stack:** Jazz `.jz`, Haskell 2010, `Data.Text`, `bytestring`, the
existing builtin/type/purity/runtime/module pipeline, Cabal component suites,
and repository queue/docs gates.

## Global Constraints

- Modify only `jazz-next/` and active documentation; `jazz-hs/` and `jazz2/`
  remain read-only.
- Follow the approved host text I/O design document exactly.
- Keep `IO` and `IOError` as ordinary explicit-import modules and export only
  the approved public types, constructors, and seven operations.
- Give every raw bridge `KernelIntrinsic` ownership with a kernel self-bridge
  and no compatibility/prelude alias.
- Preserve recoverable host failures as ordinary structural tuple values;
  never construct Jazz ADTs in Haskell.
- Keep fatal interpreter diagnostics in a separate error channel.
- Execute effects at their actual expression position, including closures and
  control flow; do not defer them to the driver.
- Keep pure runtime and module helpers source-compatible through an
  effect-disabled host.
- Decode input strictly as UTF-8 and encode output explicitly as UTF-8; do not
  depend on locale or platform exception wording.
- Catch synchronous I/O failures only. Do not translate asynchronous
  exceptions into Jazz `IOError` values.
- Keep all `.jz` module bodies at exactly two-space indentation increments.
- Prefer behavioral tests over source snapshots except for generated prelude
  reproducibility and formatting enforcement.
- Do not add bytes as a Jazz type, directory or environment APIs,
  subprocesses, networking, general effect types, stack-safe evaluation,
  lexer/parser modules, lowered IR, LLVM lowering, linking, or a native
  runtime.
- Implement every behavior test-first and commit independently reviewable
  milestones.

---

## Implementation Batch: Bootstrap Host Text I/O

### Task 1: Lock catalog, type, visibility, and purity contracts

**Files:**

- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- Modify: `jazz-next/stdlib/Prelude.jz`

- [x] Add failing catalog tests for exact symbols, names, arities,
  `KernelIntrinsic` ownership, kernel lookup, self-bridges, and missing public
  aliases.
- [x] Add failing inference tests for exact raw tuple/list/unit types and
  deterministic invalid-argument diagnostics.
- [x] Add failing purity tests proving pure bindings reject every raw `!`
  bridge while impure bindings and top-level expressions accept them.
- [x] Run the three focused suites and verify RED because the symbols do not
  exist.
- [x] Add all seven catalog constructors and checked-in prelude self-bridges.
- [x] Add exact type-instantiation cases in `TypeInference.hs`.
- [x] Run the focused suites and verify GREEN.
- [x] Commit as `feat: define private host IO intrinsics`.

### Task 2: Introduce the typed monadic runtime-host seam

**Files:**

- Create: `jazz-next/src/JazzNext/Compiler/RuntimeHost.hs`
- Create: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/HostIOTests.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`
- Modify: `jazz-next/jazz-next.cabal`

- [x] Add failing tests that instantiate a deterministic host and prove
  existing pure evaluation remains unchanged.
- [x] Run `runtime-semantics-spec` and verify RED because host entry points are
  absent.
- [x] Add `HostIOCategory`, `HostIOFailure`, `RuntimeHost m`, normalized
  category messages, and an effect-disabled host.
- [x] Convert the evaluator's expression, callable, scope, recursive-binding,
  and builtin application path to a host-parameterized monadic error channel.
- [ ] Retain every current `Either Diagnostic` entry point as a wrapper over
  the disabled `Identity` host.
- [x] Run runtime semantics and the warning matrix before adding effects.
- [x] Commit as `refactor: parameterize runtime evaluation by host`.

### Task 3: Execute private host intrinsics at expression depth

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/HostIOTests.hs`

- [x] Add failing deterministic-host tests for every successful operation,
  every category, argument order, unknown-category fallback, and recorded exit.
- [x] Add failing placement tests for closures, `if` branches, selected pattern
  arms, block terminals, and unselected branches.
- [x] Run runtime tests and verify RED because the builtins lack runtime cases.
- [x] Implement raw tuple/list/unit construction and runtime argument checks.
- [x] Validate `exit!` status against `0..255` before calling the host.
- [x] Run runtime and primitive suites and verify GREEN.
- [x] Commit as `feat: execute host IO intrinsics`.

### Task 4: Thread hosts through module and source execution

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Driver.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/HostIOTests.hs`

- [ ] Add failing injected-host tests through standalone source and a
  dependency-ordered module graph.
- [ ] Add `evaluateCompiledProgramWithHost` without changing compile-only paths.
- [ ] Add explicit source/module driver variants and route existing helpers
  through the disabled host.
- [ ] Run focused module/runtime suites and verify GREEN.
- [ ] Commit as `feat: inject runtime hosts through drivers`.

### Task 5: Add the strict UTF-8 production host and CLI injection

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/RuntimeHost.hs`
- Modify: `jazz-next/src/JazzNext/CLI/Main.hs`
- Modify: `jazz-next/test/JazzNext/CLI/CLISpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Semantics/Runtime/HostIOTests.hs`
- Modify: `jazz-next/jazz-next.cabal`

- [ ] Add failing temporary-file tests for multibyte round trips, missing paths,
  invalid UTF-8, and normalized failures.
- [ ] Add CLI tests proving run mode injects production while compile mode
  remains effect-free.
- [ ] Add `bytestring` and implement byte-oriented input, strict decoding,
  explicit UTF-8 output, arguments, exit, and reliable synchronous exception
  classification.
- [ ] Route only CLI run paths through the production host.
- [ ] Run runtime/CLI suites and verify GREEN.
- [ ] Commit as `feat: run Jazz with production host IO`.

### Task 6: Add Jazz-authored `IOError` and `IO` modules

**Files:**

- Create: `jazz-next/stdlib/IOError.jz`
- Create: `jazz-next/stdlib/IO.jz`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`

- [ ] Add failing loader tests for exact exports, all successes, every error
  category, path `Just`/`Nothing`, normalized messages, arguments, and exit.
- [ ] Add failing prelude-isolation tests for every public name.
- [ ] Run loader/prelude suites and verify RED because the modules are absent.
- [ ] Add two-space-indented `IOError.jz` with the exact approved ADTs.
- [ ] Add two-space-indented `IO.jz`, decode every token, default unknown tokens
  to `Other`, and attach paths only for file operations.
- [ ] Run loader/prelude/runtime suites and the stdlib format check.
- [ ] Commit as `feat: add Jazz host text IO modules`.

### Task 7: Close documentation and live dispatch state

**Files:**

- Modify: `docs/feature-status.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/done-archive.md`
- Modify: this plan

- [ ] Record the public API, error protocol, purity boundary, runtime-host seam,
  and native-runtime portability contract.
- [ ] Mark this plan done, archive the queue row, and curate the next bootstrap
  child without promoting an unaccepted implementation.
- [ ] Run all focused suites listed in frontmatter.
- [ ] Run the warning matrix and stdlib/queue/docs/diff gates.
- [ ] Inspect the final diff for public raw aliases, ambient-I/O tests,
  legacy-tree changes, or LLVM-coupled representations.
- [ ] Commit as `docs: close bootstrap host text IO batch`.
