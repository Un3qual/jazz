---
id: JN-RUNTIME-COMPILE-CONTRACT-001
status: ready
priority: P1
size: M
kind: impl
autonomous_ready: no
depends_on: []
last_verified: 2026-03-19
plan_section: "Milestone 1: Close the compile vs run contract gap"
target_paths:
  - jazz-next/src/JazzNext/CLI/Main.hs
  - jazz-next/src/JazzNext/Compiler/Driver.hs
  - jazz-next/test/JazzNext/CLI/CLISpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
deliverable: "Successful `compile` paths stop surfacing the misleading `/* jazz-next codegen placeholder */` output as user-facing success behavior; `--run` stays canonical, and source/module-graph compile modes share deterministic exit-code/stdout/stderr behavior."
supersedes:
  - docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md
---

# Jazz Next Runtime Architecture and Interpreter Execution Plan

> Active-path replacement for `docs/plans/spec-clarification/2026-03-02/runtime/12a-haskell-interpreter-implementation.md`. New runtime work belongs in `jazz-next/`; `jazz-hs/` remains reference-only evidence for historical planning context.

**Goal:** define the current `jazz-next` runtime pipeline, name the active file owners and verification suites, and stage the remaining work required to make the interpreter-backed execution path the canonical product surface.

**Architecture:** treat `jazz-next` as one compiler/runtime pipeline:

1. source intake from CLI or module graph,
2. optional bundled or explicit prelude merge plus bridge validation,
3. surface parse,
4. lowering into the shared core AST,
5. analyzer/type-inference canonicalization and warning promotion,
6. runtime evaluation,
7. CLI rendering of compile/runtime diagnostics and output.

**Tech stack:** Haskell modules under `jazz-next/src/JazzNext`, `runghc` suites under `jazz-next/test`, repo-root verification via `bash jazz-next/scripts/test-warning-config.sh`.

## Plan Progress

- [x] Captured the active-path runtime architecture and file ownership.
- [x] Marked legacy `12a` runtime planning as reference-only for new execution work.
- [x] Re-verified on `2026-03-19` that successful compile paths still emit `/* jazz-next codegen placeholder */`, and CLI/module tests still lock that contract.
- [ ] Milestone 1 complete: compile and run contracts no longer depend on placeholder codegen output.
- [ ] Milestone 2 complete: type-signature parsing and type grammar are rebased onto `jazz-next`.
- [ ] Milestone 3 complete: the runtime core covers the non-ADT language surface required by locked specs.
- [ ] Milestone 4 complete: ADT, `case`, and pattern semantics are rebased and implemented in `jazz-next`.
- [ ] Milestone 5 complete: module/import and stdlib execution semantics are closed on the active path.
- [ ] Milestone 6 complete: CLI/docs treat interpreter-backed execution as the canonical product path.

## Active Baseline (2026-03-18)

- `JazzNext.Compiler.Driver` already coordinates standalone source, prelude-aware source, and module-graph execution.
- `JazzNext.Compiler.Runtime` already interprets the current core subset: ints, bools, lists, closures, builtin/kernel functions, operator values and sections, `if` via canonical `ECase`, and block scope evaluation.
- `JazzNext.Compiler.TypeInference` still behaves as a light canonicalization/type-check layer; signatures remain raw `Text`, so meaningful type-directed expansion is blocked on the type-grammar rebase plan.
- `JazzNext.Compiler.ModuleResolver` already resolves and replays module graphs, but the normative module/runtime plan still needs an active-path rewrite around these files.
- Successful compile paths still emit `"/* jazz-next codegen placeholder */"`; successful run paths already return interpreter output, so compile-mode semantics and CLI contract still need tightening.

## Runtime Pipeline and Owners

| stage | current owner files | current behavior | next required closure |
| --- | --- | --- | --- |
| Source and prelude intake | `jazz-next/src/JazzNext/Compiler/Driver.hs`, `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`, `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`, `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs` | Loads bundled or explicit prelude, validates `__kernel_*` bridges, and hides bundled-prelude diagnostics where appropriate. | Keep prelude loading, bridge validation, and builtin resolution aligned with the active stdlib boundary plan. |
| Surface parse | `jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Operator.hs` plus parser specs | Parses the current language slice, including lambdas, `if`, operators, sections, lists, and module/import forms. | Extend only through active-path spec plans; do not reintroduce legacy parser targets. |
| Lowering to shared core AST | `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`, `jazz-next/src/JazzNext/Compiler/AST.hs` | Produces the core `Expr` and `Statement` tree consumed by analysis, type checking, and runtime. | Rebase type AST and signature work here before deeper type-system changes land. |
| Semantic normalization | `jazz-next/src/JazzNext/Compiler/Analyzer.hs`, `jazz-next/src/JazzNext/Compiler/TypeInference.hs`, `jazz-next/src/JazzNext/Compiler/Purity.hs` | Performs binding and warning checks plus a small set of type/runtime-compatibility checks; canonicalizes `if` and `$`. | Replace raw-text signatures and shallow inference with real typed representations and richer error taxonomy. |
| Runtime execution | `jazz-next/src/JazzNext/Compiler/Runtime.hs`, `jazz-next/src/JazzNext/Compiler/Driver.hs`, runtime/primitive/purity specs | Evaluates the current subset and returns rendered runtime output or fatal diagnostics. | Expand the runtime value model and evaluator in the same file family instead of creating a second execution pipeline. |
| Module/runtime orchestration | `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`, `jazz-next/src/JazzNext/Compiler/Driver.hs`, loader/module specs | Resolves module graphs, replays sources, strips module declarations, and routes execution back through the same prelude/analyze/run path. | Re-author the module-loader plan around these modules before adding broader module semantics. |
| CLI product surface | `jazz-next/src/JazzNext/CLI/Main.hs`, `jazz-next/test/JazzNext/CLI/CLISpec.hs` | Supports compile and run modes with explicit/no-prelude control. | Make interpreter-backed execution and deterministic exit/reporting behavior the documented default product surface. |

## Critical Dependency Map

| dependency | why it matters | what it unlocks |
| --- | --- | --- |
| `18-compiler-warning-flags.md` and current driver wiring | Warning/error promotion already shapes the compile/run contract. | Stable diagnostics while runtime milestones land. |
| `JN-TYPE-PLAN-001` and `docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md` | Signatures are still raw `Text`; meaningful type-directed runtime expansion depends on parsed type AST and active-path rules. | Real type-system phases in `TypeInference.hs` and parser/lowering changes. |
| Active parser/operator/if/primitive work (`14`, `15`, `16`) | These domains are already partially represented in `jazz-next` runtime and tests. | Safe runtime-core expansion without reopening settled syntax decisions. |
| `docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md` | ADT/pattern semantics now have an active-path owner map and milestone plan tied to the current runtime pipeline. | Constructor values, `case`, pattern matching, and related diagnostics in `jazz-next`. |
| Module-loader rebase (`09`) | Module resolution exists in code, but its normative execution plan is still legacy-targeted. | Deterministic module execution semantics, import diagnostics, and closure of multi-file runtime behavior. |
| Remaining stdlib closure (`10`) | Prelude/kernel ownership is mostly in place but not fully closed. | Kernel-only hardcoded surface and final builtin/runtime boundary cleanup. |

## Milestone Plan

### Milestone 1: Close the compile vs run contract gap

- [ ] Decide whether successful `compile` remains diagnostic-only or produces a real intermediate artifact; remove the misleading placeholder-only success contract.
- [ ] Keep `run` as the canonical interpreter-backed execution path during the transition.
- [ ] Make CLI exit-code and stdout/stderr behavior deterministic for compile-only, run-success, compile-failure, and runtime-failure paths.

Primary files:

- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/CLI/Main.hs`
- `jazz-next/test/JazzNext/CLI/CLISpec.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`

Suggested verification:

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

### Milestone 2: Rebase type signatures and type grammar onto `jazz-next`

Depends on `JN-TYPE-PLAN-001`.

- [ ] Replace raw `Text` signatures with parsed type AST owned by `jazz-next`.
- [ ] Land arrow associativity and constrained-signature rules against `JazzNext.Compiler.Parser*`, `AST`, and `TypeInference`.
- [ ] Keep runtime/evaluator expansion blocked until the type surface is represented in active-path data types rather than legacy references.

Primary files:

- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`

### Milestone 3: Expand the runtime-backed core language slice

- [ ] Extend the core AST and runtime value model only where locked specs already require it.
- [ ] Keep interpreter execution in `JazzNext.Compiler.Runtime` and avoid introducing a second backend abstraction.
- [ ] Align runtime diagnostics with the compile-time diagnostic model so CLI output stays uniform.

Primary files:

- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs`

### Milestone 4: Rebase and implement ADT/pattern runtime work

Depends on `docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md`.

- [x] Re-author the ADT/pattern plan against the active runtime pipeline in this document.
- [ ] Add constructor values, `case` branches beyond boolean canonicalization, and pattern-match diagnostics in `jazz-next`.
- [ ] Keep parser, analyzer/type, and runtime changes in one vertical slice so runtime semantics never drift from new AST shapes.

Primary files:

- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

### Milestone 5: Close module/import and stdlib execution semantics

- [ ] Rebase the module-loader plan onto the current `ModuleResolver.hs` and `Driver.hs` path instead of legacy loader files.
- [ ] Finish stdlib phase-5 kernel reduction and keep module execution/prelude ownership on the same runtime contract.
- [ ] Extend loader/runtime tests so multi-file execution and builtin/prelude ownership are verified together.

Primary files:

- `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
- `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`

### Milestone 6: Productize interpreter-first execution

- [ ] Treat file/module execution through `jazz-next` as the canonical product path in CLI and docs.
- [ ] Update README/status docs once compile/run behavior, module loading, and runtime coverage match observed behavior.
- [ ] Close this plan only after queue entries and linked docs stop pointing at `jazz-hs` runtime files for new work.

Primary files:

- `jazz-next/src/JazzNext/CLI/Main.hs`
- `README.md`
- `docs/jazz-language-state.md`
- `docs/feature-status.md`
- `docs/execution/queue.md`

## Default Verification Ladder

Use the repo-root active-path checks below as milestones land.

```bash
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
runghc -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash jazz-next/scripts/test-warning-config.sh
```

## Definition of Done

- [ ] `jazz-next` compile, run, and docs all describe one active interpreter pipeline instead of a mix of placeholder codegen and legacy runtime targets.
- [ ] Runtime, parser/lowering, analyzer/type, and CLI work all point at the concrete `jazz-next` files listed above.
- [ ] Dependent plans (`07`, `09`, `11`, `10`) reference this architecture when they are rebased.
- [ ] Queue dispatch no longer uses `jazz-hs` runtime plans as active implementation guidance.
