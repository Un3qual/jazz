---
id: JN-RUNTIME-INTERPRETER-DOC-CLOSURE-001
status: ready
priority: P2
size: S
kind: docs
autonomous_ready: yes
depends_on: []
last_verified: 2026-05-29
plan_section: "Milestone 6: Productize interpreter-first execution"
target_paths:
  - docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md
  - README.md
  - docs/jazz-language-state.md
  - docs/feature-status.md
  - docs/execution/queue.md
verification:
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
deliverable: "Align runtime/product docs around the active interpreter-backed compile/run path and keep new runtime behavior changes out of scope."
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
- [x] Re-verified on `2026-03-19` that successful compile paths still emitted an internal placeholder artifact, and CLI/module tests still locked that contract.
- [x] On `2026-04-10`, made CLI compile success diagnostic-only with deterministic empty stdout for standalone and module-graph compile paths while keeping `--run` as the canonical execution surface.
- [x] Milestone 1 complete: compile and run contracts no longer depend on placeholder codegen output.
- [x] On `2026-04-26`, rebased module/import execution work onto active owners and landed dependency-module expression isolation in the driver replay path.
- [x] On `2026-04-26`, added explicit import symbol-list visibility validation in `ModuleResolver.hs`, so excluded dependency bindings now report deterministic `E4011` before flattened replay can leak them to an importer.
- [x] On `2026-04-26`, added alias-import unqualified visibility validation in `ModuleResolver.hs`, so bindings available only through `import Foo::Bar as B` now report deterministic `E4012` if referenced by bare name.
- [x] On `2026-04-26`, landed qualified alias lookup using `Alias::symbol`, with parser/lowering support, resolver validation (`E4013`/`E4014`), and driver alias bridge bindings for module replay.
- [x] On `2026-05-22`, added default `compileModuleGraph` and `runModuleGraph` driver helpers that load the bundled prelude, with multi-file loader coverage for bundled public prelude aliases.
- [x] On `2026-05-22`, verified the execution queue has no `Ready Now` implementation entries and kept `JN-MODULE-REBASE-PLAN-001` blocked on a concrete remaining stdlib phase-5 closure contract.
- [x] On `2026-05-22`, added bundled-prelude reproducibility evidence: `BundledPrelude` now owns the checked-in prelude path and `BuiltinCatalogSpec` fails if that file drifts from catalog-generated bridge/alias source.
- [x] On `2026-05-22`, retired the internal compile placeholder artifact: successful compile driver results now contain only diagnostics, and loader/warning-flow/type/runtime coverage asserts compile success through warnings/errors only.
- [ ] Milestone 2 complete: type-signature parsing and type grammar are rebased onto `jazz-next`.
- [ ] Milestone 3 complete: the runtime core covers the non-ADT language surface required by locked specs.
- [x] Milestone 4 complete: ADT, `case`, and pattern semantics are rebased and implemented in `jazz-next`.
- [ ] Milestone 5 complete: module/import and stdlib execution semantics are closed on the active path.
- [ ] Milestone 6 complete: CLI/docs treat interpreter-backed execution as the canonical product path.

## Active Baseline (2026-03-18)

- `JazzNext.Compiler.Driver` already coordinates standalone source, prelude-aware source, and module-graph execution.
- `JazzNext.Compiler.Runtime` already interprets the current core subset: ints, bools, lists, closures, builtin/kernel functions, operator values and sections, `if` via canonical `ECase`, and block scope evaluation.
- `JazzNext.Compiler.TypeInference` still behaves as a light canonicalization/type-check layer; supported monomorphic signatures now arrive as structured parser/core payloads with right-associated chained arrows and parenthesized function-type overrides, but constrained-signature work remains blocked on the next type-grammar milestones.
- `JazzNext.Compiler.ModuleResolver` resolves module graphs, validates import symbol lists/aliases, rejects importer references to exported dependency bindings excluded by explicit import lists, prevents alias-only imports from leaking dependency exports as unqualified names, and validates `Alias::symbol` references. `JazzNext.Compiler.Driver` replays resolved modules through the shared pipeline, injecting internal alias bridge bindings for qualified lookup; dependency modules contribute declarations during replay, while executable expression statements are preserved only for the entry module.
- Successful compile paths are now diagnostic-only and keep stdout empty on success, while successful run paths continue to return interpreter output.

## Milestone 1 Closure (2026-04-10)

- Successful CLI compile paths are now diagnostic-only: standalone and module-graph compile success both exit `0`, print warnings to stderr when present, and otherwise keep stdout empty.
- `--run` remains the canonical interpreter-backed execution path and continues to be the only CLI mode that prints evaluated runtime output.
- The driver compile result no longer carries a generated-artifact field; compile-only callers inspect warnings and errors only.

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
| `JN-TYPE-PLAN-001` and `docs/plans/2026-03-18-jazz-next-type-grammar-and-signature-rebase-plan.md` | The supported monomorphic subset now uses structured parser/core signature payloads with canonical arrow associativity, but constrained-signature rules are still unresolved. | Real type-system phases in `TypeInference.hs` and the next parser/lowering grammar decisions. |
| Active parser/operator/if/primitive work (`14`, `15`, `16`) | These domains are already partially represented in `jazz-next` runtime and tests. | Safe runtime-core expansion without reopening settled syntax decisions. |
| `docs/plans/2026-03-18-jazz-next-adt-and-pattern-matching-rebase-plan.md` | ADT/pattern semantics now have an active-path owner map and milestone plan tied to the current runtime pipeline. | Constructor values, `case`, pattern matching, and related diagnostics in `jazz-next`. |
| Module-loader rebase (`09`) | Module resolution exists in code, but its normative execution plan is still legacy-targeted. | Deterministic module execution semantics, import diagnostics, and closure of multi-file runtime behavior. |
| Remaining stdlib closure (`10`) | Prelude/kernel ownership is mostly in place but not fully closed. | Kernel-only hardcoded surface and final builtin/runtime boundary cleanup. |

## Milestone Plan

### Milestone 1: Close the compile vs run contract gap

- [x] Decide whether successful `compile` remains diagnostic-only or produces a real intermediate artifact; remove the misleading placeholder-only success contract.
- [x] Keep `run` as the canonical interpreter-backed execution path during the transition.
- [x] Make CLI exit-code and stdout/stderr behavior deterministic for compile-only, run-success, compile-failure, and runtime-failure paths.

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

- [x] Replace raw `Text` signatures with parsed type AST owned by `jazz-next` for the currently supported monomorphic subset.
- [x] Land canonical right-associative arrow parsing and explicit parenthesized function-type overrides against `JazzNext.Compiler.Parser*`, `AST`, and `TypeInference`.
- [ ] Land constrained-signature rules against `JazzNext.Compiler.Parser*`, `AST`, and `TypeInference`.
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
- [x] Add constructor values, `case` branches beyond boolean canonicalization, and pattern-match diagnostics in `jazz-next`.
- [x] Keep parser, analyzer/type, and runtime changes in one vertical slice so runtime semantics never drift from new AST shapes.

Primary files:

- `jazz-next/src/JazzNext/Compiler/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/TypeInference.hs`
- `jazz-next/src/JazzNext/Compiler/Runtime.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs`

### Milestone 5: Close module/import and stdlib execution semantics

- [x] Rebase the module-loader plan onto the current `ModuleResolver.hs` and `Driver.hs` path instead of legacy loader files.
- [ ] Finish stdlib phase-5 kernel reduction and keep module execution/prelude ownership on the same runtime contract.
- [ ] Extend loader/runtime tests so multi-file execution and builtin/prelude ownership are verified together.

#### Coordination: Module/import active-path execution contract

This coordination batch completed on `2026-04-26`. It selected dependency-module expression isolation as the first active-path implementation slice, then later narrowed and landed explicit symbol-list visibility, alias-import unqualified visibility, and qualified alias lookup. Remaining module/prelude/std-lib execution closure needs a narrower implementation contract.

- [x] Rebase the module/import execution contract onto the current `ModuleResolver.hs`, `Driver.hs`, and `CLI/Main.hs` ownership boundaries.
- [x] Identify the next missing executable behavior beyond already-landed resolution, graph replay, CLI entry-module routing, and import-symbol diagnostics: dependency module expression statements were still replayed into entry-module execution.
- [x] Rewrite `JN-MODULE-REBASE-PLAN-001` to the remaining module/import semantics scope and execute the concrete dependency-expression, explicit import visibility, alias visibility, and qualified alias lookup implementation batches.

Coordination files:

- `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`
- `docs/execution/queue.md`

Coordination verification:

```bash
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 1: Dependency module expression isolation

This batch landed on `2026-04-26`. Module graph replay still flattens resolved modules into the shared active compiler pipeline, but dependency modules now contribute declarations only. The entry module remains the only module whose expression statements participate in compile/run output or runtime failure.

- [x] Keep `SLet`, `SSignature`, `SData`, and import/declaration statements from resolved dependency modules so imported declarations remain available.
- [x] Strip dependency-module `SExpr` statements during driver replay, after resolver validation and before type/runtime evaluation.
- [x] Preserve entry-module `SExpr` statements so entry-module compile/run behavior remains canonical.
- [x] Add loader coverage proving a dependency-local `1 / 0.` expression does not fail an entry module run that imports a dependency binding.

Batch 1 files:

- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`

Batch 1 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

Primary files:

- `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/src/JazzNext/Compiler/BuiltinCatalog.hs`
- `jazz-next/src/JazzNext/Compiler/PreludeContract.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`

#### Batch 2: Explicit import symbol-list visibility

This batch landed on `2026-04-26`. The resolver already validated that requested explicit import symbols existed; it now also treats the explicit symbol list as the importer-visible surface. If a module imports `Lib::Math (add).` and then references `subtract` solely because `Lib::Math` exports it, resolution fails deterministically with `E4011`.

- [x] Collect importer references from parsed surface expressions while respecting local top-level bindings, lambda parameters, and pattern binders.
- [x] Compare those references with each explicit import list after dependency exports are known.
- [x] Preserve unrestricted imports and symbols exposed through other imports as visible names.
- [x] Add loader coverage proving an exported but unlisted dependency binding is hidden from the importer.

Batch 2 files:

- `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`

Batch 2 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 3: Alias import unqualified visibility

This batch landed on `2026-04-26`. Alias imports are still future work for qualified lookup, but they no longer behave like unrestricted unqualified imports. If a module imports `Lib::Math as Math` and then references `subtract` by bare name solely because `Lib::Math` exports it, resolution fails deterministically with `E4012`.

- [x] Treat alias imports as exporting no unqualified names while preserving alias collision validation.
- [x] Preserve visibility for names exposed by a separate unrestricted or explicit symbol-list import.
- [x] Add resolver and loader coverage proving a binding imported only through an alias is hidden from unqualified lookup.

Batch 3 files:

- `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`

Batch 3 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 4: Qualified alias lookup

This batch landed on `2026-04-26`. The active surface for alias-qualified module access is `Alias::symbol`. Qualified lookup validates that `Alias` names an alias import in the current module and that the imported module exports `symbol`; replay then injects internal alias bridge bindings so the existing analyzer/type/runtime pipeline can resolve the qualified value without making the dependency export visible as a bare name.

- [x] Parse `Alias::symbol` as a qualified alias reference in expression position and lower it to an internal qualified binding name.
- [x] Validate qualified references in `ModuleResolver.hs`, with deterministic `E4013` for undeclared aliases and `E4014` for missing exported symbols.
- [x] Inject replay-time alias bridge bindings in `Driver.hs` for alias imports.
- [x] Add parser/lowering, resolver, and loader coverage for successful and invalid qualified alias lookup.

Batch 4 files:

- `jazz-next/src/JazzNext/Compiler/Parser.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`

Batch 4 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 5: Default bundled-prelude module graph helpers

This batch landed on `2026-05-22`. The standalone source helpers already loaded the bundled prelude by default, while module-graph callers had only explicit prelude/no-prelude entry points. `compileModuleGraph` and `runModuleGraph` now mirror `compileSource` and `runSource`: they load `jazz-next/stdlib/Prelude.jz` by default and delegate through the same resolved-prelude module graph path. Explicit no-prelude module graph callers continue using `compileModuleGraphWithPrelude Nothing`, `runModuleGraphWithPrelude Nothing`, or `PreludeAbsent`.

- [x] Add default bundled-prelude module graph driver helpers in `Driver.hs`.
- [x] Preserve existing explicit and resolved prelude module graph entry points.
- [x] Add loader coverage proving bundled public prelude aliases (`map`, `hd`) execute across an imported module graph.

Batch 5 files:

- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`

Batch 5 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/PreludeLoadingSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

#### Batch 6: Bundled prelude reproducibility evidence

This batch landed on `2026-05-22`. The bundled prelude source remains generated from `BuiltinCatalog`, and `jazz-next/stdlib/Prelude.jz` remains the checked-in mirror named by the stdlib boundary spec. `BundledPrelude` now exposes that canonical repository-relative path, and catalog coverage reads the file and compares it against the generated source so bridge/alias drift fails in the focused stdlib suite.

- [x] Add `bundledPreludePath` in `BundledPrelude.hs` for the checked-in prelude mirror.
- [x] Add `BuiltinCatalogSpec` coverage proving `jazz-next/stdlib/Prelude.jz` matches the catalog-generated bundled prelude source.
- [x] Keep generated bundled-prelude loading behavior unchanged.

Batch 6 files:

- `jazz-next/src/JazzNext/Compiler/BundledPrelude.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs`

Batch 6 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

### Milestone 6: Productize interpreter-first execution

- [ ] Treat file/module execution through `jazz-next` as the canonical product path in CLI and docs.
- [ ] Update README/status docs once compile/run behavior, module loading, and runtime coverage match observed behavior.
- [ ] Close this plan only after queue entries and linked docs stop pointing at `jazz-hs` runtime files for new work.

#### Batch 1: Internal compile placeholder retirement

This batch landed on `2026-05-22`. It retired the internal generated-artifact placeholder for non-CLI compile callers. The CLI already kept compile stdout diagnostic-only; this batch made the lower-level driver/test contract match that product behavior.

- [x] Change successful compile driver results to expose only warnings/errors rather than any generated-artifact field.
- [x] Update module-loader compile success coverage to assert diagnostic-only compile artifacts.
- [x] Update warning-flow and compile-success proxy coverage so success is still distinguishable by warnings/errors, not by placeholder output.

Batch 1 files:

- `jazz-next/src/JazzNext/Compiler/Driver.hs`
- `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/RebindingWarningSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/IfExpressionTypeSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs`
- `jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs`

Batch 1 verification:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Semantics/RebindingWarningSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/CLI/CLISpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
```

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
