# Jazz Feature Status Matrix

This is the canonical status matrix for top-level language claims. The top-level [README](../README.md) is a short summary, while this file is the source of truth for implemented-vs-planned status.

Last verified against commit: `c6301291c7431af47a1c65fa3ff53e9a89719ada`

## Status Rubric

- `Implemented Today`: Works end-to-end in the active repository behavior (`jazz-next/` parse/analyze/codegen/runtime path).
- `Partially Implemented / Parse-Only`: Accepted by parser and/or represented in AST, but not fully supported through analyzer/codegen/runtime.
- `Planned / Aspirational`: Project goal, roadmap, or marketing claim not implemented end-to-end today.

## Feature Matrix

| Top-level claim                    | Status                               | Evidence                                                                                                                                                             |
| ---------------------------------- | ------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| ADTs                               | `Partially Implemented / Parse-Only` | Legacy reference: `jazz-hs/src/Parser/Lang.hs` parses `data` declarations; `docs/jazz-language-state.md` lists ADTs as scaffolding and not fully end-to-end. No `jazz-next/` parser equivalent yet.                           |
| Easy to understand syntax          | `Planned / Aspirational`             | This is a project-positioning claim in `README.md`, not an implementation-verified compiler behavior.                                                                |
| Performant / LLVM backend          | `Planned / Aspirational`             | `README.md` says LLVM generation is future work; `docs/jazz-language-state.md` lists backend target as unsettled and LLVM out of active scope.                       |
| Strong static typing (core subset) | `Implemented Today`                  | Type inference and checking are part of the active `jazz-next/` pipeline in `jazz-next/src/JazzNext/Compiler/TypeInference.hs` and `jazz-next/src/JazzNext/Compiler/Analyzer.hs`; `docs/jazz-language-state.md` documents end-to-end typed core subset. |
| Type inference (core subset)       | `Implemented Today`                  | `jazz-next/src/JazzNext/Compiler/TypeInference.hs` runs analyzer/type inference; `docs/jazz-language-state.md` confirms core inference works today.                                 |
| Immutable bindings                 | `Implemented Today`                  | Legacy reference: `jazz-hs` supports value bindings (`x = expr`) with no mutable assignment form in active grammar; documented in `docs/jazz-language-state.md`. No `jazz-next/` parser equivalent yet.                       |
| First-class functions              | `Implemented Today`                  | Legacy references: Lambdas and function application are implemented end-to-end in `jazz-hs` (`jazz-hs/src/Parser/Lang.hs`, `jazz-hs/src/CodeGen/Javascript.hs`, `docs/jazz-language-state.md`). No `jazz-next/` parser/codegen equivalent yet.      |
| Functions are curried by default   | `Implemented Today`                  | Legacy references: Application is left-associative by juxtaposition and builtins are curried in `jazz-hs/src/Types.hs` and `jazz-hs/src/CodeGen/Javascript.hs`. No `jazz-next/` codegen equivalent yet.                         |
| Pattern matching                   | `Partially Implemented / Parse-Only` | Pattern syntax and `case` parse support exists, but type inference/codegen coverage is incomplete per `docs/jazz-language-state.md`.                                 |
| Tuples                             | `Partially Implemented / Parse-Only` | Legacy reference: Tuple literals parse/infer, but JS codegen errors on tuples (`docs/jazz-language-state.md`, `jazz-hs/src/CodeGen/Javascript.hs`). No `jazz-next/` codegen equivalent yet.                                    |
| Module/import syntax               | `Partially Implemented / Parse-Only` | `jazz-next` parses/lowers module/import statements and supports deterministic module-graph loading diagnostics (`jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`, `jazz-next/src/JazzNext/CLI/Main.hs`), but qualified-import name binding semantics are not yet finalized as canonical language behavior. |
| Purity marker (`!`) is enforced    | `Implemented Today`                  | `jazz-next` now enforces stub-v1 purity in analyzer/type pipeline: pure bindings reject direct calls to known `!`-suffixed callees (`jazz-next/src/JazzNext/Compiler/{Purity.hs,Analyzer.hs}`) with regression coverage in `jazz-next/test/PuritySemanticsSpec.hs`. |
| `$` low-precedence application     | `Implemented Today`                  | `$` parsing/associativity is documented in `docs/jazz-language-state.md` and supported by parser/operator behavior; legacy reference in `jazz-hs`.                     |

## Maintenance Checklist

Update this matrix whenever any of the following changes:

- parser/analyzer/codegen behavior for a listed feature
- builtin/runtime behavior used as feature evidence
- top-level README feature bullets or examples

For every status change in this file:

- include evidence path(s) to implementation/docs
- update `Last verified against commit` to the commit used for verification
- include a short rationale for why the status changed

Reviewer checklist item:

- `Does README status match docs/feature-status.md?`
