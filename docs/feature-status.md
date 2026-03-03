# Jazz Feature Status Matrix

This is the canonical status matrix for top-level language claims. The top-level [README](../README.md) is a short summary, while this file is the source of truth for implemented-vs-planned status.

Last verified against commit: `f21c502dc499817a483e0febd4059a0da36ec811`

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
| Module/import syntax               | `Partially Implemented / Parse-Only` | Legacy reference: Parser accepts `module`/`import` forms in `jazz-hs/src/Parser/Lang.hs`, but real module loading is not finalized (`docs/jazz-language-state.md`). No `jazz-next/` parser equivalent yet.                    |
| Purity marker (`!`) is enforced    | `Planned / Aspirational`             | `README.md` describes purity semantics, but `docs/jazz-language-state.md` states there is no purity/effect enforcement in `jazz-next/`.                                 |
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
