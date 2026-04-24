# Jazz Feature Status Matrix

This is the canonical status matrix for top-level language claims. The top-level [README](../README.md) is a short summary, while this file is the source of truth for implemented-vs-planned status.

Last verified against commit: `1f78e98`

## Status Rubric

- `Implemented Today`: Works end-to-end in the active repository behavior (`jazz-next/` parse/analyze/codegen/runtime path).
- `Partially Implemented / Parse-Only`: Accepted by parser and/or represented in AST, but not fully supported through analyzer/codegen/runtime.
- `Planned / Aspirational`: Project goal, roadmap, or marketing claim not implemented end-to-end today.

## Feature Matrix

| Top-level claim                    | Status                               | Evidence                                                                                                                                                             |
| ---------------------------------- | ------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| ADTs                               | `Partially Implemented / Parse-Only` | `jazz-next` now parses/lowers canonical `data` declarations, registers constructor values for analyzer/type/runtime paths, and typechecks/executes declared constructor and exact-length bracketed-list patterns (`jazz-next/src/JazzNext/Compiler/{Parser.hs,AST.hs,Parser/Lower.hs,Analyzer.hs,TypeInference.hs,Runtime.hs}`; `jazz-next/test/JazzNext/Compiler/Semantics/{AdtPatternTypeSpec.hs,AdtPatternRuntimeSpec.hs}`). Tuple patterns, cons-like list patterns, and lambda-parameter patterns remain staged. |
| Easy to understand syntax          | `Planned / Aspirational`             | This is a project-positioning claim in `README.md`, not an implementation-verified compiler behavior.                                                                |
| Performant / LLVM backend          | `Planned / Aspirational`             | `README.md` says LLVM generation is future work; `docs/jazz-language-state.md` lists backend target as unsettled and LLVM out of active scope.                       |
| Strong static typing (core subset) | `Implemented Today`                  | Type inference and checking are part of the active `jazz-next/` pipeline in `jazz-next/src/JazzNext/Compiler/TypeInference.hs` and `jazz-next/src/JazzNext/Compiler/Analyzer.hs`; `docs/jazz-language-state.md` documents end-to-end typed core subset. |
| Type inference (core subset)       | `Implemented Today`                  | `jazz-next/src/JazzNext/Compiler/TypeInference.hs` runs analyzer/type inference; `docs/jazz-language-state.md` confirms core inference works today.                                 |
| Type signatures (monomorphic subset) | `Implemented Today`                | `jazz-next` now carries adjacent monomorphic signatures through structured parser/core payloads for `Int`, `Bool`, nested concrete list types, right-associative function types, and explicit parenthesized function-type overrides in `jazz-next/src/JazzNext/Compiler/{Parser.hs,Parser/AST.hs,AST.hs,Parser/Lower.hs,TypeInference.hs}`; covered by `jazz-next/test/JazzNext/Compiler/{Parser/ParserFoundationSpec.hs,Semantics/BindingSignatureCoherenceSpec.hs}` and `jazz-next/test/JazzNext/CLI/CLISpec.hs`. |
| Immutable bindings                 | `Implemented Today`                  | Active `jazz-next` syntax uses dot-terminated immutable bindings (`name = expr.`) with no mutable assignment form in parser or runtime paths (`jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/AST.hs`, `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`). |
| First-class functions              | `Implemented Today`                  | `jazz-next` now parses canonical identifier-only lambdas, lowers them into executable core lambda nodes, infers callable function types, and executes lexical closures at runtime (`jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`, `jazz-next/src/JazzNext/Compiler/TypeInference.hs`, `jazz-next/src/JazzNext/Compiler/Runtime.hs`, `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs`, `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`). |
| Functions are curried by default   | `Implemented Today`                  | `jazz-next` preserves left-associative whitespace application and lowers multi-argument lambdas into nested unary core lambdas, so ordinary application remains curried by construction (`jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`, `jazz-next/src/JazzNext/Compiler/TypeInference.hs`, `jazz-next/src/JazzNext/Compiler/Runtime.hs`, `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs`, `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`). |
| Pattern matching                   | `Partially Implemented / Parse-Only` | `jazz-next` parses/lowers `case` arms for literal, wildcard, variable, constructor, and bracketed-list patterns; analyzer/type/runtime execution covers literal/wildcard/variable, declared constructor, and exact-length bracketed-list cases (`jazz-next/test/JazzNext/Compiler/Semantics/{AdtPatternTypeSpec.hs,AdtPatternRuntimeSpec.hs}`). Tuple, cons-like list, and lambda-parameter patterns remain staged per `docs/jazz-language-state.md`. |
| Tuples                             | `Partially Implemented / Parse-Only` | Legacy reference: Tuple literals parse/infer, but JS codegen errors on tuples (`docs/jazz-language-state.md`, `jazz-hs/src/CodeGen/Javascript.hs`). No `jazz-next/` codegen equivalent yet.                                    |
| Module/import syntax               | `Partially Implemented / Parse-Only` | `jazz-next` now parses canonical brace-bodied module declarations plus import alias/symbol-list forms, and it supports deterministic module-graph loading diagnostics across parser, resolver, and CLI paths (`jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`, `jazz-next/src/JazzNext/CLI/Main.hs`, `jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs`, `jazz-next/test/JazzNext/Compiler/Modules/{ModuleResolutionSpec.hs,LoaderSpec.hs}`, `jazz-next/test/JazzNext/CLI/CLISpec.hs`). Broader module/file-layout semantics are still being clarified in the spec work. |
| Purity marker (`!`) is enforced    | `Implemented Today`                  | `jazz-next` now enforces stub-v1 purity in analyzer/type pipeline: pure bindings reject direct calls to known `!`-suffixed callees (`jazz-next/src/JazzNext/Compiler/{Purity.hs,Analyzer.hs}`) with regression coverage in `jazz-next/test/JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs`. |
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
