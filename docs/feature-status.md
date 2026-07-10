# Jazz Feature Status Matrix

This is the canonical status matrix for top-level language claims. The top-level [README](../README.md) is a short summary, while this file is the source of truth for implemented-vs-planned status.

Last verified against commit: `9a73026`

## Status Rubric

- `Implemented Today`: Works end-to-end in the active repository behavior (`jazz-next/` parse/analyze/codegen/runtime path).
- `Partially Implemented`: Some active-path behavior works end-to-end, but the broader feature family still has staged forms or unresolved semantics.
- `Partially Implemented / Parse-Only`: Accepted by parser and/or represented in AST, but not fully supported through analyzer/codegen/runtime.
- `Planned / Aspirational`: Project goal, roadmap, or marketing claim not implemented end-to-end today.

## Feature Matrix

| Top-level claim                    | Status                               | Evidence                                                                                                                                                             |
| ---------------------------------- | ------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| ADTs                               | `Partially Implemented`              | `jazz-next` now parses/lowers canonical and generic-parameter `data` declarations, preserves declaration type parameters and bare constructor payload metadata, instantiates direct generic constructor values/applications freshly per use while keeping ordinary constructor aliases monomorphic, registers constructor values for analyzer/type/runtime paths, and typechecks/executes declared constructor, exact-length bracketed-list, cons-like list, fixed-arity tuple, and as-patterns (`jazz-next/src/JazzNext/Compiler/{Parser.hs,AST.hs,Parser/AST.hs,Parser/Lower.hs,Analyzer.hs,TypeInference.hs,Runtime.hs}`; `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs`; `jazz-next/test/JazzNext/Compiler/Semantics/{AdtPatternTypeSpec.hs,AdtPatternRuntimeSpec.hs,BindingSignatureCoherenceSpec.hs}`). |
| Easy to understand syntax          | `Planned / Aspirational`             | This is a project-positioning claim in `README.md`, not an implementation-verified compiler behavior.                                                                |
| Performant / LLVM backend          | `Planned / Aspirational`             | `README.md` says LLVM generation is future work; `docs/jazz-language-state.md` lists backend target as unsettled and LLVM out of active scope.                       |
| Interpreter-backed CLI execution   | `Implemented Today`                  | `jazz-next` compile paths are diagnostic-only on success while CLI `--run`, `runSource`, and `runModuleGraph` execute through the interpreter-backed runtime pipeline (`jazz-next/src/JazzNext/Compiler/{Driver.hs,Runtime.hs}`, `jazz-next/src/JazzNext/CLI/Main.hs`, `jazz-next/test/JazzNext/{CLI/CLISpec.hs,Compiler/Modules/LoaderSpec.hs}`). |
| Strong static typing (core subset) | `Implemented Today`                  | Type inference and checking are part of the active `jazz-next/` pipeline in `jazz-next/src/JazzNext/Compiler/TypeInference.hs` and `jazz-next/src/JazzNext/Compiler/Analyzer.hs`; `docs/jazz-language-state.md` documents end-to-end typed core subset. |
| Type inference (core subset)       | `Implemented Today`                  | `jazz-next/src/JazzNext/Compiler/TypeInference.hs` runs analyzer/type inference; `docs/jazz-language-state.md` confirms core inference works today.                                 |
| Type signatures (monomorphic subset) | `Implemented Today`                | `jazz-next` now carries adjacent monomorphic signatures through structured parser/core payloads for `Int`, `Bool`, nested concrete list types, concrete tuple types, right-associative function types, explicit parenthesized function-type overrides, and empty `@{}:` constrained wrappers in `jazz-next/src/JazzNext/Compiler/{Parser.hs,Parser/AST.hs,AST.hs,Parser/Lower.hs,TypeInference.hs}`; covered by `jazz-next/test/JazzNext/Compiler/{Parser/ParserFoundationSpec.hs,Semantics/BindingSignatureCoherenceSpec.hs}` and `jazz-next/test/JazzNext/CLI/CLISpec.hs`. |
| Immutable bindings                 | `Implemented Today`                  | Active `jazz-next` syntax uses dot-terminated immutable bindings (`name = expr.`) with no mutable assignment form in parser or runtime paths (`jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/AST.hs`, `jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs`). |
| First-class functions              | `Implemented Today`                  | `jazz-next` now parses canonical lambdas with identifier or pattern-shaped parameters, lowers them into executable unary core lambda nodes with internal pattern-case bodies where needed, infers callable function types, and executes lexical closures at runtime (`jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`, `jazz-next/src/JazzNext/Compiler/TypeInference.hs`, `jazz-next/src/JazzNext/Compiler/Runtime.hs`, `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs`, `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`). |
| Functions are curried by default   | `Implemented Today`                  | `jazz-next` preserves left-associative whitespace application and lowers multi-argument lambdas into nested unary core lambdas, so ordinary application remains curried by construction (`jazz-next/src/JazzNext/Compiler/Parser.hs`, `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`, `jazz-next/src/JazzNext/Compiler/TypeInference.hs`, `jazz-next/src/JazzNext/Compiler/Runtime.hs`, `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs`, `jazz-next/test/JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs`). |
| Pattern matching                   | `Partially Implemented`              | `jazz-next` parses/lowers `case` arms and lambda parameters for literal, wildcard, variable, constructor, bracketed-list, cons-like list, tuple, and as-patterns; analyzer/type/runtime execution covers literal/wildcard/variable, declared constructor, exact-length bracketed-list, cons-like list head/tail, fixed-arity tuple, and as-pattern cases (`jazz-next/test/JazzNext/Compiler/Parser/{AdtPatternParserSpec.hs,LambdaParserSpec.hs}`; `jazz-next/test/JazzNext/Compiler/Semantics/{AdtPatternTypeSpec.hs,AdtPatternRuntimeSpec.hs,LambdaSemanticsSpec.hs}`). |
| Tuples                             | `Partially Implemented`             | `jazz-next` now parses/lowers tuple literals, concrete tuple signature types, and fixed-arity tuple case patterns; infers fixed-arity heterogeneous tuple types; checks tuple signatures and tuple pattern arity/type compatibility; and evaluates/renders/matches tuple runtime values (`jazz-next/src/JazzNext/Compiler/{Parser.hs,Parser/AST.hs,AST.hs,Parser/Lower.hs,Analyzer.hs,TypeInference.hs,Runtime.hs}`; `jazz-next/test/JazzNext/Compiler/{Parser/ParserFoundationSpec.hs,Parser/AdtPatternParserSpec.hs,Semantics/BindingSignatureCoherenceSpec.hs,Semantics/RuntimeSemanticsSpec.hs,Semantics/AdtPatternTypeSpec.hs,Semantics/AdtPatternRuntimeSpec.hs}`). Tuple-constructor sugar remains out of scope. |
| Module/import syntax               | `Partially Implemented`              | `jazz-next` parses canonical brace-bodied module declarations with optional explicit export allowlists plus import alias/symbol-list forms into a parse-once `ResolvedProgram`. Omitted lists remain export-all; `()` exports nothing; owned values, constructors, types, and classes are selectable; and unknown or imported-only entries report `E4015`. Resolver dependencies, compiler imports, and runtime publication share the validated public typed inventory while the defining module retains its full local/compiler interface (`jazz-next/src/JazzNext/Compiler/{ModuleExports.hs,ModuleResolver.hs,ModuleInterface.hs,ModuleCompiler.hs,ModuleRuntime.hs}`). Parser, inventory, resolver, pipeline, loader, CLI, and migration coverage lives under `jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs`, `jazz-next/test/JazzNext/Compiler/Modules/{ModuleExportsSpec.hs,ModuleResolutionSpec.hs,LoaderSpec.hs,ModulePipelineContractSpec.hs}`, and `jazz-next/test/JazzNext/CLI/CLISpec.hs`; normative contracts are in `docs/spec/modules/00-module-clarification-matrix.md` through `06-explicit-export-lists.md`. Re-exports, wildcard/hiding forms, package metadata, persistent caches, and additional stdlib/catalog growth remain future work behind separate contracts. |
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
