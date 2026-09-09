# Haskell module boundaries implementation plan

**Goal:** Make the approved long modules easier to navigate through mechanical responsibility-based splits.

**Architecture:** Keep dependency discovery, statement dispatch, and scope walking in their existing owners. Move name resolution, import validation, declaration grammars, instantiation, and environment-variable tracking into focused modules. Keep the mutually recursive runtime engine together.

**Spec:** The user's approved module-boundary review in this task. RFC 0016 and the existing runtime-engine ownership decision remain intact.

**Execution:** Inline in the existing PR worktree, using the executing-plans workflow. No new language behavior, dependencies, service callbacks, or SOURCE imports. Keep existing entry points through direct re-exports where used. Shared declaration-token operations retain their implementations in one leaf module rather than creating parser cycles.

- [x] Extract `ModuleResolver.Names` (resolution context and name traversal) and `ModuleResolver.Imports` (import views and validation). Keep loading, the fused surface inventory walk, and graph ordering in `ModuleResolver.hs`.
- [x] Extract `Parser.CapabilityDeclaration` and `Parser.ModuleDeclaration`, with their existing grammar helpers. Keep shared declaration token recognition/consumption in `Parser.DeclarationTokens`; keep expression callbacks unchanged and statement dispatch in `Parser.Declaration`.
- [x] Extract `TypeInference.Instantiation` from `Scope.hs`, retaining explicit/implicit use-site instantiation and its fact recording together. Keep recursive previews and scope scheduling in `Scope.hs`.
- [x] Extract `TypeInference.Environment` from `Capabilities.hs`, retaining the environment-variable summary and its incremental updates together.
- [x] Register the modules in `jazz.cabal`, narrow imports/exports, format, and compile with development warnings. Verify relocated definitions against the original source and check that the import graph remains acyclic.
- [x] Run focused resolver/parser/inference tests, then all default suites and the CLI build. Commit verified changes and push to PR #152.

Verification commands use the pinned Nix development environment. Focused tests: module-resolution-spec, module-pipeline-contract-spec, declaration-parser-spec, module-import-parser-spec, binding-signature-coherence-spec, and recursive-bindings-spec. Final gate: `cabal build all --enable-tests --enable-benchmarks --jobs=4`, `cabal test all --jobs=4 --test-show-details=failures`, Ormolu, and `git diff --check`.

Progress: All 309 top-level definitions from the four original files are preserved after ignoring whitespace and documentation comments. The internal import graph is acyclic. All six focused suites passed with development warnings enabled. Full verification passed: all enabled components (including CLI and benchmarks), all 60 default suites, Cabal package checks, Ormolu, and Git whitespace checks. String literals in the relocated definitions also match the originals exactly.

Final boundaries: `ModuleResolver.hs` is 945 lines (from 1,944); `Parser/Declaration.hs` is 976 (from 1,965); `TypeInference/Scope.hs` is 2,344 (from 2,503); `TypeInference/Capabilities.hs` is 1,917 (from 1,998). Seven focused modules hold the extracted definitions. Runtime engine ownership, public language behavior, and dependencies are unchanged.
