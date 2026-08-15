# Jazz Haskell typeclass audit M2: structural forcing implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan.

**Goal:** Replace the hand-maintained structural recursion in `Jazz.Compiler.Force` with declaration-local `NFData` contracts while retaining every intentional phase and runtime strictness boundary.

**Architecture:** Pure compiler artifacts derive `Generic` and `NFData` next to their declarations. `Jazz.Compiler.Force` remains a narrow phase facade over `rnf`; the runtime-output path remains a bespoke render-only force. Migration proceeds leaf-first so missing coverage is a compile error.

**Tech Stack:** Haskell 2021, GHC 9.14.1, `deepseq-1.5`, Cabal, Nix development shell.

## Global constraints

- Follow `.codex/plans/2026-08-15-jazz-haskell-typeclass-audit-design.md`.
- Do not derive `NFData` for runtime closures, environments, cells, host effects, or semantic evaluator state.
- Do not replace inference's intentionally shallow pre-finalization ownership forcing.
- Preserve `forceRuntimeProgramOutputResult` as render-only rather than full runtime-value forcing.
- Preserve the existing named phase entry points used by profiling and stage-timing tests.
- Use explicit deriving strategies; do not rely on ambiguous newtype deriving defaults.
- Run the poisoned-thunk profiling tests after every structural family.
- Format and commit each green family.

---

## Task 1: Dependency and structural leaves

**Files:**

- Modify: `jazz.cabal`
- Modify: `src/Jazz/Compiler/Purity.hs`
- Modify: `src/Jazz/Compiler/FractionalLiteral.hs`
- Modify: `src/Jazz/Compiler/Name.hs`
- Modify: `src/Jazz/Compiler/DiagnosticCatalog.hs`
- Modify: `src/Jazz/Compiler/Diagnostics.hs`
- Modify: `src/Jazz/Compiler/RuntimeHints.hs`
- Modify: `src/Jazz/Compiler/BuiltinCatalog.hs`
- Modify: `src/Jazz/Compiler/ModuleExports.hs`

- [x] Run `profiling-spec` before edits and preserve its output as the characterization baseline.
- [x] Add `deepseq >= 1.5 && < 1.6` to the private `jazz-internal` library dependency list, not just test or benchmark components.
- [x] Enable `DeriveAnyClass`, `DeriveGeneric`, and `DerivingStrategies` only in modules that need them. Import `Control.DeepSeq (NFData)` and `GHC.Generics (Generic)` explicitly.
- [x] Derive stock `Generic` and anyclass `NFData` for `Purity` and `FractionalLiteralSource`.
- [x] Derive them for name leaves: `Identifier`, `NameNamespace`, `ResolvedNameOrigin`, `GeneratedNameKind`, and `Name`.
- [x] Derive them for diagnostic leaves: `ErrorCode`, `WarningCategory`, `DiagnosticSeverity`, and `DiagnosticCode` in `DiagnosticCatalog.hs`, then `SourceSpan`, `DiagnosticOrigin`, `DiagnosticLabel`, and `Diagnostic` in `Diagnostics.hs`. Do not migrate `DiagnosticSubsystem`, `DiagnosticMetadata`, or ownership enumeration types.
- [x] Derive them for `BindingRuntimeHintKey`, `BuiltinResolutionMode`, `BuiltinSymbol`, `LocatedModuleExportName`, `ModuleTypeConstructorSelector`, `ModuleExportSelector`, `ModuleExport`, and `ModuleExportInventory`. Do not migrate `ModuleImportMode`.
- [x] Run a development library build and `profiling-spec`; format and commit as `refactor: derive NFData for compiler leaves`.

## Task 2: Canonical, module, and inference structures

**Files:**

- Modify: `src/Jazz/Compiler/AST.hs`
- Modify: `src/Jazz/Compiler/ModuleGraph.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Types.hs`
- Modify: `src/Jazz/Compiler/ModuleInterface.hs`
- Modify: `src/Jazz/Compiler/TypeInference/Result.hs`

- [x] Derive `Generic` and `NFData` for every canonical structure reachable from `Expr`: `Literal`, `Pattern`, `CaseArm`, `DataConstructor`, `Expr`, `SignaturePayload`, `SignatureConstraint`, `NumericType`, `SignatureType`, `SignatureToken`, `ClassMethodSignature`, `ImplMethod`, and `Statement`.
- [x] Derive them for every `ModuleGraph` product: `DeclaredModuleExports`, `CoreModule`, `ResolvedImport`, `ResolvedModule`, and `ResolvedProgram`.
- [x] Derive them for every inference structure reachable from `InferenceResult`: `ExpressionType`, `ConstructorArgumentType`, `IntegerLiteralRange`, `NumericConstraint`, `TypeBinding`, `TypeScheme`, `TypeSchemePrimitiveConstraint`, `TypeSchemeConstraint`, `DataTypeBinding`, `ClassMethodType`, `ImplMethodType`, and `ScopeCapabilityFacts`.
- [x] Derive them for `ModuleInterface`, `CompiledPrelude`, `CompiledModule`, and `CompiledProgram`; intentionally leave `CompileInputs` unchanged.
- [x] Derive them for `InferenceResult` in `TypeInference/Result.hs`.
- [x] Build with development warnings, run inference/module/profiling strictness suites, format, and commit as `refactor: derive NFData for compiler products`.

## Task 3: Typed Core contract

**Files:**

- Modify: `src/Jazz/Compiler/TypedCore.hs`
- Modify: `test/Jazz/Compiler/ProfilingSpec.hs`

- [ ] Add one compile-time test expression in the profiling strictness test module that requires `NFData TypedProgram`; confirm it fails before implementation.
- [ ] Derive `Generic` and `NFData` for every Typed Core declaration from `TypedTypeParameterId` through `TypedProgram`, inclusive. Keep validation result and validation outcome types outside this contract.
- [ ] Build the library, run `jazz-typed-core-contract-spec`, `jazz-typed-core-expression-direct-call-spec`, and profiling strictness tests.
- [ ] Format and commit as `refactor: derive NFData for typed core`.

## Task 4: Lowered IR contract

**Files:**

- Modify: `src/Jazz/Compiler/LoweredIR.hs`
- Modify: `test/Jazz/Compiler/ProfilingSpec.hs`

- [ ] Add one compile-time test expression in the profiling strictness test module that requires `NFData LoweredProgram`; confirm it fails before implementation.
- [ ] Derive `Generic` and `NFData` for every Lowered IR declaration from `LoweredIRVersion` through `LoweredProgram`, inclusive. Keep lowering validation types outside this contract.
- [ ] Build the library, run `jazz-lowered-ir-contract-spec` and profiling strictness tests.
- [ ] Format and commit as `refactor: derive NFData for lowered IR`.

## Task 5: Collapse the forcing facade

**Files:**

- Modify: `src/Jazz/Compiler/Diagnostics/Strictness.hs`
- Rewrite: `src/Jazz/Compiler/Force.hs`
- Test: `test/Jazz/Compiler/ProfilingSpec.hs` through `profiling-spec`

- [ ] Add a focused poison test proving `forceDiagnostic` reaches nested spans and labels; confirm the existing manual implementation is the characterization baseline.
- [ ] Rewrite `forceDiagnostic = rnf` while retaining the named diagnostic ownership boundary.
- [ ] Confirm with `rg` that `forceCompiledModules`, `forceExpr`, `forceListWith`, `forceSurfaceExpr`, and `forceTokens` have no consumer outside `Force.hs`; remove those unused exports and implementations.
- [ ] Retain these pure facade functions and implement each with `rnf`: `forceCompiledProgram`, `forceCompiledModule`, `forceCompiledProgramResult`, `forceInferenceResult`, `forceLoweredProgram`, `forceResolvedModule`, and `forceTypedProgram`.
- [ ] Retain `forceRuntimeProgramOutputResult` as the existing custom result traversal that forces only rendered output. Do not require `NFData RuntimeValue`.
- [ ] Remove all now-obsolete structural helpers, manual constructor matches, and pattern-synonym imports from `Force.hs`.
- [ ] Run the complete profiling strictness suite. Specifically verify both directions: deep pure compiler metadata is forced, while unused runtime exports and unrendered partial-constructor arguments remain unforced.
- [ ] Run a development build, format both modules, and commit as `refactor: replace manual compiler forcing with NFData`.

## Task 6: M2 closeout

- [ ] Search `src` for manual recursive force helpers and confirm only intentionally selective/runtime forcing remains.
- [ ] Audit the final diff for rejected speculative instances: no new `Functor`, `Foldable`, `Traversable`, `Applicative`, `Monad`, `Read`, `Ix`, `Bits`, numeric, generic Aeson, or exception instances; no `NFData` instance for runtime values or evaluator state.
- [ ] Run `git diff --name-only a7e1cf61 -- '*.hs' | xargs bash scripts/check-haskell-format.sh`.
- [ ] Run `nix --extra-experimental-features 'nix-command flakes' develop --command cabal build all -fdevelopment`.
- [ ] Run `nix --extra-experimental-features 'nix-command flakes' develop --command cabal test all -fdevelopment --test-show-details=direct --test-option=--sequential`.
- [ ] Run `nix --extra-experimental-features 'nix-command flakes' develop --command cabal check`, `git diff --check a7e1cf61`, and `git status --short`.
- [ ] Record exact verification results below and commit the evidence update as `docs: record NFData migration verification`.

## Verification evidence

Execution appends command outcomes here before the milestone is declared complete.
