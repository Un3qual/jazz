# Jazz Next Test Layout Design

Date: 2026-03-17

## Goal

Reorganize `jazz-next/test` into subsystem-oriented folders so the active compiler workspace is easier to navigate before additional features land.

## Approved Decisions

1. Scope is limited to `jazz-next/test` plus the runner/docs that reference those paths.
2. `jazz-next/src` stays unchanged for this pass to avoid unnecessary Haskell module churn.
3. Test files keep their existing filenames and `Main` modules; only filesystem layout changes.
4. `jazz-next/test/JazzNext/TestHarness.hs` remains the shared import root for all specs.

## Target Layout

- `jazz-next/test/JazzNext/CLI/`
- `jazz-next/test/JazzNext/Compiler/Config/`
- `jazz-next/test/JazzNext/Compiler/Diagnostics/`
- `jazz-next/test/JazzNext/Compiler/Modules/`
- `jazz-next/test/JazzNext/Compiler/Parser/`
- `jazz-next/test/JazzNext/Compiler/Semantics/`

## Mapping

- CLI:
  - `CLISpec.hs`
- Config:
  - `WarningConfigSpec.hs`
- Diagnostics:
  - `StructuredErrorDiagnosticsSpec.hs`
- Modules:
  - `LoaderSpec.hs`
  - `ModuleResolutionSpec.hs`
  - `PreludeLoadingSpec.hs`
- Parser:
  - `IfExpressionParserSpec.hs`
  - `ModuleImportParserSpec.hs`
  - `OperatorFixitySpec.hs`
  - `OperatorInvalidSyntaxSpec.hs`
  - `OperatorSectionSpec.hs`
  - `ParserFoundationSpec.hs`
- Semantics:
  - `BindingSignatureCoherenceSpec.hs`
  - `BuiltinCatalogSpec.hs`
  - `IfExpressionTypeSpec.hs`
  - `PrimitiveSemanticsSpec.hs`
  - `PuritySemanticsSpec.hs`
  - `RebindingWarningSpec.hs`
  - `RuntimeSemanticsSpec.hs`

## Migration Notes

- The `runghc` include roots stay the same: `jazz-next/src` and `jazz-next/test`.
- Direct test execution continues to work because each spec is still launched by file path.
- Documentation should describe the folder layout rather than the old flat file list where practical.

## Non-Goals

- Renaming `JazzNext.Compiler.*` source modules.
- Changing test semantics, assertions, or execution order.
- Retrofitting historical implementation-plan documents that intentionally capture past file paths.
