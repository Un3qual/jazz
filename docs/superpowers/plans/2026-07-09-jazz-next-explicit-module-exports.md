---
id: JN-MODULE-EXPLICIT-EXPORT-LIST-001
status: done
priority: P1
size: M
kind: impl
autonomous_ready: yes
depends_on: []
last_verified: 2026-07-10
completed_on: 2026-07-10
plan_section: "Implementation Batch: Explicit Module Export Lists"
target_paths:
  - jazz-next/src/JazzNext/Compiler/Parser/AST.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs
  - jazz-next/src/JazzNext/Compiler/Parser.hs
  - jazz-next/src/JazzNext/Compiler/Parser/Lower.hs
  - jazz-next/src/JazzNext/Compiler/ModuleGraph.hs
  - jazz-next/src/JazzNext/Compiler/ModuleExports.hs
  - jazz-next/src/JazzNext/Compiler/ModuleResolver.hs
  - jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs
  - jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs
  - jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
  - jazz-next/test/JazzNext/Compiler/Parser/Foundation/ModulesTests.hs
  - jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
  - jazz-next/test/JazzNext/Compiler/Modules/Loader/CapabilitiesTests.hs
  - jazz-next/test/JazzNext/Compiler/Modules/Loader/VisibilityTests.hs
  - docs/spec/modules/00-module-clarification-matrix.md
  - docs/spec/modules/04-qualified-imports-and-binding.md
  - docs/spec/modules/06-explicit-export-lists.md
  - docs/feature-status.md
  - docs/jazz-language-state.md
  - docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md
  - docs/superpowers/specs/2026-07-09-jazz-next-explicit-module-exports-design.md
  - docs/execution/blocker-contracts.md
  - docs/execution/queue.md
  - docs/execution/done-archive.md
verification:
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
  - bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
  - bash jazz-next/scripts/test-warning-config.sh
  - bash scripts/check-execution-queue.sh
  - bash scripts/check-docs.sh
  - git diff --check
deliverable: "Add optional module-header export allowlists across parser, resolver, compiler interface selection, and runtime publication; preserve export-all compatibility; support value, constructor, type, and capability exports; retain private declarations for local compilation and execution; and reject unknown or imported-only exports with deterministic E4015 diagnostics without adding re-exports."
---

# Jazz-Next Explicit Module Export Lists Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add optional module-header export allowlists that define a module's
public typed inventory while preserving full local inference and evaluation.

**Architecture:** Parse an optional name list on `SSModule`, derive separate
local and public `ModuleExportInventory` values in the resolver, and carry the
validated public inventory on `ModuleGraph.ResolvedModule`. Compiler imports
and runtime publication filter the full inferred interface through that public
inventory; no second manifest is stored on `CompiledModule`.

**Tech Stack:** Haskell 2010, Megaparsec token parsing, `containers` `Map`/`Set`,
the active `jazz-next` parse-once module graph, `runghc` suites, Cabal metadata,
and Markdown queue/docs validators.

## Global Constraints

- Modify active compiler code only under `jazz-next/`; `jazz-hs/` and `jazz2/`
  remain read-only.
- Preserve export-all behavior for modules without an explicit list and for
  source-path-derived modules without a declaration.
- Permit `module Foo::Bar () { ... }` as an explicit export-nothing module.
- Module export selectors include value, constructor, type, and capability
  namespaces; existing explicit import selector eligibility remains unchanged.
- Select every same-text typed inventory entry when a module export name is
  listed.
- Keep unlisted declarations available for local resolution, inference, and
  runtime evaluation.
- Keep retained private data metadata compiler-internal and not source-visible.
- Do not add re-exports, wildcard or constructor-group shorthand, body-level
  export declarations, visibility modifiers, cross-module operators, package
  metadata, default methods, superclasses, or effect typing.
- Add only `E4015`; preserve `E4007` through `E4014` subjects, paths, and spans.
- Every task ends with focused verification and an intentional commit.

---

## Implementation Batch: Explicit Module Export Lists

### Task 1: Parse and Lower Module Export Lists

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleGraph.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/Foundation/ModulesTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs`

**Interfaces:**

- Produces: `SSModule SourceSpan [Text] (Maybe [Text])`.
- Produces: `CoreModule.coreModuleDeclaredExports :: Maybe [Text]`.
- Preserves: `Nothing` means export-all; `Just []` means export-nothing.

- [x] **Step 1: Add parser characterization tests for omitted, populated, and empty lists**

Update the existing module AST expectations so omitted lists use `Nothing`,
then add these tests to `ModuleImportParserSpec.hs`:

```haskell
testParsesModuleExportList :: IO ()
testParsesModuleExportList =
  assertEqual
    "module export list surface AST"
    ( Right
        ( SEBlock
            [ SSModule
                (SourceSpan 1 1)
                ["Lib", "Maybe"]
                (Just ["Maybe", "Just", "Nothing", "mapMaybe"]),
              SSLet "mapMaybe" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    ( parseSurfaceProgram
        "module Lib::Maybe (Maybe, Just, Nothing, mapMaybe) {\nmapMaybe = 1.\n}"
    )

testParsesEmptyModuleExportList :: IO ()
testParsesEmptyModuleExportList =
  assertEqual
    "empty module export list"
    ( Right
        ( SEBlock
            [ SSModule (SourceSpan 1 1) ["App", "Internal"] (Just []),
              SSLet "helper" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "module App::Internal () {\nhelper = 1.\n}")

testRejectsDuplicateModuleExport :: IO ()
testRejectsDuplicateModuleExport = do
  assertLeftDiagnosticContains
    "duplicate module export code"
    "E0001"
    (parseSurfaceProgram "module Lib::Value (answer, answer) {\nanswer = 1.\n}")
  assertLeftDiagnosticContains
    "duplicate module export message"
    "duplicate module export 'answer'"
    (parseSurfaceProgram "module Lib::Value (answer, answer) {\nanswer = 1.\n}")
  assertLeftDiagnosticContains
    "duplicate module export span"
    "1:28"
    (parseSurfaceProgram "module Lib::Value (answer, answer) {\nanswer = 1.\n}")

testRejectsTrailingCommaInModuleExportList :: IO ()
testRejectsTrailingCommaInModuleExportList = do
  assertLeftDiagnosticContains
    "trailing module export comma code"
    "E0001"
    (parseSurfaceProgram "module Lib::Value (answer,) {\nanswer = 1.\n}")
  assertLeftDiagnosticContains
    "trailing module export comma message"
    "expected module export name"
    (parseSurfaceProgram "module Lib::Value (answer,) {\nanswer = 1.\n}")

testRejectsUnclosedModuleExportList :: IO ()
testRejectsUnclosedModuleExportList =
  assertLeftDiagnosticContains
    "unclosed module export list"
    "expected ',' or ')'"
    (parseSurfaceProgram "module Lib::Value (answer {\nanswer = 1.\n}")

testRejectsMissingBodyAfterModuleExportList :: IO ()
testRejectsMissingBodyAfterModuleExportList =
  assertLeftDiagnosticContains
    "missing body after module export list"
    "expected '{'"
    (parseSurfaceProgram "module Lib::Value (answer).")
```

Add list entries for the six tests. Update all existing `SSModule` expected
values in this file, `Foundation/ModulesTests.hs`, and `OperatorFixitySpec.hs`
to pass `Nothing` as the fourth constructor argument.

- [x] **Step 2: Run the parser suites and verify the AST change is red**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
```

Expected: compilation fails because `SSModule` still accepts only a span and
module path.

- [x] **Step 3: Extend the surface and core module records**

Change the surface constructor in `Parser/AST.hs` to:

```haskell
  | SSModule SourceSpan [Text] (Maybe [Text])
```

Add this field to `CoreModule` in `ModuleGraph.hs` immediately after
`coreModuleDeclaredPath`:

```haskell
    coreModuleDeclaredExports :: Maybe [Text],
```

Update `Parser.leadingModuleDeclaration` to match:

```haskell
    SSModule spanValue _ _ : _ -> Just spanValue
```

- [x] **Step 4: Parse the optional module export list**

Replace `parseModuleStatementFromTokens` with:

```haskell
parseModuleStatementFromTokens ::
  ModuleBodyParser ->
  [Token] ->
  Either Diagnostic ([SurfaceStatement], [Token])
parseModuleStatementFromTokens parseModuleBody tokens =
  case tokens of
    moduleToken@Token {tokenKind = TModule} : tokensAfterModuleKeyword -> do
      (modulePath, afterModulePath) <- parseModulePath tokensAfterModuleKeyword
      (moduleExports, beforeModuleBody) <-
        case afterModulePath of
          Token {tokenKind = TLParen} : afterLeftParen -> do
            (exportNames, remaining) <- parseModuleExportList afterLeftParen
            pure (Just exportNames, remaining)
          _ -> pure (Nothing, afterModulePath)
      case beforeModuleBody of
        Token {tokenKind = TLBrace} : tokensAfterLeftBrace -> do
          (bodyStatements, remaining) <- parseModuleBody tokensAfterLeftBrace
          pure
            ( SSModule (tokenSpan moduleToken) modulePath moduleExports
                : bodyStatements,
              remaining
            )
        [] ->
          Left
            ( parseDiagnostic
                ( "expected '{' before end of input after module path at "
                    <> renderSourceSpan (tokenSpan moduleToken)
                )
            )
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected '{' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )
    [] -> Left (parseDiagnostic "expected 'module' before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected 'module' at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
```

Add this focused parser beside `parseImportSymbolList`:

```haskell
parseModuleExportList :: [Token] -> Either Diagnostic ([Text], [Token])
parseModuleExportList tokensAfterLeftParen =
  case tokensAfterLeftParen of
    Token {tokenKind = TRParen} : rest -> Right ([], rest)
    _ -> do
      (firstExport, _, afterFirstExport) <- parseModuleExport tokensAfterLeftParen
      go [firstExport] (Set.singleton firstExport) afterFirstExport
  where
    go revExports seenExports allTokens =
      case allTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextExport, exportSpan, afterNextExport) <- parseModuleExport rest
          if Set.member nextExport seenExports
            then
              Left
                ( parseDiagnostic
                    ( "duplicate module export '"
                        <> nextExport
                        <> "' at "
                        <> renderSourceSpan exportSpan
                    )
                )
            else
              go
                (nextExport : revExports)
                (Set.insert nextExport seenExports)
                afterNextExport
        Token {tokenKind = TRParen} : rest -> Right (reverse revExports, rest)
        [] -> Left (parseDiagnostic "expected ')' before end of input in module export list")
        token : _ ->
          Left
            ( parseDiagnostic
                ( "expected ',' or ')' at "
                    <> renderSourceSpan (tokenSpan token)
                    <> ", found '"
                    <> tokenLexeme token
                    <> "'"
                )
            )

parseModuleExport :: [Token] -> Either Diagnostic (Text, SourceSpan, [Token])
parseModuleExport tokens =
  case tokens of
    Token {tokenKind = TIdentifier exportName, tokenSpan = exportSpan} : rest ->
      Right (exportName, exportSpan, rest)
    [] -> Left (parseDiagnostic "expected module export name before end of input")
    token : _ ->
      Left
        ( parseDiagnostic
            ( "expected module export name at "
                <> renderSourceSpan (tokenSpan token)
                <> ", found '"
                <> tokenLexeme token
                <> "'"
            )
        )
```

- [x] **Step 5: Preserve the list through lowering**

In `lowerSurfaceModule`, replace the declaration extraction and validation with
this complete tuple-returning version. This preserves the existing `E4005` and
`E4006` codes and messages while carrying the optional export list:

```haskell
    declarations =
      [ (modulePath, moduleExports)
        | SSModule _ modulePath moduleExports <- statements
      ]

    validateDeclaration =
      case declarations of
        [] -> Right (Nothing, Nothing)
        [(declaredPath, declaredExports)]
          | declaredPath == expectedPath ->
              Right (Just declaredPath, declaredExports)
          | otherwise ->
              Left
                ( mkDiagnostic
                    "E4006"
                    ( "module declaration mismatch at '"
                        <> Text.pack sourcePath
                        <> "': expected '"
                        <> renderModulePath expectedPath
                        <> "', found '"
                        <> renderModulePath declaredPath
                        <> "'"
                    )
                )
        declaredModules ->
          Left
            ( mkDiagnostic
                "E4005"
                ( "multiple module declarations in '"
                    <> Text.pack sourcePath
                    <> "': "
                    <> Text.intercalate ", " (map (renderModulePath . fst) declaredModules)
                )
            )
```

Use it at the top of the function:

```haskell
  (declaredPath, declaredExports) <- validateDeclaration
  pure
    CoreModule
      { coreModuleDeclaredPath = declaredPath,
        coreModuleDeclaredExports = declaredExports,
        coreModuleImports = imports,
        coreModuleExpr = qualifyExprSourceSpans sourcePath loweredBody
      }
```

Update the fallback surface lowering case to ignore the list:

```haskell
    SSModule spanValue modulePath _ ->
      SModule spanValue modulePath
```

- [x] **Step 6: Run focused parser verification**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
```

Expected: all three suites pass, including omitted, populated, empty,
duplicate, and malformed module export headers.

- [x] **Step 7: Commit parser and lowering support**

```bash
git add jazz-next/src/JazzNext/Compiler/Parser/AST.hs jazz-next/src/JazzNext/Compiler/Parser/Declaration.hs jazz-next/src/JazzNext/Compiler/Parser.hs jazz-next/src/JazzNext/Compiler/Parser/Lower.hs jazz-next/src/JazzNext/Compiler/ModuleGraph.hs jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs jazz-next/test/JazzNext/Compiler/Parser/Foundation/ModulesTests.hs jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
git commit -m "feat: parse explicit module export lists"
```

### Task 2: Resolve Local and Public Export Inventories

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/ModuleExports.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleGraph.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs`

**Interfaces:**

- Produces: `declarationExportNames :: ModuleExportInventory -> Set Text`.
- Produces: `resolvedModuleExportInventory :: ModuleExportInventory`.
- Produces: resolver diagnostic `E4015` for unknown or imported-only names.
- Preserves: local resolution uses the full owned inventory.

- [x] **Step 1: Add inventory and resolver tests before production changes**

Add this inventory case to `ModuleExportsSpec.hs`:

```haskell
testDeclarationExportNames :: IO ()
testDeclarationExportNames =
  assertEqual
    "declaration export names include types"
    (Set.fromList ["answer", "Box", "HiddenType", "Eq"])
    (declarationExportNames sampleInventory)
```

Add these resolver cases to `ModuleResolutionSpec.hs`:

```haskell
testExplicitExportsKeepPrivateLocalsUsable :: IO ()
testExplicitExportsKeepPrivateLocalsUsable = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertRight "private local remains resolvable" result (const (pure ()))
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Value (answer).\nanswer.\n}"),
          ("src/Lib/Value.jz", "module Lib::Value (answer) {\nhelper = 1.\nanswer = helper.\n}")
        ]
    lookupSource path = pure (Map.lookup path sources)

testRejectsUnknownModuleExport :: IO ()
testRejectsUnknownModuleExport = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["Lib", "Value"]
  assertLeftDiagnosticCodeAndContains
    "unknown module export"
    "E4015"
    "module export 'missing' is not declared by module 'Lib::Value'"
    result
  assertLeftDiagnosticMetadata
    "unknown module export metadata"
    (Just (SourceSpanIn "src/Lib/Value.jz" 1 1))
    Nothing
    (Just "missing")
    result
  where
    sources = Map.singleton "src/Lib/Value.jz" "module Lib::Value (missing) {\nanswer = 1.\n}"
    lookupSource path = pure (Map.lookup path sources)

testRejectsImportedOnlyModuleExport :: IO ()
testRejectsImportedOnlyModuleExport = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["Lib", "Wrapper"]
  assertLeftDiagnosticCodeAndContains
    "imported-only module export"
    "E4015"
    "module export 'answer' is not declared by module 'Lib::Wrapper'"
    result
  where
    sources =
      Map.fromList
        [ ("src/Lib/Wrapper.jz", "module Lib::Wrapper (answer) {\nimport Lib::Origin (answer).\nwrapper = answer.\n}"),
          ("src/Lib/Origin.jz", "module Lib::Origin {\nanswer = 1.\n}")
        ]
    lookupSource path = pure (Map.lookup path sources)

testExplicitImportRejectsPrivateModuleBinding :: IO ()
testExplicitImportRejectsPrivateModuleBinding = do
  result <- resolveProgram testResolverConfig ResolveKernelOnly Set.empty Set.empty lookupSource ["App", "Main"]
  assertLeftDiagnosticCodeAndContains
    "private explicit import"
    "E4007"
    "import symbol 'helper' is not exported by module 'Lib::Value'"
    result
  where
    sources =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Value (helper).\nhelper.\n}"),
          ("src/Lib/Value.jz", "module Lib::Value (answer) {\nhelper = 1.\nanswer = helper.\n}")
        ]
    lookupSource path = pure (Map.lookup path sources)
```

Register all cases in their suite lists.

- [x] **Step 2: Run the inventory and resolver suites and verify red behavior**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
```

Expected: `ModuleExportsSpec` fails because `declarationExportNames` is absent;
resolver cases fail because every owned declaration is still public.

- [x] **Step 3: Add the module-declaration selector API**

Export and implement in `jazz-next/src/JazzNext/Compiler/ModuleExports.hs`:

```haskell
declarationExportNames :: ModuleExportInventory -> Set Text
declarationExportNames =
  exportNamesInNamespaces
    [ValueNamespace, ConstructorNamespace, TypeNamespace, CapabilityNamespace]
```

Keep `selectorEligibleNames` unchanged so explicit import lists still exclude
type-only names.

- [x] **Step 4: Carry public inventories on resolved modules**

Add to `ModuleGraph.ResolvedModule`:

```haskell
    resolvedModuleExportInventory :: ModuleExportInventory,
```

Import `ModuleExportInventory` from `JazzNext.Compiler.ModuleExports`.

Replace the single parsed inventory with:

```haskell
data ParsedModule = ParsedModule
  { parsedModuleImports :: [ParsedImport],
    parsedModuleLocalInventory :: ModuleExportInventory,
    parsedModulePublicInventory :: ModuleExportInventory,
    parsedModuleReferences :: Set Text,
    parsedModuleQualifiedReferences :: Set (Text, Text),
    parsedModuleCore :: ModuleGraph.CoreModule
  }
```

- [x] **Step 5: Validate explicit export names and derive the public inventory**

Import `find` from `Data.List`, `qualifySourceSpan` from diagnostics,
`declarationExportNames`, and `selectExportNames`. Add:

```haskell
explicitModuleExports :: SurfaceExpr -> Maybe (SourceSpan, [Text])
explicitModuleExports surfaceExpr =
  case surfaceExpr of
    SEBlock statements ->
      case
          [ (spanValue, exportNames)
            | SSModule spanValue _ (Just exportNames) <- statements
          ] of
        [] -> Nothing
        firstExportList : _ -> Just firstExportList
    _ -> Nothing

validatePublicExportInventory ::
  FilePath ->
  [Text] ->
  Maybe (SourceSpan, [Text]) ->
  ModuleExportInventory ->
  Either Diagnostic ModuleExportInventory
validatePublicExportInventory sourcePath modulePath maybeExplicitExports localInventory =
  case maybeExplicitExports of
    Nothing -> Right localInventory
    Just (moduleSpan, exportNames) ->
      case find (`Set.notMember` availableNames) exportNames of
        Nothing -> Right (selectExportNames (Just exportNames) localInventory)
        Just missingName ->
          Left
            ( setDiagnosticSubject missingName
                ( setDiagnosticPrimarySpan
                    (qualifySourceSpan sourcePath moduleSpan)
                    ( mkDiagnostic
                        "E4015"
                        ( "module export '"
                            <> missingName
                            <> "' is not declared by module '"
                            <> renderModulePath modulePath
                            <> "' in '"
                            <> Text.pack sourcePath
                            <> "'; available declarations: "
                            <> renderExportNames availableNames
                        )
                    )
                )
            )
  where
    availableNames = declarationExportNames localInventory

renderExportNames :: Set Text -> Text
renderExportNames names
  | Set.null names = "<none>"
  | otherwise = Text.intercalate ", " (Set.toAscList names)
```

In `parseModuleDetails`, derive and store both inventories:

```haskell
      let localInventory = collectModuleExportInventory surfaceExpr
          topLevelBindings =
            Set.union
              (exportNamesInNamespace ValueNamespace localInventory)
              (exportNamesInNamespace ConstructorNamespace localInventory)
      publicInventory <-
        validatePublicExportInventory
          sourcePath
          expectedModulePath
          (explicitModuleExports surfaceExpr)
          localInventory
      Right
        ParsedModule
          { parsedModuleImports = collectImports surfaceExpr,
            parsedModuleLocalInventory = localInventory,
            parsedModulePublicInventory = publicInventory,
            parsedModuleReferences = collectReferencedNames surfaceExpr Set.\\ topLevelBindings,
            parsedModuleQualifiedReferences = collectQualifiedReferences surfaceExpr,
            parsedModuleCore = coreModule
          }
```

- [x] **Step 6: Route local and dependency resolution through the correct inventories**

In `visitModule`:

- pass `parsedModuleLocalInventory` to local class checks and
  `resolveCoreModuleNames`;
- insert `parsedModulePublicInventory` into
  `resolvedExportInventoriesState`; and
- set `ModuleGraph.resolvedModuleExportInventory` to the public inventory.

The resolved record construction becomes:

```haskell
                              resolvedGraphModule =
                                ModuleGraph.ResolvedModule
                                  { ModuleGraph.resolvedModulePath = modulePath,
                                    ModuleGraph.resolvedSourcePath = sourcePath,
                                    ModuleGraph.resolvedModuleImports = ModuleGraph.coreModuleImports resolvedCore,
                                    ModuleGraph.resolvedModuleExportInventory = parsedModulePublicInventory parsedModule,
                                    ModuleGraph.resolvedModuleCore = resolvedCore
                                  }
```

Do not change import validation's use of
`resolvedExportInventoriesState`; after this change it already contains public
inventories only.

- [x] **Step 7: Run focused resolver verification**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
```

Expected: both suites pass, including local private visibility, export-all
compatibility, `E4015`, and downstream `E4007`.

- [x] **Step 8: Commit resolver publication semantics**

```bash
git add jazz-next/src/JazzNext/Compiler/ModuleExports.hs jazz-next/src/JazzNext/Compiler/ModuleGraph.hs jazz-next/src/JazzNext/Compiler/ModuleResolver.hs jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
git commit -m "feat: resolve explicit module exports"
```

### Task 3: Enforce Public Inventories in Compiler and Runtime

**Files:**

- Modify: `jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/CapabilitiesTests.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/Loader/VisibilityTests.hs`

**Interfaces:**

- Consumes: `ResolvedModule.resolvedModuleExportInventory`.
- Produces: compiler imports filtered first by module publication and then by
  consuming import mode.
- Produces: runtime modules publishing only public value/constructor cells and
  class-attached method cells.

- [x] **Step 1: Add compiled-interface and runtime publication tests**

Add these cases to `ModulePipelineContractSpec.hs`:

```haskell
testCompiledModuleKeepsPrivateInterfaceWithPublicInventory :: IO ()
testCompiledModuleKeepsPrivateInterfaceWithPublicInventory = do
  compiled <- compileFixtureProgram explicitExportSources
  case lookupCompiledModule ["Lib", "Value"] compiled of
    Nothing -> fail "missing compiled Lib::Value module"
    Just compiledModule -> do
      assertEqual
        "full compiled interface"
        (Set.fromList [ModuleExport ValueNamespace "answer", ModuleExport ValueNamespace "helper"])
        (Map.keysSet (interfaceValueTypes (compiledModuleInterface compiledModule)))
      assertEqual
        "public compiled inventory"
        (Set.singleton (ModuleExport ValueNamespace "answer"))
        ( exportInventoryEntries
            (ModuleGraph.resolvedModuleExportInventory (compiledResolvedModule compiledModule))
        )

testRuntimeModulePublishesExplicitExportsOnly :: IO ()
testRuntimeModulePublishesExplicitExportsOnly = do
  compiled <- compileFixtureProgram explicitExportSources
  case evaluateCompiledProgram compiled of
    Left diagnostic -> fail (Text.unpack (renderDiagnostic diagnostic))
    Right runtime ->
      case lookupRuntimeModule ["Lib", "Value"] runtime of
        Nothing -> fail "missing runtime Lib::Value module"
        Just runtimeModule ->
          assertEqual
            "public runtime exports"
            (Set.singleton (ModuleExport ValueNamespace "answer"))
            (Map.keysSet (runtimeModuleExports runtimeModule))

explicitExportSources :: Map.Map FilePath Text
explicitExportSources =
  Map.fromList
    [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Value (answer).\nanswer 41.\n}"),
      ("src/Lib/Value.jz", "module Lib::Value (answer) {\nhelper = \\(x) -> x + 1.\nanswer = \\(x) -> helper x.\n}")
    ]
```

Register both cases. Extend the imports exactly as follows:

```haskell
import JazzNext.Compiler.ModuleExports
  ( ModuleExport (..),
    exportInventoryEntries
  )
import qualified JazzNext.Compiler.ModuleGraph as ModuleGraph
import JazzNext.Compiler.ModuleInterface
  ( CompiledModule (compiledModuleInterface, compiledResolvedModule),
    CompiledProgram (compiledProgramErrors),
    ModuleInterface (interfaceValueTypes),
    emptyCompileInputs,
    lookupCompiledModule
  )
```

- [x] **Step 2: Add loader cases for private closures, aliases, and classes**

Add to `VisibilityTests.hs`:

```haskell
testRunModuleGraphExecutesPublicClosureWithPrivateHelper :: IO ()
testRunModuleGraphExecutesPublicClosureWithPrivateHelper = do
  result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "private helper closure compile errors" [] (runCompileErrors result)
  assertEqual "private helper closure runtime errors" [] (runRuntimeErrors result)
  assertEqual "private helper closure output" (Just "42") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Value (answer).\nanswer 41.\n}"),
          ("src/Lib/Value.jz", "module Lib::Value (answer) {\nhelper = \\(x) -> x + 1.\nanswer = \\(x) -> helper x.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphRejectsPrivateAliasMember :: IO ()
testCompileModuleGraphRejectsPrivateAliasMember = do
  result <- compileModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  case compileErrors result of
    [diagnostic] -> do
      assertContains "private alias code" "E4014" (renderDiagnostic diagnostic)
      assertContains "private alias member" "helper" (renderDiagnostic diagnostic)
    diagnostics -> failTest ("expected one E4014 diagnostic, got " <> Text.pack (show diagnostics))
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Value as Value.\nValue::helper.\n}"),
          ("src/Lib/Value.jz", "module Lib::Value (answer) {\nhelper = 1.\nanswer = helper.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphSupportsOpaqueExportedType :: IO ()
testCompileModuleGraphSupportsOpaqueExportedType = do
  result <- compileModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "opaque exported type compile errors" [] (compileErrors result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Box.\nvalue :: Box.\nvalue = boxed.\nvalue.\n}"),
          ("src/Lib/Box.jz", "module Lib::Box (Box, boxed) {\ndata Box = Pack Int.\nboxed = Pack 1.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphImportsExportedConstructorWithoutTypeName :: IO ()
testRunModuleGraphImportsExportedConstructorWithoutTypeName = do
  result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "constructor-only export compile errors" [] (runCompileErrors result)
  assertEqual "constructor-only export runtime errors" [] (runRuntimeErrors result)
  assertEqual "constructor-only export output" (Just "Pack(1)") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Box (Pack).\nPack 1.\n}"),
          ("src/Lib/Box.jz", "module Lib::Box (Pack) {\ndata Box = Pack Int.\n}")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testRunModuleGraphKeepsPrivateEntryBindingsUsable :: IO ()
testRunModuleGraphKeepsPrivateEntryBindingsUsable = do
  result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "private entry binding compile errors" [] (runCompileErrors result)
  assertEqual "private entry binding runtime errors" [] (runRuntimeErrors result)
  assertEqual "private entry binding output" (Just "41") (runOutput result)
  where
    sourceMap = Map.singleton "src/App/Main.jz" "module App::Main () {\nhelper = 41.\nhelper.\n}"
    lookupSource path = pure (Map.lookup path sourceMap)
```

Add these public/private capability cases to `CapabilitiesTests.hs`:

```haskell
testRunModuleGraphPublishesExplicitlyExportedClass :: IO ()
testRunModuleGraphPublishesExplicitlyExportedClass = do
  result <- runModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  assertEqual "public class compile errors" [] (runCompileErrors result)
  assertEqual "public class runtime errors" [] (runRuntimeErrors result)
  assertEqual "public class runtime output" (Just "True") (runOutput result)
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Facts (Eq).\nEq::equals 1 1.\n}"),
          ( "src/Lib/Facts.jz",
            "module Lib::Facts (Eq) {\nclass Eq(a) {\nequals :: a -> a -> Bool.\n}.\nclass Hidden(a) { }.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)

testCompileModuleGraphRejectsPrivateExplicitClassImport :: IO ()
testCompileModuleGraphRejectsPrivateExplicitClassImport = do
  result <- compileModuleGraphWithPrelude defaultWarningSettings Nothing resolverConfig ["App", "Main"] lookupSource
  case compileErrors result of
    [diagnostic] -> do
      assertContains "private class import code" "E4007" (renderDiagnostic diagnostic)
      assertContains "private class import name" "Hidden" (renderDiagnostic diagnostic)
    diagnostics -> failTest ("expected one E4007 diagnostic, got " <> Text.pack (show diagnostics))
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "module App::Main {\nimport Lib::Facts (Hidden).\n0.\n}"),
          ( "src/Lib/Facts.jz",
            "module Lib::Facts (Eq) {\nclass Eq(a) {\nequals :: a -> a -> Bool.\n}.\nclass Hidden(a) { }.\nimpl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n}"
          )
        ]
    lookupSource path = pure (Map.lookup path sourceMap)
```

Register all seven loader cases in their suite lists.

- [x] **Step 3: Run the pipeline and loader tests and verify publication is red**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
```

Expected: the full interface test passes, but private runtime cells and private
compiler imports still leak because consumers use the full interface inventory.

- [x] **Step 4: Filter compiler imports through the resolved public inventory**

Import `ModuleExportInventory` in
`jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs`. Change the selection
function to:

```haskell
importSelectedInterface ::
  ResolvedNameOrigin ->
  Maybe Text ->
  Maybe [Text] ->
  ModuleExportInventory ->
  ModuleInterface ->
  ImportedInterface
importSelectedInterface origin maybeAlias maybeSymbols publicInventory moduleInterface =
  ImportedInterface
    { importedTypes = selectedTypes,
      importedDataTypes = retainedDataTypes,
      importedCapabilities = selectedCapabilities,
      importedClassNames = selectedClassNames
    }
  where
    importMode = maybe UnqualifiedImport (const QualifiedAliasImport) maybeAlias
    selectedInventory = visibleImportInventory importMode maybeSymbols publicInventory
```

Leave the bodies of `selectedValueTypes`, `selectedClassNames`,
`selectedClassFacts`, and `selectedCapabilities` byte-for-byte unchanged. The
only selection-body change is that `selectedInventory` now calls
`visibleImportInventory importMode maybeSymbols publicInventory`; retained
`interfaceDataTypes` remains unfiltered compiler metadata.

Update callers exactly as follows:

```haskell
importWholeInterface origin moduleInterface =
  importSelectedInterface
    origin
    Nothing
    Nothing
    (moduleInterfaceExportInventory moduleInterface)
    moduleInterface

dependencyImportInterface importDecl compiledModule =
  importSelectedInterface
    (ImportedModule (resolvedImportPath importDecl))
    (resolvedImportAlias importDecl)
    (resolvedImportSymbols importDecl)
    (resolvedModuleExportInventory (compiledResolvedModule compiledModule))
    (compiledModuleInterface compiledModule)
```

- [x] **Step 5: Publish runtime cells through the same public inventory**

Change runtime publication signatures to:

```haskell
publishEnvironment ::
  ResolvedNameOrigin ->
  ModuleExportInventory ->
  ModuleInterface ->
  RuntimeEnv ->
  RuntimeEnv

publishExports ::
  ResolvedNameOrigin ->
  ModuleExportInventory ->
  ModuleInterface ->
  RuntimeEnv ->
  Map ModuleExport RuntimeCell

interfaceExports :: ModuleExportInventory -> ModuleInterface -> [ModuleExport]
```

Implement `interfaceExports` as:

```haskell
interfaceExports publicInventory moduleInterface =
  [ export
    | export <- Set.toList (exportInventoryEntries publicInventory),
      moduleExportNamespace export `elem` [ValueNamespace, ConstructorNamespace]
  ]
    <> [ ModuleExport ValueNamespace methodKey
         | methodKey <- Map.keys (interfaceClassMethods moduleInterface),
           Just (className, _) <- [splitQualifiedMethodKey methodKey],
           Set.member className publicClassNames
       ]
  where
    publicClassNames =
      exportNamesInNamespace CapabilityNamespace publicInventory
```

For the prelude, pass `moduleInterfaceExportInventory preludeInterface`. For a
compiled module, pass
`resolvedModuleExportInventory (compiledResolvedModule compiledModule)`.

Change `runtimeExportSelected` to accept the dependency public inventory and
derive its selected inventory from that argument rather than from
`moduleInterfaceExportInventory moduleInterface`:

```haskell
runtimeExportSelected ::
  ResolvedImport ->
  ModuleExportInventory ->
  ModuleExport ->
  Bool
```

The interface parameter is no longer needed. In `importRuntimeModule`, obtain
the public inventory from `compiledDependency` and pass it to every selection
check.

- [x] **Step 6: Run focused compiler/runtime verification**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
```

Expected: both suites pass, including private closure capture, runtime
publication, alias rejection, opaque type support, constructor-only selection,
and class/impl payload filtering.

- [x] **Step 7: Commit compiler and runtime enforcement**

```bash
git add jazz-next/src/JazzNext/Compiler/ModuleCompiler.hs jazz-next/src/JazzNext/Compiler/ModuleRuntime.hs jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs jazz-next/test/JazzNext/Compiler/Modules/Loader/CapabilitiesTests.hs jazz-next/test/JazzNext/Compiler/Modules/Loader/VisibilityTests.hs
git commit -m "feat: enforce explicit module exports"
```

### Task 4: Align the Contract and Close the Queue Child

**Files:**

- Modify: `docs/spec/modules/06-explicit-export-lists.md`
- Modify: `docs/spec/modules/00-module-clarification-matrix.md`
- Modify: `docs/spec/modules/04-qualified-imports-and-binding.md`
- Modify: `docs/feature-status.md`
- Modify: `docs/jazz-language-state.md`
- Modify: `docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md`
- Modify: `docs/superpowers/specs/2026-07-09-jazz-next-explicit-module-exports-design.md`
- Modify: `docs/superpowers/plans/2026-07-09-jazz-next-explicit-module-exports.md`
- Modify: `docs/execution/blocker-contracts.md`
- Modify: `docs/execution/queue.md`
- Modify: `docs/execution/done-archive.md`

**Interfaces:**

- Produces: normative explicit-export syntax, namespace, diagnostic, and
  compatibility contract.
- Produces: completed child metadata and a terminal-empty queue unless another
  separately accepted candidate exists.

- [x] **Step 1: Activate and link the normative export-list contract**

After implementation behavior passes, change the status in
`06-explicit-export-lists.md` from:

```markdown
Status: planned; implementation pending
```

to:

```markdown
Status: active explicit module export contract
```

Keep every approved syntax, namespace, ownership, visibility, diagnostic, and
non-goal rule already recorded in that file. Add the spec to the module
clarification matrix and link it from the qualified-import contract.

- [x] **Step 2: Refresh architecture and feature evidence**

Update `docs/feature-status.md` and `docs/jazz-language-state.md` to state:

```markdown
Active `jazz-next` module headers now accept optional explicit export
allowlists. Resolver dependencies, compiled imports, and runtime publication
share the validated public typed inventory while unlisted declarations remain
available inside the defining module.
```

Mark the explicit-export follow-up complete in the runtime/module umbrella and
change the design status to:

```markdown
Implemented and verified on `2026-07-10`.
```

- [x] **Step 3: Close execution metadata atomically**

After product and docs verification passes:

1. Change this plan to `status: done`, add `completed_on: 2026-07-10`, and mark
   every checkbox complete.
2. Remove `JN-MODULE-EXPLICIT-EXPORT-LIST-001` from `Ready Now`.
3. Add a `done-archive.md` row with exact focused and full verification.
4. Restore `JN-MODULE-REBASE-PLAN-001` to `Candidate child: none currently`
   and record explicit export lists as landed evidence.
5. Restore the terminal-empty queue status unless a separately accepted child
   appeared during execution.
6. Do not promote re-exports, wildcard shorthand, package work, cross-module
   operators, default methods, superclasses, or effects from this child.

- [x] **Step 4: Run the complete verification ladder**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ModuleImportParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ParserFoundationSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleExportsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModulePipelineContractSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
bash jazz-next/scripts/test-warning-config.sh
bash scripts/check-execution-queue.sh
bash scripts/check-docs.sh
git diff --check
```

Expected: every focused suite, full compatibility harness, queue/docs gate, and
whitespace check passes.

- [x] **Step 5: Review scope before closeout**

Run:

```bash
git diff --stat "$(git merge-base main HEAD)"..HEAD
rg -n 're-export|\.\.\)|public |private |operator export|superclass|default method' jazz-next/src/JazzNext/Compiler jazz-next/test/JazzNext/Compiler
git diff --check
```

Expected: changes stay on the declared parser/module/test/docs surfaces; the
scope scan finds only tests or comments proving excluded behavior, not an
implementation of excluded syntax.

- [x] **Step 6: Commit verified contract closeout**

```bash
git add docs/spec/modules/00-module-clarification-matrix.md docs/spec/modules/04-qualified-imports-and-binding.md docs/spec/modules/06-explicit-export-lists.md docs/feature-status.md docs/jazz-language-state.md docs/plans/2026-03-18-jazz-next-runtime-architecture-and-interpreter-execution-plan.md docs/superpowers/specs/2026-07-09-jazz-next-explicit-module-exports-design.md docs/superpowers/plans/2026-07-09-jazz-next-explicit-module-exports.md docs/execution/blocker-contracts.md docs/execution/queue.md docs/execution/done-archive.md
git commit -m "docs: close explicit module exports batch"
```
