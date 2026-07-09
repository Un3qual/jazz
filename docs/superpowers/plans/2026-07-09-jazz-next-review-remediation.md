# Jazz Next Compiler Review Remediation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix the five validated `jazz-next` review findings without changing accepted Jazz syntax or intended language behavior beyond rejecting genuinely ambiguous imports.

**Architecture:** Extend the existing resolver and diagnostic contracts at their current ownership boundaries instead of redesigning the module graph. Encode the parser's lambda invariant in the surface AST, add source ownership to spans before replay flattens modules, and expose the already-existing CLI/spec programs through Cabal.

**Tech Stack:** Haskell 2010, Cabal 3.0 package metadata, `containers`, `filepath`, `text`, the existing `JazzNext.TestHarness`, and repository shell verification scripts.

## Global Constraints

- Modify only the active compiler under `jazz-next/` plus its active documentation; `jazz-hs/` and `jazz2/` remain read-only.
- Breaking changes to the unreleased Haskell API are allowed.
- Preserve accepted Jazz syntax and intended semantics, except that ambiguous unqualified imports must become deterministic E4008 errors.
- Do not add symlink canonicalization to the pure module resolver.
- Follow red-green-refactor for compiler behavior; the approved Cabal configuration change uses command-level red/green evidence instead of a synthetic unit test.
- Run focused tests after each change and commit every independently testable task.

---

### Task 1: Reject every ambiguous unqualified import combination

**Files:**
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs:28-74,604-654`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs:699-730`
- Modify: `docs/spec/modules/04-qualified-imports-and-binding.md:42-85,128-160`

**Interfaces:**
- Consumes: `ParsedImport`, `BindingOrigin`, `parsedModuleExports`, and `parsedModuleClassNames` already owned by `ModuleResolver`.
- Produces: `validateImportSymbols :: Map Text BindingOrigin -> ParsedImport -> Either Diagnostic (Map Text BindingOrigin)` that records all names exposed by non-aliased imports and emits the existing E4008 diagnostic for a second origin.

- [ ] **Step 1: Add failing bare/bare and bare/symbol-list collision tests**

Add both test registrations beside the existing symbol collision case:

```haskell
    ("reports import symbol collisions across bare imports", testReportsBareImportSymbolCollision),
    ("reports import symbol collisions across bare and symbol-list imports", testReportsMixedImportSymbolCollision),
```

Add these tests beside `testReportsImportSymbolCollision`:

```haskell
testReportsBareImportSymbolCollision :: IO ()
testReportsBareImportSymbolCollision = do
  assertCollision "A then B" "import A::Ops.\nimport B::Ops.\nmain = map."
  assertCollision "B then A" "import B::Ops.\nimport A::Ops.\nmain = map."
  where
    assertCollision label importerSource = do
      let result = resolveModuleGraph config (sourceFiles importerSource) ["App", "Main"]
      assertLeftContains (label <> " collision code") "E4008" result
      assertLeftContains (label <> " collision symbol") "symbol 'map'" result
      assertLeftDiagnosticMetadata
        (label <> " collision metadata")
        (Just (SourceSpan 2 1))
        (Just (SourceSpan 1 1))
        (Just "map")
        result

    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles importerSource =
      Map.fromList
        [ ("src/App/Main.jz", importerSource),
          ("src/A/Ops.jz", "map = 1."),
          ("src/B/Ops.jz", "map = 2.")
        ]

testReportsMixedImportSymbolCollision :: IO ()
testReportsMixedImportSymbolCollision = do
  let result = resolveModuleGraph config sourceFiles ["App", "Main"]
  assertLeftContains "mixed collision code" "E4008" result
  assertLeftContains "mixed collision symbol" "symbol 'map'" result
  assertLeftDiagnosticMetadata
    "mixed collision metadata"
    (Just (SourceSpan 2 1))
    (Just (SourceSpan 1 1))
    (Just "map")
    result
  where
    config = ModuleResolutionConfig {moduleRoots = ["src"], moduleExtension = ".jz"}
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import A::Ops.\nimport B::Ops (map).\nmain = map."),
          ("src/A/Ops.jz", "map = 1."),
          ("src/B/Ops.jz", "map = 2.")
        ]
```

- [ ] **Step 2: Run the resolver spec and verify the new cases fail**

Run from the repository root:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
```

Expected: FAIL because both new cases receive `Right` instead of E4008; the existing explicit/explicit collision case remains green.

- [ ] **Step 3: Generalize import-symbol validation**

Replace `validateImportSymbols` with this logic:

```haskell
    validateImportSymbols :: Map Text BindingOrigin -> ParsedImport -> Either Diagnostic (Map Text BindingOrigin)
    validateImportSymbols seenSymbols importDecl =
      case parsedImportAlias importDecl of
        Just _ ->
          Right seenSymbols
        Nothing ->
          case Map.lookup (parsedImportModulePath importDecl) exportsByModule of
            Nothing ->
              Left
                ( mkDiagnostic
                    "E4010"
                    ( "internal resolver error while validating imports for '"
                        <> renderModulePath importerPath
                        <> "': missing exports for module '"
                        <> renderModulePath (parsedImportModulePath importDecl)
                        <> "'"
                    )
                )
            Just exportedSymbols ->
              let exportedClassNames =
                    Map.findWithDefault Set.empty (parsedImportModulePath importDecl) classExportsByModule
                  exportedImportSymbols =
                    Set.union exportedSymbols exportedClassNames
                  importedSymbolNames =
                    case parsedImportSymbols importDecl of
                      Nothing -> Set.toAscList exportedImportSymbols
                      Just explicitSymbolNames -> explicitSymbolNames
               in
                foldM
                  (validateImportSymbol importDecl exportedImportSymbols)
                  seenSymbols
                  importedSymbolNames
```

Alias imports still contribute no unqualified names. Bare imports now insert their complete importable export inventory into the same collision map used by symbol-list imports. `Set.toAscList` makes the first reported collision deterministic when several names overlap.

Replace `validateImportSymbol` as well so a repeated import of the same module
remains an idempotent duplicate rather than a false ambiguity:

```haskell
    validateImportSymbol ::
      ParsedImport ->
      Set Text ->
      Map Text BindingOrigin ->
      Text ->
      Either Diagnostic (Map Text BindingOrigin)
    validateImportSymbol importDecl exportedSymbols seenSymbols symbolName
      | not (Set.member symbolName exportedSymbols) =
          Left (mkMissingImportSymbolError symbolName importDecl exportedSymbols)
      | otherwise =
          case Map.lookup symbolName seenSymbols of
            Just previousOrigin
              | bindingOriginModulePath previousOrigin == parsedImportModulePath importDecl ->
                  Right seenSymbols
              | otherwise ->
                  Left (mkImportSymbolCollisionError symbolName previousOrigin importDecl)
            Nothing ->
              Right
                ( Map.insert
                    symbolName
                    BindingOrigin
                      { bindingOriginModulePath = parsedImportModulePath importDecl,
                        bindingOriginSpan = parsedImportSpan importDecl
                      }
                    seenSymbols
                )
```

- [ ] **Step 4: Run the focused resolver spec**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
```

Expected: PASS, including explicit/explicit, bare/bare in both declaration orders, mixed bare/explicit, and alias isolation.

- [ ] **Step 5: Update the qualified-import specification**

Replace the symbol-list-only E4008 wording with:

```markdown
Every non-aliased import contributes its visible unqualified names to one
import-binding namespace. If imports from different module origins expose the
same name, resolution fails with `E4008`. This includes bare/bare,
bare/symbol-list, and symbol-list/symbol-list collisions. Repeating the same
module import remains idempotent. Declaration order controls only which import
span is primary versus related; it never selects a winning definition.
```

Update the diagnostics table entry to:

```markdown
| `E4008` | non-aliased imports from different modules expose the same unqualified name |
```

Add truth-table rows for `import A::Ops.` plus `import B::Ops.` and for a bare import combined with `import B::Ops (map).`, both resulting in E4008.

- [ ] **Step 6: Commit the import collision fix**

```bash
git add jazz-next/src/JazzNext/Compiler/ModuleResolver.hs jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs docs/spec/modules/04-qualified-imports-and-binding.md
git commit -m "fix: reject ambiguous unqualified imports"
```

---

### Task 2: Normalize lexical module roots before ambiguity checks

**Files:**
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs:40-55,321-354`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs:49,258-266`
- Modify: `docs/spec/modules/01-file-layout-and-package-roots.md:30-50`
- Modify: `docs/spec/modules/02-resolution-algorithm-and-cycles.md:22-38`

**Interfaces:**
- Consumes: `System.FilePath.normalise`, existing `appendRelativePath`, and ordered `moduleRoots`.
- Produces: normalized candidate paths that are deduplicated before source lookup while preserving the first root's order.

- [ ] **Step 1: Add a failing equivalent-root regression**

Register:

```haskell
    ("deduplicates lexically equivalent module roots before ambiguity checks", testDeduplicatesEquivalentRoots),
```

Add beside `testDeduplicatesDuplicateRoots`:

```haskell
testDeduplicatesEquivalentRoots :: IO ()
testDeduplicatesEquivalentRoots =
  assertRight
    "equivalent roots are not treated as ambiguity"
    (resolveModuleGraph config sourceFiles ["App", "Main"])
    (\modules -> assertEqual "resolved modules" expectedModules modules)
  where
    config =
      ModuleResolutionConfig
        { moduleRoots = ["src", "src/."],
          moduleExtension = ".jz"
        }
    sourceFiles =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Util.\nutil."),
          ("src/Lib/Util.jz", "util = 1.")
        ]
    expectedModules =
      [ ResolvedModule ["Lib", "Util"] "src/Lib/Util.jz" [],
        ResolvedModule ["App", "Main"] "src/App/Main.jz" [["Lib", "Util"]]
      ]
```

- [ ] **Step 2: Run the resolver spec and verify E4002**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
```

Expected: FAIL; the new case reports E4002 because `src/Lib/Util.jz` and `src/./Lib/Util.jz` are distinct raw strings.

- [ ] **Step 3: Normalize candidates before deduplication**

Change the import and candidate construction:

```haskell
import System.FilePath
  ( normalise,
    (</>)
  )
```

```haskell
          candidatePaths =
            dedupePreservingOrder
              (map (normalise . appendRelativePath relativePath) (moduleRoots config))
```

Do not call `canonicalizePath`; source lookup remains abstract and lexical.

- [ ] **Step 4: Run the focused resolver spec**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
```

Expected: PASS; genuinely distinct roots still produce E4002 in `testReportsAmbiguousImport`.

- [ ] **Step 5: Document normalization and the symlink boundary**

Add this contract to both module-root/candidate sections:

```markdown
Candidate paths are lexically normalized before deduplication and lookup.
Roots such as `src`, `src/.`, and paths with reducible `..` components therefore
refer to one candidate. The pure resolver does not resolve symlinks or compare
filesystem identities; physically equivalent symlink roots remain distinct.
```

- [ ] **Step 6: Commit the root normalization fix**

```bash
git add jazz-next/src/JazzNext/Compiler/ModuleResolver.hs jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs docs/spec/modules/01-file-layout-and-package-roots.md docs/spec/modules/02-resolution-algorithm-and-cycles.md
git commit -m "fix: normalize module lookup roots"
```

---

### Task 3: Carry source paths through semantic diagnostics

**Files:**
- Modify: `jazz-next/test/JazzNext/Compiler/Diagnostics/StructuredErrorDiagnosticsSpec.hs:5-40`
- Modify: `jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs:52-145,330-420`
- Modify: `jazz-next/src/JazzNext/Compiler/Diagnostics.hs:6-23,34-40,177-180`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleReplay.hs:20-54,129-143`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs:14-25,285-289`
- Modify: `docs/spec/modules/03-loader-behavior-and-diagnostics.md:88-105,135-165`

**Interfaces:**
- Consumes: existing `SourceSpan` fields carried by core `Statement`, `ClassMethodSignature`, and `ImplMethod` nodes.
- Produces: `SourceSpanIn FilePath Int Int`, `qualifySourceSpan :: FilePath -> SourceSpan -> SourceSpan`, and module replay that qualifies all original statement spans before analysis.

- [ ] **Step 1: Add a failing source-qualified rendering test**

Register a second diagnostics test and add:

```haskell
testRenderDiagnosticWithSourceQualifiedSpans :: IO ()
testRenderDiagnosticWithSourceQualifiedSpans = do
  let rendered =
        renderDiagnostic $
          setDiagnosticRelatedSpan
            (SourceSpanIn "src/Lib/Bad.jz" 2 1)
            ( setDiagnosticPrimarySpan
                (SourceSpanIn "src/Lib/Bad.jz" 1 1)
                (mkDiagnostic "E2005" "binding 'x' declared as Int but inferred as Bool")
            )
  assertContains "source-qualified primary span" "src/Lib/Bad.jz:1:1" rendered
  assertContains "source-qualified related span" "related src/Lib/Bad.jz:2:1" rendered
```

- [ ] **Step 2: Run the diagnostics spec and verify it does not compile**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Diagnostics/StructuredErrorDiagnosticsSpec.hs
```

Expected: FAIL at compile time because `SourceSpanIn` does not exist.

- [ ] **Step 3: Extend `SourceSpan` without breaking two-argument construction**

Export `qualifySourceSpan` and replace the span definition/rendering with:

```haskell
data SourceSpan
  = SourceSpan
      { spanLine :: Int,
        spanColumn :: Int
      }
  | SourceSpanIn
      { spanSourcePath :: FilePath,
        spanLine :: Int,
        spanColumn :: Int
      }
  deriving (Eq, Ord, Show)

qualifySourceSpan :: FilePath -> SourceSpan -> SourceSpan
qualifySourceSpan sourcePath spanValue =
  SourceSpanIn sourcePath (spanLine spanValue) (spanColumn spanValue)

renderSourceSpan :: SourceSpan -> Text
renderSourceSpan spanValue =
  renderSourcePath spanValue
    <> Text.pack (show (spanLine spanValue))
    <> ":"
    <> Text.pack (show (spanColumn spanValue))
  where
    renderSourcePath sourceSpan =
      case sourceSpan of
        SourceSpan {} -> ""
        SourceSpanIn sourcePath _ _ -> Text.pack sourcePath <> ":"
```

Add `qualifySourceSpan` to the module export list. Existing `SourceSpan line column` construction and total `spanLine`/`spanColumn` selectors remain valid.

Import `renderSourceSpan` in `Parser/Lexer.hs` and make its compact renderer
delegate to the now-total shared renderer:

```haskell
renderSpanValue :: SourceSpan -> Text
renderSpanValue = renderSourceSpan
```

- [ ] **Step 4: Run the diagnostics spec**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Diagnostics/StructuredErrorDiagnosticsSpec.hs
```

Expected: PASS for both unqualified `2:1` rendering and source-qualified path rendering.

- [ ] **Step 5: Add the failing module-graph semantic provenance test**

Register:

```haskell
    ("compile module graph qualifies semantic diagnostic spans with source paths", testCompileModuleGraphQualifiesSemanticDiagnosticSpans),
```

Add near the other dependency validation tests:

```haskell
testCompileModuleGraphQualifiesSemanticDiagnosticSpans :: IO ()
testCompileModuleGraphQualifiesSemanticDiagnosticSpans = do
  result <-
    compileModuleGraphWithPrelude
      defaultWarningSettings
      Nothing
      resolverConfig
      ["App", "Main"]
      lookupSource
  case compileErrors result of
    [err] -> do
      assertContains "semantic error code" "E2005" (renderDiagnostic err)
      assertContains "semantic primary source" "src/Lib/Bad.jz:1:1" (renderDiagnostic err)
      assertContains "semantic related source" "related src/Lib/Bad.jz:2:1" (renderDiagnostic err)
    _ -> failTest "expected exactly one source-qualified dependency semantic error"
  where
    sourceMap =
      Map.fromList
        [ ("src/App/Main.jz", "import Lib::Bad (x).\nx."),
          ("src/Lib/Bad.jz", "x :: Int.\nx = True.")
        ]
    lookupSource path = pure (Map.lookup path sourceMap)
```

- [ ] **Step 6: Run the loader spec and verify spans still lack paths**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
```

Expected: FAIL because the E2005 diagnostic renders `1:1` and `related 2:1` without `src/Lib/Bad.jz`.

- [ ] **Step 7: Qualify all lowered module statement spans before replay**

Import `qualifySourceSpan`, then change the successful branch of `parseAndLowerResolvedModule`:

```haskell
    Right loweredSource ->
      Right
        ( qualifyExprSourceSpans
            (resolvedSourcePath resolvedModule)
            loweredSource
        )
```

Add the complete traversal below `parseAndLowerResolvedModule`:

```haskell
qualifyExprSourceSpans :: FilePath -> Expr -> Expr
qualifyExprSourceSpans sourcePath expr =
  case expr of
    ELit literal -> ELit literal
    EVar name -> EVar name
    ELambda parameter body -> ELambda parameter (go body)
    EOperatorValue symbol -> EOperatorValue symbol
    EList items -> EList (map go items)
    ETuple items -> ETuple (map go items)
    EApply function argument -> EApply (go function) (go argument)
    ETypeApplication function signatureType -> ETypeApplication (go function) signatureType
    EIf condition trueBranch falseBranch -> EIf (go condition) (go trueBranch) (go falseBranch)
    ECase condition trueBranch falseBranch -> ECase (go condition) (go trueBranch) (go falseBranch)
    EPatternCase scrutinee arms -> EPatternCase (go scrutinee) (map qualifyCaseArm arms)
    EBinary symbol left right -> EBinary symbol (go left) (go right)
    ESectionLeft left symbol -> ESectionLeft (go left) symbol
    ESectionRight symbol right -> ESectionRight symbol (go right)
    EBlock statements -> EBlock (map qualifyStatement statements)
  where
    go = qualifyExprSourceSpans sourcePath
    qualifySpan = qualifySourceSpan sourcePath

    qualifyCaseArm (CaseArm patternValue guardExpr bodyExpr) =
      CaseArm patternValue (fmap go guardExpr) (go bodyExpr)

    qualifyClassMethod (ClassMethodSignature name spanValue payload) =
      ClassMethodSignature name (qualifySpan spanValue) payload

    qualifyImplMethod (ImplMethod name spanValue bodyExpr) =
      ImplMethod name (qualifySpan spanValue) (go bodyExpr)

    qualifyStatement statement =
      case statement of
        SLet name spanValue valueExpr -> SLet name (qualifySpan spanValue) (go valueExpr)
        SSignature name spanValue payload -> SSignature name (qualifySpan spanValue) payload
        SData spanValue name parameters constructors -> SData (qualifySpan spanValue) name parameters constructors
        SClass spanValue name parameters methods ->
          SClass (qualifySpan spanValue) name parameters (map qualifyClassMethod methods)
        SImpl spanValue name arguments methods ->
          SImpl (qualifySpan spanValue) name arguments (map qualifyImplMethod methods)
        SModule spanValue path -> SModule (qualifySpan spanValue) path
        SImport spanValue path alias symbols -> SImport (qualifySpan spanValue) path alias symbols
        SExpr spanValue valueExpr -> SExpr (qualifySpan spanValue) (go valueExpr)
```

- [ ] **Step 8: Run focused diagnostics and loader specs**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Diagnostics/StructuredErrorDiagnosticsSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs
```

Expected: PASS. Standalone parser/analyzer tests continue constructing and comparing ordinary `SourceSpan` values.

- [ ] **Step 9: Document semantic source provenance**

Add to module-graph pipeline step 6 and the diagnostics section:

```markdown
After each resolved source is lowered, every statement-owned span is qualified
with that module's resolved source path before replay combines modules.
Semantic primary and related locations therefore render as
`path:line:column`, including cross-module diagnostics. Standalone-source spans
remain `line:column` because their source ownership is already supplied by the
standalone invocation.
```

- [ ] **Step 10: Commit the diagnostic provenance fix**

```bash
git add jazz-next/src/JazzNext/Compiler/Diagnostics.hs jazz-next/src/JazzNext/Compiler/ModuleReplay.hs jazz-next/src/JazzNext/Compiler/Parser/Lexer.hs jazz-next/test/JazzNext/Compiler/Diagnostics/StructuredErrorDiagnosticsSpec.hs jazz-next/test/JazzNext/Compiler/Modules/LoaderSpec.hs docs/spec/modules/03-loader-behavior-and-diagnostics.md
git commit -m "fix: preserve module diagnostic source paths"
```

---

### Task 4: Encode non-empty surface lambda parameters

**Files:**
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/AST.hs:9-20,80-90`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser.hs:20-70,1912-1987`
- Modify: `jazz-next/src/JazzNext/Compiler/Parser/Lower.hs:5-15,89-112`
- Modify: `jazz-next/src/JazzNext/Compiler/ModuleResolver.hs:15-35,394-410`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs:5-42,49-205`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs`
- Modify: `jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs`

**Interfaces:**
- Consumes: the grammar guarantee that a lambda has a first parameter before `parseLambdaParameters` succeeds.
- Produces: `SELambda (NonEmpty SurfaceLambdaParameter) SurfaceExpr`; zero-parameter Jazz syntax remains the same parser error.

- [ ] **Step 1: Convert test expectations to `NonEmpty` and remove the impossible crash test**

In each test file that constructs `SELambda`, import:

```haskell
import Data.List.NonEmpty (NonEmpty (..))
```

Apply these exact representation changes everywhere:

```haskell
SELambda [parameter] body
-- becomes
SELambda (parameter :| []) body

SELambda [firstParameter, secondParameter] body
-- becomes
SELambda (firstParameter :| [secondParameter]) body
```

Delete `testLowerRejectsImpossibleEmptyLambda`, its test registration, and the now-unused `Control.Exception`, qualified `Data.Text`, `assertContains`, and `failTest` imports from `LambdaParserSpec.hs`. Keep `testRejectsEmptyLambdaParameters`; it locks the unchanged Jazz syntax error for `\() -> ...`.

Use this command as the completeness check:

```bash
rg -n 'SELambda \[' jazz-next/test
```

Expected after the test edit: no test expectation still uses list syntax.

- [ ] **Step 2: Run the lambda parser spec and verify the API mismatch**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs
```

Expected: FAIL at compile time because `SELambda` still accepts `[SurfaceLambdaParameter]`.

- [ ] **Step 3: Change the surface AST and parser to construct `NonEmpty` directly**

Import `NonEmpty (..)` in `Parser/AST.hs` and change:

```haskell
  | SELambda (NonEmpty SurfaceLambdaParameter) SurfaceExpr
```

Import `NonEmpty (..)` in `Parser.hs`, change the signature, and replace the accumulator:

```haskell
parseLambdaParameters :: [Token] -> Either Diagnostic (NonEmpty SurfaceLambdaParameter, [Token])
parseLambdaParameters tokensAfterLeftParen =
  case tokensAfterLeftParen of
    token@(Token {tokenKind = TRParen}) : _ ->
      Left
        ( parseDiagnostic
            ( "expected lambda parameter before ')' at "
                <> renderSourceSpan (tokenSpan token)
            )
        )
    _ -> do
      (firstParameter, afterFirstParameter) <- parseLambdaParameter tokensAfterLeftParen
      go firstParameter [] afterFirstParameter
  where
    go firstParameter revRemainingParameters allTokens =
      case allTokens of
        Token {tokenKind = TComma} : rest -> do
          (nextParameter, afterNextParameter) <- parseLambdaParameter rest
          go firstParameter (nextParameter : revRemainingParameters) afterNextParameter
        Token {tokenKind = TRParen} : rest ->
          Right (firstParameter :| reverse revRemainingParameters, rest)
        [] ->
          Left (parseDiagnostic "expected ')' before end of input in lambda parameter list")
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
```

- [ ] **Step 4: Make lowering total and adapt resolver traversal**

In `Parser/Lower.hs`, import `Data.List.NonEmpty (NonEmpty)` and qualified `Data.List.NonEmpty as NonEmpty`, then replace the partial helper:

```haskell
lowerSurfaceLambda :: NonEmpty SurfaceLambdaParameter -> SurfaceExpr -> Expr
lowerSurfaceLambda parameters bodyExpr =
  foldr
    lowerParameter
    (lowerSurfaceExpr bodyExpr)
    (zip [1 :: Int ..] (NonEmpty.toList parameters))
  where
    lowerParameter (_, SurfaceLambdaIdentifier parameterName) loweredBody =
      ELambda parameterName loweredBody
    lowerParameter (parameterIndex, SurfaceLambdaPattern parameterPattern) loweredBody =
      let generatedName =
            mkIdentifier
              (Text.pack "$lambda_pattern_arg_" <> Text.pack (show parameterIndex))
       in ELambda
            generatedName
            ( EPatternCase
                (EVar generatedName)
                [CaseArm (lowerSurfacePattern parameterPattern) Nothing loweredBody]
            )
```

In `ModuleResolver.hs`, import qualified `Data.List.NonEmpty as NonEmpty` and convert parameters only where the existing list-only `map` calls require it:

```haskell
    SELambda params body ->
      let parameterList = NonEmpty.toList params
       in Set.union
            (Set.unions (map collectLambdaParameterReferences parameterList))
            ( collectExprReferences
                (Set.union boundNames (Set.unions (map collectLambdaParameterBinders parameterList)))
                body
            )
```

Other `SELambda _ body` pattern matches need no change.

- [ ] **Step 5: Run all surface-AST consumers**

Run:

```bash
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
bash jazz-next/scripts/runghc.sh -i./jazz-next/src -i./jazz-next/test jazz-next/test/JazzNext/Compiler/Modules/ModuleResolutionSpec.hs
```

Expected: PASS. `rg -n 'empty lambda parameter list in lowerSurfaceLambda|SELambda \[\]' jazz-next/src jazz-next/test` returns no matches, while the parser rejection for `\()` still passes.

- [ ] **Step 6: Commit the total lowering boundary**

```bash
git add jazz-next/src/JazzNext/Compiler/Parser/AST.hs jazz-next/src/JazzNext/Compiler/Parser.hs jazz-next/src/JazzNext/Compiler/Parser/Lower.hs jazz-next/src/JazzNext/Compiler/ModuleResolver.hs jazz-next/test/JazzNext/Compiler/Parser/LambdaParserSpec.hs jazz-next/test/JazzNext/Compiler/Parser/ExpressionParserSpec.hs jazz-next/test/JazzNext/Compiler/Parser/AdtPatternParserSpec.hs jazz-next/test/JazzNext/Compiler/Parser/OperatorFixitySpec.hs
git commit -m "refactor: make surface lambda parameters non-empty"
```

---

### Task 5: Package the CLI and register the complete Cabal test suite

**Files:**
- Create: `jazz-next/app/Main.hs`
- Modify: `jazz-next/jazz-next.cabal:1-48`
- Modify: `jazz-next/README.md:13-90`

**Interfaces:**
- Consumes: public `JazzNext.CLI.Main.main` and all 29 existing `*Spec.hs` entry points.
- Produces: Cabal executable target `jazz-next` and 29 `exitcode-stdio-1.0` test targets sharing one dependency configuration.

- [ ] **Step 1: Record command-level RED evidence**

Run from `jazz-next/`:

```bash
cabal test all
cabal run jazz-next -- --help
```

Expected before editing: `cabal test all` reports `No tests to run`; `cabal run jazz-next` fails because no executable target exists. This is the approved configuration-file exception to unit-test-first TDD.

- [ ] **Step 2: Add the executable entry point**

Create `jazz-next/app/Main.hs`:

```haskell
module Main (main) where

import qualified JazzNext.CLI.Main as CLI

main :: IO ()
main = CLI.main
```

- [ ] **Step 3: Add Cabal executable and shared test configuration**

Append after the library stanza:

```cabal

executable jazz-next
  hs-source-dirs: app
  main-is: Main.hs
  default-language: Haskell2010
  build-depends:
      base >= 4.17 && < 4.18,
      jazz-next

common test-common
  hs-source-dirs: test
  other-modules:
      JazzNext.Compiler.Parser.TestSupport
      JazzNext.TestHarness
  default-language: Haskell2010
  build-depends:
      base >= 4.17 && < 4.18,
      containers >= 0.6 && < 0.7,
      directory >= 1.3 && < 1.4,
      filepath >= 1.4 && < 1.5,
      jazz-next,
      text >= 2.0 && < 2.1
```

- [ ] **Step 4: Register all 29 current spec entry points**

Append the complete target list:

```cabal
test-suite cli-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/CLI/CLISpec.hs

test-suite warning-config-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Config/WarningConfigSpec.hs

test-suite structured-error-diagnostics-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Diagnostics/StructuredErrorDiagnosticsSpec.hs

test-suite loader-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Modules/LoaderSpec.hs

test-suite module-resolution-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Modules/ModuleResolutionSpec.hs

test-suite prelude-loading-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Modules/PreludeLoadingSpec.hs

test-suite adt-pattern-parser-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Parser/AdtPatternParserSpec.hs

test-suite declaration-parser-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Parser/DeclarationParserSpec.hs

test-suite expression-parser-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Parser/ExpressionParserSpec.hs

test-suite if-expression-parser-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Parser/IfExpressionParserSpec.hs

test-suite lambda-parser-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Parser/LambdaParserSpec.hs

test-suite module-import-parser-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Parser/ModuleImportParserSpec.hs

test-suite operator-fixity-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Parser/OperatorFixitySpec.hs

test-suite operator-invalid-syntax-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Parser/OperatorInvalidSyntaxSpec.hs

test-suite operator-section-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Parser/OperatorSectionSpec.hs

test-suite parser-foundation-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Parser/ParserFoundationSpec.hs

test-suite pattern-parser-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Parser/PatternParserSpec.hs

test-suite token-parser-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Parser/TokenParserSpec.hs

test-suite adt-pattern-runtime-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Semantics/AdtPatternRuntimeSpec.hs

test-suite adt-pattern-type-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Semantics/AdtPatternTypeSpec.hs

test-suite binding-signature-coherence-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Semantics/BindingSignatureCoherenceSpec.hs

test-suite builtin-catalog-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Semantics/BuiltinCatalogSpec.hs

test-suite if-expression-type-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Semantics/IfExpressionTypeSpec.hs

test-suite lambda-semantics-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Semantics/LambdaSemanticsSpec.hs

test-suite primitive-semantics-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Semantics/PrimitiveSemanticsSpec.hs

test-suite purity-semantics-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Semantics/PuritySemanticsSpec.hs

test-suite rebinding-warning-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Semantics/RebindingWarningSpec.hs

test-suite recursive-bindings-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Semantics/RecursiveBindingsSpec.hs

test-suite runtime-semantics-spec
  import: test-common
  type: exitcode-stdio-1.0
  main-is: JazzNext/Compiler/Semantics/RuntimeSemanticsSpec.hs
```

- [ ] **Step 5: Verify Cabal discovers, builds, and runs every target**

Run from `jazz-next/`:

```bash
cabal build all
cabal test all
cabal run jazz-next -- --help
```

Expected: build succeeds; the test summary lists 29 suites and all pass; CLI help begins with the existing Jazz usage text.

- [ ] **Step 6: Replace README run/test commands with packaged commands**

Use repository-root commands in the examples:

```bash
cabal run --project-dir=jazz-next jazz-next -- first.jz
cabal run --project-dir=jazz-next jazz-next -- --run first.jz
printf '40 + 2.' | cabal run --project-dir=jazz-next jazz-next -- --run -
cabal run --project-dir=jazz-next jazz-next -- --help
cabal test --project-dir=jazz-next all
```

Describe `scripts/test-warning-config.sh` as the compatibility/helper runner rather than the only test entry point.

- [ ] **Step 7: Commit the Cabal package integration**

```bash
git add jazz-next/app/Main.hs jazz-next/jazz-next.cabal jazz-next/README.md
git commit -m "build: package jazz-next cli and tests"
```

---

### Task 6: Run complete regression and repository verification

**Files:**
- Verify only; modify a file only if a verification failure exposes a defect caused by Tasks 1-5, then rerun the focused red-green cycle before amending the owning task's commit.

**Interfaces:**
- Consumes: all five fixed contracts and the repository documentation checks.
- Produces: fresh completion evidence with a clean working tree.

- [ ] **Step 1: Run formatting and static diff checks**

Run from the repository root:

```bash
git diff --check HEAD~5..HEAD
```

Then run from `jazz-next/`:

```bash
cabal check
```

Expected: no whitespace errors; Cabal reports no package-description errors.

- [ ] **Step 2: Run the complete Cabal build and test suite**

Run:

```bash
cabal build --project-dir=jazz-next all
cabal test --project-dir=jazz-next all
```

Expected: every library/executable/test component builds and all 29 suites pass.

- [ ] **Step 3: Run the existing compatibility test runner**

Run:

```bash
bash jazz-next/scripts/test-warning-config.sh
```

Expected: all 29 existing spec programs pass through the repository's runghc wrapper.

- [ ] **Step 4: Smoke-test packaged compile and run behavior**

Run:

```bash
printf '40 + 2.' | cabal run --project-dir=jazz-next jazz-next -- --run -
```

Expected stdout ends with `42`; no compile/runtime diagnostic is emitted.

- [ ] **Step 5: Run repository documentation checks**

Run:

```bash
bash scripts/check-docs.sh
bash scripts/check-execution-queue.sh
```

Expected: both checks pass without modifying queue state.

- [ ] **Step 6: Verify scope and commit state**

Run:

```bash
git status --short
git diff --name-only HEAD~5..HEAD
git log --oneline -7
```

Expected: clean status; changed implementation files are under `jazz-next/` and active `docs/`; no file under `jazz-hs/` or `jazz2/` appears; the log contains the design, plan, and focused implementation commits.
