{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CorePhase (Lowered, Resolved),
    Expr,
    Literal (..),
  )
import Jazz.Compiler.Analyzer
  ( AnalysisResult (..),
  )
import qualified Jazz.Compiler.Analyzer as Analyzer
import Jazz.Compiler.BundledPrelude
  ( bundledPreludeSource,
  )
import Jazz.Compiler.DiagnosticCatalog
  ( DiagnosticSeverity (..),
    WarningCategory (..),
    diagnosticCodeText,
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    diagnosticCode,
    diagnosticPrimarySpan,
    diagnosticRelatedSpan,
    diagnosticSeverity,
    diagnosticSubject,
    diagnosticWarningCategory,
    sourceSpanStart,
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Driver
  ( CompileResult (..),
    compileErrors,
    compileExpr,
    compileSource,
    compileSourceWithPrelude,
    compileWarnings,
    runCompileErrors,
    runDiagnostics,
    runRuntimeErrors,
    runSource,
    runWarnings,
  )
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleResolver (resolveStandaloneExprNames)
import Jazz.Compiler.Parser.Lower (reindexLoweredExpr)
import Jazz.Compiler.TypeRepresentation (SignatureType (..))
import Jazz.Compiler.WarningConfig
  ( WarningSettings,
    defaultWarningSettings,
    resolveWarningSettings,
  )
import Jazz.TestCore
  ( loweredApply,
    loweredBlock,
    loweredConstructorAt,
    loweredData,
    loweredExpression,
    loweredImpl,
    loweredImplMethod,
    loweredLambda,
    loweredLet,
    loweredLiteral,
    loweredVariable,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "RebindingWarning" tests

tests :: [NamedTest]
tests =
  [ ("disabled warning category emits nothing", testDisabledCategoryEmitsNoWarnings),
    ("enabled warning emits one same-scope rebinding warning", testEnabledCategoryEmitsWarning),
    ("repeated same-scope rebinding order is deterministic", testDeterministicWarningOrder),
    ("deep application errors retain left-to-right order", testApplicationDiagnosticOrder),
    ("constructor rebinding emits same-scope warning", testConstructorRebindingEmitsWarning),
    ("nested scope shadowing does not emit same-scope warning", testNestedScopeShadowingNoWarning),
    ("disabled outer-scope shadowing emits nothing", testDisabledOuterScopeShadowingEmitsNoWarnings),
    ("enabled outer-scope shadowing emits nested-let warning", testNestedLetShadowingEmitsWarning),
    ("enabled outer-scope shadowing emits lambda-parameter warning", testLambdaParameterShadowingEmitsWarning),
    ("outer-scope shadowing uses expression statement span for lambda warning", testLambdaExpressionShadowingUsesStatementSpan),
    ("outer-scope shadowing promotion reports compile errors", testPromotedOuterScopeShadowingReportsCompileErrors),
    ("outer-scope shadowing ignores same-scope rebinding", testOuterScopeShadowingIgnoresSameScopeRebinding),
    ("disabled unused-binding emits nothing", testDisabledUnusedBindingEmitsNoWarnings),
    ("enabled unused-binding emits ordinary-let warning", testUnusedBindingEmitsWarning),
    ("used ordinary let emits no unused-binding warning", testUsedOrdinaryLetEmitsNoWarning),
    ("impl method body reference counts as binding use", testImplMethodBodyReferenceCountsAsUse),
    ("pre-declaration references do not count as unused-binding use", testPreDeclarationReferenceDoesNotCountAsUse),
    ("same-name rebinding does not count later use for earlier binding", testSameNameRebindingKeepsEarlierBindingUnused),
    ("self-referential right hand side does not count as unused-binding use", testSelfReferentialRhsStillUnused),
    ("forward-referencing recursive peers count as binding uses under promotion", testRecursiveForwardReferenceCountsAsUseUnderPromotion),
    ("unused-binding suppresses rebinding-site duplicate when W0001 also emits", testUnusedBindingSuppressesRebindingSiteDuplicate),
    ("unused-binding suppresses constructor-rebinding duplicate when W0001 also emits", testUnusedBindingSuppressesConstructorRebindingSiteDuplicate),
    ("unused-binding promotion reports compile errors", testPromotedUnusedBindingReportsCompileErrors),
    ("bundled default prelude aliases do not trigger same-scope rebinding", testBundledPreludeAliasShadowingNoWarning),
    ("source references count as uses of explicit prelude bindings", testExplicitPreludeExternalUses),
    ("explicit prelude constructor rebinding retains both declaration spans", testExplicitPreludeConstructorRebinding),
    ("explicit prelude text matching bundled source still emits rebinding warnings", testExplicitPreludeMatchingBundledSourceEmitsWarning),
    ("driver keeps warning-only success diagnostics", testDriverKeepsWarningOnlySuccessDiagnosticOnly),
    ("driver stores native compile failures in one diagnostic stream", testDriverStoresNativeCompileFailure),
    ("driver reports promoted warnings as compile errors", testDriverReportsPromotedWarningsAsCompileErrors),
    ("driver orders compile warnings before runtime failures", testDriverOrdersCompileWarningsBeforeRuntimeFailures)
  ]

testDisabledCategoryEmitsNoWarnings :: IO ()
testDisabledCategoryEmitsNoWarnings = do
  warnings <- analyzeRebindingWarnings defaultWarningSettings sampleProgram
  assertEqual "warning count" 0 (length warnings)

testEnabledCategoryEmitsWarning :: IO ()
testEnabledCategoryEmitsWarning = do
  settings <- enabledSettings
  warnings <- analyzeRebindingWarnings settings sampleProgram
  case warnings of
    [warning] -> do
      assertEqual "warning severity" SeverityWarning (diagnosticSeverity warning)
      assertEqual "warning category" (Just SameScopeRebinding) (diagnosticWarningCategory warning)
      assertEqual "warning code" "W0001" (diagnosticCodeText (diagnosticCode warning))
      assertEqual "warning variable" (Just "x") (diagnosticSubject warning)
      assertEqual "warning span" (Just (SourceSpan 2 1)) (diagnosticPrimarySpan warning)
      assertEqual "previous span" (Just (SourceSpan 1 1)) (diagnosticRelatedSpan warning)
    _ -> failTest "expected exactly one warning diagnostic"

testDeterministicWarningOrder :: IO ()
testDeterministicWarningOrder = do
  settings <- enabledSettings
  warnings <- analyzeRebindingWarnings settings repeatedProgram
  case warnings of
    [firstWarning, secondWarning] -> do
      assertEqual "first warning span" (Just (SourceSpan 2 1)) (diagnosticPrimarySpan firstWarning)
      assertEqual "first previous span" (Just (SourceSpan 1 1)) (diagnosticRelatedSpan firstWarning)
      assertEqual "second warning span" (Just (SourceSpan 3 1)) (diagnosticPrimarySpan secondWarning)
      assertEqual "second previous span" (Just (SourceSpan 2 1)) (diagnosticRelatedSpan secondWarning)
    _ -> failTest "expected exactly two warning records"

testApplicationDiagnosticOrder :: IO ()
testApplicationDiagnosticOrder = do
  result <-
    analyzeProgram
      defaultWarningSettings
      (foldl1 loweredApply [loweredVariable "missing0", loweredVariable "missing1", loweredVariable "missing2", loweredVariable "missing3"])
  assertEqual
    "application diagnostic subjects"
    [Just "missing0", Just "missing1", Just "missing2", Just "missing3"]
    (map diagnosticSubject (analysisDiagnostics result))

testConstructorRebindingEmitsWarning :: IO ()
testConstructorRebindingEmitsWarning = do
  settings <- enabledSettings
  warnings <- analyzeRebindingWarnings settings constructorRebindingProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" (Just SameScopeRebinding) (diagnosticWarningCategory warning)
      assertEqual "warning code" "W0001" (diagnosticCodeText (diagnosticCode warning))
      assertEqual "warning variable" (Just "Nothing") (diagnosticSubject warning)
      assertEqual "warning span" (Just (SourceSpan 2 1)) (diagnosticPrimarySpan warning)
      assertEqual "previous span" (Just (SourceSpan 1 1)) (diagnosticRelatedSpan warning)
    _ -> failTest "expected exactly one warning record"

testNestedScopeShadowingNoWarning :: IO ()
testNestedScopeShadowingNoWarning = do
  settings <- enabledSettings
  warnings <- analyzeRebindingWarnings settings nestedScopeProgram
  assertEqual "warning count" 0 (length warnings)

testDisabledOuterScopeShadowingEmitsNoWarnings :: IO ()
testDisabledOuterScopeShadowingEmitsNoWarnings = do
  warnings <- analyzeRebindingWarnings defaultWarningSettings nestedScopeProgram
  assertEqual "warning count" 0 (length warnings)

testNestedLetShadowingEmitsWarning :: IO ()
testNestedLetShadowingEmitsWarning = do
  settings <- shadowingEnabledSettings
  warnings <- analyzeRebindingWarnings settings nestedScopeProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" (Just ShadowingOuterScope) (diagnosticWarningCategory warning)
      assertEqual "warning code" "W0002" (diagnosticCodeText (diagnosticCode warning))
      assertEqual "warning variable" (Just "x") (diagnosticSubject warning)
      assertEqual "warning span" (Just (SourceSpan 2 3)) (diagnosticPrimarySpan warning)
      assertEqual "previous span" (Just (SourceSpan 1 1)) (diagnosticRelatedSpan warning)
    _ -> failTest "expected exactly one outer-scope shadowing warning record"

testLambdaParameterShadowingEmitsWarning :: IO ()
testLambdaParameterShadowingEmitsWarning = do
  settings <- shadowingEnabledSettings
  warnings <- analyzeRebindingWarnings settings lambdaShadowingProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" (Just ShadowingOuterScope) (diagnosticWarningCategory warning)
      assertEqual "warning code" "W0002" (diagnosticCodeText (diagnosticCode warning))
      assertEqual "warning variable" (Just "x") (diagnosticSubject warning)
      assertEqual "warning span" (Just (SourceSpan 2 1)) (diagnosticPrimarySpan warning)
      assertEqual "previous span" (Just (SourceSpan 1 1)) (diagnosticRelatedSpan warning)
    _ -> failTest "expected exactly one lambda-parameter shadowing warning record"

testLambdaExpressionShadowingUsesStatementSpan :: IO ()
testLambdaExpressionShadowingUsesStatementSpan = do
  settings <- shadowingEnabledSettings
  warnings <- analyzeRebindingWarnings settings lambdaExpressionShadowingProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" (Just ShadowingOuterScope) (diagnosticWarningCategory warning)
      assertEqual "warning variable" (Just "x") (diagnosticSubject warning)
      assertEqual "warning span" (Just (SourceSpan 2 1)) (diagnosticPrimarySpan warning)
      assertEqual "previous span" (Just (SourceSpan 1 1)) (diagnosticRelatedSpan warning)
    _ -> failTest "expected exactly one expression lambda shadowing warning record"

testPromotedOuterScopeShadowingReportsCompileErrors :: IO ()
testPromotedOuterScopeShadowingReportsCompileErrors = do
  settings <- shadowingPromotedSettings
  result <- compileExpr settings nestedScopeProgram
  assertEqual "error count" 1 (length (compileErrors result))
  assertEqual "warning count" 0 (length (compileWarnings result))

testOuterScopeShadowingIgnoresSameScopeRebinding :: IO ()
testOuterScopeShadowingIgnoresSameScopeRebinding = do
  settings <- shadowingEnabledSettings
  warnings <- analyzeRebindingWarnings settings sampleProgram
  assertEqual "warning count" 0 (length warnings)

testDisabledUnusedBindingEmitsNoWarnings :: IO ()
testDisabledUnusedBindingEmitsNoWarnings = do
  warnings <- analyzeRebindingWarnings defaultWarningSettings unusedBindingProgram
  assertEqual "warning count" 0 (length warnings)

testUnusedBindingEmitsWarning :: IO ()
testUnusedBindingEmitsWarning = do
  settings <- unusedBindingEnabledSettings
  warnings <- analyzeRebindingWarnings settings unusedBindingProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" (Just UnusedBinding) (diagnosticWarningCategory warning)
      assertEqual "warning code" "W0003" (diagnosticCodeText (diagnosticCode warning))
      assertEqual "warning variable" (Just "unused") (diagnosticSubject warning)
      assertEqual "warning span" (Just (SourceSpan 1 1)) (diagnosticPrimarySpan warning)
      assertEqual "previous span" Nothing (diagnosticRelatedSpan warning)
    _ -> failTest "expected exactly one unused-binding warning record"

testUsedOrdinaryLetEmitsNoWarning :: IO ()
testUsedOrdinaryLetEmitsNoWarning = do
  settings <- unusedBindingEnabledSettings
  warnings <- analyzeRebindingWarnings settings usedOrdinaryLetProgram
  assertEqual "warning count" 0 (length warnings)

testImplMethodBodyReferenceCountsAsUse :: IO ()
testImplMethodBodyReferenceCountsAsUse = do
  settings <- unusedBindingEnabledSettings
  warnings <- analyzeRebindingWarnings settings implMethodUsesBindingProgram
  assertEqual "warning count" 0 (length warnings)

testPreDeclarationReferenceDoesNotCountAsUse :: IO ()
testPreDeclarationReferenceDoesNotCountAsUse = do
  settings <- unusedBindingEnabledSettings
  warnings <- analyzeRebindingWarnings settings preDeclarationReferenceProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" (Just UnusedBinding) (diagnosticWarningCategory warning)
      assertEqual "warning variable" (Just "x") (diagnosticSubject warning)
      assertEqual "warning span" (Just (SourceSpan 2 1)) (diagnosticPrimarySpan warning)
    _ -> failTest "expected pre-declaration reference not to satisfy use"

testSameNameRebindingKeepsEarlierBindingUnused :: IO ()
testSameNameRebindingKeepsEarlierBindingUnused = do
  settings <- unusedBindingEnabledSettings
  warnings <- analyzeRebindingWarnings settings sameNameRebindingUsedProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" (Just UnusedBinding) (diagnosticWarningCategory warning)
      assertEqual "warning variable" (Just "x") (diagnosticSubject warning)
      assertEqual "warning span" (Just (SourceSpan 1 1)) (diagnosticPrimarySpan warning)
    _ -> failTest "expected later same-name use to belong to the rebinding only"

testSelfReferentialRhsStillUnused :: IO ()
testSelfReferentialRhsStillUnused = do
  settings <- unusedBindingEnabledSettings
  warnings <- analyzeRebindingWarnings settings selfReferentialUnusedProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" (Just UnusedBinding) (diagnosticWarningCategory warning)
      assertEqual "warning variable" (Just "loop") (diagnosticSubject warning)
      assertEqual "warning span" (Just (SourceSpan 1 1)) (diagnosticPrimarySpan warning)
    _ -> failTest "expected self-referential binding to remain unused"

testRecursiveForwardReferenceCountsAsUseUnderPromotion :: IO ()
testRecursiveForwardReferenceCountsAsUseUnderPromotion = do
  settings <- unusedBindingPromotedSettings
  result <-
    compileSource
      settings
      """
      left = if True then \\(x) -> x else right.
      right = if False then \\(x) -> x else left.
      left 1.
      """
  assertEqual "recursive pair compile errors" [] (compileErrors result)
  assertEqual "recursive pair warnings" [] (compileWarnings result)

testUnusedBindingSuppressesRebindingSiteDuplicate :: IO ()
testUnusedBindingSuppressesRebindingSiteDuplicate = do
  settings <- rebindingAndUnusedEnabledSettings
  warnings <- analyzeRebindingWarnings settings sampleProgram
  case warnings of
    [firstWarning, secondWarning] -> do
      assertEqual "first warning category" (Just UnusedBinding) (diagnosticWarningCategory firstWarning)
      assertEqual "first warning span" (Just (SourceSpan 1 1)) (diagnosticPrimarySpan firstWarning)
      assertEqual "second warning category" (Just SameScopeRebinding) (diagnosticWarningCategory secondWarning)
      assertEqual "second warning span" (Just (SourceSpan 2 1)) (diagnosticPrimarySpan secondWarning)
    _ -> failTest "expected first binding unused and rebinding site to emit only W0001"

testUnusedBindingSuppressesConstructorRebindingSiteDuplicate :: IO ()
testUnusedBindingSuppressesConstructorRebindingSiteDuplicate = do
  settings <- rebindingAndUnusedEnabledSettings
  warnings <- analyzeRebindingWarnings settings letRebindsConstructorProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" (Just SameScopeRebinding) (diagnosticWarningCategory warning)
      assertEqual "warning variable" (Just "Just") (diagnosticSubject warning)
      assertEqual "warning span" (Just (SourceSpan 2 1)) (diagnosticPrimarySpan warning)
      assertEqual "previous span" (Just (SourceSpan 1 1)) (diagnosticRelatedSpan warning)
    _ -> failTest "expected constructor rebinding site to emit W0001 without W0003"

testPromotedUnusedBindingReportsCompileErrors :: IO ()
testPromotedUnusedBindingReportsCompileErrors = do
  settings <- unusedBindingPromotedSettings
  result <- compileExpr settings unusedBindingProgram
  assertEqual "error count" 1 (length (compileErrors result))
  assertEqual "warning count" 0 (length (compileWarnings result))

testBundledPreludeAliasShadowingNoWarning :: IO ()
testBundledPreludeAliasShadowingNoWarning = do
  settings <- promotedSettings
  result <- compileSource settings "map = (+ 1). map 2."
  assertEqual "compile errors" [] (compileErrors result)
  assertEqual "warning count" 0 (length (compileWarnings result))

testExplicitPreludeExternalUses :: IO ()
testExplicitPreludeExternalUses = do
  settings <- unusedBindingPromotedSettings
  used <- compileSourceWithPrelude settings (Just "identity = \\(x) -> x.") "identity 1."
  assertEqual "used prelude binding" [] (compileErrors used)
  unused <- compileSourceWithPrelude settings (Just "identity = \\(x) -> x.") "1."
  assertEqual "unused prelude binding retains promoted warning" 1 (length (compileErrors unused))

testExplicitPreludeConstructorRebinding :: IO ()
testExplicitPreludeConstructorRebinding = do
  settings <- enabledSettings
  result <- compileSourceWithPrelude settings (Just "data Earlier = Shared.") "\ndata Later = Shared."
  assertEqual "constructor rebinding compiles" [] (compileErrors result)
  case compileWarnings result of
    [warning] -> do
      assertEqual "constructor warning category" (Just SameScopeRebinding) (diagnosticWarningCategory warning)
      assertEqual "constructor warning subject" (Just "Shared") (diagnosticSubject warning)
      assertEqual "source declaration span" (Just (SourceSpan 2 1)) (sourceSpanStart <$> diagnosticPrimarySpan warning)
      assertEqual "prelude declaration span" (Just (SourceSpanIn "<explicit-prelude>" 1 1)) (sourceSpanStart <$> diagnosticRelatedSpan warning)
    warnings -> failTest ("expected one constructor rebinding warning, got " <> Text.pack (show warnings))
  promoted <- promotedSettings
  rejected <- compileSourceWithPrelude promoted (Just "data Earlier = Shared.") "data Later = Shared."
  assertEqual "constructor rebinding promotion" 1 (length (compileErrors rejected))

testExplicitPreludeMatchingBundledSourceEmitsWarning :: IO ()
testExplicitPreludeMatchingBundledSourceEmitsWarning = do
  settings <- promotedSettings
  result <- compileSourceWithPrelude settings (Just bundledPreludeSource) "map = (+ 1). map 2."
  assertEqual "warning count" 0 (length (compileWarnings result))
  assertEqual "error count" 1 (length (compileErrors result))

testDriverKeepsWarningOnlySuccessDiagnosticOnly :: IO ()
testDriverKeepsWarningOnlySuccessDiagnosticOnly = do
  settings <- enabledSettings
  result <- compileExpr settings sampleProgram
  assertEqual "error count" 0 (length (compileErrors result))
  assertEqual "warning count" 1 (length (compileWarnings result))
  assertEqual "warning-only stream" (compileWarnings result) (compileDiagnostics result)

testDriverStoresNativeCompileFailure :: IO ()
testDriverStoresNativeCompileFailure = do
  result <- compileSource defaultWarningSettings "missing."
  assertEqual "native warning count" 0 (length (compileWarnings result))
  assertEqual "native error count" 1 (length (compileErrors result))
  assertEqual "native error stream" (compileErrors result) (compileDiagnostics result)

testDriverReportsPromotedWarningsAsCompileErrors :: IO ()
testDriverReportsPromotedWarningsAsCompileErrors = do
  settings <- promotedSettings
  result <- compileExpr settings sampleProgram
  assertEqual "error count" 1 (length (compileErrors result))
  assertEqual "warning count" 0 (length (compileWarnings result))
  assertEqual "promoted stream count" 1 (length (compileDiagnostics result))
  assertEqual "promoted stream membership" (compileErrors result) (compileDiagnostics result)

testDriverOrdersCompileWarningsBeforeRuntimeFailures :: IO ()
testDriverOrdersCompileWarningsBeforeRuntimeFailures = do
  settings <- enabledSettings
  result <- runSource settings "x = 1. x = 2. hd []."
  assertEqual "compile error count" 0 (length (runCompileErrors result))
  assertEqual "warning count" 1 (length (runWarnings result))
  assertEqual "runtime error count" 1 (length (runRuntimeErrors result))
  assertEqual
    "compile-before-runtime stream"
    (runWarnings result <> runRuntimeErrors result)
    (runDiagnostics result)

enabledSettings :: IO WarningSettings
enabledSettings =
  case resolveWarningSettings ["-Wsame-scope-rebinding"] Nothing Nothing Nothing of
    Left err -> failTest ("failed to resolve enabled settings: " <> renderDiagnostic err)
    Right settings -> pure settings

promotedSettings :: IO WarningSettings
promotedSettings =
  case resolveWarningSettings ["-Werror=same-scope-rebinding"] Nothing Nothing Nothing of
    Left err -> failTest ("failed to resolve promoted settings: " <> renderDiagnostic err)
    Right settings -> pure settings

shadowingEnabledSettings :: IO WarningSettings
shadowingEnabledSettings =
  case resolveWarningSettings ["-Wshadowing-outer-scope"] Nothing Nothing Nothing of
    Left err -> failTest ("failed to resolve shadowing settings: " <> renderDiagnostic err)
    Right settings -> pure settings

shadowingPromotedSettings :: IO WarningSettings
shadowingPromotedSettings =
  case resolveWarningSettings ["-Werror=shadowing-outer-scope"] Nothing Nothing Nothing of
    Left err -> failTest ("failed to resolve promoted shadowing settings: " <> renderDiagnostic err)
    Right settings -> pure settings

unusedBindingEnabledSettings :: IO WarningSettings
unusedBindingEnabledSettings =
  case resolveWarningSettings ["-Wunused-binding"] Nothing Nothing Nothing of
    Left err -> failTest ("failed to resolve unused-binding settings: " <> renderDiagnostic err)
    Right settings -> pure settings

unusedBindingPromotedSettings :: IO WarningSettings
unusedBindingPromotedSettings =
  case resolveWarningSettings ["-Werror=unused-binding"] Nothing Nothing Nothing of
    Left err -> failTest ("failed to resolve promoted unused-binding settings: " <> renderDiagnostic err)
    Right settings -> pure settings

rebindingAndUnusedEnabledSettings :: IO WarningSettings
rebindingAndUnusedEnabledSettings =
  case resolveWarningSettings ["-Wsame-scope-rebinding", "-Wunused-binding"] Nothing Nothing Nothing of
    Left err -> failTest ("failed to resolve rebinding plus unused-binding settings: " <> renderDiagnostic err)
    Right settings -> pure settings

analyzeProgram :: WarningSettings -> Expr 'Lowered -> IO AnalysisResult
analyzeProgram settings expression = do
  let resolved = resolveForAnalyzer expression
  Analyzer.analyzeProgram settings resolved

analyzeRebindingWarnings :: WarningSettings -> Expr 'Lowered -> IO [Diagnostic]
analyzeRebindingWarnings settings expression = do
  let resolved = resolveForAnalyzer expression
  Analyzer.analyzeRebindingWarnings settings resolved

resolveForAnalyzer :: Expr 'Lowered -> Expr 'Resolved
resolveForAnalyzer = resolveStandaloneExprNames (exportInventory []) . reindexLoweredExpr

sampleProgram :: Expr 'Lowered
sampleProgram =
  loweredBlock
    [ loweredLet "x" (SourceSpan 1 1) (loweredLiteral (LInt 1)),
      loweredLet "x" (SourceSpan 2 1) (loweredLiteral (LInt 2))
    ]

repeatedProgram :: Expr 'Lowered
repeatedProgram =
  loweredBlock
    [ loweredLet "x" (SourceSpan 1 1) (loweredLiteral (LInt 1)),
      loweredLet "x" (SourceSpan 2 1) (loweredLiteral (LInt 2)),
      loweredLet "x" (SourceSpan 3 1) (loweredLiteral (LInt 3))
    ]

constructorRebindingProgram :: Expr 'Lowered
constructorRebindingProgram =
  loweredBlock
    [ loweredLet "Nothing" (SourceSpan 1 1) (loweredLiteral (LInt 1)),
      loweredData (SourceSpan 2 1) "Maybe" [] [loweredConstructorAt (SourceSpan 2 1) "Nothing" []]
    ]

nestedScopeProgram :: Expr 'Lowered
nestedScopeProgram =
  loweredBlock
    [ loweredLet "x" (SourceSpan 1 1) (loweredLiteral (LInt 1)),
      loweredExpression
        (SourceSpan 2 1)
        ( loweredBlock
            [ loweredLet "x" (SourceSpan 2 3) (loweredLiteral (LInt 2))
            ]
        ),
      loweredExpression (SourceSpan 4 1) (loweredVariable "x")
    ]

lambdaShadowingProgram :: Expr 'Lowered
lambdaShadowingProgram =
  loweredBlock
    [ loweredLet "x" (SourceSpan 1 1) (loweredLiteral (LInt 1)),
      loweredLet "f" (SourceSpan 2 1) (loweredLambda "x" (loweredVariable "x"))
    ]

lambdaExpressionShadowingProgram :: Expr 'Lowered
lambdaExpressionShadowingProgram =
  loweredBlock
    [ loweredLet "x" (SourceSpan 1 1) (loweredLiteral (LInt 1)),
      loweredExpression (SourceSpan 2 1) (loweredLambda "x" (loweredVariable "x"))
    ]

unusedBindingProgram :: Expr 'Lowered
unusedBindingProgram =
  loweredBlock
    [ loweredLet "unused" (SourceSpan 1 1) (loweredLiteral (LInt 1))
    ]

usedOrdinaryLetProgram :: Expr 'Lowered
usedOrdinaryLetProgram =
  loweredBlock
    [ loweredLet "x" (SourceSpan 1 1) (loweredLiteral (LInt 1)),
      loweredLet "y" (SourceSpan 2 1) (loweredVariable "x"),
      loweredExpression (SourceSpan 3 1) (loweredVariable "y")
    ]

implMethodUsesBindingProgram :: Expr 'Lowered
implMethodUsesBindingProgram =
  loweredBlock
    [ loweredLet "helper" (SourceSpan 1 1) (loweredLiteral (LInt 1)),
      loweredImpl
        (SourceSpan 2 1)
        "Eq"
        []
        [loweredImplMethod "equals" (SourceSpan 2 10) (loweredVariable "helper")]
    ]

preDeclarationReferenceProgram :: Expr 'Lowered
preDeclarationReferenceProgram =
  loweredBlock
    [ loweredExpression (SourceSpan 1 1) (loweredVariable "x"),
      loweredLet "x" (SourceSpan 2 1) (loweredLiteral (LInt 1))
    ]

sameNameRebindingUsedProgram :: Expr 'Lowered
sameNameRebindingUsedProgram =
  loweredBlock
    [ loweredLet "x" (SourceSpan 1 1) (loweredLiteral (LInt 1)),
      loweredLet "x" (SourceSpan 2 1) (loweredLiteral (LInt 2)),
      loweredExpression (SourceSpan 3 1) (loweredVariable "x")
    ]

selfReferentialUnusedProgram :: Expr 'Lowered
selfReferentialUnusedProgram =
  loweredBlock
    [ loweredLet "loop" (SourceSpan 1 1) (loweredVariable "loop")
    ]

letRebindsConstructorProgram :: Expr 'Lowered
letRebindsConstructorProgram =
  loweredBlock
    [ loweredData (SourceSpan 1 1) "Maybe" [] [loweredConstructorAt (SourceSpan 1 1) "Just" [TypeInt]],
      loweredLet "Just" (SourceSpan 2 1) (loweredLiteral (LInt 1))
    ]
