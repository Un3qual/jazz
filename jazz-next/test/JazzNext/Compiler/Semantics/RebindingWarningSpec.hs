{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.AST
  ( DataConstructorArgument (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    Statement (..)
  )
import JazzNext.Compiler.Analyzer
  ( analyzeRebindingWarnings
  )
import JazzNext.Compiler.BundledPrelude
  ( bundledPreludeSource
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    WarningRecord (..),
    renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileExpr,
    compileSource,
    compileSourceWithPrelude
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    defaultWarningSettings,
    resolveWarningSettings
  )
import JazzNext.Compiler.Warnings
  ( WarningCategory (..)
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    failTest,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "RebindingWarning" tests

tests :: [NamedTest]
tests =
  [ ("disabled warning category emits nothing", testDisabledCategoryEmitsNoWarnings),
    ("enabled warning emits one same-scope rebinding warning", testEnabledCategoryEmitsWarning),
    ("repeated same-scope rebinding order is deterministic", testDeterministicWarningOrder),
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
    ("unused-binding suppresses rebinding-site duplicate when W0001 also emits", testUnusedBindingSuppressesRebindingSiteDuplicate),
    ("unused-binding suppresses constructor-rebinding duplicate when W0001 also emits", testUnusedBindingSuppressesConstructorRebindingSiteDuplicate),
    ("unused-binding promotion reports compile errors", testPromotedUnusedBindingReportsCompileErrors),
    ("bundled default prelude aliases do not trigger same-scope rebinding", testBundledPreludeAliasShadowingNoWarning),
    ("explicit prelude text matching bundled source still emits rebinding warnings", testExplicitPreludeMatchingBundledSourceEmitsWarning),
    ("driver keeps warning-only success diagnostics", testDriverKeepsWarningOnlySuccessDiagnosticOnly),
    ("driver reports promoted warnings as compile errors", testDriverReportsPromotedWarningsAsCompileErrors)
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
      assertEqual "warning category" SameScopeRebinding (warningCategory warning)
      assertEqual "warning code" "W0001" (warningCodeText warning)
      assertEqual "warning variable" "x" (warningVariableName warning)
      assertEqual "warning span" (SourceSpan 2 1) (warningPrimarySpan warning)
      assertEqual "previous span" (Just (SourceSpan 1 1)) (warningPreviousSpan warning)
    _ -> failTest "expected exactly one warning record"

testDeterministicWarningOrder :: IO ()
testDeterministicWarningOrder = do
  settings <- enabledSettings
  warnings <- analyzeRebindingWarnings settings repeatedProgram
  case warnings of
    [firstWarning, secondWarning] -> do
      assertEqual "first warning span" (SourceSpan 2 1) (warningPrimarySpan firstWarning)
      assertEqual "first previous span" (Just (SourceSpan 1 1)) (warningPreviousSpan firstWarning)
      assertEqual "second warning span" (SourceSpan 3 1) (warningPrimarySpan secondWarning)
      assertEqual "second previous span" (Just (SourceSpan 2 1)) (warningPreviousSpan secondWarning)
    _ -> failTest "expected exactly two warning records"

testConstructorRebindingEmitsWarning :: IO ()
testConstructorRebindingEmitsWarning = do
  settings <- enabledSettings
  warnings <- analyzeRebindingWarnings settings constructorRebindingProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" SameScopeRebinding (warningCategory warning)
      assertEqual "warning code" "W0001" (warningCodeText warning)
      assertEqual "warning variable" "Nothing" (warningVariableName warning)
      assertEqual "warning span" (SourceSpan 2 1) (warningPrimarySpan warning)
      assertEqual "previous span" (Just (SourceSpan 1 1)) (warningPreviousSpan warning)
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
      assertEqual "warning category" ShadowingOuterScope (warningCategory warning)
      assertEqual "warning code" "W0002" (warningCodeText warning)
      assertEqual "warning variable" "x" (warningVariableName warning)
      assertEqual "warning span" (SourceSpan 2 3) (warningPrimarySpan warning)
      assertEqual "previous span" (Just (SourceSpan 1 1)) (warningPreviousSpan warning)
    _ -> failTest "expected exactly one outer-scope shadowing warning record"

testLambdaParameterShadowingEmitsWarning :: IO ()
testLambdaParameterShadowingEmitsWarning = do
  settings <- shadowingEnabledSettings
  warnings <- analyzeRebindingWarnings settings lambdaShadowingProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" ShadowingOuterScope (warningCategory warning)
      assertEqual "warning code" "W0002" (warningCodeText warning)
      assertEqual "warning variable" "x" (warningVariableName warning)
      assertEqual "warning span" (SourceSpan 2 1) (warningPrimarySpan warning)
      assertEqual "previous span" (Just (SourceSpan 1 1)) (warningPreviousSpan warning)
    _ -> failTest "expected exactly one lambda-parameter shadowing warning record"

testLambdaExpressionShadowingUsesStatementSpan :: IO ()
testLambdaExpressionShadowingUsesStatementSpan = do
  settings <- shadowingEnabledSettings
  warnings <- analyzeRebindingWarnings settings lambdaExpressionShadowingProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" ShadowingOuterScope (warningCategory warning)
      assertEqual "warning variable" "x" (warningVariableName warning)
      assertEqual "warning span" (SourceSpan 2 1) (warningPrimarySpan warning)
      assertEqual "previous span" (Just (SourceSpan 1 1)) (warningPreviousSpan warning)
    _ -> failTest "expected exactly one expression lambda shadowing warning record"

testPromotedOuterScopeShadowingReportsCompileErrors :: IO ()
testPromotedOuterScopeShadowingReportsCompileErrors = do
  settings <- shadowingPromotedSettings
  result <- compileExpr settings nestedScopeProgram
  assertEqual "error count" 1 (length (compileErrors result))
  assertEqual "warning count" 1 (length (compileWarnings result))

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
      assertEqual "warning category" UnusedBinding (warningCategory warning)
      assertEqual "warning code" "W0003" (warningCodeText warning)
      assertEqual "warning variable" "unused" (warningVariableName warning)
      assertEqual "warning span" (SourceSpan 1 1) (warningPrimarySpan warning)
      assertEqual "previous span" Nothing (warningPreviousSpan warning)
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
      assertEqual "warning category" UnusedBinding (warningCategory warning)
      assertEqual "warning variable" "x" (warningVariableName warning)
      assertEqual "warning span" (SourceSpan 2 1) (warningPrimarySpan warning)
    _ -> failTest "expected pre-declaration reference not to satisfy use"

testSameNameRebindingKeepsEarlierBindingUnused :: IO ()
testSameNameRebindingKeepsEarlierBindingUnused = do
  settings <- unusedBindingEnabledSettings
  warnings <- analyzeRebindingWarnings settings sameNameRebindingUsedProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" UnusedBinding (warningCategory warning)
      assertEqual "warning variable" "x" (warningVariableName warning)
      assertEqual "warning span" (SourceSpan 1 1) (warningPrimarySpan warning)
    _ -> failTest "expected later same-name use to belong to the rebinding only"

testSelfReferentialRhsStillUnused :: IO ()
testSelfReferentialRhsStillUnused = do
  settings <- unusedBindingEnabledSettings
  warnings <- analyzeRebindingWarnings settings selfReferentialUnusedProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" UnusedBinding (warningCategory warning)
      assertEqual "warning variable" "loop" (warningVariableName warning)
      assertEqual "warning span" (SourceSpan 1 1) (warningPrimarySpan warning)
    _ -> failTest "expected self-referential binding to remain unused"

testUnusedBindingSuppressesRebindingSiteDuplicate :: IO ()
testUnusedBindingSuppressesRebindingSiteDuplicate = do
  settings <- rebindingAndUnusedEnabledSettings
  warnings <- analyzeRebindingWarnings settings sampleProgram
  case warnings of
    [firstWarning, secondWarning] -> do
      assertEqual "first warning category" UnusedBinding (warningCategory firstWarning)
      assertEqual "first warning span" (SourceSpan 1 1) (warningPrimarySpan firstWarning)
      assertEqual "second warning category" SameScopeRebinding (warningCategory secondWarning)
      assertEqual "second warning span" (SourceSpan 2 1) (warningPrimarySpan secondWarning)
    _ -> failTest "expected first binding unused and rebinding site to emit only W0001"

testUnusedBindingSuppressesConstructorRebindingSiteDuplicate :: IO ()
testUnusedBindingSuppressesConstructorRebindingSiteDuplicate = do
  settings <- rebindingAndUnusedEnabledSettings
  warnings <- analyzeRebindingWarnings settings letRebindsConstructorProgram
  case warnings of
    [warning] -> do
      assertEqual "warning category" SameScopeRebinding (warningCategory warning)
      assertEqual "warning variable" "Just" (warningVariableName warning)
      assertEqual "warning span" (SourceSpan 2 1) (warningPrimarySpan warning)
      assertEqual "previous span" (Just (SourceSpan 1 1)) (warningPreviousSpan warning)
    _ -> failTest "expected constructor rebinding site to emit W0001 without W0003"

testPromotedUnusedBindingReportsCompileErrors :: IO ()
testPromotedUnusedBindingReportsCompileErrors = do
  settings <- unusedBindingPromotedSettings
  result <- compileExpr settings unusedBindingProgram
  assertEqual "error count" 1 (length (compileErrors result))
  assertEqual "warning count" 1 (length (compileWarnings result))

testBundledPreludeAliasShadowingNoWarning :: IO ()
testBundledPreludeAliasShadowingNoWarning = do
  settings <- promotedSettings
  result <- compileSource settings "map = (+ 1). map 2."
  assertEqual "compile errors" [] (compileErrors result)
  assertEqual "warning count" 0 (length (compileWarnings result))

testExplicitPreludeMatchingBundledSourceEmitsWarning :: IO ()
testExplicitPreludeMatchingBundledSourceEmitsWarning = do
  settings <- promotedSettings
  result <- compileSourceWithPrelude settings (Just bundledPreludeSource) "map = (+ 1). map 2."
  assertEqual "warning count" 1 (length (compileWarnings result))
  assertEqual "error count" 1 (length (compileErrors result))

testDriverKeepsWarningOnlySuccessDiagnosticOnly :: IO ()
testDriverKeepsWarningOnlySuccessDiagnosticOnly = do
  settings <- enabledSettings
  result <- compileExpr settings sampleProgram
  assertEqual "error count" 0 (length (compileErrors result))
  assertEqual "warning count" 1 (length (compileWarnings result))

testDriverReportsPromotedWarningsAsCompileErrors :: IO ()
testDriverReportsPromotedWarningsAsCompileErrors = do
  settings <- promotedSettings
  result <- compileExpr settings sampleProgram
  assertEqual "error count" 1 (length (compileErrors result))
  assertEqual "warning count" 1 (length (compileWarnings result))

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

sampleProgram :: Expr
sampleProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (ELit (LInt 1)),
      SLet "x" (SourceSpan 2 1) (ELit (LInt 2))
    ]

repeatedProgram :: Expr
repeatedProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (ELit (LInt 1)),
      SLet "x" (SourceSpan 2 1) (ELit (LInt 2)),
      SLet "x" (SourceSpan 3 1) (ELit (LInt 3))
    ]

constructorRebindingProgram :: Expr
constructorRebindingProgram =
  EBlock
    [ SLet "Nothing" (SourceSpan 1 1) (ELit (LInt 1)),
      SData (SourceSpan 2 1) "Maybe" [] [DataConstructor "Nothing" []]
    ]

nestedScopeProgram :: Expr
nestedScopeProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (ELit (LInt 1)),
      SExpr
        (SourceSpan 2 1)
        ( EBlock
            [ SLet "x" (SourceSpan 2 3) (ELit (LInt 2))
            ]
        ),
      SExpr (SourceSpan 4 1) (EVar "x")
    ]

lambdaShadowingProgram :: Expr
lambdaShadowingProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (ELit (LInt 1)),
      SLet "f" (SourceSpan 2 1) (ELambda "x" (EVar "x"))
    ]

lambdaExpressionShadowingProgram :: Expr
lambdaExpressionShadowingProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (ELit (LInt 1)),
      SExpr (SourceSpan 2 1) (ELambda "x" (EVar "x"))
    ]

unusedBindingProgram :: Expr
unusedBindingProgram =
  EBlock
    [ SLet "unused" (SourceSpan 1 1) (ELit (LInt 1))
    ]

usedOrdinaryLetProgram :: Expr
usedOrdinaryLetProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (ELit (LInt 1)),
      SLet "y" (SourceSpan 2 1) (EVar "x"),
      SExpr (SourceSpan 3 1) (EVar "y")
    ]

implMethodUsesBindingProgram :: Expr
implMethodUsesBindingProgram =
  EBlock
    [ SLet "helper" (SourceSpan 1 1) (ELit (LInt 1)),
      SImpl
        (SourceSpan 2 1)
        "Eq"
        []
        [ImplMethod "equals" (SourceSpan 2 10) (EVar "helper")]
    ]

preDeclarationReferenceProgram :: Expr
preDeclarationReferenceProgram =
  EBlock
    [ SExpr (SourceSpan 1 1) (EVar "x"),
      SLet "x" (SourceSpan 2 1) (ELit (LInt 1))
    ]

sameNameRebindingUsedProgram :: Expr
sameNameRebindingUsedProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (ELit (LInt 1)),
      SLet "x" (SourceSpan 2 1) (ELit (LInt 2)),
      SExpr (SourceSpan 3 1) (EVar "x")
    ]

selfReferentialUnusedProgram :: Expr
selfReferentialUnusedProgram =
  EBlock
    [ SLet "loop" (SourceSpan 1 1) (EVar "loop")
    ]

letRebindsConstructorProgram :: Expr
letRebindsConstructorProgram =
  EBlock
    [ SData (SourceSpan 1 1) "Maybe" [] [DataConstructor "Just" [DataConstructorArgumentName "value"]],
      SLet "Just" (SourceSpan 2 1) (ELit (LInt 1))
    ]
