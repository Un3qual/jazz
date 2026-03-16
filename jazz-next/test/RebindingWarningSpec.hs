{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  )
import JazzNext.Compiler.Analyzer
  ( analyzeRebindingWarnings
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    WarningRecord (..),
    renderDiagnostic
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileExpr
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
    assertJust,
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
    ("nested scope shadowing does not emit same-scope warning", testNestedScopeShadowingNoWarning),
    ("driver keeps JS output when warning is not promoted", testDriverKeepsOutputWhenNotPromoted),
    ("driver suppresses JS output when warning is promoted to error", testDriverSuppressesOutputWhenPromoted)
  ]

testDisabledCategoryEmitsNoWarnings :: IO ()
testDisabledCategoryEmitsNoWarnings = do
  warnings <- analyzeRebindingWarnings defaultWarningSettings sampleProgram
  assertEqual "warning count" 0 (length warnings)

testEnabledCategoryEmitsWarning :: IO ()
testEnabledCategoryEmitsWarning = do
  settings <- enabledSettings
  warnings <- analyzeRebindingWarnings settings sampleProgram
  assertEqual "warning count" 1 (length warnings)
  let warning = head warnings
  assertEqual "warning category" SameScopeRebinding (warningCategory warning)
  assertEqual "warning code" "W0001" (warningCodeText warning)
  assertEqual "warning variable" "x" (warningVariableName warning)
  assertEqual "warning span" (SourceSpan 2 1) (warningPrimarySpan warning)
  assertEqual "previous span" (Just (SourceSpan 1 1)) (warningPreviousSpan warning)

testDeterministicWarningOrder :: IO ()
testDeterministicWarningOrder = do
  settings <- enabledSettings
  warnings <- analyzeRebindingWarnings settings repeatedProgram
  assertEqual "warning count" 2 (length warnings)
  let firstWarning = warnings !! 0
      secondWarning = warnings !! 1
  assertEqual "first warning span" (SourceSpan 2 1) (warningPrimarySpan firstWarning)
  assertEqual "first previous span" (Just (SourceSpan 1 1)) (warningPreviousSpan firstWarning)
  assertEqual "second warning span" (SourceSpan 3 1) (warningPrimarySpan secondWarning)
  assertEqual "second previous span" (Just (SourceSpan 2 1)) (warningPreviousSpan secondWarning)

testNestedScopeShadowingNoWarning :: IO ()
testNestedScopeShadowingNoWarning = do
  settings <- enabledSettings
  warnings <- analyzeRebindingWarnings settings nestedScopeProgram
  assertEqual "warning count" 0 (length warnings)

testDriverKeepsOutputWhenNotPromoted :: IO ()
testDriverKeepsOutputWhenNotPromoted = do
  settings <- enabledSettings
  result <- compileExpr settings sampleProgram
  assertEqual "error count" 0 (length (compileErrors result))
  assertEqual "warning count" 1 (length (compileWarnings result))
  assertJust "generated JS" (generatedJs result)

testDriverSuppressesOutputWhenPromoted :: IO ()
testDriverSuppressesOutputWhenPromoted = do
  settings <- promotedSettings
  result <- compileExpr settings sampleProgram
  assertEqual "error count" 1 (length (compileErrors result))
  assertEqual "warning count" 1 (length (compileWarnings result))
  assertEqual "generated JS" Nothing (generatedJs result)

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

nestedScopeProgram :: Expr
nestedScopeProgram =
  EBlock
    [ SLet "x" (SourceSpan 1 1) (ELit (LInt 1)),
      SExpr
        (SourceSpan 2 1)
        ( EBlock
            [ SLet "x" (SourceSpan 2 1) (ELit (LInt 2))
            ]
        ),
      SExpr (SourceSpan 4 1) (EVar "x")
    ]
