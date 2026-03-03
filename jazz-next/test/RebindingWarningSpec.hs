module Main (main) where

import Control.Exception (Exception, catch, throwIO)
import JazzNext.Compiler.Analyzer
  ( Expr (..),
    Statement (..),
    analyzeRebindingWarnings
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    WarningRecord (..)
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
import System.Exit (exitFailure, exitSuccess)

newtype TestFailure = TestFailure String

instance Show TestFailure where
  show (TestFailure msg) = msg

instance Exception TestFailure

main :: IO ()
main = do
  failures <- sequence tests
  let failed = length (filter id failures)
  if failed == 0
    then do
      putStrLn "All RebindingWarning tests passed."
      exitSuccess
    else do
      putStrLn (show failed ++ " RebindingWarning test(s) failed.")
      exitFailure

tests :: [IO Bool]
tests =
  [ run "disabled warning category emits nothing" testDisabledCategoryEmitsNoWarnings,
    run "enabled warning emits one same-scope rebinding warning" testEnabledCategoryEmitsWarning,
    run "repeated same-scope rebinding order is deterministic" testDeterministicWarningOrder,
    run "nested scope shadowing does not emit same-scope warning" testNestedScopeShadowingNoWarning,
    run "driver keeps JS output when warning is not promoted" testDriverKeepsOutputWhenNotPromoted,
    run "driver suppresses JS output when warning is promoted to error" testDriverSuppressesOutputWhenPromoted
  ]

run :: String -> IO () -> IO Bool
run name action = do
  failed <- (action >> pure False) `catchFailure` \message -> do
    putStrLn ("FAIL: " ++ name ++ "\n  " ++ message)
    pure True
  if not failed then putStrLn ("PASS: " ++ name) else pure ()
  pure failed

catchFailure :: IO a -> (String -> IO a) -> IO a
catchFailure action handler = action `catch` \(TestFailure msg) -> handler msg

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  if expected == actual
    then pure ()
    else failTest (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

assertJust :: String -> Maybe a -> IO ()
assertJust label value =
  case value of
    Just _ -> pure ()
    Nothing -> failTest (label ++ ": expected Just, got Nothing")

failTest :: String -> IO a
failTest = throwIO . TestFailure

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
    Left err -> failTest ("failed to resolve enabled settings: " ++ err)
    Right settings -> pure settings

promotedSettings :: IO WarningSettings
promotedSettings =
  case resolveWarningSettings ["-Werror=same-scope-rebinding"] Nothing Nothing Nothing of
    Left err -> failTest ("failed to resolve promoted settings: " ++ err)
    Right settings -> pure settings

sampleProgram :: Expr
sampleProgram =
  EScope
    [ SLet "x" (SourceSpan 1 1) (EInt 1),
      SLet "x" (SourceSpan 2 1) (EInt 2)
    ]

repeatedProgram :: Expr
repeatedProgram =
  EScope
    [ SLet "x" (SourceSpan 1 1) (EInt 1),
      SLet "x" (SourceSpan 2 1) (EInt 2),
      SLet "x" (SourceSpan 3 1) (EInt 3)
    ]

nestedScopeProgram :: Expr
nestedScopeProgram =
  EScope
    [ SLet "x" (SourceSpan 1 1) (EInt 1),
      SExpr
        ( EScope
            [ SLet "x" (SourceSpan 2 1) (EInt 2)
            ]
        ),
      SExpr (EVar "x")
    ]
