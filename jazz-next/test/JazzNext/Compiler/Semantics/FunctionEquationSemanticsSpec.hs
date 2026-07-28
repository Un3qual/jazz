{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text
  ( Text
  )
import JazzNext.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    Pattern (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
    runSource
  )
import JazzNext.Compiler.Name
  ( GeneratedNameKind (..),
    generatedName
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertRight,
    assertSingleDiagnosticCode,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "FunctionEquationSemantics" tests

tests :: [NamedTest]
tests =
  [ ("lowers equations to curried lambdas and one ordered pattern case", testExactLowering),
    ("lowers one-argument equations with a direct scrutinee", testOneArgumentLowering),
    ("matches constructor patterns at runtime", testConstructorPatternRuntime),
    ("supports currying and partial application", testPartialApplicationRuntime),
    ("preserves ordered clause fallthrough", testOrderedFallthroughRuntime),
    ("applies a signature to the whole equation group", testSignatureRuntime),
    ("supports recursive equation groups", testRecursiveRuntime),
    ("reports the existing non-exhaustive pattern failure", testNonExhaustiveRuntime)
  ]

testExactLowering :: IO ()
testExactLowering =
  assertRight
    "parse function equations for lowering"
    ( parseSurfaceProgram
        """
        choose Nothing fallback = fallback.
        choose (Just item) _ = item.
        """
    )
    (\surfaceProgram -> assertEqual "lowered function equations" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    firstArgument = generatedName (FunctionEquationArgument 1)
    secondArgument = generatedName (FunctionEquationArgument 2)
    expectedProgram =
      EBlock
        [ SLet
            "choose"
            (SourceSpan 1 1)
            ( ELambda
                firstArgument
                ( ELambda
                    secondArgument
                    ( EPatternCase
                        (ETuple [EVar firstArgument, EVar secondArgument])
                        [ CaseArm
                            (PTuple [PConstructor "Nothing" [], PVariable "fallback"])
                            Nothing
                            (EVar "fallback"),
                          CaseArm
                            (PTuple [PConstructor "Just" [PVariable "item"], PWildcard])
                            Nothing
                            (EVar "item")
                        ]
                    )
                )
            )
        ]

testOneArgumentLowering :: IO ()
testOneArgumentLowering =
  assertRight
    "parse one-argument equation"
    (parseSurfaceProgram "identity item = item.")
    (\surfaceProgram -> assertEqual "direct one-argument scrutinee" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    argument = generatedName (FunctionEquationArgument 1)
    expectedProgram =
      EBlock
        [ SLet
            "identity"
            (SourceSpan 1 1)
            ( ELambda
                argument
                ( EPatternCase
                    (EVar argument)
                    [CaseArm (PVariable "item") Nothing (EVar "item")]
                )
            )
        ]

testConstructorPatternRuntime :: IO ()
testConstructorPatternRuntime = do
  result <-
    runSource
      defaultWarningSettings
      """
      data Maybe a
        = Nothing
        | Just a.
      choose Nothing fallback = fallback.
      choose (Just item) _ = item.
      choose (Just 42) 0.
      """
  assertSuccessfulRuntime "constructor equation" (Just "42") result

testPartialApplicationRuntime :: IO ()
testPartialApplicationRuntime = do
  result <-
    runSource
      defaultWarningSettings
      """
      first left _ = left.
      keep = first 42.
      keep 0.
      """
  assertSuccessfulRuntime "partial function equation application" (Just "42") result

testOrderedFallthroughRuntime :: IO ()
testOrderedFallthroughRuntime = do
  result <-
    runSource
      defaultWarningSettings
      """
      pick 0 = 10.
      pick _ = 20.
      (pick 0, pick 1).
      """
  assertSuccessfulRuntime "ordered clause fallthrough" (Just "(10, 20)") result

testSignatureRuntime :: IO ()
testSignatureRuntime = do
  result <-
    runSource
      defaultWarningSettings
      """
      identity :: Int -> Int.
      identity item = item.
      identity 42.
      """
  assertSuccessfulRuntime "signed function equations" (Just "42") result

testRecursiveRuntime :: IO ()
testRecursiveRuntime = do
  result <-
    runSource
      defaultWarningSettings
      """
      factorial 0 = 1.
      factorial number = number * factorial (number - 1).
      factorial 5.
      """
  assertSuccessfulRuntime "recursive function equations" (Just "120") result

testNonExhaustiveRuntime :: IO ()
testNonExhaustiveRuntime = do
  result <- runSource defaultWarningSettings "onlyZero 0 = 1. onlyZero 1."
  assertEqual "non-exhaustive compile errors" [] (runCompileErrors result)
  assertSingleDiagnosticCode
    "non-exhaustive runtime code"
    "E3022"
    (runRuntimeErrors result)
  assertEqual "non-exhaustive runtime output" Nothing (runOutput result)

assertSuccessfulRuntime :: Text -> Maybe Text -> RunResult -> IO ()
assertSuccessfulRuntime label expectedOutput result = do
  assertEqual label [] (runCompileErrors result)
  assertEqual label [] (runRuntimeErrors result)
  assertEqual label expectedOutput (runOutput result)
