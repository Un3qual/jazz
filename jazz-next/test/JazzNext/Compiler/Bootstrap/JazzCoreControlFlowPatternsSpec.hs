{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import JazzNext.Compiler.Bootstrap.JazzCoreParity
  ( expectedControlFlowPatternsBatchRendering,
    expectedControlFlowPatternsSourceBatchRendering,
    runJazzControlFlowPatternsBatch,
    runJazzControlFlowPatternsSourceBatch,
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runCompileErrors,
    runRuntimeErrors,
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource,
  )
import JazzNext.Compiler.Parser.AST
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    failTest,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "JazzCoreControlFlowPatterns" tests

tests :: [NamedTest]
tests =
  [ ("matches stage 0 for conditions, cases, and every pattern", testControlFlowParity),
    ("matches stage 0 through the hosted parser twice", testComposedParity),
    ("rejects later-child forms at every nested boundary", testUnsupportedBoundary)
  ]

testControlFlowParity :: IO ()
testControlFlowParity = do
  assertEqual "direct fixture names" expectedControlFlowFixtureNames (map fst controlFlowFixtures)
  expected <- expectRight "control-flow expected values" (expectedControlFlowPatternsBatchRendering controlFlowExpressions)
  assertContains "fractional literal pattern" "CoreLiteralPattern(CoreFractionalLiteral(\"1\", \"5\", Nothing))" expected
  assertContains "guarded case arm" "CoreCaseArm(CoreVariablePattern(CoreSourceName(\"item\")), Just(CoreVariableExpression(CoreSourceName(\"keep\")))" expected
  assertContains
    "mixed parameter keeps source index two"
    "CoreLambdaExpression(CoreGeneratedName(CoreLambdaPatternArgument(2)), CorePatternCaseExpression(CoreVariableExpression(CoreGeneratedName(CoreLambdaPatternArgument(2)))"
    expected
  assertContains
    "first pattern parameter uses index one"
    "CoreLambdaExpression(CoreGeneratedName(CoreLambdaPatternArgument(1)), CorePatternCaseExpression(CoreVariableExpression(CoreGeneratedName(CoreLambdaPatternArgument(1)))"
    expected
  assertContains
    "second pattern parameter uses index two"
    "CoreLambdaExpression(CoreGeneratedName(CoreLambdaPatternArgument(2)), CorePatternCaseExpression(CoreVariableExpression(CoreGeneratedName(CoreLambdaPatternArgument(2)))"
    expected
  first <- runJazzControlFlowPatternsBatch controlFlowExpressions
  second <- runJazzControlFlowPatternsBatch controlFlowExpressions
  assertSuccessfulOutput "control-flow parity first run" expected first
  assertSuccessfulOutput "control-flow parity second run" expected second
  assertEqual "control-flow parity deterministic output" (runOutput first) (runOutput second)

testComposedParity :: IO ()
testComposedParity = do
  assertEqual "composed fixture count" 14 (length composedSources)
  expected <- expectRight "composed control-flow expected values" (expectedControlFlowPatternsSourceBatchRendering composedSources)
  first <- runJazzControlFlowPatternsSourceBatch composedSources
  second <- runJazzControlFlowPatternsSourceBatch composedSources
  assertSuccessfulOutput "composed control-flow first run" expected first
  assertSuccessfulOutput "composed control-flow second run" expected second
  assertEqual "composed control-flow deterministic output" (runOutput first) (runOutput second)

testUnsupportedBoundary :: IO ()
testUnsupportedBoundary = do
  assertEqual "unsupported fixture names" expectedUnsupportedFixtureNames (map fst unsupportedFixtures)
  assertEqual "unsupported fixture count" 12 (length unsupportedExpressions)
  first <- runJazzControlFlowPatternsBatch unsupportedExpressions
  second <- runJazzControlFlowPatternsBatch unsupportedExpressions
  let expected = Just ("[" <> Text.intercalate ", " (replicate 12 "Nothing") <> "]")
  assertEqual "unsupported first compile errors" [] (runCompileErrors first)
  assertEqual "unsupported first runtime errors" [] (runRuntimeErrors first)
  assertEqual "unsupported first output" expected (runOutput first)
  assertEqual "unsupported second compile errors" [] (runCompileErrors second)
  assertEqual "unsupported second runtime errors" [] (runRuntimeErrors second)
  assertEqual "unsupported second output" expected (runOutput second)
  assertEqual "unsupported deterministic output" (runOutput first) (runOutput second)

expectedControlFlowFixtureNames :: [Text.Text]
expectedControlFlowFixtureNames =
  [ "if-basic",
    "if-nested",
    "if-collection-branches",
    "if-block-branches",
    "case-empty-arms",
    "case-pattern-inventory",
    "case-guarded",
    "case-nested-scrutinee",
    "case-nested-body",
    "lambda-identifier",
    "lambda-identifiers-multiple",
    "lambda-pattern-wildcard",
    "lambda-pattern-composite",
    "lambda-pattern-or",
    "lambda-mixed-parameters",
    "lambda-two-pattern-parameters",
    "lambda-nested-control-flow",
    "block-control-flow"
  ]

controlFlowExpressions :: [SurfaceExpr]
controlFlowExpressions = map snd controlFlowFixtures

controlFlowFixtures :: [(Text.Text, SurfaceExpr)]
controlFlowFixtures =
  [ ("if-basic", SEIf (seBool True) (seInt 1) (seInt 0)),
    ("if-nested", SEIf (SEVar "outer") (SEIf (SEVar "inner") (seInt 1) (seInt 2)) (seInt 3)),
    ("if-collection-branches", SEIf (SEVar "condition") (SEList [seInt 1, SEIf (seBool False) (seInt 2) (seInt 3)]) (SETuple [seInt 4, seInt 5])),
    ( "if-block-branches",
      SEIf
        (SEVar "condition")
        (SEBlock [SSLet "left" span1 (seInt 1), SSExpr span1 (SEVar "left")])
        (SEBlock [SSLet "right" span1 (seInt 2), SSExpr span1 (SEVar "right")])
    ),
    ("case-empty-arms", SECase (SEVar "value") []),
    ("case-pattern-inventory", patternInventory),
    ( "case-guarded",
      SECase
        (SEVar "value")
        [ SurfaceCaseArm (SPVariable "item") (Just (SEVar "keep")) (SEVar "item"),
          SurfaceCaseArm SPWildcard Nothing (seInt 0)
        ]
    ),
    ( "case-nested-scrutinee",
      SECase
        (SEIf (SEVar "condition") (SEVar "left") (SEVar "right"))
        [SurfaceCaseArm SPWildcard Nothing (seInt 0)]
    ),
    ( "case-nested-body",
      SECase
        (SEVar "outer")
        [ SurfaceCaseArm
            (SPConstructor "Just" [SPVariable "item"])
            Nothing
            (SECase (SEVar "item") [SurfaceCaseArm SPWildcard Nothing (seInt 1)]),
          SurfaceCaseArm (SPConstructor "Nothing" []) Nothing (seInt 0)
        ]
    ),
    ( "lambda-identifier",
      SELambda (SurfaceLambdaIdentifier "value" :| []) (SEVar "value")
    ),
    ( "lambda-identifiers-multiple",
      SELambda
        (SurfaceLambdaIdentifier "left" :| [SurfaceLambdaIdentifier "right"])
        (SEVar "left")
    ),
    ( "lambda-pattern-wildcard",
      SELambda (SurfaceLambdaPattern SPWildcard :| []) (seInt 0)
    ),
    ( "lambda-pattern-composite",
      SELambda
        (SurfaceLambdaPattern (SPAs "whole" (SPConsList (SPVariable "head") (SPVariable "tail"))) :| [])
        (SEVar "head")
    ),
    ( "lambda-pattern-or",
      SELambda
        (SurfaceLambdaPattern (SPOr [SPConstructor "Just" [SPVariable "item"], SPConstructor "Nothing" []]) :| [])
        (SEVar "item")
    ),
    ( "lambda-mixed-parameters",
      SELambda
        ( SurfaceLambdaIdentifier "first"
            :| [ SurfaceLambdaPattern (SPConstructor "Just" [SPVariable "second"]),
                 SurfaceLambdaIdentifier "third"
               ]
        )
        (SEVar "second")
    ),
    ( "lambda-two-pattern-parameters",
      SELambda
        ( SurfaceLambdaPattern (SPList [SPVariable "head", SPVariable "tail"])
            :| [SurfaceLambdaPattern (SPTuple [SPVariable "left", SPVariable "right"])]
        )
        (SEVar "left")
    ),
    ( "lambda-nested-control-flow",
      SELambda
        (SurfaceLambdaIdentifier "value" :| [])
        ( SEIf
            (SEVar "condition")
            (SECase (SEVar "value") [SurfaceCaseArm SPWildcard Nothing (seInt 1)])
            (seInt 0)
        )
    ),
    ( "block-control-flow",
      SEBlock
        [ SSLet
            "choose"
            span1
            ( SELambda
                (SurfaceLambdaPattern (SPConstructor "Just" [SPVariable "item"]) :| [])
                (SEIf (SEVar "keep") (SEVar "item") (seInt 0))
            ),
          SSExpr span1 (SEVar "choose")
        ]
    )
  ]

patternInventory :: SurfaceExpr
patternInventory =
  SECase
    (SEVar "value")
    [ SurfaceCaseArm SPWildcard Nothing (seInt 0),
      SurfaceCaseArm (SPVariable "name") Nothing (seInt 1),
      SurfaceCaseArm (SPLiteral (SLInt 2)) Nothing (seInt 2),
      SurfaceCaseArm (SPLiteral (SLFloat 1.5 (mkFractionalLiteralSource 1 5 1) Nothing)) Nothing (seInt 3),
      SurfaceCaseArm (SPLiteral (SLBool True)) Nothing (seInt 4),
      SurfaceCaseArm (SPLiteral (SLChar 'x')) Nothing (seInt 5),
      SurfaceCaseArm (SPLiteral (SLText "Jazz")) Nothing (seInt 6),
      SurfaceCaseArm (SPConstructor "Just" [SPVariable "item"]) Nothing (seInt 7),
      SurfaceCaseArm (SPList [SPVariable "head", SPVariable "tail"]) Nothing (seInt 8),
      SurfaceCaseArm (SPConsList (SPVariable "head") (SPVariable "tail")) Nothing (seInt 9),
      SurfaceCaseArm (SPTuple []) Nothing (seInt 10),
      SurfaceCaseArm (SPTuple [SPVariable "left", SPVariable "right"]) Nothing (seInt 11),
      SurfaceCaseArm (SPAs "whole" (SPConstructor "Nothing" [])) Nothing (seInt 12),
      SurfaceCaseArm (SPOr [SPConstructor "Just" [SPVariable "item"], SPConstructor "Nothing" []]) Nothing (seInt 13)
    ]

composedSources :: [Text.Text]
composedSources =
  [ "if True then 1 else 0.",
    "if outer then if inner then 1 else 2 else 3.",
    "if cond then { result = 1. result. } else { result = 2. result. }.",
    "case subject { | Just item -> item | Nothing -> 0 }.",
    "case subject { | Just item if keep -> item | Nothing -> 0 }.",
    "case subject { | _ -> 0 | name -> 1 | 2 -> 2 | 'x' -> 3 | \"x\" -> 4 | True -> 5 | Just item -> 6 | [head, tail] -> 7 | [head | tail] -> 8 | () -> 9 | (left, right) -> 10 | whole@Nothing -> 11 | Just item | Nothing -> 12 }.",
    "case if cond then left else right { | _ -> 0 }.",
    "case outer { | Just item -> case item { | _ -> 1 } | Nothing -> 0 }.",
    "\\(subject) -> subject.",
    "\\(left, right) -> left.",
    "\\([head | tail]) -> head.",
    "\\(Just item | Nothing) -> item.",
    "\\(first, Just second, third) -> second.",
    "{ loop = \\(subject) -> case subject { | Just next -> loop next | _ -> if False then subject else subject }. loop. }."
  ]

expectedUnsupportedFixtureNames :: [Text.Text]
expectedUnsupportedFixtureNames =
  [ "type-application-root",
    "type-application-condition",
    "type-application-case-scrutinee",
    "type-application-case-guard",
    "type-application-lambda-body",
    "dollar-case-body",
    "signature-if-block",
    "data-case-block",
    "class-lambda-block",
    "impl-lambda-block",
    "operator-storage-nested-block",
    "module-import-nested-block"
  ]

unsupportedFixtures :: [(Text.Text, SurfaceExpr)]
unsupportedFixtures =
  [ ("type-application-root", SETypeApplication (SEVar "identity") span1 SurfaceTypeInt),
    ("type-application-condition", SEIf (SETypeApplication (SEVar "condition") span1 SurfaceTypeBool) (seInt 1) (seInt 0)),
    ( "type-application-case-scrutinee",
      SECase
      (SETypeApplication (SEVar "identity") span1 SurfaceTypeInt)
      [SurfaceCaseArm SPWildcard Nothing (seInt 0)]
    ),
    ( "type-application-case-guard",
      SECase
        (SEVar "value")
        [SurfaceCaseArm SPWildcard (Just (SETypeApplication (SEVar "keep") span1 SurfaceTypeBool)) (seInt 0)]
    ),
    ( "type-application-lambda-body",
      SELambda
        (SurfaceLambdaIdentifier "value" :| [])
        (SETypeApplication (SEVar "identity") span1 SurfaceTypeInt)
    ),
    ( "dollar-case-body",
      SECase
        (SEVar "value")
        [SurfaceCaseArm SPWildcard Nothing (SEBinary "$" (SEVar "function") (seInt 1))]
    ),
    ( "signature-if-block",
      SEIf
        (SEVar "condition")
        (SEBlock [SSSignature "value" span1 (SurfaceSignatureType SurfaceTypeInt)])
        (seInt 0)
    ),
    ( "data-case-block",
      SECase
        (SEVar "value")
        [SurfaceCaseArm SPWildcard Nothing (SEBlock [SSData span1 "Thing" [] []])]
    ),
    ( "class-lambda-block",
      SELambda
        (SurfaceLambdaIdentifier "value" :| [])
        (SEBlock [SSClass span1 "Show" ["a"] []])
    ),
    ( "impl-lambda-block",
      SELambda
        (SurfaceLambdaIdentifier "value" :| [])
        (SEBlock [SSImpl span1 "Show" [SurfaceTypeText] []])
    ),
    ( "operator-storage-nested-block",
      SEBlock
        [SSLet "nested" span1 (SEBlock [SSLet "$operator:2B" span1 (SEVar "add")])]
    ),
    ( "module-import-nested-block",
      SEBlock
        [ SSModule span1 ["App", "Main"] Nothing,
          SSImport span1 ["Core", "Text"] Nothing Nothing,
          SSExpr span1 (seInt 0)
        ]
    )
  ]

unsupportedExpressions :: [SurfaceExpr]
unsupportedExpressions = map snd unsupportedFixtures

span1 :: SourceSpan
span1 = SourceSpan 1 1

seInt :: Integer -> SurfaceExpr
seInt = SELit . SLInt

seBool :: Bool -> SurfaceExpr
seBool = SELit . SLBool

assertSuccessfulOutput :: Text.Text -> Text.Text -> RunResult -> IO ()
assertSuccessfulOutput label expected result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " output") (Just expected) (runOutput result)

expectRight :: Show err => Text.Text -> Either err value -> IO value
expectRight label value =
  case value of
    Left err -> failTest (label <> ": expected Right, got Left " <> Text.pack (show err))
    Right ok -> pure ok
