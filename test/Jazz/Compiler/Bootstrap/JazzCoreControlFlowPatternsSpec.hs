{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.JazzCoreParity
  ( expectedControlFlowPatternsBatchRendering,
    expectedControlFlowPatternsSourceBatchRendering,
    runJazzControlFlowPatternsBatch,
    runJazzControlFlowPatternsSourceBatch,
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Driver
  ( RunResult,
    runCompileErrors,
    runOutput,
    runRuntimeErrors,
  )
import Jazz.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource,
  )
import Jazz.Compiler.Name (Identifier)
import Jazz.Compiler.Parser.AST
import Jazz.Compiler.TypeRepresentation
  ( SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.TestHarness
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
  assertContains
    "pattern lambda clauses share one ordered tuple case"
    "CorePatternCaseExpression(CoreTupleExpression([CoreVariableExpression(CoreGeneratedName(CoreLambdaPatternArgument(1))), CoreVariableExpression(CoreGeneratedName(CoreLambdaPatternArgument(2)))]), [CoreCaseArm(CoreTuplePattern([CoreConstructorPattern(CoreSourceName(\"Nothing\"), []), CoreVariablePattern(CoreSourceName(\"fallback\"))])"
    expected
  first <- runJazzControlFlowPatternsBatch controlFlowExpressions
  second <- runJazzControlFlowPatternsBatch controlFlowExpressions
  assertSuccessfulOutput "control-flow parity first run" expected first
  assertSuccessfulOutput "control-flow parity second run" expected second
  assertEqual "control-flow parity deterministic output" (runOutput first) (runOutput second)

testComposedParity :: IO ()
testComposedParity = do
  expected <- expectRight "composed control-flow expected values" (expectedControlFlowPatternsSourceBatchRendering composedSources)
  first <- runJazzControlFlowPatternsSourceBatch composedSources
  second <- runJazzControlFlowPatternsSourceBatch composedSources
  assertSuccessfulOutput "composed control-flow first run" expected first
  assertSuccessfulOutput "composed control-flow second run" expected second
  assertEqual "composed control-flow deterministic output" (runOutput first) (runOutput second)

testUnsupportedBoundary :: IO ()
testUnsupportedBoundary = do
  assertEqual "unsupported fixture names" expectedUnsupportedFixtureNames (map fst unsupportedFixtures)
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
    "pattern-lambda-clauses",
    "lambda-nested-control-flow",
    "block-control-flow"
  ]

controlFlowExpressions :: [SurfaceExpr]
controlFlowExpressions = map snd controlFlowFixtures

controlFlowFixtures :: [(Text.Text, SurfaceExpr)]
controlFlowFixtures =
  [ ("if-basic", seIf (seBool True) (seInt 1) (seInt 0)),
    ("if-nested", seIf (seVar "outer") (seIf (seVar "inner") (seInt 1) (seInt 2)) (seInt 3)),
    ("if-collection-branches", seIf (seVar "condition") (seList [seInt 1, seIf (seBool False) (seInt 2) (seInt 3)]) (seTuple [seInt 4, seInt 5])),
    ( "if-block-branches",
      seIf
        (seVar "condition")
        (seBlock [SSLet "left" span1 (seInt 1), SSExpr span1 (seVar "left")])
        (seBlock [SSLet "right" span1 (seInt 2), SSExpr span1 (seVar "right")])
    ),
    ("case-empty-arms", seCase (seVar "value") []),
    ("case-pattern-inventory", patternInventory),
    ( "case-guarded",
      seCase
        (seVar "value")
        [ SurfaceCaseArm (spVariable "item") (Just (seVar "keep")) (seVar "item"),
          SurfaceCaseArm spWildcard Nothing (seInt 0)
        ]
    ),
    ( "case-nested-scrutinee",
      seCase
        (seIf (seVar "condition") (seVar "left") (seVar "right"))
        [SurfaceCaseArm spWildcard Nothing (seInt 0)]
    ),
    ( "case-nested-body",
      seCase
        (seVar "outer")
        [ SurfaceCaseArm
            (spConstructor "Just" [spVariable "item"])
            Nothing
            (seCase (seVar "item") [SurfaceCaseArm spWildcard Nothing (seInt 1)]),
          SurfaceCaseArm (spConstructor "Nothing" []) Nothing (seInt 0)
        ]
    ),
    ( "lambda-identifier",
      seLambda (SurfaceLambdaIdentifier span1 "value" :| []) (seVar "value")
    ),
    ( "lambda-identifiers-multiple",
      seLambda
        (SurfaceLambdaIdentifier span1 "left" :| [SurfaceLambdaIdentifier span1 "right"])
        (seVar "left")
    ),
    ( "lambda-pattern-wildcard",
      seLambda (SurfaceLambdaPattern spWildcard :| []) (seInt 0)
    ),
    ( "lambda-pattern-composite",
      seLambda
        (SurfaceLambdaPattern (spAs "whole" (spConsList (spVariable "head") (spVariable "tail"))) :| [])
        (seVar "head")
    ),
    ( "lambda-pattern-or",
      seLambda
        (SurfaceLambdaPattern (spOr [spConstructor "Just" [spVariable "item"], spConstructor "Nothing" []]) :| [])
        (seVar "item")
    ),
    ( "lambda-mixed-parameters",
      seLambda
        ( SurfaceLambdaIdentifier span1 "first"
            :| [ SurfaceLambdaPattern (spConstructor "Just" [spVariable "second"]),
                 SurfaceLambdaIdentifier span1 "third"
               ]
        )
        (seVar "second")
    ),
    ( "lambda-two-pattern-parameters",
      seLambda
        ( SurfaceLambdaPattern (spList [spVariable "head", spVariable "tail"])
            :| [SurfaceLambdaPattern (spTuple [spVariable "left", spVariable "right"])]
        )
        (seVar "left")
    ),
    ( "pattern-lambda-clauses",
      sePatternLambda
        ( SurfacePatternLambdaClause
            span1
            (spConstructor "Nothing" [] :| [spVariable "fallback"])
            (seVar "fallback")
            :| [ SurfacePatternLambdaClause
                   span1
                   (spConstructor "Just" [spVariable "item"] :| [spWildcard])
                   (seVar "item")
               ]
        )
    ),
    ( "lambda-nested-control-flow",
      seLambda
        (SurfaceLambdaIdentifier span1 "value" :| [])
        ( seIf
            (seVar "condition")
            (seCase (seVar "value") [SurfaceCaseArm spWildcard Nothing (seInt 1)])
            (seInt 0)
        )
    ),
    ( "block-control-flow",
      seBlock
        [ SSLet
            "choose"
            span1
            ( seLambda
                (SurfaceLambdaPattern (spConstructor "Just" [spVariable "item"]) :| [])
                (seIf (seVar "keep") (seVar "item") (seInt 0))
            ),
          SSExpr span1 (seVar "choose")
        ]
    )
  ]

patternInventory :: SurfaceExpr
patternInventory =
  seCase
    (seVar "value")
    [ SurfaceCaseArm spWildcard Nothing (seInt 0),
      SurfaceCaseArm (spVariable "name") Nothing (seInt 1),
      SurfaceCaseArm (spLiteral (SLInt 2)) Nothing (seInt 2),
      SurfaceCaseArm (spLiteral (SLFloat 1.5 (mkFractionalLiteralSource 1 5 1) Nothing)) Nothing (seInt 3),
      SurfaceCaseArm (spLiteral (SLBool True)) Nothing (seInt 4),
      SurfaceCaseArm (spLiteral (SLChar 'x')) Nothing (seInt 5),
      SurfaceCaseArm (spLiteral (SLText "Jazz")) Nothing (seInt 6),
      SurfaceCaseArm (spConstructor "Just" [spVariable "item"]) Nothing (seInt 7),
      SurfaceCaseArm (spList [spVariable "head", spVariable "tail"]) Nothing (seInt 8),
      SurfaceCaseArm (spConsList (spVariable "head") (spVariable "tail")) Nothing (seInt 9),
      SurfaceCaseArm (spTuple []) Nothing (seInt 10),
      SurfaceCaseArm (spTuple [spVariable "left", spVariable "right"]) Nothing (seInt 11),
      SurfaceCaseArm (spAs "whole" (spConstructor "Nothing" [])) Nothing (seInt 12),
      SurfaceCaseArm (spOr [spConstructor "Just" [spVariable "item"], spConstructor "Nothing" []]) Nothing (seInt 13)
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
    "\\|(Nothing, fallback) -> fallback |(Just item, _) -> item.",
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
  [ ("type-application-root", seTypeApplication (seVar "identity") span1 TypeInt),
    ("type-application-condition", seIf (seTypeApplication (seVar "condition") span1 TypeBool) (seInt 1) (seInt 0)),
    ( "type-application-case-scrutinee",
      seCase
        (seTypeApplication (seVar "identity") span1 TypeInt)
        [SurfaceCaseArm spWildcard Nothing (seInt 0)]
    ),
    ( "type-application-case-guard",
      seCase
        (seVar "value")
        [SurfaceCaseArm spWildcard (Just (seTypeApplication (seVar "keep") span1 TypeBool)) (seInt 0)]
    ),
    ( "type-application-lambda-body",
      seLambda
        (SurfaceLambdaIdentifier span1 "value" :| [])
        (seTypeApplication (seVar "identity") span1 TypeInt)
    ),
    ( "dollar-case-body",
      seCase
        (seVar "value")
        [SurfaceCaseArm spWildcard Nothing (seBinary "$" (seVar "function") (seInt 1))]
    ),
    ( "signature-if-block",
      seIf
        (seVar "condition")
        (seBlock [SSSignature "value" span1 (SignatureType TypeInt)])
        (seInt 0)
    ),
    ( "data-case-block",
      seCase
        (seVar "value")
        [SurfaceCaseArm spWildcard Nothing (seBlock [SSData span1 "Thing" [] []])]
    ),
    ( "class-lambda-block",
      seLambda
        (SurfaceLambdaIdentifier span1 "value" :| [])
        (seBlock [SSClass span1 "Show" ["a"] []])
    ),
    ( "impl-lambda-block",
      seLambda
        (SurfaceLambdaIdentifier span1 "value" :| [])
        (seBlock [SSImpl span1 "Show" [TypeText] []])
    ),
    ( "operator-storage-nested-block",
      seBlock
        [SSLet "nested" span1 (seBlock [SSLet "$operator:2B" span1 (seVar "add")])]
    ),
    ( "module-import-nested-block",
      seBlock
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

se :: SurfaceExprForm -> SurfaceExpr
se = SurfaceExpr span1

sp :: SurfacePatternForm -> SurfacePattern
sp = SurfacePattern span1

seBinary :: Text.Text -> SurfaceExpr -> SurfaceExpr -> SurfaceExpr
seBinary operator left right = se (SEBinary operator left right)

seBlock :: [SurfaceStatement] -> SurfaceExpr
seBlock = se . SEBlock

seCase :: SurfaceExpr -> [SurfaceCaseArm] -> SurfaceExpr
seCase scrutinee arms = se (SECase scrutinee arms)

seIf :: SurfaceExpr -> SurfaceExpr -> SurfaceExpr -> SurfaceExpr
seIf condition thenBranch elseBranch = se (SEIf condition thenBranch elseBranch)

seLambda :: NonEmpty SurfaceLambdaParameter -> SurfaceExpr -> SurfaceExpr
seLambda parameters body = se (SELambda parameters body)

seList :: [SurfaceExpr] -> SurfaceExpr
seList = se . SEList

sePatternLambda :: NonEmpty SurfacePatternLambdaClause -> SurfaceExpr
sePatternLambda = se . SEPatternLambda

seTuple :: [SurfaceExpr] -> SurfaceExpr
seTuple = se . SETuple

seTypeApplication :: SurfaceExpr -> SourceSpan -> SurfaceSignatureType -> SurfaceExpr
seTypeApplication function typeApplicationSpan signatureType =
  se (SETypeApplication function typeApplicationSpan signatureType)

seVar :: Identifier -> SurfaceExpr
seVar = se . SEVar

spAs :: Identifier -> SurfacePattern -> SurfacePattern
spAs name patternValue = sp (SPAs name patternValue)

spConsList :: SurfacePattern -> SurfacePattern -> SurfacePattern
spConsList headPattern tailPattern = sp (SPConsList headPattern tailPattern)

spConstructor :: Identifier -> [SurfacePattern] -> SurfacePattern
spConstructor name arguments = sp (SPConstructor name arguments)

spList :: [SurfacePattern] -> SurfacePattern
spList = sp . SPList

spLiteral :: SurfaceLiteral -> SurfacePattern
spLiteral = sp . SPLiteral

spOr :: [SurfacePattern] -> SurfacePattern
spOr = sp . SPOr

spTuple :: [SurfacePattern] -> SurfacePattern
spTuple = sp . SPTuple

spVariable :: Identifier -> SurfacePattern
spVariable = sp . SPVariable

spWildcard :: SurfacePattern
spWildcard = sp SPWildcard

seInt :: Integer -> SurfaceExpr
seInt = se . SELit . SLInt

seBool :: Bool -> SurfaceExpr
seBool = se . SELit . SLBool

assertSuccessfulOutput :: Text.Text -> Text.Text -> RunResult -> IO ()
assertSuccessfulOutput label expected result = do
  assertEqual (label <> " compile errors") [] (runCompileErrors result)
  assertEqual (label <> " runtime errors") [] (runRuntimeErrors result)
  assertEqual (label <> " output") (Just expected) (runOutput result)

expectRight :: (Show err) => Text.Text -> Either err value -> IO value
expectRight label value =
  case value of
    Left err -> failTest (label <> ": expected Right, got Left " <> Text.pack (show err))
    Right ok -> pure ok
