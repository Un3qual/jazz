{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.CaptureRecursionTests where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallFixtures.Source (sourceFixtureNoExports)
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.Support
import Jazz.Compiler.TypeInference
import Jazz.Compiler.TypeRepresentation (NumericType (..), SemanticType (..))
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate (validatedTypedProgram)
import Jazz.TestHarness (assertEqual, failTest)

testNarrowLiteralDirectCall :: IO ()
testNarrowLiteralDirectCall =
  assertCompleteProduction "narrow literal direct call" (producerEdgeFixture "narrow-literal-direct-call")

testNarrowCompositeFunctionResult :: IO ()
testNarrowCompositeFunctionResult =
  assertCompleteProduction "narrow composite function result" (producerEdgeFixture "narrow-composite-function-result")

testNarrowComparisonOperand :: IO ()
testNarrowComparisonOperand =
  assertCompleteProduction "narrow comparison operand" (producerEdgeFixture "narrow-comparison-operand")

testNarrowRootBinaryDirectCall :: IO ()
testNarrowRootBinaryDirectCall =
  assertCompleteProduction "narrow root binary direct call" (producerEdgeFixture "narrow-root-binary-direct-call")

testEquivalentScalarAliasSpecialization :: IO ()
testEquivalentScalarAliasSpecialization =
  assertCompleteProduction
    "equivalent scalar alias specialization"
    (producerEdgeFixture "equivalent-scalar-alias-specialization")

testEarlierCallerTransitiveCaptureAvailability :: IO ()
testEarlierCallerTransitiveCaptureAvailability = do
  let fixture = producerEdgeFixture "earlier-caller-transitive-recursive-capture"
      expectedFailures =
        [ TypedCoreProductionFailure
            (TypedCoreProductionStatementPath ["App", "Main"] 1)
            TypedCoreCaptureUnsupported
            (TypedCoreNameDetail "caller")
        ]
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "earlier caller transitive capture repeatability" firstRun secondRun
  assertProductionUnsupported
    "earlier caller transitive capture rejection"
    expectedFailures
    (typedCoreProductionBuildResult firstRun)

-- These programs exercise the analyzed input boundary. The former tests built
-- provisional trees with independently chosen binding and reference types.
testCapturedNumericSourcePrograms :: IO ()
testCapturedNumericSourcePrograms = mapM_ check programs
  where
    programs =
      [ ("captured alias", "copy = seed. copy.", [("copy", narrow)]),
        ("captured composite", "copy = seed + 1. copy.", [("copy", narrow)]),
        ("captured comparison", "seed > 0.", []),
        ("captured function", "bump :: UInt8 -> UInt8. bump = \\(item) -> item + seed. bump 2.", [("bump", SemanticFunction narrow narrow)]),
        ("captured callable argument", "apply :: (UInt8 -> UInt8) -> UInt8. apply = \\(callback) -> callback seed. apply (\\(item) -> item + 1).", [("apply", SemanticFunction (SemanticFunction narrow narrow) narrow)]),
        ("captured callable result", "get :: UInt8 -> UInt8. get = \\(item) -> seed. copy = get 1. copy.", [("copy", narrow)]),
        ("captured tuple", "(seed, loop 1).", []),
        ("captured anonymous callable", "\\(item) -> item + seed.", []),
        ("capture after signatures", "copy :: UInt8. copy = seed. copy.", [("copy", narrow)])
      ]
    narrow = SemanticNumeric NumericUInt8
    check (label, ending, expectedTypes) = do
      let source =
            Text.unlines
              [ "seed :: UInt8. seed = 1.",
                "loop :: UInt8 -> UInt8.",
                "loop = \\(item) -> loop seed.",
                ending
              ]
          fixture = sourceFixtureNoExports label source
      production <- produceFixture fixture
      case typedCoreProductionBuildResult production of
        TypedCoreProductionSucceeded validated -> do
          let TypedProgram _ modules _ = validatedTypedProgram validated
              statements = concat [body | TypedModule _ _ _ _ _ _ body _ <- modules]
              bindingTypes = Map.fromList [(name, expressionType) | TypedLetStatement _ (TypedResolvedName _ _ name) _ (TypedScheme _ _ _ _ expressionType _ _) _ <- statements]
          assertEqual (label <> " captured width") (Just narrow) (Map.lookup "seed" bindingTypes)
          mapM_ (\(name, expected) -> assertEqual (label <> " binding " <> name) (Just expected) (Map.lookup name bindingTypes)) expectedTypes
          assertCompleteProduction label fixture
        result -> failTest (label <> " did not produce typed core: " <> Text.pack (show result) <> " " <> Text.pack (show (typedCoreProductionInferenceResult production)))
