{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.CaptureRecursionTests where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (Literal (..), NumericType (NumericUInt8))
import Jazz.Compiler.Bootstrap.TypedCoreExpressionDirectCallSpec.BoundaryTests
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.LoweredIR.Lower
import Jazz.Compiler.LoweredIR.Validate (validateLoweredProgram)
import Jazz.Compiler.TypeInference hiding (InferenceResult (..))
import Jazz.Compiler.TypeInference.Elaboration
  ( finalizeValidatedTypedCoreExpressionDirectCall,
    typedCoreProductionOutcomeStatus,
  )
import Jazz.Compiler.TypeInference.Elaboration.Types
  ( ProvisionalCallableDeclaration (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
  )
import Jazz.Compiler.TypeInference.State (initialInferState)
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType (TBoolType, TFunctionType, TIntegerLiteralType, TNumericType),
    IntegerLiteralRange (..),
    TypeBinding (PlainTypeBinding),
  )
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Validate (validateTypedProgram)
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
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionStatementPath ["App", "Main"] 1)
              TypedCoreCaptureUnsupported
              (TypedCoreNameDetail "caller")
          ]
  firstRun <- produceFixture fixture
  secondRun <- produceFixture fixture
  assertEqual "earlier caller transitive capture repeatability" firstRun secondRun
  assertEqual "earlier caller transitive capture rejection" expected (typedCoreProductionStatus firstRun)

testCapturedNumericScalarReferenceSpecialization :: IO ()
testCapturedNumericScalarReferenceSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [1])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalScalarBinding
              2
              "copy"
              spanValue
              literalType
              (ProvisionalVariableExpression "seed" literalType),
            ProvisionalTerminalExpression
              3
              spanValue
              ( ProvisionalApplyExpression
                  uint8Type
                  (ProvisionalVariableExpression "loop" functionType)
                  (ProvisionalLiteralExpression (LInt 1) uint8Type)
              )
          ]
  assertProvisionalProductionCompletes "captured numeric scalar" provisionalScope

testCapturedCompositeScalarSpecialization :: IO ()
testCapturedCompositeScalarSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [1])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              2
              spanValue
              ( ProvisionalBinaryExpression
                  "+"
                  literalType
                  literalType
                  (ProvisionalVariableExpression "seed" literalType)
                  (ProvisionalLiteralExpression (LInt 1) literalType)
              )
          ]
  assertProvisionalProductionCompletes "captured composite scalar specialization" provisionalScope

testCapturedCompositeScalarBinderSpecialization :: IO ()
testCapturedCompositeScalarBinderSpecialization = do
  let spanValue = SourceSpan 1 1
      seedType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      otherType = TIntegerLiteralType (IntegerLiteralRange 2 2)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          2
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [2])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              seedType
              (ProvisionalLiteralExpression (LInt 1) seedType),
            ProvisionalScalarBinding
              1
              "other"
              spanValue
              otherType
              (ProvisionalLiteralExpression (LInt 2) otherType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              3
              spanValue
              ( ProvisionalBinaryExpression
                  "+"
                  seedType
                  seedType
                  (ProvisionalVariableExpression "seed" seedType)
                  (ProvisionalVariableExpression "other" otherType)
              )
          ]
  assertProvisionalProductionCompletes "captured composite scalar binder specialization" provisionalScope

testCapturedComparisonResultSpecialization :: IO ()
testCapturedComparisonResultSpecialization = do
  let spanValue = SourceSpan 1 1
      seedType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      otherType = TIntegerLiteralType (IntegerLiteralRange 2 2)
      comparisonOperandType = TIntegerLiteralType (IntegerLiteralRange 1 2)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [1])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              seedType
              (ProvisionalLiteralExpression (LInt 1) seedType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalScalarBinding
              2
              "flag"
              spanValue
              TBoolType
              ( ProvisionalBinaryExpression
                  "<"
                  TBoolType
                  comparisonOperandType
                  (ProvisionalVariableExpression "seed" seedType)
                  (ProvisionalLiteralExpression (LInt 2) otherType)
              ),
            ProvisionalTerminalExpression
              3
              spanValue
              (ProvisionalVariableExpression "flag" TBoolType)
          ]
  assertProvisionalProductionCompletes "captured comparison result specialization" provisionalScope

testCapturedFunctionBodySpecialization :: IO ()
testCapturedFunctionBodySpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalLiteralExpression (LInt 1) literalType)
                  )
              ),
            ProvisionalTerminalExpression
              3
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "helper" helperFunctionType)
                  (ProvisionalLiteralExpression (LInt 1) literalType)
              )
          ]
  assertProvisionalProductionCompletes "captured function body specialization" provisionalScope

testCapturedFunctionParameterSpecialization :: IO ()
testCapturedFunctionParameterSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalTerminalExpression
              3
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "helper" helperFunctionType)
                  (ProvisionalLiteralExpression (LInt 1) literalType)
              )
          ]
  assertProvisionalProductionTypes
    "captured function parameter specialization"
    [("helper", typedUInt8UnaryType)]
    Nothing
    provisionalScope

testCapturedCallableParameterApplicationSpecialization :: IO ()
testCapturedCallableParameterApplicationSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      callbackFunctionType = TFunctionType literalType literalType
      helperFunctionType = TFunctionType callbackFunctionType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "function"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      ( ProvisionalApplyExpression
                          literalType
                          (ProvisionalVariableExpression "function" callbackFunctionType)
                          (ProvisionalVariableExpression "seed" literalType)
                      )
                      (ProvisionalVariableExpression "seed" literalType)
                  )
              ),
            ProvisionalTerminalExpression
              3
              spanValue
              (ProvisionalVariableExpression "helper" helperFunctionType)
          ]
  assertProvisionalProductionTypes
    "captured callable parameter application specialization"
    [("helper", typedUInt8HigherOrderType)]
    (Just typedUInt8HigherOrderType)
    provisionalScope

testCapturedFunctionScalarBinderSpecialization :: IO ()
testCapturedFunctionScalarBinderSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          2
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [2])
      helperDeclaration =
        ProvisionalCallableDeclaration
          3
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalScalarBinding
              1
              "other"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 2) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "other" literalType)
                  )
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "helper" helperFunctionType)
                  (ProvisionalLiteralExpression (LInt 1) literalType)
              )
          ]
  assertProvisionalProductionTypes
    "captured function scalar binder specialization"
    [("other", typedUInt8Type), ("helper", typedIntToUInt8Type)]
    Nothing
    provisionalScope

testCapturedFunctionArgumentScalarBinderSpecialization :: IO ()
testCapturedFunctionArgumentScalarBinderSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      otherType = TIntegerLiteralType (IntegerLiteralRange 2 2)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          2
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [2])
      helperDeclaration =
        ProvisionalCallableDeclaration
          3
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalScalarBinding
              1
              "other"
              spanValue
              otherType
              (ProvisionalLiteralExpression (LInt 2) otherType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "helper" helperFunctionType)
                  (ProvisionalVariableExpression "other" otherType)
              )
          ]
  assertProvisionalProductionTypes
    "captured function argument scalar binder specialization"
    [("other", typedUInt8Type), ("helper", typedUInt8UnaryType)]
    Nothing
    provisionalScope

testCapturedFunctionResultScalarBinderSpecialization :: IO ()
testCapturedFunctionResultScalarBinderSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalScalarBinding
              3
              "result"
              spanValue
              literalType
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "helper" helperFunctionType)
                  (ProvisionalLiteralExpression (LInt 1) literalType)
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              (ProvisionalVariableExpression "result" literalType)
          ]
  assertProvisionalProductionTypes
    "captured function result scalar binder specialization"
    [("helper", typedUInt8UnaryType), ("result", typedUInt8Type)]
    Nothing
    provisionalScope

testCapturedHigherOrderCallableArgumentSpecialization :: IO ()
testCapturedHigherOrderCallableArgumentSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      applyFunctionType = TFunctionType helperFunctionType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      applyDeclaration =
        ProvisionalCallableDeclaration
          3
          "apply"
          spanValue
          applyFunctionType
          (Just (PlainTypeBinding applyFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalFunctionBinding
              applyDeclaration
              ( ProvisionalLambdaExpression
                  "function"
                  applyFunctionType
                  ( ProvisionalApplyExpression
                      literalType
                      (ProvisionalVariableExpression "function" helperFunctionType)
                      (ProvisionalLiteralExpression (LInt 1) literalType)
                  )
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "apply" applyFunctionType)
                  (ProvisionalVariableExpression "helper" helperFunctionType)
              )
          ]
  assertProvisionalProductionTypes
    "captured higher-order callable argument specialization"
    [("helper", typedUInt8UnaryType), ("apply", typedUInt8HigherOrderType)]
    Nothing
    provisionalScope

testCapturedForwardedHigherOrderCallableArgumentSpecialization :: IO ()
testCapturedForwardedHigherOrderCallableArgumentSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      applyFunctionType = TFunctionType helperFunctionType literalType
      forwardFunctionType = TFunctionType helperFunctionType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      applyDeclaration =
        ProvisionalCallableDeclaration
          3
          "apply"
          spanValue
          applyFunctionType
          (Just (PlainTypeBinding applyFunctionType))
          Nothing
      forwardDeclaration =
        ProvisionalCallableDeclaration
          4
          "forward"
          spanValue
          forwardFunctionType
          (Just (PlainTypeBinding forwardFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalFunctionBinding
              applyDeclaration
              ( ProvisionalLambdaExpression
                  "function"
                  applyFunctionType
                  ( ProvisionalApplyExpression
                      literalType
                      (ProvisionalVariableExpression "function" helperFunctionType)
                      (ProvisionalLiteralExpression (LInt 1) literalType)
                  )
              ),
            ProvisionalFunctionBinding
              forwardDeclaration
              ( ProvisionalLambdaExpression
                  "function"
                  forwardFunctionType
                  ( ProvisionalApplyExpression
                      literalType
                      (ProvisionalVariableExpression "apply" applyFunctionType)
                      (ProvisionalVariableExpression "function" helperFunctionType)
                  )
              ),
            ProvisionalTerminalExpression
              5
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "forward" forwardFunctionType)
                  (ProvisionalVariableExpression "helper" helperFunctionType)
              )
          ]
  assertProvisionalProductionTypes
    "captured forwarded higher-order callable argument specialization"
    [ ("helper", typedUInt8UnaryType),
      ("apply", typedUInt8HigherOrderType),
      ("forward", typedUInt8HigherOrderType)
    ]
    Nothing
    provisionalScope

testCapturedTerminalAnonymousCallableSpecialization :: IO ()
testCapturedTerminalAnonymousCallableSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      anonymousFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              2
              spanValue
              ( ProvisionalLambdaExpression
                  "item"
                  anonymousFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              )
          ]
  assertProvisionalProductionTypes
    "captured terminal anonymous callable specialization"
    []
    (Just typedUInt8UnaryType)
    provisionalScope

testCapturedNamedCallerSpecialization :: IO ()
testCapturedNamedCallerSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      recursiveFunctionType = TFunctionType uint8Type uint8Type
      helperFunctionType = TFunctionType literalType literalType
      consumerFunctionType = TFunctionType literalType literalType
      loopDeclaration =
        ProvisionalCallableDeclaration
          1
          "loop"
          spanValue
          recursiveFunctionType
          (Just (PlainTypeBinding recursiveFunctionType))
          (Just [1])
      helperDeclaration =
        ProvisionalCallableDeclaration
          2
          "helper"
          spanValue
          helperFunctionType
          (Just (PlainTypeBinding helperFunctionType))
          Nothing
      consumerDeclaration =
        ProvisionalCallableDeclaration
          3
          "consumer"
          spanValue
          consumerFunctionType
          (Just (PlainTypeBinding consumerFunctionType))
          Nothing
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  recursiveFunctionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" recursiveFunctionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalFunctionBinding
              helperDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  helperFunctionType
                  ( ProvisionalBinaryExpression
                      "+"
                      literalType
                      literalType
                      (ProvisionalVariableExpression "seed" literalType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalFunctionBinding
              consumerDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  consumerFunctionType
                  ( ProvisionalApplyExpression
                      literalType
                      (ProvisionalVariableExpression "helper" helperFunctionType)
                      (ProvisionalVariableExpression "item" literalType)
                  )
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              ( ProvisionalApplyExpression
                  literalType
                  (ProvisionalVariableExpression "consumer" consumerFunctionType)
                  (ProvisionalLiteralExpression (LInt 1) literalType)
              )
          ]
  assertProvisionalProductionTypes
    "captured named caller specialization"
    [("helper", typedUInt8UnaryType), ("consumer", typedUInt8UnaryType)]
    Nothing
    provisionalScope

testCapturedScalarAliasSourceSpecialization :: IO ()
testCapturedScalarAliasSourceSpecialization = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          2
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [2])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalScalarBinding
              1
              "copy"
              spanValue
              literalType
              (ProvisionalVariableExpression "seed" literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "copy" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              3
              spanValue
              ( ProvisionalApplyExpression
                  uint8Type
                  (ProvisionalVariableExpression "loop" functionType)
                  (ProvisionalLiteralExpression (LInt 1) uint8Type)
              )
          ]
  assertProvisionalProductionCompletes "captured scalar alias source specialization" provisionalScope

testRecordedScalarStatementIndices :: IO ()
testRecordedScalarStatementIndices = do
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          3
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [3])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              1
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalScalarBinding
              2
              "copy"
              spanValue
              literalType
              (ProvisionalVariableExpression "seed" literalType),
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "copy" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              ( ProvisionalApplyExpression
                  uint8Type
                  (ProvisionalVariableExpression "loop" functionType)
                  (ProvisionalLiteralExpression (LInt 1) uint8Type)
              )
          ]
  assertProvisionalProductionCompletes "recorded scalar statement indices" provisionalScope

testEagerRecursiveClosureCaptureAvailability :: IO ()
testEagerRecursiveClosureCaptureAvailability = do
  resolvedModule <- resolveFixtureModule (fixtureByName "unit-entry")
  let spanValue = SourceSpan 1 1
      literalType = TIntegerLiteralType (IntegerLiteralRange 1 1)
      uint8Type = TNumericType NumericUInt8
      functionType = TFunctionType uint8Type uint8Type
      loopDeclaration =
        ProvisionalCallableDeclaration
          3
          "loop"
          spanValue
          functionType
          (Just (PlainTypeBinding functionType))
          (Just [3])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalScalarBinding
              0
              "result"
              spanValue
              uint8Type
              ( ProvisionalApplyExpression
                  uint8Type
                  (ProvisionalVariableExpression "loop" functionType)
                  (ProvisionalLiteralExpression (LInt 1) uint8Type)
              ),
            ProvisionalScalarBinding
              1
              "seed"
              spanValue
              literalType
              (ProvisionalLiteralExpression (LInt 1) literalType),
            ProvisionalSignature 2 "loop" spanValue functionType,
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  functionType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" functionType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              4
              spanValue
              ProvisionalUnitExpression
          ]
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 0 [0])
              TypedCoreCaptureUnsupported
              (TypedCoreNameDetail "loop")
          ]
      status =
        typedCoreProductionOutcomeStatus
          ( finalizeValidatedTypedCoreExpressionDirectCall
              (TypedSourcePath "src/App/Main.jz")
              resolvedModule
              initialInferState
              provisionalScope
          )
  assertEqual "eager recursive closure capture rejection" expected status

testEagerNestedClosureCaptureAvailability :: IO ()
testEagerNestedClosureCaptureAvailability = do
  resolvedModule <- resolveFixtureModule (fixtureByName "unit-entry")
  let spanValue = SourceSpan 1 1
      uint8Type = TNumericType NumericUInt8
      callbackType = TFunctionType uint8Type uint8Type
      invokeType = TFunctionType callbackType uint8Type
      invokeDeclaration =
        ProvisionalCallableDeclaration
          0
          "invoke"
          spanValue
          invokeType
          (Just (PlainTypeBinding invokeType))
          Nothing
      loopDeclaration =
        ProvisionalCallableDeclaration
          4
          "loop"
          spanValue
          callbackType
          (Just (PlainTypeBinding callbackType))
          (Just [4])
      provisionalScope =
        ProvisionalScopeStatements
          [ ProvisionalFunctionBinding
              invokeDeclaration
              ( ProvisionalLambdaExpression
                  "callback"
                  invokeType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "callback" callbackType)
                      (ProvisionalLiteralExpression (LInt 1) uint8Type)
                  )
              ),
            ProvisionalScalarBinding
              1
              "result"
              spanValue
              uint8Type
              ( ProvisionalApplyExpression
                  uint8Type
                  (ProvisionalVariableExpression "invoke" invokeType)
                  ( ProvisionalLambdaExpression
                      "item"
                      callbackType
                      ( ProvisionalApplyExpression
                          uint8Type
                          (ProvisionalVariableExpression "loop" callbackType)
                          (ProvisionalVariableExpression "item" uint8Type)
                      )
                  )
              ),
            ProvisionalScalarBinding
              2
              "seed"
              spanValue
              uint8Type
              (ProvisionalLiteralExpression (LInt 1) uint8Type),
            ProvisionalSignature 3 "loop" spanValue callbackType,
            ProvisionalFunctionBinding
              loopDeclaration
              ( ProvisionalLambdaExpression
                  "item"
                  callbackType
                  ( ProvisionalApplyExpression
                      uint8Type
                      (ProvisionalVariableExpression "loop" callbackType)
                      (ProvisionalVariableExpression "seed" uint8Type)
                  )
              ),
            ProvisionalTerminalExpression
              5
              spanValue
              ProvisionalUnitExpression
          ]
      expected =
        TypedCoreProductionUnsupported
          [ TypedCoreProductionFailure
              (TypedCoreProductionExpressionPath ["App", "Main"] 1 [0, 1])
              TypedCoreCaptureUnsupported
              (TypedCoreNameDetail "loop")
          ]
      status =
        typedCoreProductionOutcomeStatus
          ( finalizeValidatedTypedCoreExpressionDirectCall
              (TypedSourcePath "src/App/Main.jz")
              resolvedModule
              initialInferState
              provisionalScope
          )
  assertEqual "eager nested closure capture rejection" expected status

assertProvisionalProductionCompletes :: Text -> ProvisionalTypedExpr -> IO ()
assertProvisionalProductionCompletes label =
  assertProvisionalProductionTypes label [] Nothing

assertProvisionalProductionTypes :: Text -> [(Text, TypedType)] -> Maybe TypedType -> ProvisionalTypedExpr -> IO ()
assertProvisionalProductionTypes label expectedBindingTypes expectedTerminalType provisionalScope = do
  resolvedModule <- resolveFixtureModule (fixtureByName "unit-entry")
  let status =
        typedCoreProductionOutcomeStatus
          ( finalizeValidatedTypedCoreExpressionDirectCall
              (TypedSourcePath "src/App/Main.jz")
              resolvedModule
              initialInferState
              provisionalScope
          )
  case status of
    TypedCoreProductionSucceeded programValue -> do
      assertEqual (label <> " typed-core validation") [] (validateTypedProgram programValue)
      case programValue of
        TypedProgram _ [TypedModule _ _ _ _ _ _ statements _] _ -> do
          let bindingTypes =
                Map.fromList
                  [ (identifier, typeValue)
                  | TypedLetStatement
                      _
                      (TypedResolvedName TypedCurrentModule TypedValueNamespace identifier)
                      _
                      (TypedScheme _ _ _ _ typeValue _ _)
                      _ <-
                      statements
                  ]
              selectedBindingTypes =
                [ (identifier, Map.lookup identifier bindingTypes)
                | (identifier, _) <- expectedBindingTypes
                ]
          assertEqual
            (label <> " specialized binding types")
            [(identifier, Just typeValue) | (identifier, typeValue) <- expectedBindingTypes]
            selectedBindingTypes
          case expectedTerminalType of
            Just expectedType ->
              case reverse statements of
                TypedExpressionStatement _ expression : _ ->
                  assertEqual
                    (label <> " specialized terminal type")
                    expectedType
                    (typedNodeType (typedExpressionInfo expression))
                _ -> failTest (label <> " typed program has no terminal expression")
            Nothing -> pure ()
        _ -> failTest (label <> " typed program has an unexpected module shape")
      case lowerTypedCoreExpressionDirectCall programValue of
        LoweredIRSucceeded loweredProgram ->
          assertEqual (label <> " lowered-IR validation") [] (validateLoweredProgram loweredProgram)
        other -> failTest (label <> " did not lower: " <> Text.pack (show other))
    other -> failTest (label <> " did not produce typed core: " <> Text.pack (show other))

typedUInt8Type :: TypedType
typedUInt8Type = TypedNumericType TypedUInt8Type

typedUInt8UnaryType :: TypedType
typedUInt8UnaryType = TypedFunctionType typedUInt8Type typedUInt8Type

typedIntToUInt8Type :: TypedType
typedIntToUInt8Type = TypedFunctionType TypedIntType typedUInt8Type

typedUInt8HigherOrderType :: TypedType
typedUInt8HigherOrderType = TypedFunctionType typedUInt8UnaryType typedUInt8Type
