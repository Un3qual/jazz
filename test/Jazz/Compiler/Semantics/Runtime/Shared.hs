{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Semantics.Runtime.Shared
  ( patternCaseNoMatchExpr,
    overAppliedConstructorExpr,
    qualifiedMethodStructuralEqualityExpr,
    ambiguousQualifiedMethodRuntimeExpr,
    runtimeEqSource,
    runtimeExpr,
    closureValue,
    builtinValue,
    targetedFloat,
    targetedInt,
    untypedFloatOne,
    untypedFloatTwo,
    tooLargeFloat64Integer,
    assertRuntimeBool,
    assertCallableRuntimeEqualityRejected,
    assertRuntimeErrorContains,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CorePhase (Analyzed),
    Expr,
    Literal (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource,
  )
import Jazz.Compiler.Name (UnresolvedName, qualifiedName)
import Jazz.Compiler.Runtime
  ( RuntimeValue (..),
  )
import Jazz.Compiler.Semantics.Runtime.Fixtures
import Jazz.Compiler.Semantics.Runtime.ResolvedFixture
import Jazz.Compiler.TypeRepresentation
  ( SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.TestHarness
  ( assertContains,
    assertEqual,
    failTest,
  )

patternCaseNoMatchExpr :: Expr 'Analyzed
patternCaseNoMatchExpr =
  expressionPatternCase
    (expressionLiteral (LInt 1))
    [ caseArm
        (patternLiteral (LInt 0))
        Nothing
        (expressionLiteral (LInt 2))
    ]

overAppliedConstructorExpr :: Expr 'Analyzed
overAppliedConstructorExpr =
  expressionBlock
    [ statementData
        (SourceSpan 1 1)
        "Maybe"
        []
        [dataConstructor "Just" [TypeInt]],
      statementExpression
        (SourceSpan 1 20)
        (expressionApply (expressionApply (expressionConstructor "Just") (expressionLiteral (LInt 1))) (expressionLiteral (LInt 2)))
    ]

qualifiedMethodStructuralEqualityExpr :: Expr 'Analyzed
qualifiedMethodStructuralEqualityExpr =
  expressionBlock
    [ statementClass
        (SourceSpan 1 1)
        "RuntimeEq"
        ["a"]
        [ classMethodSignature
            "equals"
            (SourceSpan 2 1)
            ( ConstrainedSignature
                []
                ( TypeFunction
                    (fixtureTypeVariable "a")
                    (TypeFunction (fixtureTypeVariable "a") (TypeBool))
                )
            )
        ],
      statementImpl
        (SourceSpan 3 1)
        "RuntimeEq"
        [TypeInt]
        [ implMethod
            "equals"
            (SourceSpan 4 1)
            (expressionLambda "left" (expressionLambda "right" (expressionKernelBinary "==" (expressionVariable "left") (expressionVariable "right"))))
        ],
      statementExpression
        (SourceSpan 5 1)
        ( expressionKernelBinary
            "=="
            (expressionList [expressionVariable (qualifiedName "RuntimeEq" "equals")])
            (expressionList [expressionVariable (qualifiedName "RuntimeEq" "equals")])
        )
    ]

ambiguousQualifiedMethodRuntimeExpr :: Expr 'Analyzed
ambiguousQualifiedMethodRuntimeExpr =
  expressionBlock
    [ statementClass
        (SourceSpan 1 1)
        "RuntimePick"
        ["a"]
        [ classMethodSignature
            "choose"
            (SourceSpan 2 1)
            ( ConstrainedSignature
                []
                (TypeFunction (TypeInt) (TypeBool))
            )
        ],
      statementImpl
        (SourceSpan 3 1)
        "RuntimePick"
        [TypeInt]
        [implMethod "choose" (SourceSpan 4 1) (expressionLambda "itemValue" (expressionLiteral (LBool True)))],
      statementImpl
        (SourceSpan 5 1)
        "RuntimePick"
        [TypeBool]
        [implMethod "choose" (SourceSpan 6 1) (expressionLambda "itemValue" (expressionLiteral (LBool False)))],
      statementExpression
        (SourceSpan 7 1)
        (expressionApply (expressionVariable (qualifiedName "RuntimePick" "choose")) (expressionLiteral (LInt 1)))
    ]

runtimeEqSource :: Text
runtimeEqSource =
  """
  class RuntimeEq(a) {
  equals :: a -> a -> Bool.
  }.
  impl RuntimeEq(Int) {
  equals = __kernel_equals.
  }.

  """

runtimeExpr :: Expr 'Analyzed -> Expr 'Analyzed
runtimeExpr expr =
  expressionBlock
    [ statementExpression
        (SourceSpan 1 1)
        expr
    ]

closureValue :: Expr 'Analyzed
closureValue =
  expressionLambda "itemValue" (expressionVariable "itemValue")

builtinValue :: Expr 'Analyzed
builtinValue =
  expressionVariable "__kernel_hd"

targetedFloat :: UnresolvedName -> Expr 'Analyzed
targetedFloat conversionName =
  expressionApply (expressionVariable conversionName) (expressionLiteral (LInt 1))

targetedInt :: UnresolvedName -> Expr 'Analyzed
targetedInt conversionName =
  expressionApply (expressionVariable conversionName) (expressionLiteral (LInt 1))

untypedFloatOne :: Expr 'Analyzed
untypedFloatOne =
  expressionLiteral (LFloat 1.0 (mkFractionalLiteralSource 1 0 1) Nothing)

untypedFloatTwo :: Expr 'Analyzed
untypedFloatTwo =
  expressionLiteral (LFloat 2.0 (mkFractionalLiteralSource 2 0 1) Nothing)

tooLargeFloat64Integer :: Expr 'Analyzed
tooLargeFloat64Integer =
  expressionLiteral (LInt ((floor (1.7976931348623157e308 :: Double) :: Integer) + 1))

assertRuntimeBool :: Text -> Bool -> Either Diagnostic (Maybe RuntimeValue) -> IO ()
assertRuntimeBool label expected result =
  case result of
    Right (Just (VBool actual)) ->
      assertEqual label expected actual
    Right otherValue ->
      failTest ("expected " <> label <> " to produce Bool, got " <> Text.pack (show otherValue))
    Left runtimeError ->
      failTest ("expected " <> label <> " to succeed, got " <> renderDiagnostic runtimeError)

assertCallableRuntimeEqualityRejected :: Text -> Expr 'Analyzed -> IO ()
assertCallableRuntimeEqualityRejected label expr = do
  let result = evaluateFixture (runtimeExpr expr)
  assertRuntimeErrorContains (label <> " code") "E3007" result
  assertRuntimeErrorContains
    (label <> " callable text")
    "callable values are not equality-supported"
    result

assertRuntimeErrorContains :: Text -> Text -> Either Diagnostic (Maybe a) -> IO ()
assertRuntimeErrorContains label expectedCode result =
  case result of
    Left runtimeError ->
      assertContains label expectedCode (renderDiagnostic runtimeError)
    Right _ ->
      failTest ("expected runtime error containing " <> expectedCode <> ", but evaluation succeeded")
