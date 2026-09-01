{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Semantics.Runtime.Shared
  ( patternCaseNoMatchExpr,
    overAppliedConstructorExpr,
    qualifiedMethodStructuralEqualityExpr,
    runtimeTypedCallableArgumentHintExpr,
    runtimeTypedCallableArgumentHintThroughPrefixDollarExpr,
    runtimePickStatements,
    ambiguousQualifiedMethodRuntimeExpr,
    runtimeEqSource,
    runtimeExpr,
    closureValue,
    builtinValue,
    operatorValue,
    leftSectionValue,
    rightSectionValue,
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
  ( CorePhase (Resolved),
    Expr,
    Literal (..),
    Statement,
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
    evaluateRuntimeExpr,
  )
import Jazz.Compiler.Semantics.Runtime.Fixtures
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.TestHarness
  ( assertContains,
    assertEqual,
    failTest,
  )

patternCaseNoMatchExpr :: Expr 'Resolved
patternCaseNoMatchExpr =
  expressionPatternCase
    (expressionLiteral (LInt 1))
    [ caseArm
        (patternLiteral (LInt 0))
        Nothing
        (expressionLiteral (LInt 2))
    ]

overAppliedConstructorExpr :: Expr 'Resolved
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

qualifiedMethodStructuralEqualityExpr :: Expr 'Resolved
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
            (expressionLambda "left" (expressionLambda "right" (expressionBinary "==" (expressionVariable "left") (expressionVariable "right"))))
        ],
      statementExpression
        (SourceSpan 5 1)
        ( expressionBinary
            "=="
            (expressionList [expressionVariable (qualifiedName "RuntimeEq" "equals")])
            (expressionList [expressionVariable (qualifiedName "RuntimeEq" "equals")])
        )
    ]

runtimeTypedCallableArgumentHintExpr :: Expr 'Resolved -> Expr 'Resolved
runtimeTypedCallableArgumentHintExpr callableExpr =
  expressionBlock
    ( runtimePickStatements
        ++ [ statementLet "choose" (SourceSpan 9 1) callableExpr,
             statementExpression (SourceSpan 10 1) (expressionApply (expressionVariable "choose") (expressionLiteral (LInt 1)))
           ]
    )

runtimeTypedCallableArgumentHintThroughPrefixDollarExpr :: Expr 'Resolved -> Expr 'Resolved
runtimeTypedCallableArgumentHintThroughPrefixDollarExpr callableExpr =
  expressionBlock
    ( runtimePickStatements
        ++ [ statementLet "choose" (SourceSpan 9 1) callableExpr,
             statementExpression (SourceSpan 10 1) (expressionApply (expressionApply (expressionOperatorValue "$") (expressionVariable "choose")) (expressionLiteral (LInt 1)))
           ]
    )

runtimePickStatements :: [Statement 'Resolved]
runtimePickStatements =
  [ statementClass
      (SourceSpan 1 1)
      "RuntimePick"
      ["a"]
      [ classMethodSignature
          "pick"
          (SourceSpan 2 1)
          (ConstrainedSignature [] (TypeFunction (fixtureTypeVariable "a") (TypeBool)))
      ],
    statementImpl
      (SourceSpan 3 1)
      "RuntimePick"
      [TypeInt]
      [implMethod "pick" (SourceSpan 4 1) (expressionLambda "itemValue" (expressionLiteral (LBool True)))],
    statementImpl
      (SourceSpan 5 1)
      "RuntimePick"
      [TypeNumeric NumericUInt8]
      [implMethod "pick" (SourceSpan 6 1) (expressionLambda "itemValue" (expressionLiteral (LBool False)))]
  ]

ambiguousQualifiedMethodRuntimeExpr :: Expr 'Resolved
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
  equals = \\(left, right) -> left == right.
  }.

  """

runtimeExpr :: Expr 'Resolved -> Expr 'Resolved
runtimeExpr expr =
  expressionBlock
    [ statementExpression
        (SourceSpan 1 1)
        expr
    ]

closureValue :: Expr 'Resolved
closureValue =
  expressionLambda "itemValue" (expressionVariable "itemValue")

builtinValue :: Expr 'Resolved
builtinValue =
  expressionVariable "__kernel_hd"

operatorValue :: Expr 'Resolved
operatorValue =
  expressionOperatorValue "+"

leftSectionValue :: Expr 'Resolved
leftSectionValue =
  expressionSectionLeft (expressionLiteral (LInt 1)) "+"

rightSectionValue :: Expr 'Resolved
rightSectionValue =
  expressionSectionRight "+" (expressionLiteral (LInt 1))

targetedFloat :: UnresolvedName -> Expr 'Resolved
targetedFloat conversionName =
  expressionApply (expressionVariable conversionName) (expressionLiteral (LInt 1))

targetedInt :: UnresolvedName -> Expr 'Resolved
targetedInt conversionName =
  expressionApply (expressionVariable conversionName) (expressionLiteral (LInt 1))

untypedFloatOne :: Expr 'Resolved
untypedFloatOne =
  expressionLiteral (LFloat 1.0 (mkFractionalLiteralSource 1 0 1) Nothing)

untypedFloatTwo :: Expr 'Resolved
untypedFloatTwo =
  expressionLiteral (LFloat 2.0 (mkFractionalLiteralSource 2 0 1) Nothing)

tooLargeFloat64Integer :: Expr 'Resolved
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

assertCallableRuntimeEqualityRejected :: Text -> Expr 'Resolved -> IO ()
assertCallableRuntimeEqualityRejected label expr = do
  let result = evaluateRuntimeExpr (runtimeExpr expr)
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
