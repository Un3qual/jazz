{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.Runtime.Shared
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
    assertRuntimeErrorContains
  ) where


import Control.Exception
  ( SomeException,
    try
  )
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    ConstraintSignatureType (..),
    DataConstructorArgument (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    Pattern (..),
    SignaturePayload (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    renderDiagnostic
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..)
  )
import JazzNext.Compiler.Driver
  ( RunResult (..),
    runSource,
    runSourceWithPrelude
  )
import JazzNext.Compiler.FractionalLiteral
  ( mkFractionalLiteralSource
  )
import JazzNext.Compiler.Name (Name, qualifiedName)
import JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    evaluateRuntimeExpr,
    evaluateRuntimeExprWithBuiltinsAndBindingHints,
    renderRuntimeValue,
    runtimeValueExactlyMatchesConstraint
  )
import JazzNext.Compiler.RuntimeHints
  ( bindingRuntimeHintKey
  )
import JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpressionWithBuiltins
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertLeftDiagnosticCodeAndContains,
    assertEqual,
    assertSingleDiagnosticContains,
    failTest,
    runTestSuite
  )
import System.Timeout
  ( timeout
  )

patternCaseNoMatchExpr :: Expr
patternCaseNoMatchExpr =
  EPatternCase
    (ELit (LInt 1))
    [ CaseArm
        (PLiteral (LInt 0))
        Nothing
        (ELit (LInt 2))
    ]

overAppliedConstructorExpr :: Expr
overAppliedConstructorExpr =
  EBlock
    [ SData
        (SourceSpan 1 1)
        "Maybe"
        []
        [DataConstructor "Just" [DataConstructorArgumentName "value"]],
      SExpr
        (SourceSpan 1 20)
        (EApply (EApply (EVar "Just") (ELit (LInt 1))) (ELit (LInt 2)))
    ]

qualifiedMethodStructuralEqualityExpr :: Expr
qualifiedMethodStructuralEqualityExpr =
  EBlock
    [ SClass
        (SourceSpan 1 1)
        "RuntimeEq"
        ["a"]
        [ ClassMethodSignature
            "equals"
            (SourceSpan 2 1)
            ( ConstrainedSignature
                []
                ( ConstraintTypeFunction
                    (ConstraintTypeName "a")
                    (ConstraintTypeFunction (ConstraintTypeName "a") (ConstraintTypeName "Bool"))
                )
            )
        ],
      SImpl
        (SourceSpan 3 1)
        "RuntimeEq"
        [ConstraintTypeName "Int"]
        [ ImplMethod
            "equals"
            (SourceSpan 4 1)
            (ELambda "left" (ELambda "right" (EBinary "==" (EVar "left") (EVar "right"))))
        ],
      SExpr
        (SourceSpan 5 1)
        ( EBinary
            "=="
            (EList [EVar (qualifiedName "RuntimeEq" "equals")])
            (EList [EVar (qualifiedName "RuntimeEq" "equals")])
        )
    ]

runtimeTypedCallableArgumentHintExpr :: Expr -> Expr
runtimeTypedCallableArgumentHintExpr callableExpr =
  EBlock
    ( runtimePickStatements
        ++ [ SLet "choose" (SourceSpan 9 1) callableExpr,
             SExpr (SourceSpan 10 1) (EApply (EVar "choose") (ELit (LInt 1)))
           ]
    )

runtimeTypedCallableArgumentHintThroughPrefixDollarExpr :: Expr -> Expr
runtimeTypedCallableArgumentHintThroughPrefixDollarExpr callableExpr =
  EBlock
    ( runtimePickStatements
        ++ [ SLet "choose" (SourceSpan 9 1) callableExpr,
             SExpr (SourceSpan 10 1) (EApply (EApply (EOperatorValue "$") (EVar "choose")) (ELit (LInt 1)))
           ]
    )

runtimePickStatements :: [Statement]
runtimePickStatements =
  [ SClass
      (SourceSpan 1 1)
      "RuntimePick"
      ["a"]
      [ ClassMethodSignature
          "pick"
          (SourceSpan 2 1)
          (ConstrainedSignature [] (ConstraintTypeFunction (ConstraintTypeName "a") (ConstraintTypeName "Bool")))
      ],
    SImpl
      (SourceSpan 3 1)
      "RuntimePick"
      [ConstraintTypeName "Int"]
      [ImplMethod "pick" (SourceSpan 4 1) (ELambda "value" (ELit (LBool True)))],
    SImpl
      (SourceSpan 5 1)
      "RuntimePick"
      [ConstraintTypeName "UInt8"]
      [ImplMethod "pick" (SourceSpan 6 1) (ELambda "value" (ELit (LBool False)))]
  ]

ambiguousQualifiedMethodRuntimeExpr :: Expr
ambiguousQualifiedMethodRuntimeExpr =
  EBlock
    [ SClass
        (SourceSpan 1 1)
        "RuntimePick"
        ["a"]
        [ ClassMethodSignature
            "choose"
            (SourceSpan 2 1)
            ( ConstrainedSignature
                []
                (ConstraintTypeFunction (ConstraintTypeName "Int") (ConstraintTypeName "Bool"))
            )
        ],
      SImpl
        (SourceSpan 3 1)
        "RuntimePick"
        [ConstraintTypeName "Int"]
        [ImplMethod "choose" (SourceSpan 4 1) (ELambda "value" (ELit (LBool True)))],
      SImpl
        (SourceSpan 5 1)
        "RuntimePick"
        [ConstraintTypeName "Bool"]
        [ImplMethod "choose" (SourceSpan 6 1) (ELambda "value" (ELit (LBool False)))],
      SExpr
        (SourceSpan 7 1)
        (EApply (EVar (qualifiedName "RuntimePick" "choose")) (ELit (LInt 1)))
    ]

runtimeEqSource :: Text
runtimeEqSource =
  "class RuntimeEq(a) {\nequals :: a -> a -> Bool.\n}.\nimpl RuntimeEq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.\n"

runtimeExpr :: Expr -> Expr
runtimeExpr expr =
  EBlock
    [ SExpr
        (SourceSpan 1 1)
        expr
    ]

closureValue :: Expr
closureValue =
  ELambda "value" (EVar "value")

builtinValue :: Expr
builtinValue =
  EVar "__kernel_hd"

operatorValue :: Expr
operatorValue =
  EOperatorValue "+"

leftSectionValue :: Expr
leftSectionValue =
  ESectionLeft (ELit (LInt 1)) "+"

rightSectionValue :: Expr
rightSectionValue =
  ESectionRight "+" (ELit (LInt 1))

targetedFloat :: Name -> Expr
targetedFloat conversionName =
  EApply (EVar conversionName) (ELit (LInt 1))

targetedInt :: Name -> Expr
targetedInt conversionName =
  EApply (EVar conversionName) (ELit (LInt 1))

untypedFloatOne :: Expr
untypedFloatOne =
  ELit (LFloat 1.0 (mkFractionalLiteralSource 1 0 1) Nothing)

untypedFloatTwo :: Expr
untypedFloatTwo =
  ELit (LFloat 2.0 (mkFractionalLiteralSource 2 0 1) Nothing)

tooLargeFloat64Integer :: Expr
tooLargeFloat64Integer =
  ELit (LInt ((floor (1.7976931348623157e308 :: Double) :: Integer) + 1))

assertRuntimeBool :: Text -> Bool -> Either Diagnostic (Maybe RuntimeValue) -> IO ()
assertRuntimeBool label expected result =
  case result of
    Right (Just (VBool actual)) ->
      assertEqual label expected actual
    Right otherValue ->
      failTest ("expected " <> label <> " to produce Bool, got " <> Text.pack (show otherValue))
    Left runtimeError ->
      failTest ("expected " <> label <> " to succeed, got " <> renderDiagnostic runtimeError)

assertCallableRuntimeEqualityRejected :: Text -> Expr -> IO ()
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
