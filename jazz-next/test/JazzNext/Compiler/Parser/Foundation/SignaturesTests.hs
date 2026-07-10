{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser.Foundation.SignaturesTests
  ( signatureTests
  ) where


import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( ClassMethodSignature (..),
    ConstraintSignatureType (..),
    Expr (..),
    Literal (..),
    NumericType (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceClassMethodSignature (..),
    SurfaceConstrainedSignatureType (..),
    SurfaceExpr (..),
    SurfaceLiteral (..),
    SurfaceNumericType (..),
    SurfaceSignatureConstraint (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureToken (..),
    SurfaceSignatureType (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )
import JazzNext.Compiler.Name (qualifiedName)
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertLeftDiagnosticContains,
    assertRight,
    runTestSuite
  )
import JazzNext.Compiler.Parser.Foundation.Shared

signatureTests :: [NamedTest]
signatureTests =
  [ ("parses signature statement with source span", testParseSignatureSpan)
    , ("parses parenthesized function signature into structured nodes", testParseParenthesizedFunctionSignature)
    , ("parses tuple signature into structured nodes", testParseTupleSignature)
    , ("parses Unit value and signature into structured nodes", testParseUnitValueAndSignature)
    , ("parses constrained Unit signature into structured nodes", testParseConstrainedUnitSignature)
    , ("parses numeric width signature names into structured nodes", testParseNumericWidthSignatureTypes)
    , ("parses chained function signature right associatively", testParseChainedFunctionSignature)
    , ("parses parenthesized function override into structured nodes", testParseParenthesizedFunctionOverrideSignature)
    , ("parses list of parenthesized function types", testParseFunctionListSignature)
    , ("parses constrained signature into structured nodes", testParseConstrainedSignaturePayload)
    , ("parses constrained signature with empty constraint block", testParseEmptyConstraintBlockSignaturePayload)
    , ("parses constrained tuple signature into structured nodes", testParseConstrainedTupleSignaturePayload)
    , ("parses explicit type application expression", testParseExplicitTypeApplicationExpression)
    , ("lowers explicit type application expression", testLowerExplicitTypeApplicationExpression)
    , ("lowered explicit type application needs no post-pass", testLoweredExplicitTypeApplicationIsCanonical)
    , ("lowers tuple literal and signature into analyzer AST", testLowerTupleLiteralAndSignatureProgram)
    , ("lowers Unit value and signature into analyzer AST", testLowerUnitValueAndSignature)
    , ("lowers numeric width signature names into analyzer AST", testLowerNumericWidthSignatureProgram)
    , ("lowers structured signature payload into analyzer AST", testLowerStructuredSignatureProgram)
    , ("lowers right-associated function signature into analyzer AST", testLowerRightAssociativeFunctionSignatureProgram)
    , ("lowers list of function signature into analyzer AST", testLowerFunctionListSignatureProgram)
    , ("lowers constrained signature payload into analyzer AST", testLowerConstrainedSignatureProgram)
    , ("lowers constrained tuple signature payload into analyzer AST", testLowerConstrainedTupleSignatureProgram)
    , ("parses abstraction keywords as ordinary signature names", testParsesAbstractionKeywordsAsSignatureNames)
    , ("parses operator keyword as an ordinary signature name", testParsesOperatorKeywordAsSignatureName)
    , ("parses class method signature metadata", testParsesClassMethodSignatureMetadata)
  ]

testParseSignatureSpan :: IO ()
testParseSignatureSpan =
  assertEqual
    "signature span"
    ( Right
        ( SEBlock
            [ SSSignature "x" (SourceSpan 1 1) (SurfaceSignatureType (SurfaceTypeInt)),
              SSLet "x" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "x :: Int.\nx = 1.")

testParseParenthesizedFunctionSignature :: IO ()
testParseParenthesizedFunctionSignature =
  assertEqual
    "parenthesized function signature"
    ( Right
        ( SEBlock
            [ SSSignature
                "f"
                (SourceSpan 1 1)
                ( SurfaceSignatureType
                    (SurfaceTypeFunction
                      (SurfaceTypeList SurfaceTypeInt)
                      (SurfaceTypeList SurfaceTypeInt)
                    )
                ),
              SSLet "f" (SourceSpan 2 1) (SEOperatorValue "+")
            ]
        )
    )
    (parseSurfaceProgram "f :: ([Int]) -> ([Int]).\nf = (+).")

testParseTupleSignature :: IO ()
testParseTupleSignature =
  assertEqual
    "tuple signature"
    ( Right
        ( SEBlock
            [ SSSignature
                "pair"
                (SourceSpan 1 1)
                (SurfaceSignatureType (SurfaceTypeTuple [SurfaceTypeInt, SurfaceTypeBool])),
              SSLet "pair" (SourceSpan 2 1) (SETuple [SELit (SLInt 1), SELit (SLBool True)])
            ]
        )
    )
    (parseSurfaceProgram "pair :: (Int, Bool).\npair = (1, True).")

testParseUnitValueAndSignature :: IO ()
testParseUnitValueAndSignature =
  assertEqual
    "Unit value and signature"
    ( Right
        ( SEBlock
            [ SSSignature
                "unit"
                (SourceSpan 1 1)
                (SurfaceSignatureType (SurfaceTypeTuple [])),
              SSLet "unit" (SourceSpan 2 1) (SETuple [])
            ]
        )
    )
    (parseSurfaceProgram "unit :: ().\nunit = ().")

testParseConstrainedUnitSignature :: IO ()
testParseConstrainedUnitSignature =
  assertEqual
    "constrained Unit signature"
    ( Right
        ( SEBlock
            [ SSSignature
                "unit"
                (SourceSpan 1 1)
                (SurfaceConstrainedSignature [] (SurfaceConstrainedTypeTuple [])),
              SSLet "unit" (SourceSpan 2 1) (SETuple [])
            ]
        )
    )
    (parseSurfaceProgram "unit :: @{}: ().\nunit = ().")

testParseNumericWidthSignatureTypes :: IO ()
testParseNumericWidthSignatureTypes = do
  assertEqual
    "Int8 signature"
    ( Right
        ( SEBlock
            [ SSSignature "x" (SourceSpan 1 1) (SurfaceSignatureType (SurfaceTypeNumeric SurfaceNumericInt8)),
              SSLet "x" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "x :: Int8.\nx = 1.")
  assertEqual
    "Float alias signature"
    ( Right
        ( SEBlock
            [ SSSignature
                "f"
                (SourceSpan 1 1)
                ( SurfaceSignatureType
                    (SurfaceTypeFunction SurfaceTypeFloat (SurfaceTypeNumeric SurfaceNumericFloat64))
                ),
              SSLet "f" (SourceSpan 2 1) (SEOperatorValue "+")
            ]
        )
    )
    (parseSurfaceProgram "f :: Float -> Float64.\nf = (+).")

testParseChainedFunctionSignature :: IO ()
testParseChainedFunctionSignature =
  assertEqual
    "right-associated function signature"
    ( Right
        ( SEBlock
            [ SSSignature
                "f"
                (SourceSpan 1 1)
                ( SurfaceSignatureType
                    (SurfaceTypeFunction SurfaceTypeInt (SurfaceTypeFunction SurfaceTypeInt SurfaceTypeInt))
                ),
              SSLet "f" (SourceSpan 2 1) (SEOperatorValue "+")
            ]
        )
    )
    (parseSurfaceProgram "f :: Int -> Int -> Int.\nf = (+).")

testParseParenthesizedFunctionOverrideSignature :: IO ()
testParseParenthesizedFunctionOverrideSignature =
  assertEqual
    "parenthesized function override signature"
    ( Right
        ( SEBlock
            [ SSSignature
                "f"
                (SourceSpan 1 1)
                ( SurfaceSignatureType
                    (SurfaceTypeFunction (SurfaceTypeFunction SurfaceTypeInt SurfaceTypeInt) SurfaceTypeInt)
                ),
              SSLet "f" (SourceSpan 2 1) (SEVar "applyToOne")
            ]
        )
    )
    (parseSurfaceProgram "f :: (Int -> Int) -> Int.\nf = applyToOne.")

testParseFunctionListSignature :: IO ()
testParseFunctionListSignature =
  assertEqual
    "list of parenthesized function types"
    ( Right
        ( SEBlock
            [ SSSignature
                "fns"
                (SourceSpan 1 1)
                ( SurfaceSignatureType
                    (SurfaceTypeList (SurfaceTypeFunction SurfaceTypeInt SurfaceTypeInt))
                ),
              SSLet "fns" (SourceSpan 2 1) (SEList [SESectionRight "+" (SELit (SLInt 1))])
            ]
        )
    )
    (parseSurfaceProgram "fns :: [(Int -> Int)].\nfns = [(+ 1)].")

testParseConstrainedSignaturePayload :: IO ()
testParseConstrainedSignaturePayload =
  assertEqual
    "constrained signature payload"
    ( Right
        ( SEBlock
            [ SSSignature
                "f"
                (SourceSpan 1 1)
                ( SurfaceConstrainedSignature
                    [ SurfaceSignatureConstraint "Eq" [SurfaceConstrainedTypeName "a"],
                      SurfaceSignatureConstraint "Ord" [SurfaceConstrainedTypeName "b"]
                    ]
                    ( SurfaceConstrainedTypeFunction
                        (SurfaceConstrainedTypeName "a")
                        (SurfaceConstrainedTypeFunction (SurfaceConstrainedTypeName "b") (SurfaceConstrainedTypeName "c"))
                    )
                ),
              SSLet "f" (SourceSpan 2 1) (SEVar "combine")
            ]
        )
    )
    (parseSurfaceProgram "f :: @{Eq(a), Ord(b)}: a -> b -> c.\nf = combine.")

testParseEmptyConstraintBlockSignaturePayload :: IO ()
testParseEmptyConstraintBlockSignaturePayload =
  assertEqual
    "empty constrained signature payload"
    ( Right
        ( SEBlock
            [ SSSignature
                "f"
                (SourceSpan 1 1)
                (SurfaceConstrainedSignature [] (SurfaceConstrainedTypeName "Int")),
              SSLet "f" (SourceSpan 2 1) (SEVar "value")
            ]
        )
    )
    (parseSurfaceProgram "f :: @{}: Int.\nf = value.")

testParseConstrainedTupleSignaturePayload :: IO ()
testParseConstrainedTupleSignaturePayload =
  assertEqual
    "constrained tuple signature payload"
    ( Right
        ( SEBlock
            [ SSSignature
                "pair"
                (SourceSpan 1 1)
                ( SurfaceConstrainedSignature
                    []
                    (SurfaceConstrainedTypeTuple [SurfaceConstrainedTypeName "Int", SurfaceConstrainedTypeName "Bool"])
                ),
              SSLet "pair" (SourceSpan 2 1) (SETuple [SELit (SLInt 1), SELit (SLBool True)])
            ]
        )
    )
    (parseSurfaceProgram "pair :: @{}: (Int, Bool).\npair = (1, True).")

testParseExplicitTypeApplicationExpression :: IO ()
testParseExplicitTypeApplicationExpression =
  assertRight
    "explicit type application parse"
    (parseSurfaceProgram "value = id @Int 1.\nvalue.")
    ( \surfaceProgram -> do
        let rendered = Text.pack (show surfaceProgram)
        assertContains "surface type application" "SETypeApplication" rendered
        assertContains "surface type application argument" "SurfaceTypeInt" rendered
    )

testLowerExplicitTypeApplicationExpression :: IO ()
testLowerExplicitTypeApplicationExpression =
  assertRight
    "explicit type application lowering"
    (parseSurfaceProgram "value = id @Int 1.\nvalue.")
    ( \surfaceProgram -> do
        let rendered = Text.pack (show (lowerSurfaceExpr surfaceProgram))
        assertContains "lowered type application" "ETypeApplication" rendered
        assertContains "lowered type application argument" "TypeInt" rendered
    )

testLoweredExplicitTypeApplicationIsCanonical :: IO ()
testLoweredExplicitTypeApplicationIsCanonical =
  assertRight
    "parse + canonical lower explicit type application"
    (parseSurfaceProgram "value = id @Int 1.\nvalue.")
    ( \surfaceProgram ->
        assertEqual
          "canonical lowered type application AST"
          expectedProgram
          (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedProgram =
      EBlock
        [ SLet
            "value"
            (SourceSpan 1 1)
            (EApply (ETypeApplication (EVar "id") TypeInt) (ELit (LInt 1))),
          SExpr (SourceSpan 2 1) (EVar "value")
        ]

testLowerTupleLiteralAndSignatureProgram :: IO ()
testLowerTupleLiteralAndSignatureProgram =
  assertRight
    "parse + lower tuple literal/signature"
    (parseSurfaceProgram "pair :: (Int, Bool).\npair = (1, True).")
    ( \surfaceProgram ->
        assertEqual
          "lowered tuple AST"
          ( EBlock
              [ SSignature
                  "pair"
                  (SourceSpan 1 1)
                  (SignatureType (TypeTuple [TypeInt, TypeBool])),
                SLet
                  "pair"
                  (SourceSpan 2 1)
                  (ETuple [ELit (LInt 1), ELit (LBool True)])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerUnitValueAndSignature :: IO ()
testLowerUnitValueAndSignature =
  assertRight
    "parse + lower Unit value/signature"
    (parseSurfaceProgram "unit :: ().\nunit = ().")
    ( \surfaceProgram ->
        assertEqual
          "lowered Unit AST"
          ( EBlock
              [ SSignature
                  "unit"
                  (SourceSpan 1 1)
                  (SignatureType (TypeTuple [])),
                SLet "unit" (SourceSpan 2 1) (ETuple [])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerNumericWidthSignatureProgram :: IO ()
testLowerNumericWidthSignatureProgram =
  assertRight
    "parse + lower numeric width signatures"
    (parseSurfaceProgram "f :: UInt8 -> Int64 -> Float.\nf = (+).")
    ( \surfaceProgram ->
        assertEqual
          "lowered numeric width signature AST"
          ( EBlock
              [ SSignature
                  "f"
                  (SourceSpan 1 1)
                  ( SignatureType
                      ( TypeFunction
                          (TypeNumeric NumericUInt8)
                          (TypeFunction (TypeNumeric NumericInt64) TypeFloat)
                      )
                  ),
                SLet "f" (SourceSpan 2 1) (EOperatorValue "+")
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerStructuredSignatureProgram :: IO ()
testLowerStructuredSignatureProgram =
  assertRight
    "parse + lower structured signature"
    (parseSurfaceProgram "x :: [[Bool]].\nx = [[True], [False]].")
    ( \surfaceProgram ->
        assertEqual
          "lowered signature AST"
          ( EBlock
              [ SSignature
                  "x"
                  (SourceSpan 1 1)
                  (SignatureType (TypeList (TypeList TypeBool))),
                SLet
                  "x"
                  (SourceSpan 2 1)
                  (EList [EList [ELit (LBool True)], EList [ELit (LBool False)]])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerRightAssociativeFunctionSignatureProgram :: IO ()
testLowerRightAssociativeFunctionSignatureProgram =
  assertRight
    "parse + lower right-associated function signature"
    (parseSurfaceProgram "f :: Int -> Int -> Int.\nf = (+).")
    ( \surfaceProgram ->
        assertEqual
          "lowered right-associated signature AST"
          ( EBlock
              [ SSignature
                  "f"
                  (SourceSpan 1 1)
                  (SignatureType (TypeFunction TypeInt (TypeFunction TypeInt TypeInt))),
                SLet "f" (SourceSpan 2 1) (EOperatorValue "+")
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerFunctionListSignatureProgram :: IO ()
testLowerFunctionListSignatureProgram =
  assertRight
    "parse + lower list of function signature"
    (parseSurfaceProgram "fns :: [(Int -> Int)].\nfns = [(+ 1)].")
    ( \surfaceProgram ->
        assertEqual
          "lowered list of function signature AST"
          ( EBlock
              [ SSignature
                  "fns"
                  (SourceSpan 1 1)
                  (SignatureType (TypeList (TypeFunction TypeInt TypeInt))),
                SLet
                  "fns"
                  (SourceSpan 2 1)
                  (EList [ESectionRight "+" (ELit (LInt 1))])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerConstrainedSignatureProgram :: IO ()
testLowerConstrainedSignatureProgram =
  assertRight
    "parse + lower constrained signature"
    (parseSurfaceProgram "f :: @{Eq(a)}: a -> a.\nf = identity.")
    ( \surfaceProgram ->
        assertEqual
          "lowered constrained signature AST"
          ( EBlock
              [ SSignature
                  "f"
                  (SourceSpan 1 1)
                  ( ConstrainedSignature
                      [SignatureConstraint "Eq" [ConstraintTypeName "a"]]
                      (ConstraintTypeFunction (ConstraintTypeName "a") (ConstraintTypeName "a"))
                  ),
                SLet "f" (SourceSpan 2 1) (EVar "identity")
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerConstrainedTupleSignatureProgram :: IO ()
testLowerConstrainedTupleSignatureProgram =
  assertRight
    "parse + lower constrained tuple signature"
    (parseSurfaceProgram "pair :: @{}: (Int, Bool).\npair = (1, True).")
    ( \surfaceProgram ->
        assertEqual
          "lowered constrained tuple signature AST"
          ( EBlock
              [ SSignature
                  "pair"
                  (SourceSpan 1 1)
                  ( ConstrainedSignature
                      []
                      (ConstraintTypeTuple [ConstraintTypeName "Int", ConstraintTypeName "Bool"])
                  ),
                SLet
                  "pair"
                  (SourceSpan 2 1)
                  (ETuple [ELit (LInt 1), ELit (LBool True)])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testParsesAbstractionKeywordsAsSignatureNames :: IO ()
testParsesAbstractionKeywordsAsSignatureNames =
  assertEqual
    "abstraction keyword signature names"
    ( Right
        ( SEBlock
            [ SSSignature "class" (SourceSpan 1 1) (SurfaceSignatureType SurfaceTypeInt),
              SSLet "class" (SourceSpan 2 1) (SELit (SLInt 1)),
              SSSignature "impl" (SourceSpan 3 1) (SurfaceSignatureType SurfaceTypeBool),
              SSLet "impl" (SourceSpan 4 1) (SELit (SLBool True)),
              SSSignature "trait" (SourceSpan 5 1) (SurfaceSignatureType SurfaceTypeInt),
              SSLet "trait" (SourceSpan 6 1) (SELit (SLInt 2))
            ]
        )
    )
    (parseSurfaceProgram "class :: Int.\nclass = 1.\nimpl :: Bool.\nimpl = True.\ntrait :: Int.\ntrait = 2.")

testParsesOperatorKeywordAsSignatureName :: IO ()
testParsesOperatorKeywordAsSignatureName =
  assertEqual
    "operator keyword signature name"
    ( Right
        ( SEBlock
            [ SSSignature "operator" (SourceSpan 1 1) (SurfaceSignatureType SurfaceTypeInt),
              SSLet "operator" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "operator :: Int.\noperator = 1.")

testParsesClassMethodSignatureMetadata :: IO ()
testParsesClassMethodSignatureMetadata =
  assertRight
    "surface class method signature parse"
    (parseSurfaceProgram "class Eq(a) {\nequals :: a -> a -> Bool.\nnotEquals :: a -> a -> Bool.\n}.")
    ( \surfaceProgram -> do
        let surfacePayload =
              SurfaceUnsupportedSignature
                [ SurfaceSignatureNameToken "a",
                  SurfaceSignatureArrowToken,
                  SurfaceSignatureNameToken "a",
                  SurfaceSignatureArrowToken,
                  SurfaceSignatureNameToken "Bool"
                ]
            corePayload =
              UnsupportedSignature
                [ SignatureNameToken "a",
                  SignatureArrowToken,
                  SignatureNameToken "a",
                  SignatureArrowToken,
                  SignatureNameToken "Bool"
                ]
        assertEqual
          "surface class method metadata"
          ( SEBlock
              [ SSClass
                  (SourceSpan 1 1)
                  "Eq"
                  ["a"]
                  [ SurfaceClassMethodSignature "equals" (SourceSpan 2 1) surfacePayload,
                    SurfaceClassMethodSignature "notEquals" (SourceSpan 3 1) surfacePayload
                  ]
              ]
          )
          surfaceProgram
        assertEqual
          "lowered class method metadata"
          ( EBlock
              [ SClass
                  (SourceSpan 1 1)
                  "Eq"
                  ["a"]
                  [ ClassMethodSignature "equals" (SourceSpan 2 1) corePayload,
                    ClassMethodSignature "notEquals" (SourceSpan 3 1) corePayload
                  ]
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )
