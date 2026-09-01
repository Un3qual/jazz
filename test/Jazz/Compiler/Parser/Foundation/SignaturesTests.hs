{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Parser.Foundation.SignaturesTests
  ( signatureTests,
  )
where

import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( Literal (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Parser
  ( parseSurfaceProgram,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceClassMethodSignature (..),
    SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceLiteral (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceExpr,
  )
import Jazz.Compiler.TypeRepresentation
  ( NumericType (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.TestCore
  ( assertLoweredCoreEqual,
    loweredApply,
    loweredBlock,
    loweredClass,
    loweredClassMethodSignature,
    loweredExpression,
    loweredLet,
    loweredList,
    loweredLiteral,
    loweredOperatorValue,
    loweredSectionRight,
    loweredSignature,
    loweredTuple,
    loweredTypeApplication,
    loweredVariable,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertRight,
  )

signatureTests :: [NamedTest]
signatureTests =
  [ ("parses signature statement with source span", testParseSignatureSpan),
    ("parses Char and Text signatures", testParsesCharAndTextSignatures),
    ("parses generic named signatures", testParsesGenericNamedSignatures),
    ("normalizes List application syntax", testNormalizesListApplicationSyntax),
    ("parses parenthesized function signature into structured nodes", testParseParenthesizedFunctionSignature),
    ("parses tuple signature into structured nodes", testParseTupleSignature),
    ("parses Unit value and signature into structured nodes", testParseUnitValueAndSignature),
    ("parses constrained Unit signature into structured nodes", testParseConstrainedUnitSignature),
    ("parses numeric width signature names into structured nodes", testParseNumericWidthSignatureTypes),
    ("parses chained function signature right associatively", testParseChainedFunctionSignature),
    ("parses parenthesized function override into structured nodes", testParseParenthesizedFunctionOverrideSignature),
    ("parses list of parenthesized function types", testParseFunctionListSignature),
    ("parses constrained signature into structured nodes", testParseConstrainedSignaturePayload),
    ("parses constrained signature with empty constraint block", testParseEmptyConstraintBlockSignaturePayload),
    ("parses constrained tuple signature into structured nodes", testParseConstrainedTupleSignaturePayload),
    ("parses explicit type application expression", testParseExplicitTypeApplicationExpression),
    ("lowers explicit type application expression", testLowerExplicitTypeApplicationExpression),
    ("lowered explicit type application needs no post-pass", testLoweredExplicitTypeApplicationIsCanonical),
    ("lowers tuple literal and signature into analyzer AST", testLowerTupleLiteralAndSignatureProgram),
    ("lowers Unit value and signature into analyzer AST", testLowerUnitValueAndSignature),
    ("lowers numeric width signature names into analyzer AST", testLowerNumericWidthSignatureProgram),
    ("lowers structured signature payload into analyzer AST", testLowerStructuredSignatureProgram),
    ("lowers right-associated function signature into analyzer AST", testLowerRightAssociativeFunctionSignatureProgram),
    ("lowers list of function signature into analyzer AST", testLowerFunctionListSignatureProgram),
    ("lowers constrained signature payload into analyzer AST", testLowerConstrainedSignatureProgram),
    ("lowers constrained tuple signature payload into analyzer AST", testLowerConstrainedTupleSignatureProgram),
    ("parses abstraction keywords as ordinary signature names", testParsesAbstractionKeywordsAsSignatureNames),
    ("parses operator keyword as an ordinary signature name", testParsesOperatorKeywordAsSignatureName),
    ("parses class method signature metadata", testParsesClassMethodSignatureMetadata)
  ]

testParseSignatureSpan :: IO ()
testParseSignatureSpan =
  -- Explicit escapes are intentional: this case asserts exact whitespace or source spans.
  assertEqual
    "signature span"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature "x" (SourceSpan 1 1) (SignatureType (TypeInt)),
                SSLet "x" (SourceSpan 2 1) (e 2 5 $ SELit (SLInt 1))
              ]
        )
    )
    (parseSurfaceProgram "x :: Int.\nx = 1.")

testParsesCharAndTextSignatures :: IO ()
testParsesCharAndTextSignatures =
  assertEqual
    "Char/Text signatures"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature "character" (SourceSpan 1 1) (SignatureType TypeChar),
                SSSignature "message" (SourceSpan 2 1) (SignatureType TypeText),
                SSSignature
                  "render"
                  (SourceSpan 3 1)
                  (SignatureType (TypeFunction TypeChar TypeText))
              ]
        )
    )
    ( parseSurfaceProgram
        """
        character :: Char.
        message :: Text.
        render :: Char -> Text.
        """
    )

testParsesGenericNamedSignatures :: IO ()
testParsesGenericNamedSignatures =
  assertEqual
    "generic named signatures"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature
                  "maybeCharacter"
                  (SourceSpan 1 1)
                  ( SignatureType
                      (TypeApplication "Maybe" [TypeChar])
                  ),
                SSSignature
                  "map"
                  (SourceSpan 2 1)
                  ( SignatureType
                      ( TypeFunction
                          (TypeFunction (TypeVariable "a") (TypeVariable "b"))
                          ( TypeFunction
                              (TypeList (TypeVariable "a"))
                              (TypeList (TypeVariable "b"))
                          )
                      )
                  )
              ]
        )
    )
    ( parseSurfaceProgram
        """
        maybeCharacter :: Maybe(Char).
        map :: (a -> b) -> List(a) -> [b].
        """
    )

testNormalizesListApplicationSyntax :: IO ()
testNormalizesListApplicationSyntax =
  assertEqual
    "List(a) and [a] normalization"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature
                  "left"
                  (SourceSpan 1 1)
                  (SignatureType (TypeList (TypeVariable "a"))),
                SSSignature
                  "right"
                  (SourceSpan 2 1)
                  (SignatureType (TypeList (TypeVariable "a")))
              ]
        )
    )
    ( parseSurfaceProgram
        """
        left :: List(a).
        right :: [a].
        """
    )

testParseParenthesizedFunctionSignature :: IO ()
testParseParenthesizedFunctionSignature =
  assertEqual
    "parenthesized function signature"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature
                  "f"
                  (SourceSpan 1 1)
                  ( SignatureType
                      ( TypeFunction
                          (TypeList TypeInt)
                          (TypeList TypeInt)
                      )
                  ),
                SSLet "f" (SourceSpan 2 1) (e 2 5 $ SEOperatorValue "+")
              ]
        )
    )
    ( parseSurfaceProgram
        """
        f :: ([Int]) -> ([Int]).
        f = (+).
        """
    )

testParseTupleSignature :: IO ()
testParseTupleSignature =
  assertEqual
    "tuple signature"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature
                  "pair"
                  (SourceSpan 1 1)
                  (SignatureType (TypeTuple [TypeInt, TypeBool])),
                SSLet "pair" (SourceSpan 2 1) (e 2 8 $ SETuple [e 2 9 $ SELit (SLInt 1), e 2 12 $ SELit (SLBool True)])
              ]
        )
    )
    ( parseSurfaceProgram
        """
        pair :: (Int, Bool).
        pair = (1, True).
        """
    )

testParseUnitValueAndSignature :: IO ()
testParseUnitValueAndSignature =
  assertEqual
    "Unit value and signature"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature
                  "unit"
                  (SourceSpan 1 1)
                  (SignatureType (TypeTuple [])),
                SSLet "unit" (SourceSpan 2 1) (e 2 8 $ SETuple [])
              ]
        )
    )
    ( parseSurfaceProgram
        """
        unit :: ().
        unit = ().
        """
    )

testParseConstrainedUnitSignature :: IO ()
testParseConstrainedUnitSignature =
  assertEqual
    "constrained Unit signature"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature
                  "unit"
                  (SourceSpan 1 1)
                  (ConstrainedSignature [] (TypeTuple [])),
                SSLet "unit" (SourceSpan 2 1) (e 2 8 $ SETuple [])
              ]
        )
    )
    ( parseSurfaceProgram
        """
        unit :: @{}: ().
        unit = ().
        """
    )

testParseNumericWidthSignatureTypes :: IO ()
testParseNumericWidthSignatureTypes = do
  assertEqual
    "Int8 signature"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature "x" (SourceSpan 1 1) (SignatureType (TypeNumeric NumericInt8)),
                SSLet "x" (SourceSpan 2 1) (e 2 5 $ SELit (SLInt 1))
              ]
        )
    )
    ( parseSurfaceProgram
        """
        x :: Int8.
        x = 1.
        """
    )
  assertEqual
    "Float alias signature"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature
                  "f"
                  (SourceSpan 1 1)
                  ( SignatureType
                      (TypeFunction TypeFloat (TypeNumeric NumericFloat64))
                  ),
                SSLet "f" (SourceSpan 2 1) (e 2 5 $ SEOperatorValue "+")
              ]
        )
    )
    ( parseSurfaceProgram
        """
        f :: Float -> Float64.
        f = (+).
        """
    )

testParseChainedFunctionSignature :: IO ()
testParseChainedFunctionSignature =
  assertEqual
    "right-associated function signature"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature
                  "f"
                  (SourceSpan 1 1)
                  ( SignatureType
                      (TypeFunction TypeInt (TypeFunction TypeInt TypeInt))
                  ),
                SSLet "f" (SourceSpan 2 1) (e 2 5 $ SEOperatorValue "+")
              ]
        )
    )
    ( parseSurfaceProgram
        """
        f :: Int -> Int -> Int.
        f = (+).
        """
    )

testParseParenthesizedFunctionOverrideSignature :: IO ()
testParseParenthesizedFunctionOverrideSignature =
  assertEqual
    "parenthesized function override signature"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature
                  "f"
                  (SourceSpan 1 1)
                  ( SignatureType
                      (TypeFunction (TypeFunction TypeInt TypeInt) TypeInt)
                  ),
                SSLet "f" (SourceSpan 2 1) (e 2 5 $ SEVar "applyToOne")
              ]
        )
    )
    ( parseSurfaceProgram
        """
        f :: (Int -> Int) -> Int.
        f = applyToOne.
        """
    )

testParseFunctionListSignature :: IO ()
testParseFunctionListSignature =
  assertEqual
    "list of parenthesized function types"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature
                  "fns"
                  (SourceSpan 1 1)
                  ( SignatureType
                      (TypeList (TypeFunction TypeInt TypeInt))
                  ),
                SSLet "fns" (SourceSpan 2 1) (e 2 7 $ SEList [e 2 8 $ SESectionRight "+" (e 2 11 $ SELit (SLInt 1))])
              ]
        )
    )
    ( parseSurfaceProgram
        """
        fns :: [(Int -> Int)].
        fns = [(+ 1)].
        """
    )

testParseConstrainedSignaturePayload :: IO ()
testParseConstrainedSignaturePayload =
  assertEqual
    "constrained signature payload"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature
                  "f"
                  (SourceSpan 1 1)
                  ( ConstrainedSignature
                      [ SignatureConstraint "Eq" [TypeVariable "a"],
                        SignatureConstraint "Ord" [TypeVariable "b"]
                      ]
                      ( TypeFunction
                          (TypeVariable "a")
                          (TypeFunction (TypeVariable "b") (TypeVariable "c"))
                      )
                  ),
                SSLet "f" (SourceSpan 2 1) (e 2 5 $ SEVar "combine")
              ]
        )
    )
    ( parseSurfaceProgram
        """
        f :: @{Eq(a), Ord(b)}: a -> b -> c.
        f = combine.
        """
    )

testParseEmptyConstraintBlockSignaturePayload :: IO ()
testParseEmptyConstraintBlockSignaturePayload =
  assertEqual
    "empty constrained signature payload"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature
                  "f"
                  (SourceSpan 1 1)
                  (ConstrainedSignature [] TypeInt),
                SSLet "f" (SourceSpan 2 1) (e 2 5 $ SEVar "input")
              ]
        )
    )
    ( parseSurfaceProgram
        """
        f :: @{}: Int.
        f = input.
        """
    )

testParseConstrainedTupleSignaturePayload :: IO ()
testParseConstrainedTupleSignaturePayload =
  assertEqual
    "constrained tuple signature payload"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature
                  "pair"
                  (SourceSpan 1 1)
                  ( ConstrainedSignature
                      []
                      (TypeTuple [TypeInt, TypeBool])
                  ),
                SSLet "pair" (SourceSpan 2 1) (e 2 8 $ SETuple [e 2 9 $ SELit (SLInt 1), e 2 12 $ SELit (SLBool True)])
              ]
        )
    )
    ( parseSurfaceProgram
        """
        pair :: @{}: (Int, Bool).
        pair = (1, True).
        """
    )

testParseExplicitTypeApplicationExpression :: IO ()
testParseExplicitTypeApplicationExpression =
  assertRight
    "explicit type application parse"
    ( parseSurfaceProgram
        """
        result = id @Int 1.
        result.
        """
    )
    ( \surfaceProgram -> do
        let rendered = Text.pack (show surfaceProgram)
        assertContains "surface type application" "SETypeApplication" rendered
        assertContains "surface type application argument" "TypeInt" rendered
    )

testLowerExplicitTypeApplicationExpression :: IO ()
testLowerExplicitTypeApplicationExpression =
  assertRight
    "explicit type application lowering"
    ( parseSurfaceProgram
        """
        result = id @Int 1.
        result.
        """
    )
    ( \surfaceProgram -> do
        let rendered = Text.pack (show (lowerSurfaceExpr surfaceProgram))
        assertContains "lowered type application" "ETypeApplication" rendered
        assertContains "lowered type application argument" "TypeInt" rendered
    )

testLoweredExplicitTypeApplicationIsCanonical :: IO ()
testLoweredExplicitTypeApplicationIsCanonical =
  assertRight
    "parse + canonical lower explicit type application"
    ( parseSurfaceProgram
        """
        result = id @Int 1.
        result.
        """
    )
    ( \surfaceProgram ->
        assertLoweredCoreEqual
          "canonical lowered type application AST"
          expectedProgram
          (lowerSurfaceExpr surfaceProgram)
    )
  where
    expectedProgram =
      loweredBlock
        [ loweredLet
            "result"
            (SourceSpan 1 1)
            (loweredApply (loweredTypeApplication (loweredVariable "id") (SourceSpan 1 13) TypeInt) (loweredLiteral (LInt 1))),
          loweredExpression (SourceSpan 2 1) (loweredVariable "result")
        ]

testLowerTupleLiteralAndSignatureProgram :: IO ()
testLowerTupleLiteralAndSignatureProgram =
  assertRight
    "parse + lower tuple literal/signature"
    ( parseSurfaceProgram
        """
        pair :: (Int, Bool).
        pair = (1, True).
        """
    )
    ( \surfaceProgram ->
        assertLoweredCoreEqual
          "lowered tuple AST"
          ( loweredBlock
              [ loweredSignature
                  "pair"
                  (SourceSpan 1 1)
                  (SignatureType (TypeTuple [TypeInt, TypeBool])),
                loweredLet
                  "pair"
                  (SourceSpan 2 1)
                  (loweredTuple [loweredLiteral (LInt 1), loweredLiteral (LBool True)])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerUnitValueAndSignature :: IO ()
testLowerUnitValueAndSignature =
  assertRight
    "parse + lower Unit value/signature"
    ( parseSurfaceProgram
        """
        unit :: ().
        unit = ().
        """
    )
    ( \surfaceProgram ->
        assertLoweredCoreEqual
          "lowered Unit AST"
          ( loweredBlock
              [ loweredSignature
                  "unit"
                  (SourceSpan 1 1)
                  (SignatureType (TypeTuple [])),
                loweredLet "unit" (SourceSpan 2 1) (loweredTuple [])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerNumericWidthSignatureProgram :: IO ()
testLowerNumericWidthSignatureProgram =
  assertRight
    "parse + lower numeric width signatures"
    ( parseSurfaceProgram
        """
        f :: UInt8 -> Int64 -> Float.
        f = (+).
        """
    )
    ( \surfaceProgram ->
        assertLoweredCoreEqual
          "lowered numeric width signature AST"
          ( loweredBlock
              [ loweredSignature
                  "f"
                  (SourceSpan 1 1)
                  ( SignatureType
                      ( TypeFunction
                          (TypeNumeric NumericUInt8)
                          (TypeFunction (TypeNumeric NumericInt64) TypeFloat)
                      )
                  ),
                loweredLet "f" (SourceSpan 2 1) (loweredOperatorValue "+")
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerStructuredSignatureProgram :: IO ()
testLowerStructuredSignatureProgram =
  assertRight
    "parse + lower structured signature"
    ( parseSurfaceProgram
        """
        x :: [[Bool]].
        x = [[True], [False]].
        """
    )
    ( \surfaceProgram ->
        assertLoweredCoreEqual
          "lowered signature AST"
          ( loweredBlock
              [ loweredSignature
                  "x"
                  (SourceSpan 1 1)
                  (SignatureType (TypeList (TypeList TypeBool))),
                loweredLet
                  "x"
                  (SourceSpan 2 1)
                  (loweredList [loweredList [loweredLiteral (LBool True)], loweredList [loweredLiteral (LBool False)]])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerRightAssociativeFunctionSignatureProgram :: IO ()
testLowerRightAssociativeFunctionSignatureProgram =
  assertRight
    "parse + lower right-associated function signature"
    ( parseSurfaceProgram
        """
        f :: Int -> Int -> Int.
        f = (+).
        """
    )
    ( \surfaceProgram ->
        assertLoweredCoreEqual
          "lowered right-associated signature AST"
          ( loweredBlock
              [ loweredSignature
                  "f"
                  (SourceSpan 1 1)
                  (SignatureType (TypeFunction TypeInt (TypeFunction TypeInt TypeInt))),
                loweredLet "f" (SourceSpan 2 1) (loweredOperatorValue "+")
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerFunctionListSignatureProgram :: IO ()
testLowerFunctionListSignatureProgram =
  assertRight
    "parse + lower list of function signature"
    ( parseSurfaceProgram
        """
        fns :: [(Int -> Int)].
        fns = [(+ 1)].
        """
    )
    ( \surfaceProgram ->
        assertLoweredCoreEqual
          "lowered list of function signature AST"
          ( loweredBlock
              [ loweredSignature
                  "fns"
                  (SourceSpan 1 1)
                  (SignatureType (TypeList (TypeFunction TypeInt TypeInt))),
                loweredLet
                  "fns"
                  (SourceSpan 2 1)
                  (loweredList [loweredSectionRight "+" (loweredLiteral (LInt 1))])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerConstrainedSignatureProgram :: IO ()
testLowerConstrainedSignatureProgram =
  assertRight
    "parse + lower constrained signature"
    ( parseSurfaceProgram
        """
        f :: @{Eq(a)}: a -> a.
        f = identity.
        """
    )
    ( \surfaceProgram ->
        assertLoweredCoreEqual
          "lowered constrained signature AST"
          ( loweredBlock
              [ loweredSignature
                  "f"
                  (SourceSpan 1 1)
                  ( ConstrainedSignature
                      [SignatureConstraint "Eq" [TypeVariable "a"]]
                      (TypeFunction (TypeVariable "a") (TypeVariable "a"))
                  ),
                loweredLet "f" (SourceSpan 2 1) (loweredVariable "identity")
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowerConstrainedTupleSignatureProgram :: IO ()
testLowerConstrainedTupleSignatureProgram =
  assertRight
    "parse + lower constrained tuple signature"
    ( parseSurfaceProgram
        """
        pair :: @{}: (Int, Bool).
        pair = (1, True).
        """
    )
    ( \surfaceProgram ->
        assertLoweredCoreEqual
          "lowered constrained tuple signature AST"
          ( loweredBlock
              [ loweredSignature
                  "pair"
                  (SourceSpan 1 1)
                  ( ConstrainedSignature
                      []
                      (TypeTuple [TypeInt, TypeBool])
                  ),
                loweredLet
                  "pair"
                  (SourceSpan 2 1)
                  (loweredTuple [loweredLiteral (LInt 1), loweredLiteral (LBool True)])
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testParsesAbstractionKeywordsAsSignatureNames :: IO ()
testParsesAbstractionKeywordsAsSignatureNames =
  assertEqual
    "abstraction keyword signature names"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature "class" (SourceSpan 1 1) (SignatureType TypeInt),
                SSLet "class" (SourceSpan 2 1) (e 2 9 $ SELit (SLInt 1)),
                SSSignature "impl" (SourceSpan 3 1) (SignatureType TypeBool),
                SSLet "impl" (SourceSpan 4 1) (e 4 8 $ SELit (SLBool True)),
                SSSignature "trait" (SourceSpan 5 1) (SignatureType TypeInt),
                SSLet "trait" (SourceSpan 6 1) (e 6 9 $ SELit (SLInt 2))
              ]
        )
    )
    ( parseSurfaceProgram
        """
        class :: Int.
        class = 1.
        impl :: Bool.
        impl = True.
        trait :: Int.
        trait = 2.
        """
    )

testParsesOperatorKeywordAsSignatureName :: IO ()
testParsesOperatorKeywordAsSignatureName =
  assertEqual
    "operator keyword signature name"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature "operator" (SourceSpan 1 1) (SignatureType TypeInt),
                SSLet "operator" (SourceSpan 2 1) (e 2 12 $ SELit (SLInt 1))
              ]
        )
    )
    ( parseSurfaceProgram
        """
        operator :: Int.
        operator = 1.
        """
    )

testParsesClassMethodSignatureMetadata :: IO ()
testParsesClassMethodSignatureMetadata =
  assertRight
    "surface class method signature parse"
    ( parseSurfaceProgram
        """
        class Eq(a) {
        equals :: a -> a -> Bool.
        notEquals :: a -> a -> Bool.
        }.
        """
    )
    ( \surfaceProgram -> do
        let surfacePayload =
              SignatureType
                ( TypeFunction
                    (TypeVariable "a")
                    (TypeFunction (TypeVariable "a") TypeBool)
                )
            corePayload =
              SignatureType
                ( TypeFunction
                    (TypeVariable "a")
                    (TypeFunction (TypeVariable "a") TypeBool)
                )
        assertEqual
          "surface class method metadata"
          ( e 1 1 $
              SEBlock
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
        assertLoweredCoreEqual
          "lowered class method metadata"
          ( loweredBlock
              [ loweredClass
                  (SourceSpan 1 1)
                  "Eq"
                  ["a"]
                  [ loweredClassMethodSignature "equals" (SourceSpan 2 1) corePayload,
                    loweredClassMethodSignature "notEquals" (SourceSpan 3 1) corePayload
                  ]
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

e :: Int -> Int -> SurfaceExprForm -> SurfaceExpr
e line column = SurfaceExpr (SourceSpan line column)
