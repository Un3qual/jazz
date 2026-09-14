{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Parser.Foundation.SignaturesTests
  ( signatureTests,
  )
where

import Jazz.Compiler.AST
  ( Literal (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Name
  ( mkQualifiedIdentifier,
    qualifiedName,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceClassMethodSignature (..),
    SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceName (..),
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
    parseSurfaceProgramPoints,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertRight,
    failTest,
  )

signatureTests :: [NamedTest]
signatureTests =
  [ ("parses signature statement with source span", testParseSignatureSpan),
    ("parses qualified result types after arrows", testParseQualifiedResultTypes),
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
    ("parses and lowers alias-qualified class constraint", testAliasQualifiedClassConstraint),
    ("parses constrained signature with empty constraint block", testParseEmptyConstraintBlockSignaturePayload),
    ("parses constrained tuple signature into structured nodes", testParseConstrainedTupleSignaturePayload),
    ("parses explicit type application expression", testParseExplicitTypeApplicationExpression),
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

testParseQualifiedResultTypes :: IO ()
testParseQualifiedResultTypes =
  assertRight
    "qualified result type"
    (parseSurfaceProgramPoints "f :: Int -> Facts::OnlyType.")
    ( assertEqual
        "qualified result signature"
        ( e
            1
            1
            ( SEBlock
                [ SSSignature
                    "f"
                    (SourceSpan 1 1)
                    (SignatureType (TypeFunction TypeInt (TypeName (SurfaceName (mkQualifiedIdentifier "Facts" "OnlyType") (SourceSpan 1 20) (Just (SourceSpan 1 13))))))
                ]
            )
        )
    )

testParseSignatureSpan :: IO ()
testParseSignatureSpan =
  -- Explicit escapes are intentional: this case asserts exact whitespace or source spans.
  assertEqual
    "signature span"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSSignature "x" (SourceSpan 1 1) (SignatureType (TypeInt)),
                SSLet "x" (SourceSpan 2 1) (e 2 5 $ SELit (LInt 1))
              ]
        )
    )
    (parseSurfaceProgramPoints "x :: Int.\nx = 1.")

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
    ( parseSurfaceProgramPoints
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
                      (TypeApplication (SurfaceName "Maybe" (SourceSpan 1 19) Nothing) [TypeChar])
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
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
                SSLet "pair" (SourceSpan 2 1) (e 2 8 $ SETuple [e 2 9 $ SELit (LInt 1), e 2 12 $ SELit (LBool True)])
              ]
        )
    )
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
                SSLet "x" (SourceSpan 2 1) (e 2 5 $ SELit (LInt 1))
              ]
        )
    )
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
                SSLet "fns" (SourceSpan 2 1) (e 2 7 $ SEList [e 2 8 $ SESectionRight "+" (e 2 11 $ SELit (LInt 1))])
              ]
        )
    )
    ( parseSurfaceProgramPoints
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
                      [ SignatureConstraint (SurfaceName "Eq" (SourceSpan 1 8) Nothing) [TypeVariable "a"],
                        SignatureConstraint (SurfaceName "Ord" (SourceSpan 1 15) Nothing) [TypeVariable "b"]
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
    ( parseSurfaceProgramPoints
        """
        f :: @{Eq(a), Ord(b)}: a -> b -> c.
        f = combine.
        """
    )

testAliasQualifiedClassConstraint :: IO ()
testAliasQualifiedClassConstraint =
  assertRight
    "parse alias-qualified class constraint"
    ( parseSurfaceProgramPoints
        """
        same :: @{Facts::Eq(a)}: a -> a -> Bool.
        same = identity.
        """
    )
    ( \surfaceProgram -> do
        assertEqual
          "alias-qualified constraint surface name"
          ( e 1 1 $
              SEBlock
                [ SSSignature
                    "same"
                    (SourceSpan 1 1)
                    ( ConstrainedSignature
                        [ SignatureConstraint
                            (SurfaceName (mkQualifiedIdentifier "Facts" "Eq") (SourceSpan 1 18) (Just (SourceSpan 1 11)))
                            [TypeVariable "a"]
                        ]
                        (TypeFunction (TypeVariable "a") (TypeFunction (TypeVariable "a") TypeBool))
                    ),
                  SSLet "same" (SourceSpan 2 1) (e 2 8 $ SEVar "identity")
                ]
          )
          surfaceProgram
        assertLoweredCoreEqual
          "lowered alias-qualified class constraint"
          ( loweredBlock
              [ loweredSignature
                  "same"
                  (SourceSpan 1 1)
                  ( ConstrainedSignature
                      [SignatureConstraint (qualifiedName "Facts" "Eq") [TypeVariable "a"]]
                      (TypeFunction (TypeVariable "a") (TypeFunction (TypeVariable "a") TypeBool))
                  ),
                loweredLet "same" (SourceSpan 2 1) (loweredVariable "identity")
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
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
    ( parseSurfaceProgramPoints
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
                SSLet "pair" (SourceSpan 2 1) (e 2 8 $ SETuple [e 2 9 $ SELit (LInt 1), e 2 12 $ SELit (LBool True)])
              ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        pair :: @{}: (Int, Bool).
        pair = (1, True).
        """
    )

testParseExplicitTypeApplicationExpression :: IO ()
testParseExplicitTypeApplicationExpression =
  assertRight
    "explicit type application parse"
    ( parseSurfaceProgramPoints
        """
        result = id @Int 1.
        result.
        """
    )
    ( \surfaceProgram ->
        case surfaceExprForm surfaceProgram of
          SEBlock [SSLet result _ (SurfaceExpr _ (SEApply (SurfaceExpr _ (SETypeApplication (SurfaceExpr _ (SEVar function)) _ TypeInt)) (SurfaceExpr _ (SELit (LInt 1))))), SSExpr _ (SurfaceExpr _ (SEVar output))] -> do
            assertEqual "binding name" "result" result
            assertEqual "applied function" "id" function
            assertEqual "result reference" "result" output
          _ -> failTest "expected result = (id @Int) 1 followed by result"
    )

testLoweredExplicitTypeApplicationIsCanonical :: IO ()
testLoweredExplicitTypeApplicationIsCanonical =
  assertRight
    "parse + canonical lower explicit type application"
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
    ( parseSurfaceProgramPoints
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
                SSLet "class" (SourceSpan 2 1) (e 2 9 $ SELit (LInt 1)),
                SSSignature "impl" (SourceSpan 3 1) (SignatureType TypeBool),
                SSLet "impl" (SourceSpan 4 1) (e 4 8 $ SELit (LBool True)),
                SSSignature "trait" (SourceSpan 5 1) (SignatureType TypeInt),
                SSLet "trait" (SourceSpan 6 1) (e 6 9 $ SELit (LInt 2))
              ]
        )
    )
    ( parseSurfaceProgramPoints
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
                SSLet "operator" (SourceSpan 2 1) (e 2 12 $ SELit (LInt 1))
              ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        operator :: Int.
        operator = 1.
        """
    )

testParsesClassMethodSignatureMetadata :: IO ()
testParsesClassMethodSignatureMetadata =
  assertRight
    "surface class method signature parse"
    ( parseSurfaceProgramPoints
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
                    []
                    []
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
