{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
import JazzNext.Compiler.Desugar
  ( desugarExpr
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
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertLeftDiagnosticContains,
    assertRight,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "ParserFoundation" tests

tests :: [NamedTest]
tests =
  [ ("parses let binding and expression statement", testParseLetAndExpr),
    ("parseSurfaceProgram accepts Text input", testParseSurfaceProgramAcceptsTextInput),
    ("parses signature statement with source span", testParseSignatureSpan),
    ("parses parenthesized function signature into structured nodes", testParseParenthesizedFunctionSignature),
    ("parses tuple literal into structured nodes", testParseTupleLiteral),
    ("parses tuple signature into structured nodes", testParseTupleSignature),
    ("parses Unit value and signature into structured nodes", testParseUnitValueAndSignature),
    ("parses constrained Unit signature into structured nodes", testParseConstrainedUnitSignature),
    ("parses numeric width signature names into structured nodes", testParseNumericWidthSignatureTypes),
    ("parses fractional literal without treating decimal dot as statement terminator", testParseFractionalLiteral),
    ("parses fractional literal suffixes as concrete float targets", testParseFractionalLiteralSuffixes),
    ("rejects non-finite fractional literals", testRejectsNonFiniteFractionalLiteral),
    ("rejects source-exact Float64 fractional literal overflow", testRejectsSourceExactFloat64FractionalLiteralOverflow),
    ("rejects fractional literal case patterns", testRejectsFractionalLiteralCasePatterns),
    ("rejects fractional literal lambda patterns", testRejectsFractionalLiteralLambdaPatterns),
    ("parses chained function signature right associatively", testParseChainedFunctionSignature),
    ("parses parenthesized function override into structured nodes", testParseParenthesizedFunctionOverrideSignature),
    ("parses list of parenthesized function types", testParseFunctionListSignature),
    ("parses constrained signature into structured nodes", testParseConstrainedSignaturePayload),
    ("parses constrained signature with empty constraint block", testParseEmptyConstraintBlockSignaturePayload),
    ("parses constrained tuple signature into structured nodes", testParseConstrainedTupleSignaturePayload),
    ("ignores hash line comments between statements", testIgnoresHashLineComments),
    ("tracks tab-aligned expression spans", testTabAlignedExpressionSpan),
    ("parses nested scope expression", testParseNestedScopeExpression),
    ("parses block argument expression with stable inner spans", testParseBlockArgumentExpression),
    ("parses explicit type application expression", testParseExplicitTypeApplicationExpression),
    ("rejects unsupported explicit type application argument", testRejectsUnsupportedExplicitTypeApplicationArgument),
    ("lowers parsed surface AST into analyzer AST", testLowerSurfaceProgram),
    ("lowers explicit type application expression", testLowerExplicitTypeApplicationExpression),
    ("desugars lowered explicit type application expression", testDesugarExplicitTypeApplicationExpression),
    ("lowers tuple literal and signature into analyzer AST", testLowerTupleLiteralAndSignatureProgram),
    ("lowers Unit value and signature into analyzer AST", testLowerUnitValueAndSignature),
    ("lowers numeric width signature names into analyzer AST", testLowerNumericWidthSignatureProgram),
    ("lowers fractional literal into analyzer AST", testLowerFractionalLiteralProgram),
    ("lowers fractional literal suffixes into analyzer AST", testLowerFractionalLiteralSuffixesProgram),
    ("lowers structured signature payload into analyzer AST", testLowerStructuredSignatureProgram),
    ("lowers right-associated function signature into analyzer AST", testLowerRightAssociativeFunctionSignatureProgram),
    ("lowers list of function signature into analyzer AST", testLowerFunctionListSignatureProgram),
    ("lowers constrained signature payload into analyzer AST", testLowerConstrainedSignatureProgram),
    ("lowers constrained tuple signature payload into analyzer AST", testLowerConstrainedTupleSignatureProgram),
    ("rejects missing statement terminator", testRejectsMissingDotTerminator),
    ("rejects unterminated block expression", testRejectsUnterminatedBlockExpression),
    ("rejects signature missing terminator before next statement", testRejectsMissingSignatureDot),
    ("rejects signature missing terminator before class declaration", testRejectsMissingSignatureDotBeforeClass),
    ("parses integer literals beyond host Int", testParsesLargeIntegerLiteral),
    ("rejects negative literal syntax for now", testRejectsNegativeLiteralSyntax),
    ("parses abstraction keywords as ordinary binding names", testParsesAbstractionKeywordsAsBindingNames),
    ("parses abstraction keywords as ordinary signature names", testParsesAbstractionKeywordsAsSignatureNames),
    ("parses operator keyword as an ordinary binding name", testParsesOperatorKeywordAsBindingName),
    ("parses operator keyword as an ordinary signature name", testParsesOperatorKeywordAsSignatureName),
    ("parses operator keyword as a module-body binding name", testParsesOperatorKeywordAsModuleBodyBindingName),
    ("parses operator keyword as a nested block binding name", testParsesOperatorKeywordAsNestedBlockBindingName),
    ("parses trait as an ordinary import alias", testParsesTraitAsImportAlias),
    ("lowers class-qualified method reference as variable", testLowersClassQualifiedMethodReference),
    ("rejects class capability declarations without parameters", testRejectsClassCapabilityDeclarationWithoutParameters),
    ("rejects class capability declarations with multiple parameters", testRejectsClassCapabilityDeclarationWithMultipleParameters),
    ("parses explicit-parameter class capability declarations into surface AST", testParsesParameterizedClassCapabilityDeclaration),
    ("parses impl capability declarations into surface AST", testParsesImplCapabilityDeclaration),
    ("lowers class and impl capability declarations as inert AST nodes", testLowersCapabilityDeclarations),
    ("parses class and impl capability declarations inside module bodies", testParsesCapabilityDeclarationsInModuleBody),
    ("parses class method signature metadata", testParsesClassMethodSignatureMetadata),
    ("rejects class method body syntax", testRejectsClassMethodBodySyntax),
    ("rejects duplicate class method signatures", testRejectsDuplicateClassMethodSignatures),
    ("rejects non-signature class body items", testRejectsNonSignatureClassBodyItem),
    ("parses impl method binding metadata", testParsesImplMethodBindingMetadata),
    ("lowers impl method binding metadata", testLowersImplMethodBindingMetadata),
    ("rejects variable-target impl method bindings", testRejectsVariableTargetImplMethodBindings),
    ("rejects variable-target impl declarations with empty bodies", testRejectsVariableTargetEmptyImplDeclarations),
    ("rejects duplicate impl method bindings", testRejectsDuplicateImplMethodBindings),
    ("rejects non-binding impl body items", testRejectsNonBindingImplBodyItem),
    ("rejects duplicate class parameters", testRejectsDuplicateClassParameters),
    ("rejects concrete class parameters", testRejectsConcreteClassParameters),
    ("rejects malformed class capability headers", testRejectsMalformedClassCapabilityHeader),
    ("rejects trait abstraction declarations as non-canonical syntax", testRejectsTraitAbstractionSyntax),
    ("rejects lowercase trait abstraction declarations", testRejectsLowercaseTraitAbstractionSyntax),
    ("rejects trait abstraction declarations inside module bodies", testRejectsTraitAbstractionSyntaxInModuleBody)
  ]

testParseLetAndExpr :: IO ()
testParseLetAndExpr =
  assertEqual
    "surface AST"
    ( Right
        ( SEBlock
            [ SSLet "x" (SourceSpan 1 1) (SELit (SLInt 1)),
              SSExpr (SourceSpan 2 1) (SEVar "x")
            ]
        )
    )
    (parseSurfaceProgram "x = 1.\nx.")

testParseSurfaceProgramAcceptsTextInput :: IO ()
testParseSurfaceProgramAcceptsTextInput = do
  let sourceText :: Text
      sourceText = "x = 1.\nx."
  assertEqual
    "surface AST from Text source"
    ( Right
        ( SEBlock
            [ SSLet "x" (SourceSpan 1 1) (SELit (SLInt 1)),
              SSExpr (SourceSpan 2 1) (SEVar "x")
            ]
        )
    )
    (parseSurfaceProgram sourceText)

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

testParseTupleLiteral :: IO ()
testParseTupleLiteral =
  assertEqual
    "tuple literal surface AST"
    ( Right
        ( SEBlock
            [ SSExpr
                (SourceSpan 1 1)
                (SETuple [SELit (SLInt 1), SELit (SLBool True)])
            ]
        )
    )
    (parseSurfaceProgram "(1, True).")

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

testParseFractionalLiteral :: IO ()
testParseFractionalLiteral =
  assertRight
    "fractional literal parse"
    (parseSurfaceProgram "x = 1.5.\ny = 2.")
    ( \surfaceProgram ->
        assertContains
          "surface fractional literal"
          "SLFloat 1.5"
          (Text.pack (show surfaceProgram))
    )

testParseFractionalLiteralSuffixes :: IO ()
testParseFractionalLiteralSuffixes =
  assertRight
    "fractional literal suffix parse"
    (parseSurfaceProgram "x16 = 1.5f16.\nx32 = 2.5f32.\nx64 = 3.5f64.")
    ( \surfaceProgram -> do
        let renderedProgram = Text.pack (show surfaceProgram)
        assertContains "Float16 suffix target" "Just SurfaceNumericFloat16" renderedProgram
        assertContains "Float32 suffix target" "Just SurfaceNumericFloat32" renderedProgram
        assertContains "Float64 suffix target" "Just SurfaceNumericFloat64" renderedProgram
    )

testRejectsNonFiniteFractionalLiteral :: IO ()
testRejectsNonFiniteFractionalLiteral =
  assertLeftDiagnosticContains
    "non-finite fractional literal"
    "invalid fractional literal"
    (parseSurfaceProgram (Text.pack ("x = " <> replicate 400 '9' <> ".0.")))

testRejectsSourceExactFloat64FractionalLiteralOverflow :: IO ()
testRejectsSourceExactFloat64FractionalLiteralOverflow =
  assertLeftDiagnosticContains
    "source-exact Float64 fractional literal overflow"
    "invalid fractional literal"
    (parseSurfaceProgram (Text.pack ("x = " <> show (float64MaxFiniteInteger + 1) <> ".0.")))

testRejectsFractionalLiteralCasePatterns :: IO ()
testRejectsFractionalLiteralCasePatterns =
  assertLeftDiagnosticContains
    "fractional literal case pattern"
    "fractional literal patterns"
    (parseSurfaceProgram "x = case 1 { | 1.5 -> True | _ -> False }.")

testRejectsFractionalLiteralLambdaPatterns :: IO ()
testRejectsFractionalLiteralLambdaPatterns =
  assertLeftDiagnosticContains
    "fractional literal lambda pattern"
    "fractional literal patterns"
    (parseSurfaceProgram "f = \\(1.5) -> True.")

float64MaxFiniteInteger :: Integer
float64MaxFiniteInteger =
  ceiling (float64MaxFinite :: Double)

float64MaxFinite :: Double
float64MaxFinite =
  encodeFloat
    (floatRadix sample ^ floatDigits sample - 1)
    (snd (floatRange sample) - floatDigits sample)
  where
    sample = 0 :: Double

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

testIgnoresHashLineComments :: IO ()
testIgnoresHashLineComments =
  assertEqual
    "comments ignored"
    ( Right
        ( SEBlock
            [ SSLet "x" (SourceSpan 1 1) (SELit (SLInt 1)),
              SSExpr (SourceSpan 3 1) (SEVar "x")
            ]
        )
    )
    (parseSurfaceProgram "x = 1.\n# parser should ignore this line comment\nx.")

testTabAlignedExpressionSpan :: IO ()
testTabAlignedExpressionSpan =
  assertEqual
    "tab-aligned span"
    ( Right
        ( SEBlock
            [ SSExpr (SourceSpan 1 9) (SEVar "x")
            ]
        )
    )
    (parseSurfaceProgram "\tx.")

testParseNestedScopeExpression :: IO ()
testParseNestedScopeExpression =
  assertEqual
    "nested block AST"
    ( Right
        ( SEBlock
            [ SSLet "x" (SourceSpan 1 1) (SELit (SLInt 1)),
              SSExpr
                (SourceSpan 2 1)
                ( SEBlock
                    [SSExpr (SourceSpan 2 3) (SEVar "x")]
                )
            ]
        )
    )
    (parseSurfaceProgram "x = 1.\n{ x. }.")

testParseBlockArgumentExpression :: IO ()
testParseBlockArgumentExpression =
  assertEqual
    "block argument AST"
    ( Right
        ( SEBlock
            [ SSLet
                "result"
                (SourceSpan 1 1)
                ( SEApply
                    (SEVar "f")
                    ( SEBlock
                        [ SSLet "x" (SourceSpan 2 3) (SELit (SLInt 1)),
                          SSExpr (SourceSpan 3 3) (SEVar "x")
                        ]
                    )
                )
            ]
        )
    )
    (parseSurfaceProgram "result = f {\n  x = 1.\n  x.\n}.")

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

testRejectsUnsupportedExplicitTypeApplicationArgument :: IO ()
testRejectsUnsupportedExplicitTypeApplicationArgument =
  assertLeftDiagnosticContains
    "unsupported explicit type application argument"
    "unsupported explicit type application argument after '@'"
    (parseSurfaceProgram "value = id @a 1.\nvalue.")

testLowerSurfaceProgram :: IO ()
testLowerSurfaceProgram =
  assertRight
    "parse + lower"
    (parseSurfaceProgram "x = 1.\nx.")
    (\surfaceProgram -> assertEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SLet "x" (SourceSpan 1 1) (ELit (LInt 1)),
          SExpr (SourceSpan 2 1) (EVar "x")
        ]

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

testDesugarExplicitTypeApplicationExpression :: IO ()
testDesugarExplicitTypeApplicationExpression =
  assertRight
    "parse + lower + desugar explicit type application"
    (parseSurfaceProgram "value = id @Int 1.\nvalue.")
    ( \surfaceProgram ->
        assertEqual
          "desugared type application AST"
          expectedProgram
          (desugarExpr (lowerSurfaceExpr surfaceProgram))
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

testLowerFractionalLiteralProgram :: IO ()
testLowerFractionalLiteralProgram =
  assertRight
    "surface parse"
    (parseSurfaceProgram "1.5.")
    ( \surfaceProgram ->
        assertContains
          "lowered fractional literal"
          "LFloat 1.5"
          (Text.pack (show (lowerSurfaceExpr surfaceProgram)))
    )

testLowerFractionalLiteralSuffixesProgram :: IO ()
testLowerFractionalLiteralSuffixesProgram =
  assertRight
    "parse + lower suffixed fractional literals"
    (parseSurfaceProgram "x16 = 1.5f16.\nx32 = 2.5f32.\nx64 = 3.5f64.")
    ( \surfaceProgram -> do
        let renderedProgram = Text.pack (show (lowerSurfaceExpr surfaceProgram))
        assertContains "lowered Float16 suffix target" "Just NumericFloat16" renderedProgram
        assertContains "lowered Float32 suffix target" "Just NumericFloat32" renderedProgram
        assertContains "lowered Float64 suffix target" "Just NumericFloat64" renderedProgram
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

testRejectsMissingDotTerminator :: IO ()
testRejectsMissingDotTerminator =
  assertLeftDiagnosticContains
    "missing dot error"
    "expected '.'"
    (parseSurfaceProgram "x = 1 y = 2.")

testRejectsUnterminatedBlockExpression :: IO ()
testRejectsUnterminatedBlockExpression =
  assertLeftDiagnosticContains
    "unterminated block expression"
    "expected '}'"
    (parseSurfaceProgram "x = { y = 1. y.")

testRejectsMissingSignatureDot :: IO ()
testRejectsMissingSignatureDot =
  assertLeftDiagnosticContains
    "missing signature dot error"
    "expected '.'"
    (parseSurfaceProgram "x :: Int\nx = 1.")

testRejectsMissingSignatureDotBeforeClass :: IO ()
testRejectsMissingSignatureDotBeforeClass =
  assertLeftDiagnosticContains
    "missing signature dot before class"
    "expected '.' before 'class'"
    (parseSurfaceProgram "x :: Int\nclass Eq { }.")

testParsesLargeIntegerLiteral :: IO ()
testParsesLargeIntegerLiteral =
  assertRight
    "large integer literal"
    (parseSurfaceProgram "x = 9223372036854775808.")
    ( assertEqual
        "large integer surface AST"
        ( SEBlock
            [ SSLet "x" (SourceSpan 1 1) (SELit (SLInt 9223372036854775808))
            ]
        )
    )

testRejectsNegativeLiteralSyntax :: IO ()
testRejectsNegativeLiteralSyntax =
  assertLeftDiagnosticContains
    "negative literal unsupported"
    "expected expression"
    (parseSurfaceProgram "x = -1.")

testParsesAbstractionKeywordsAsBindingNames :: IO ()
testParsesAbstractionKeywordsAsBindingNames =
  assertEqual
    "abstraction keyword binding names"
    ( Right
        ( SEBlock
            [ SSLet "class" (SourceSpan 1 1) (SELit (SLInt 1)),
              SSLet "impl" (SourceSpan 2 1) (SEVar "class"),
              SSLet "trait" (SourceSpan 3 1) (SEVar "impl")
            ]
        )
    )
    (parseSurfaceProgram "class = 1.\nimpl = class.\ntrait = impl.")

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

testParsesOperatorKeywordAsBindingName :: IO ()
testParsesOperatorKeywordAsBindingName =
  assertEqual
    "operator keyword binding name"
    ( Right
        ( SEBlock
            [ SSLet "operator" (SourceSpan 1 1) (SELit (SLInt 1)),
              SSLet "value" (SourceSpan 2 1) (SEVar "operator")
            ]
        )
    )
    (parseSurfaceProgram "operator = 1.\nvalue = operator.")

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

testParsesOperatorKeywordAsModuleBodyBindingName :: IO ()
testParsesOperatorKeywordAsModuleBodyBindingName =
  assertEqual
    "operator keyword module-body binding name"
    ( Right
        ( SEBlock
            [ SSModule (SourceSpan 1 1) ["App", "Core"],
              SSLet "operator" (SourceSpan 2 1) (SELit (SLInt 1)),
              SSLet "value" (SourceSpan 3 1) (SEVar "operator")
            ]
        )
    )
    (parseSurfaceProgram "module App::Core {\noperator = 1.\nvalue = operator.\n}")

testParsesOperatorKeywordAsNestedBlockBindingName :: IO ()
testParsesOperatorKeywordAsNestedBlockBindingName =
  assertEqual
    "operator keyword nested block binding name"
    ( Right
        ( SEBlock
            [ SSLet
                "scope"
                (SourceSpan 1 1)
                ( SEBlock
                    [ SSLet "operator" (SourceSpan 2 3) (SELit (SLInt 1)),
                      SSExpr (SourceSpan 3 3) (SEVar "operator")
                    ]
                )
            ]
        )
    )
    (parseSurfaceProgram "scope = {\n  operator = 1.\n  operator.\n}.")

testParsesTraitAsImportAlias :: IO ()
testParsesTraitAsImportAlias =
  assertEqual
    "trait import alias lookup"
    ( Right
        ( SEBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "trait") Nothing,
              SSExpr (SourceSpan 2 1) (SEQualifiedVar "trait" "subtract")
            ]
        )
    )
    (parseSurfaceProgram "import Lib::Math as trait.\ntrait::subtract.")

testLowersClassQualifiedMethodReference :: IO ()
testLowersClassQualifiedMethodReference =
  assertRight
    "parse + lower class-qualified method reference"
    (parseSurfaceProgram "result = Eq::equals 1 1.\nresult.")
    ( \surfaceProgram ->
        assertEqual
          "lowered class-qualified method reference"
          ( EBlock
              [ SLet
                  "result"
                  (SourceSpan 1 1)
                  ( EApply
                      (EApply (EVar "Eq::equals") (ELit (LInt 1)))
                      (ELit (LInt 1))
                  ),
                SExpr (SourceSpan 2 1) (EVar "result")
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testRejectsClassCapabilityDeclarationWithoutParameters :: IO ()
testRejectsClassCapabilityDeclarationWithoutParameters =
  assertLeftDiagnosticContains
    "class capability declaration without parameters"
    "explicit parameter list"
    (parseSurfaceProgram "class Eq { }.")

testRejectsClassCapabilityDeclarationWithMultipleParameters :: IO ()
testRejectsClassCapabilityDeclarationWithMultipleParameters =
  assertLeftDiagnosticContains
    "class capability declaration with multiple parameters"
    "exactly one parameter"
    (parseSurfaceProgram "class Eq(a, b) { }.")

testParsesParameterizedClassCapabilityDeclaration :: IO ()
testParsesParameterizedClassCapabilityDeclaration =
  assertEqual
    "parameterized class capability declaration"
    ( Right
        ( SEBlock
            [ SSClass (SourceSpan 1 1) "Eq" ["a"] []
            ]
        )
    )
    (parseSurfaceProgram "class Eq(a) { }.")

testParsesImplCapabilityDeclaration :: IO ()
testParsesImplCapabilityDeclaration =
  assertEqual
    "impl capability declaration"
    ( Right
        ( SEBlock
            [ SSImpl (SourceSpan 1 1) "Eq"
                [SurfaceConstrainedTypeName "Int"]
                []
            ]
        )
    )
    (parseSurfaceProgram "impl Eq(Int) { }.")

testLowersCapabilityDeclarations :: IO ()
testLowersCapabilityDeclarations =
  assertRight
    "surface parse"
    (parseSurfaceProgram "class Eq(a) { }.\nimpl Eq(Int) { }.")
    ( \surfaceProgram ->
        assertEqual
          "lowered capability declarations"
          ( EBlock
              [ SClass (SourceSpan 1 1) "Eq" ["a"] [],
                SImpl (SourceSpan 2 1) "Eq" [ConstraintTypeName "Int"] []
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testParsesCapabilityDeclarationsInModuleBody :: IO ()
testParsesCapabilityDeclarationsInModuleBody =
  assertEqual
    "module body capability declarations"
    ( Right
        ( SEBlock
            [ SSModule (SourceSpan 1 1) ["App", "Core"],
              SSClass (SourceSpan 2 1) "Eq" ["a"] [],
              SSImpl (SourceSpan 3 1) "Eq" [SurfaceConstrainedTypeName "Int"] []
            ]
        )
    )
    (parseSurfaceProgram "module App::Core {\nclass Eq(a) { }.\nimpl Eq(Int) { }.\n}")

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

testRejectsClassMethodBodySyntax :: IO ()
testRejectsClassMethodBodySyntax =
  assertLeftDiagnosticContains
    "class method body syntax"
    "method body/default syntax"
    (parseSurfaceProgram "class Eq(a) { equals = \\value -> value. }.")

testRejectsDuplicateClassMethodSignatures :: IO ()
testRejectsDuplicateClassMethodSignatures =
  assertLeftDiagnosticContains
    "duplicate class method signature"
    "duplicate method signature 'equals'"
    (parseSurfaceProgram "class Eq(a) { equals :: Int. equals :: Bool. }.")

testRejectsNonSignatureClassBodyItem :: IO ()
testRejectsNonSignatureClassBodyItem =
  assertLeftDiagnosticContains
    "non-signature class body item"
    "signature-only method declaration"
    (parseSurfaceProgram "class Eq(a) { 1. }.")

testParsesImplMethodBindingMetadata :: IO ()
testParsesImplMethodBindingMetadata =
  assertRight
    "surface impl method binding metadata parse"
    (parseSurfaceProgram "impl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.")
    ( \surfaceProgram -> do
        let rendered = Text.pack (show surfaceProgram)
        assertContains "surface impl method metadata" "SurfaceImplMethod" rendered
        assertContains "surface impl method name" "identifierText = \"equals\"" rendered
        assertContains "surface impl method expression" "SEBinary \"==\"" rendered
    )

testLowersImplMethodBindingMetadata :: IO ()
testLowersImplMethodBindingMetadata =
  assertRight
    "surface impl method binding metadata parse"
    (parseSurfaceProgram "impl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.")
    ( \surfaceProgram -> do
        let rendered = Text.pack (show (lowerSurfaceExpr surfaceProgram))
        assertContains "lowered impl method metadata" "ImplMethod" rendered
        assertContains "lowered impl method name" "identifierText = \"equals\"" rendered
        assertContains "lowered impl method expression" "EBinary \"==\"" rendered
    )

testRejectsVariableTargetImplMethodBindings :: IO ()
testRejectsVariableTargetImplMethodBindings =
  assertLeftDiagnosticContains
    "variable-target impl method binding"
    "concrete impl target"
    (parseSurfaceProgram "impl Eq(a) { equals = 1. }.")

testRejectsVariableTargetEmptyImplDeclarations :: IO ()
testRejectsVariableTargetEmptyImplDeclarations =
  assertLeftDiagnosticContains
    "variable-target empty impl declaration"
    "concrete impl target"
    (parseSurfaceProgram "impl Eq(a) { }.")

testRejectsDuplicateImplMethodBindings :: IO ()
testRejectsDuplicateImplMethodBindings =
  assertLeftDiagnosticContains
    "duplicate impl method binding"
    "duplicate method binding 'equals'"
    (parseSurfaceProgram "impl Eq(Int) { equals = 1. equals = 2. }.")

testRejectsNonBindingImplBodyItem :: IO ()
testRejectsNonBindingImplBodyItem =
  assertLeftDiagnosticContains
    "non-binding impl body item"
    "ordinary method binding"
    (parseSurfaceProgram "impl Eq(Int) { equals :: Int. }.")

testRejectsDuplicateClassParameters :: IO ()
testRejectsDuplicateClassParameters =
  assertLeftDiagnosticContains
    "duplicate class parameter"
    "duplicate class parameter 'a'"
    (parseSurfaceProgram "class Eq(a, a) { }.")

testRejectsConcreteClassParameters :: IO ()
testRejectsConcreteClassParameters =
  assertLeftDiagnosticContains
    "concrete class parameter"
    "class parameters must be lowercase type variables"
    (parseSurfaceProgram "class Eq(Int) { }.")

testRejectsMalformedClassCapabilityHeader :: IO ()
testRejectsMalformedClassCapabilityHeader =
  assertLeftDiagnosticContains
    "malformed class capability header"
    "unexpected token 'Bar' in class declaration header"
    (parseSurfaceProgram "class Foo Bar Baz(Int, String) { }.")

testRejectsTraitAbstractionSyntax :: IO ()
testRejectsTraitAbstractionSyntax =
  assertLeftDiagnosticContains
    "trait abstraction syntax non-canonical"
    "unsupported abstraction syntax 'trait'"
    (parseSurfaceProgram "trait Eq { }.")

testRejectsLowercaseTraitAbstractionSyntax :: IO ()
testRejectsLowercaseTraitAbstractionSyntax =
  assertLeftDiagnosticContains
    "lowercase trait abstraction syntax non-canonical"
    "unsupported abstraction syntax 'trait'"
    (parseSurfaceProgram "trait eq { }.")

testRejectsTraitAbstractionSyntaxInModuleBody :: IO ()
testRejectsTraitAbstractionSyntaxInModuleBody =
  assertLeftDiagnosticContains
    "trait abstraction syntax in module body"
    "unsupported abstraction syntax 'trait'"
    (parseSurfaceProgram "module App::Core {\ntrait Eq { }.\n}")
