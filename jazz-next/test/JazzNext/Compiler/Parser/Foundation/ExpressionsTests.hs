{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser.Foundation.ExpressionsTests
  ( expressionTests
) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
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
  ( SurfaceExpr (..),
    SurfaceLiteral (..),
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
    assertRight
  )

expressionTests :: [NamedTest]
expressionTests =
  [ ("parses let binding and expression statement", testParseLetAndExpr)
    , ("parseSurfaceProgram accepts Text input", testParseSurfaceProgramAcceptsTextInput)
    , ("parses tuple literal into structured nodes", testParseTupleLiteral)
    , ("lowers Char and Text literals into analyzer AST", testLowersCharAndTextLiterals)
    , ("parses fractional literal without treating decimal dot as statement terminator", testParseFractionalLiteral)
    , ("parses fractional literal suffixes as concrete float targets", testParseFractionalLiteralSuffixes)
    , ("ignores hash line comments between statements", testIgnoresHashLineComments)
    , ("tracks tab-aligned expression spans", testTabAlignedExpressionSpan)
    , ("parses nested scope expression", testParseNestedScopeExpression)
    , ("parses block argument expression with stable inner spans", testParseBlockArgumentExpression)
    , ("lowers parsed surface AST into analyzer AST", testLowerSurfaceProgram)
    , ("lowers fractional literal into analyzer AST", testLowerFractionalLiteralProgram)
    , ("lowers fractional literal suffixes into analyzer AST", testLowerFractionalLiteralSuffixesProgram)
    , ("parses integer literals beyond host Int", testParsesLargeIntegerLiteral)
    , ("parses abstraction keywords as ordinary binding names", testParsesAbstractionKeywordsAsBindingNames)
    , ("parses operator keyword as an ordinary binding name", testParsesOperatorKeywordAsBindingName)
    , ("parses operator keyword as a nested block binding name", testParsesOperatorKeywordAsNestedBlockBindingName)
    , ("parses explicit-parameter class capability declarations into surface AST", testParsesParameterizedClassCapabilityDeclaration)
    , ("parses impl capability declarations into surface AST", testParsesImplCapabilityDeclaration)
    , ("lowers class and impl capability declarations as inert AST nodes", testLowersCapabilityDeclarations)
    , ("parses impl method binding metadata", testParsesImplMethodBindingMetadata)
    , ("lowers impl method binding metadata", testLowersImplMethodBindingMetadata)
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

testLowersCharAndTextLiterals :: IO ()
testLowersCharAndTextLiterals = do
  assertEqual "lower Char" (ELit (LChar 'a')) (lowerSurfaceExpr (SELit (SLChar 'a')))
  assertEqual "lower Text" (ELit (LText "Jazz")) (lowerSurfaceExpr (SELit (SLText "Jazz")))

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
                [SurfaceTypeInt]
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
                SImpl (SourceSpan 2 1) "Eq" [TypeInt] []
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testParsesImplMethodBindingMetadata :: IO ()
testParsesImplMethodBindingMetadata =
  assertRight
    "surface impl method binding metadata parse"
    (parseSurfaceProgram "impl Eq(Int) {\nequals = \\(left) -> \\(right) -> left == right.\n}.")
    ( \surfaceProgram -> do
        let rendered = Text.pack (show surfaceProgram)
        assertContains "surface impl method metadata" "SurfaceImplMethod" rendered
        assertContains "surface impl method name" "Identifier \"equals\" Pure" rendered
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
        assertContains "lowered impl method name" "SourceName (Identifier \"equals\" Pure)" rendered
        assertContains "lowered impl method expression" "EBinary \"==\"" rendered
    )
