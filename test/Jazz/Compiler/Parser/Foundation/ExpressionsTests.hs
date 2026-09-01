{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Parser.Foundation.ExpressionsTests
  ( expressionTests,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Parser
  ( parseSurfaceProgram,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceCaseArm (..),
    SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceLiteral (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceExpr,
  )
import Jazz.Compiler.TypeRepresentation (SignatureType (..))
import Jazz.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertRight,
    failTest,
  )

expressionTests :: [NamedTest]
expressionTests =
  [ ("parses let binding and expression statement", testParseLetAndExpr),
    ("parseSurfaceProgram accepts Text input", testParseSurfaceProgramAcceptsTextInput),
    ("parses tuple literal into structured nodes", testParseTupleLiteral),
    ("tracks every nested expression location", testTracksEveryNestedExpressionLocation),
    ("lowers Char and Text literals into analyzer AST", testLowersCharAndTextLiterals),
    ("parses fractional literal without treating decimal dot as statement terminator", testParseFractionalLiteral),
    ("parses fractional literal suffixes as concrete float targets", testParseFractionalLiteralSuffixes),
    ("ignores hash line comments between statements", testIgnoresHashLineComments),
    ("tracks tab-aligned expression spans", testTabAlignedExpressionSpan),
    ("parses nested scope expression", testParseNestedScopeExpression),
    ("parses block argument expression with stable inner spans", testParseBlockArgumentExpression),
    ("lowers parsed surface AST into analyzer AST", testLowerSurfaceProgram),
    ("lowers fractional literal into analyzer AST", testLowerFractionalLiteralProgram),
    ("lowers fractional literal suffixes into analyzer AST", testLowerFractionalLiteralSuffixesProgram),
    ("parses integer literals beyond host Int", testParsesLargeIntegerLiteral),
    ("parses abstraction keywords as ordinary binding names", testParsesAbstractionKeywordsAsBindingNames),
    ("parses operator keyword as an ordinary binding name", testParsesOperatorKeywordAsBindingName),
    ("parses operator keyword as a nested block binding name", testParsesOperatorKeywordAsNestedBlockBindingName),
    ("parses explicit-parameter class capability declarations into surface AST", testParsesParameterizedClassCapabilityDeclaration),
    ("parses impl capability declarations into surface AST", testParsesImplCapabilityDeclaration),
    ("lowers class and impl capability declarations as inert AST nodes", testLowersCapabilityDeclarations),
    ("parses impl method binding metadata", testParsesImplMethodBindingMetadata),
    ("lowers impl method binding metadata", testLowersImplMethodBindingMetadata)
  ]

testTracksEveryNestedExpressionLocation :: IO ()
testTracksEveryNestedExpressionLocation =
  assertRight
    "nested expression locations parse"
    ( parseSurfaceProgram
        ( Text.unlines
            [ "entry = \\(input) ->",
              "  (f @Int input,",
              "   [if True then case input { | Just item -> item + 1 | _ -> 0 } else 0],",
              "   (10 +),",
              "   (+ 20))."
            ]
        )
    )
    assertLocations
  where
    assertLocations surfaceProgram =
      case surfaceExprForm surfaceProgram of
        SEBlock [SSLet _ _ lambdaExpr] -> do
          assertExprLocation "program block" (SourceSpan 1 1) surfaceProgram
          assertExprLocation "lambda" (SourceSpan 1 9) lambdaExpr
          case surfaceExprForm lambdaExpr of
            SELambda _ tupleExpr -> do
              assertExprLocation "tuple" (SourceSpan 2 3) tupleExpr
              case surfaceExprForm tupleExpr of
                SETuple tupleElements -> assertTupleLocations tupleElements
                _ -> unexpected "tuple" tupleExpr
            _ -> unexpected "lambda" lambdaExpr
        _ -> failTest ("nested expression locations: unexpected AST " <> Text.pack (show surfaceProgram))

    assertTupleLocations tupleElements =
      case tupleElements of
        [applicationExpr, listExpr, leftSectionExpr, rightSectionExpr] -> do
          assertExprLocation "application" (SourceSpan 2 4) applicationExpr
          case surfaceExprForm applicationExpr of
            SEApply typeApplicationExpr argumentExpr -> do
              assertExprLocation "type application" (SourceSpan 2 4) typeApplicationExpr
              assertExprLocation "application argument" (SourceSpan 2 11) argumentExpr
              case surfaceExprForm typeApplicationExpr of
                SETypeApplication functionExpr _ _ ->
                  assertExprLocation "type-applied function" (SourceSpan 2 4) functionExpr
                _ -> unexpected "type application" typeApplicationExpr
            _ -> unexpected "application" applicationExpr
          assertExprLocation "list" (SourceSpan 3 4) listExpr
          case surfaceExprForm listExpr of
            SEList [ifExpr] -> assertIfLocations ifExpr
            _ -> unexpected "list" listExpr
          assertExprLocation "left section" (SourceSpan 4 4) leftSectionExpr
          case surfaceExprForm leftSectionExpr of
            SESectionLeft leftSectionValue _ ->
              assertExprLocation "left section value" (SourceSpan 4 5) leftSectionValue
            _ -> unexpected "left section" leftSectionExpr
          assertExprLocation "right section" (SourceSpan 5 4) rightSectionExpr
          case surfaceExprForm rightSectionExpr of
            SESectionRight _ rightSectionValue ->
              assertExprLocation "right section value" (SourceSpan 5 7) rightSectionValue
            _ -> unexpected "right section" rightSectionExpr
        _ -> failTest ("nested expression locations: unexpected tuple " <> Text.pack (show tupleElements))

    assertIfLocations ifExpr = do
      assertExprLocation "if" (SourceSpan 3 5) ifExpr
      case surfaceExprForm ifExpr of
        SEIf conditionExpr caseExpr elseExpr -> do
          assertExprLocation "if condition" (SourceSpan 3 8) conditionExpr
          assertExprLocation "case" (SourceSpan 3 18) caseExpr
          assertExprLocation "if else branch" (SourceSpan 3 71) elseExpr
          case surfaceExprForm caseExpr of
            SECase scrutineeExpr caseArms -> do
              assertExprLocation "case scrutinee" (SourceSpan 3 23) scrutineeExpr
              assertCaseArmLocations caseArms
            _ -> unexpected "case" caseExpr
        _ -> unexpected "if" ifExpr

    assertCaseArmLocations caseArms =
      case caseArms of
        [SurfaceCaseArm _ Nothing binaryExpr, SurfaceCaseArm _ Nothing fallbackExpr] -> do
          assertExprLocation "case binary body" (SourceSpan 3 46) binaryExpr
          case surfaceExprForm binaryExpr of
            SEBinary _ leftExpr rightExpr -> do
              assertExprLocation "case binary left" (SourceSpan 3 46) leftExpr
              assertExprLocation "case binary right" (SourceSpan 3 53) rightExpr
            _ -> unexpected "binary" binaryExpr
          assertExprLocation "case fallback body" (SourceSpan 3 62) fallbackExpr
        _ -> failTest ("nested expression locations: unexpected case arms " <> Text.pack (show caseArms))

    assertExprLocation label expected expression =
      assertEqual label expected (surfaceExprSpan expression)

    unexpected label expression =
      failTest ("nested expression locations: unexpected " <> label <> " " <> Text.pack (show expression))

testParseLetAndExpr :: IO ()
testParseLetAndExpr =
  assertEqual
    "surface AST"
    ( Right
        (e 1 1 $ SEBlock [SSLet "x" (SourceSpan 1 1) (e 1 5 $ SELit (SLInt 1)), SSExpr (SourceSpan 2 1) (e 2 1 $ SEVar "x")])
    )
    ( parseSurfaceProgram
        """
        x = 1.
        x.
        """
    )

testParseSurfaceProgramAcceptsTextInput :: IO ()
testParseSurfaceProgramAcceptsTextInput = do
  let sourceText :: Text
      sourceText =
        """
        x = 1.
        x.
        """
  assertEqual
    "surface AST from Text source"
    ( Right
        (e 1 1 $ SEBlock [SSLet "x" (SourceSpan 1 1) (e 1 5 $ SELit (SLInt 1)), SSExpr (SourceSpan 2 1) (e 2 1 $ SEVar "x")])
    )
    (parseSurfaceProgram sourceText)

testParseTupleLiteral :: IO ()
testParseTupleLiteral =
  assertEqual
    "tuple literal surface AST"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSExpr
                  (SourceSpan 1 1)
                  (e 1 1 $ SETuple [e 1 2 $ SELit (SLInt 1), e 1 5 $ SELit (SLBool True)])
              ]
        )
    )
    (parseSurfaceProgram "(1, True).")

testLowersCharAndTextLiterals :: IO ()
testLowersCharAndTextLiterals = do
  assertEqual "lower Char" (ELit (LChar 'a')) (lowerSurfaceExpr (e 1 1 $ SELit (SLChar 'a')))
  assertEqual "lower Text" (ELit (LText "Jazz")) (lowerSurfaceExpr (e 1 1 $ SELit (SLText "Jazz")))

testParseFractionalLiteral :: IO ()
testParseFractionalLiteral =
  assertRight
    "fractional literal parse"
    ( parseSurfaceProgram
        """
        x = 1.5.
        y = 2.
        """
    )
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
    ( parseSurfaceProgram
        """
        x16 = 1.5f16.
        x32 = 2.5f32.
        x64 = 3.5f64.
        """
    )
    ( \surfaceProgram -> do
        let renderedProgram = Text.pack (show surfaceProgram)
        assertContains "Float16 suffix target" "Just NumericFloat16" renderedProgram
        assertContains "Float32 suffix target" "Just NumericFloat32" renderedProgram
        assertContains "Float64 suffix target" "Just NumericFloat64" renderedProgram
    )

testIgnoresHashLineComments :: IO ()
testIgnoresHashLineComments =
  -- Explicit escapes are intentional: this case asserts exact whitespace or source spans.
  assertEqual
    "comments ignored"
    ( Right
        (e 1 1 $ SEBlock [SSLet "x" (SourceSpan 1 1) (e 1 5 $ SELit (SLInt 1)), SSExpr (SourceSpan 3 1) (e 3 1 $ SEVar "x")])
    )
    (parseSurfaceProgram "x = 1.\n# parser should ignore this line comment\nx.")

testTabAlignedExpressionSpan :: IO ()
testTabAlignedExpressionSpan =
  assertEqual
    "tab-aligned span"
    ( Right
        (e 1 9 $ SEBlock [SSExpr (SourceSpan 1 9) (e 1 9 $ SEVar "x")])
    )
    (parseSurfaceProgram "\tx.")

testParseNestedScopeExpression :: IO ()
testParseNestedScopeExpression =
  assertEqual
    "nested block AST"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSLet "x" (SourceSpan 1 1) (e 1 5 $ SELit (SLInt 1)),
                SSExpr
                  (SourceSpan 2 1)
                  (e 2 1 $ SEBlock [SSExpr (SourceSpan 2 3) (e 2 3 $ SEVar "x")])
              ]
        )
    )
    ( parseSurfaceProgram
        """
        x = 1.
        { x. }.
        """
    )

testParseBlockArgumentExpression :: IO ()
testParseBlockArgumentExpression =
  assertEqual
    "block argument AST"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSLet
                  "result"
                  (SourceSpan 1 1)
                  ( e 1 10 $
                      SEApply
                        (e 1 10 $ SEVar "f")
                        ( e 1 12 $
                            SEBlock
                              [ SSLet "x" (SourceSpan 2 3) (e 2 7 $ SELit (SLInt 1)),
                                SSExpr (SourceSpan 3 3) (e 3 3 $ SEVar "x")
                              ]
                        )
                  )
              ]
        )
    )
    ( parseSurfaceProgram
        """
        result = f {
          x = 1.
          x.
        }.
        """
    )

testLowerSurfaceProgram :: IO ()
testLowerSurfaceProgram =
  assertRight
    "parse + lower"
    ( parseSurfaceProgram
        """
        x = 1.
        x.
        """
    )
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
    ( parseSurfaceProgram
        """
        x16 = 1.5f16.
        x32 = 2.5f32.
        x64 = 3.5f64.
        """
    )
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
        (e 1 1 $ SEBlock [SSLet "x" (SourceSpan 1 1) (e 1 5 $ SELit (SLInt 9223372036854775808))])
    )

testParsesAbstractionKeywordsAsBindingNames :: IO ()
testParsesAbstractionKeywordsAsBindingNames =
  assertEqual
    "abstraction keyword binding names"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSLet "class" (SourceSpan 1 1) (e 1 9 $ SELit (SLInt 1)),
                SSLet "impl" (SourceSpan 2 1) (e 2 8 $ SEVar "class"),
                SSLet "trait" (SourceSpan 3 1) (e 3 9 $ SEVar "impl")
              ]
        )
    )
    ( parseSurfaceProgram
        """
        class = 1.
        impl = class.
        trait = impl.
        """
    )

testParsesOperatorKeywordAsBindingName :: IO ()
testParsesOperatorKeywordAsBindingName =
  assertEqual
    "operator keyword binding name"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSLet "operator" (SourceSpan 1 1) (e 1 12 $ SELit (SLInt 1)),
                SSLet "result" (SourceSpan 2 1) (e 2 10 $ SEVar "operator")
              ]
        )
    )
    ( parseSurfaceProgram
        """
        operator = 1.
        result = operator.
        """
    )

testParsesOperatorKeywordAsNestedBlockBindingName :: IO ()
testParsesOperatorKeywordAsNestedBlockBindingName =
  assertEqual
    "operator keyword nested block binding name"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSLet
                  "scope"
                  (SourceSpan 1 1)
                  ( e 1 9 $
                      SEBlock
                        [ SSLet "operator" (SourceSpan 2 3) (e 2 14 $ SELit (SLInt 1)),
                          SSExpr (SourceSpan 3 3) (e 3 3 $ SEVar "operator")
                        ]
                  )
              ]
        )
    )
    ( parseSurfaceProgram
        """
        scope = {
          operator = 1.
          operator.
        }.
        """
    )

testParsesParameterizedClassCapabilityDeclaration :: IO ()
testParsesParameterizedClassCapabilityDeclaration =
  assertEqual
    "parameterized class capability declaration"
    ( Right
        ( e 1 1 $
            SEBlock
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
        ( e 1 1 $
            SEBlock
              [ SSImpl
                  (SourceSpan 1 1)
                  "Eq"
                  [TypeInt]
                  []
              ]
        )
    )
    (parseSurfaceProgram "impl Eq(Int) { }.")

testLowersCapabilityDeclarations :: IO ()
testLowersCapabilityDeclarations =
  assertRight
    "surface parse"
    ( parseSurfaceProgram
        """
        class Eq(a) { }.
        impl Eq(Int) { }.
        """
    )
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
    ( parseSurfaceProgram
        """
        impl Eq(Int) {
        equals = \\(left, right) -> left == right.
        }.
        """
    )
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
    ( parseSurfaceProgram
        """
        impl Eq(Int) {
        equals = \\(left, right) -> left == right.
        }.
        """
    )
    ( \surfaceProgram -> do
        let rendered = Text.pack (show (lowerSurfaceExpr surfaceProgram))
        assertContains "lowered impl method metadata" "ImplMethod" rendered
        assertContains "lowered impl method name" "SourceName (Identifier \"equals\" Pure)" rendered
        assertContains "lowered impl method expression" "EBinary \"==\"" rendered
    )

e :: Int -> Int -> SurfaceExprForm -> SurfaceExpr
e line column = SurfaceExpr (SourceSpan line column)
