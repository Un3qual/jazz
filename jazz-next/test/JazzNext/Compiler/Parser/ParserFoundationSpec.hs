{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.AST
  ( ConstraintSignatureType (..),
    Expr (..),
    Literal (..),
    SignatureConstraint (..),
    SignaturePayload (..),
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
  ( SurfaceConstrainedSignatureType (..),
    SurfaceExpr (..),
    SurfaceLiteral (..),
    SurfaceSignatureConstraint (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureType (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )
import JazzNext.TestHarness
  ( NamedTest,
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
    ("parses chained function signature right associatively", testParseChainedFunctionSignature),
    ("parses parenthesized function override into structured nodes", testParseParenthesizedFunctionOverrideSignature),
    ("parses list of parenthesized function types", testParseFunctionListSignature),
    ("parses constrained signature into structured nodes", testParseConstrainedSignaturePayload),
    ("parses constrained signature with empty constraint block", testParseEmptyConstraintBlockSignaturePayload),
    ("parses constrained tuple signature into structured nodes", testParseConstrainedTupleSignaturePayload),
    ("ignores hash line comments between statements", testIgnoresHashLineComments),
    ("tracks tab-aligned expression spans", testTabAlignedExpressionSpan),
    ("parses nested scope expression", testParseNestedScopeExpression),
    ("lowers parsed surface AST into analyzer AST", testLowerSurfaceProgram),
    ("lowers tuple literal and signature into analyzer AST", testLowerTupleLiteralAndSignatureProgram),
    ("lowers structured signature payload into analyzer AST", testLowerStructuredSignatureProgram),
    ("lowers right-associated function signature into analyzer AST", testLowerRightAssociativeFunctionSignatureProgram),
    ("lowers list of function signature into analyzer AST", testLowerFunctionListSignatureProgram),
    ("lowers constrained signature payload into analyzer AST", testLowerConstrainedSignatureProgram),
    ("lowers constrained tuple signature payload into analyzer AST", testLowerConstrainedTupleSignatureProgram),
    ("rejects missing statement terminator", testRejectsMissingDotTerminator),
    ("rejects signature missing terminator before next statement", testRejectsMissingSignatureDot),
    ("rejects integer literal overflow", testRejectsIntOverflow),
    ("rejects negative literal syntax for now", testRejectsNegativeLiteralSyntax),
    ("parses abstraction keywords as ordinary binding names", testParsesAbstractionKeywordsAsBindingNames),
    ("parses abstraction keywords as ordinary signature names", testParsesAbstractionKeywordsAsSignatureNames),
    ("parses trait as an ordinary import alias", testParsesTraitAsImportAlias),
    ("rejects class abstraction declarations as deferred syntax", testRejectsClassAbstractionSyntax),
    ("rejects impl abstraction declarations as deferred syntax", testRejectsImplAbstractionSyntax),
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

testRejectsMissingSignatureDot :: IO ()
testRejectsMissingSignatureDot =
  assertLeftDiagnosticContains
    "missing signature dot error"
    "expected '.'"
    (parseSurfaceProgram "x :: Int\nx = 1.")

testRejectsIntOverflow :: IO ()
testRejectsIntOverflow =
  assertLeftDiagnosticContains
    "integer overflow"
    "integer literal out of range"
    (parseSurfaceProgram "x = 9999999999999999999999999999999999999.")

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

testRejectsClassAbstractionSyntax :: IO ()
testRejectsClassAbstractionSyntax =
  assertLeftDiagnosticContains
    "class abstraction syntax deferred"
    "unsupported abstraction syntax 'class'"
    (parseSurfaceProgram "class Eq { }.")

testRejectsImplAbstractionSyntax :: IO ()
testRejectsImplAbstractionSyntax =
  assertLeftDiagnosticContains
    "impl abstraction syntax deferred"
    "unsupported abstraction syntax 'impl'"
    (parseSurfaceProgram "impl Eq { }.")

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
