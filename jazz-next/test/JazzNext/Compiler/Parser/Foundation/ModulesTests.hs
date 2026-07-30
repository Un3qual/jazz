{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser.Foundation.ModulesTests
  ( moduleTests
  ) where

import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.ModuleExports
  ( ModuleExportSelector (..)
  )
import JazzNext.Compiler.Name
  ( NameNamespace (ValueNamespace),
    qualifiedName
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
    assertEqual,
    assertRight
  )

moduleTests :: [NamedTest]
moduleTests =
  [ ("parses operator keyword as a module-body binding name", testParsesOperatorKeywordAsModuleBodyBindingName)
    , ("parses reserved value as an export selector", testParsesValueExportSelector)
    , ("parses trait as an ordinary import alias", testParsesTraitAsImportAlias)
    , ("lowers class-qualified method reference as variable", testLowersClassQualifiedMethodReference)
    , ("parses class and impl capability declarations inside module bodies", testParsesCapabilityDeclarationsInModuleBody)
  ]

testParsesValueExportSelector :: IO ()
testParsesValueExportSelector =
  assertEqual
    "reserved value export selector"
    ( Right
        ( SEBlock
            [ SSModule
                (SourceSpan 1 1)
                ["Example"]
                (Just [ModuleExportSelector (Just ValueNamespace) "answer"]),
              SSLet "answer" (SourceSpan 2 3) (SELit (SLInt 42))
            ]
        )
    )
    ( parseSurfaceProgram
        """
        module Example (value answer) {
          answer = 42.
        }
        """
    )

testParsesOperatorKeywordAsModuleBodyBindingName :: IO ()
testParsesOperatorKeywordAsModuleBodyBindingName =
  assertEqual
    "operator keyword module-body binding name"
    ( Right
        ( SEBlock
            [ SSModule (SourceSpan 1 1) ["App", "Core"] Nothing,
              SSLet "operator" (SourceSpan 2 1) (SELit (SLInt 1)),
              SSLet "result" (SourceSpan 3 1) (SEVar "operator")
            ]
        )
    )
    (parseSurfaceProgram """
    module App::Core {
    operator = 1.
    result = operator.
    }
    """)

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
    (parseSurfaceProgram """
    import Lib::Math as trait.
    trait::subtract.
    """)

testLowersClassQualifiedMethodReference :: IO ()
testLowersClassQualifiedMethodReference =
  assertRight
    "parse + lower class-qualified method reference"
    (parseSurfaceProgram """
    result = Eq::equals 1 1.
    result.
    """)
    ( \surfaceProgram ->
        assertEqual
          "lowered class-qualified method reference"
          ( EBlock
              [ SLet
                  "result"
                  (SourceSpan 1 1)
                  ( EApply
                      (EApply (EVar (qualifiedName "Eq" "equals")) (ELit (LInt 1)))
                      (ELit (LInt 1))
                  ),
                SExpr (SourceSpan 2 1) (EVar "result")
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
            [ SSModule (SourceSpan 1 1) ["App", "Core"] Nothing,
              SSClass (SourceSpan 2 1) "Eq" ["a"] [],
              SSImpl (SourceSpan 3 1) "Eq" [SurfaceTypeInt] []
            ]
        )
    )
    (parseSurfaceProgram """
    module App::Core {
    class Eq(a) { }.
    impl Eq(Int) { }.
    }
    """)
