{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Parser.Foundation.ModulesTests
  ( moduleTests,
  )
where

import Jazz.Compiler.AST
  ( Literal (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExportSelector (..),
  )
import Jazz.Compiler.Name
  ( NameNamespace (ValueNamespace),
    qualifiedName,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceLiteral (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceExpr,
  )
import Jazz.Compiler.TypeRepresentation (SignatureType (..))
import Jazz.TestCore
  ( assertLoweredCoreEqual,
    loweredApply,
    loweredBlock,
    loweredExpression,
    loweredLet,
    loweredLiteral,
    loweredVariable,
    parseSurfaceProgramPoints,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertRight,
  )

moduleTests :: [NamedTest]
moduleTests =
  [ ("parses operator keyword as a module-body binding name", testParsesOperatorKeywordAsModuleBodyBindingName),
    ("parses reserved value as an export selector", testParsesValueExportSelector),
    ("parses trait as an ordinary import alias", testParsesTraitAsImportAlias),
    ("lowers class-qualified method reference as variable", testLowersClassQualifiedMethodReference),
    ("parses class and impl capability declarations inside module bodies", testParsesCapabilityDeclarationsInModuleBody)
  ]

testParsesValueExportSelector :: IO ()
testParsesValueExportSelector =
  assertEqual
    "reserved value export selector"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSModule
                  (SourceSpan 1 1)
                  ["Example"]
                  (Just [ModuleExportSelector (Just ValueNamespace) "answer"]),
                SSLet "answer" (SourceSpan 2 3) (e 2 12 $ SELit (SLInt 42))
              ]
        )
    )
    ( parseSurfaceProgramPoints
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
        ( e 1 1 $
            SEBlock
              [ SSModule (SourceSpan 1 1) ["App", "Core"] Nothing,
                SSLet "operator" (SourceSpan 2 1) (e 2 12 $ SELit (SLInt 1)),
                SSLet "result" (SourceSpan 3 1) (e 3 10 $ SEVar "operator")
              ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        module App::Core {
        operator = 1.
        result = operator.
        }
        """
    )

testParsesTraitAsImportAlias :: IO ()
testParsesTraitAsImportAlias =
  assertEqual
    "trait import alias lookup"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "trait") Nothing,
                SSExpr (SourceSpan 2 1) (e 2 1 $ SEQualifiedVar "trait" "subtract")
              ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        import Lib::Math as trait.
        trait::subtract.
        """
    )

testLowersClassQualifiedMethodReference :: IO ()
testLowersClassQualifiedMethodReference =
  assertRight
    "parse + lower class-qualified method reference"
    ( parseSurfaceProgramPoints
        """
        result = Eq::equals 1 1.
        result.
        """
    )
    ( \surfaceProgram ->
        assertLoweredCoreEqual
          "lowered class-qualified method reference"
          ( loweredBlock
              [ loweredLet
                  "result"
                  (SourceSpan 1 1)
                  ( loweredApply
                      (loweredApply (loweredVariable (qualifiedName "Eq" "equals")) (loweredLiteral (LInt 1)))
                      (loweredLiteral (LInt 1))
                  ),
                loweredExpression (SourceSpan 2 1) (loweredVariable "result")
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testParsesCapabilityDeclarationsInModuleBody :: IO ()
testParsesCapabilityDeclarationsInModuleBody =
  assertEqual
    "module body capability declarations"
    ( Right
        ( e 1 1 $
            SEBlock
              [ SSModule (SourceSpan 1 1) ["App", "Core"] Nothing,
                SSClass (SourceSpan 2 1) "Eq" ["a"] [],
                SSImpl (SourceSpan 3 1) "Eq" [TypeInt] []
              ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        module App::Core {
        class Eq(a) { }.
        impl Eq(Int) { }.
        }
        """
    )

e :: Int -> Int -> SurfaceExprForm -> SurfaceExpr
e line column = SurfaceExpr (SourceSpan line column)
