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
  ( LocatedModuleExportName (..),
    ModuleExportSelector (..),
  )
import Jazz.Compiler.Name
  ( NameNamespace (ValueNamespace),
    mkQualifiedIdentifier,
    qualifiedMethodName,
    qualifiedName,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceName (..),
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
    loweredImpl,
    loweredLet,
    loweredLiteral,
    loweredVariable,
    parseSurfaceProgramPoints,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertRight,
    failTest,
  )

moduleTests :: [NamedTest]
moduleTests =
  [ ("parses operator keyword as a module-body binding name", testParsesOperatorKeywordAsModuleBodyBindingName),
    ("parses reserved value as an export selector", testParsesValueExportSelector),
    ("parses trait as an ordinary import alias", testParsesTraitAsImportAlias),
    ("lowers class-qualified method reference as variable", testLowersClassQualifiedMethodReference),
    ("parses and lowers alias-qualified class method reference", testLowersAliasQualifiedClassMethodReference),
    ("parses and lowers alias-qualified impl head", testLowersAliasQualifiedImplHead),
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
                  (Just [ModuleExportSelector (Just ValueNamespace) (LocatedModuleExportName "answer" (SourceSpan 1 23))]),
                SSLet "answer" (SourceSpan 2 3) (e 2 12 $ SELit (LInt 42))
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
                SSLet "operator" (SourceSpan 2 1) (e 2 12 $ SELit (LInt 1)),
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
        result = Equatable::equals 1 1.
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
                      (loweredApply (loweredVariable (qualifiedName "Equatable" "equals")) (loweredLiteral (LInt 1)))
                      (loweredLiteral (LInt 1))
                  ),
                loweredExpression (SourceSpan 2 1) (loweredVariable "result")
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowersAliasQualifiedClassMethodReference :: IO ()
testLowersAliasQualifiedClassMethodReference =
  assertRight
    "parse alias-qualified class method reference"
    (parseSurfaceProgramPoints "Facts::Equatable::equals 1 1.")
    ( \surfaceProgram -> do
        case surfaceProgram of
          SurfaceExpr
            _
            ( SEBlock
                [ SSExpr
                    _
                    ( SurfaceExpr
                        _
                        ( SEApply
                            ( SurfaceExpr
                                _
                                ( SEApply
                                    (SurfaceExpr _ (SEQualifiedMethod aliasName className methodName _ _ _))
                                    _
                                  )
                              )
                            _
                          )
                      )
                  ]
              ) ->
              assertEqual
                "alias-qualified method components"
                ("Facts", "Equatable", "equals")
                (aliasName, className, methodName)
          _ -> failTest "expected an alias-qualified class method application"
        assertLoweredCoreEqual
          "lowered alias-qualified class method reference"
          ( loweredBlock
              [ loweredExpression
                  (SourceSpan 1 1)
                  ( loweredApply
                      ( loweredApply
                          (loweredVariable (qualifiedMethodName "Facts" "Equatable" "equals"))
                          (loweredLiteral (LInt 1))
                      )
                      (loweredLiteral (LInt 1))
                  )
              ]
          )
          (lowerSurfaceExpr surfaceProgram)
    )

testLowersAliasQualifiedImplHead :: IO ()
testLowersAliasQualifiedImplHead =
  assertRight
    "parse alias-qualified impl head"
    (parseSurfaceProgramPoints "impl Facts::Equatable(Int) { }.")
    ( \surfaceProgram -> do
        assertEqual
          "alias-qualified impl surface name"
          ( e 1 1 $
              SEBlock
                [ SSImpl
                    (SourceSpan 1 1)
                    (SurfaceName (mkQualifiedIdentifier "Facts" "Equatable") (SourceSpan 1 13) (Just (SourceSpan 1 6)))
                    [TypeInt]
                    []
                    []
                ]
          )
          surfaceProgram
        assertLoweredCoreEqual
          "lowered alias-qualified impl head"
          (loweredBlock [loweredImpl (SourceSpan 1 1) (qualifiedName "Facts" "Equatable") [TypeInt] []])
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
                SSClass (SourceSpan 2 1) "Equatable" ["a"] [] [] [],
                SSImpl (SourceSpan 3 1) (SurfaceName "Equatable" (SourceSpan 3 6) Nothing) [TypeInt] [] []
              ]
        )
    )
    ( parseSurfaceProgramPoints
        """
        module App::Core {
        class Equatable(a) { }.
        impl Equatable(Int) { }.
        }
        """
    )

e :: Int -> Int -> SurfaceExprForm -> SurfaceExpr
e line column = SurfaceExpr (SourceSpan line column)
