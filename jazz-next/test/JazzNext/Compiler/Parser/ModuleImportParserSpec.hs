{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
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
    assertEqual,
    assertLeftDiagnosticContains,
    assertRight,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "ModuleImportParser" tests

tests :: [NamedTest]
tests =
  [ ("parses module declaration statement", testParsesModuleDeclaration),
    ("parses import statement bare dot", testParsesImportBare),
    ("parses import statement with alias", testParsesImportAlias),
    ("parses qualified alias lookup expression", testParsesQualifiedAliasLookup),
    ("parses lowercase alias qualified lookup expression", testParsesLowercaseQualifiedAliasLookup),
    ("parses lowercase qualified lookup before alias import", testParsesLowercaseQualifiedAliasLookupBeforeImport),
    ("parses lowercase qualified lookup inside nested block", testParsesNestedLowercaseQualifiedAliasLookup),
    ("parses constructor-style signature when not an alias", testParsesConstructorStyleSignatureWhenNotAlias),
    ("parses lowercase signature payload when not an alias", testParsesLowercaseSignaturePayloadWhenNotAlias),
    ("parses import statement with symbol list", testParsesImportSymbolList),
    ("lowers module and import statements into core AST", testLowersModuleImportStatements),
    ("lowers qualified alias lookup expression into internal qualified name", testLowersQualifiedAliasLookup),
    ("rejects qualified alias lookup with non-identifier member", testRejectsNonIdentifierQualifiedMember),
    ("rejects legacy dot-only module declaration syntax", testRejectsLegacyDotOnlyModuleDeclaration),
    ("rejects trailing top-level statements after module body", testRejectsTrailingTopLevelStatementsAfterModuleBody),
    ("rejects module declaration after earlier top-level statement", testRejectsModuleDeclarationAfterTopLevelStatement),
    ("rejects module declaration nested inside module body", testRejectsModuleDeclarationNestedInsideModuleBody),
    ("rejects module declaration nested inside block expression", testRejectsModuleDeclarationNestedInsideBlock),
    ("rejects module statement with missing path", testRejectsModuleMissingPath),
    ("rejects module statement with trailing separator using separator span", testRejectsModuleTrailingSeparatorSpan),
    ("rejects import statement with empty symbol list", testRejectsImportEmptySymbolList),
    ("rejects import statement with empty symbol list using rparen span", testRejectsImportEmptySymbolListSpan),
    ("rejects import statement with duplicate symbols", testRejectsImportDuplicateSymbols),
    ("rejects import statement with alias and symbol list together", testRejectsImportAliasWithSymbolList),
    ("rejects import statement with symbol list then alias", testRejectsImportSymbolListWithAlias)
  ]

testParsesModuleDeclaration :: IO ()
testParsesModuleDeclaration =
  assertEqual
    "module surface AST"
    ( Right
        ( SEBlock
            [ SSModule (SourceSpan 1 1) ["App", "Core"],
              SSLet "x" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "module App::Core {\nx = 1.\n}")

testParsesImportBare :: IO ()
testParsesImportBare =
  assertEqual
    "import bare-dot surface AST"
    (Right (SEBlock [SSImport (SourceSpan 1 1) ["A", "B"] Nothing Nothing]))
    (parseSurfaceProgram "import A::B.")

testParsesImportAlias :: IO ()
testParsesImportAlias =
  assertEqual
    "import alias surface AST"
    ( Right
        ( SEBlock
            [ SSImport (SourceSpan 1 1) ["Std", "List"] (Just "List") Nothing,
              SSExpr (SourceSpan 2 1) (SEVar "List")
            ]
        )
    )
    (parseSurfaceProgram "import Std::List as List.\nList.")

testParsesQualifiedAliasLookup :: IO ()
testParsesQualifiedAliasLookup =
  assertEqual
    "qualified alias lookup surface AST"
    ( Right
        ( SEBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "Math") Nothing,
              SSExpr (SourceSpan 2 1) (SEQualifiedVar "Math" "subtract")
            ]
        )
    )
    (parseSurfaceProgram "import Lib::Math as Math.\nMath::subtract.")

testParsesLowercaseQualifiedAliasLookup :: IO ()
testParsesLowercaseQualifiedAliasLookup =
  assertEqual
    "lowercase qualified alias lookup surface AST"
    ( Right
        ( SEBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "math") Nothing,
              SSExpr (SourceSpan 2 1) (SEQualifiedVar "math" "subtract")
            ]
        )
    )
    (parseSurfaceProgram "import Lib::Math as math.\nmath::subtract.")

testParsesLowercaseQualifiedAliasLookupBeforeImport :: IO ()
testParsesLowercaseQualifiedAliasLookupBeforeImport =
  assertEqual
    "lowercase qualified alias lookup before import surface AST"
    ( Right
        ( SEBlock
            [ SSExpr (SourceSpan 1 1) (SEQualifiedVar "math" "subtract"),
              SSImport (SourceSpan 2 1) ["Lib", "Math"] (Just "math") Nothing
            ]
        )
    )
    (parseSurfaceProgram "math::subtract.\nimport Lib::Math as math.")

testParsesNestedLowercaseQualifiedAliasLookup :: IO ()
testParsesNestedLowercaseQualifiedAliasLookup =
  assertEqual
    "nested lowercase qualified alias lookup surface AST"
    ( Right
        ( SEBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "math") Nothing,
              SSLet
                "result"
                (SourceSpan 2 1)
                (SEBlock [SSExpr (SourceSpan 3 3) (SEQualifiedVar "math" "subtract")])
            ]
        )
    )
    (parseSurfaceProgram "import Lib::Math as math.\nresult = {\n  math::subtract.\n}.")

testParsesConstructorStyleSignatureWhenNotAlias :: IO ()
testParsesConstructorStyleSignatureWhenNotAlias =
  assertEqual
    "constructor-style signature surface AST"
    ( Right
        ( SEBlock
            [ SSSignature "Result" (SourceSpan 1 1) (SurfaceSignatureType SurfaceTypeInt),
              SSLet "Result" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "Result :: Int.\nResult = 1.")

testParsesLowercaseSignaturePayloadWhenNotAlias :: IO ()
testParsesLowercaseSignaturePayloadWhenNotAlias =
  assertEqual
    "lowercase signature payload surface AST"
    ( Right
        ( SEBlock
            [ SSSignature "value" (SourceSpan 1 1) (SurfaceUnsupportedSignature [SurfaceSignatureNameToken "a"]),
              SSLet "value" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "value :: a.\nvalue = 1.")

testParsesImportSymbolList :: IO ()
testParsesImportSymbolList =
  assertEqual
    "import symbol-list surface AST"
    ( Right
        ( SEBlock
            [ SSImport
                (SourceSpan 1 1)
                ["Std", "List"]
                Nothing
                (Just ["map", "filter"]),
              SSExpr (SourceSpan 2 1) (SEVar "map")
            ]
        )
    )
    (parseSurfaceProgram "import Std::List (map, filter).\nmap.")

testLowersModuleImportStatements :: IO ()
testLowersModuleImportStatements =
  assertRight
    "parse + lower module/import"
    (parseSurfaceProgram "module App::Core {\nimport Std::List (map).\nmap.\n}")
    (\surfaceProgram -> assertEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SModule (SourceSpan 1 1) ["App", "Core"],
          SImport (SourceSpan 2 1) ["Std", "List"] Nothing (Just ["map"]),
          SExpr (SourceSpan 3 1) (EVar "map")
        ]

testLowersQualifiedAliasLookup :: IO ()
testLowersQualifiedAliasLookup =
  assertRight
    "parse + lower qualified alias lookup"
    (parseSurfaceProgram "import Lib::Math as Math.\nMath::subtract.")
    (\surfaceProgram -> assertEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EBlock
        [ SImport (SourceSpan 1 1) ["Lib", "Math"] (Just "Math") Nothing,
          SExpr (SourceSpan 2 1) (EVar "Math::subtract")
        ]

testRejectsNonIdentifierQualifiedMember :: IO ()
testRejectsNonIdentifierQualifiedMember =
  assertLeftDiagnosticContains
    "non-identifier qualified alias member"
    "expected member name after '::'"
    (parseSurfaceProgram "import Lib::Math as Math.\nMath::1.")

testRejectsLegacyDotOnlyModuleDeclaration :: IO ()
testRejectsLegacyDotOnlyModuleDeclaration =
  assertLeftDiagnosticContains
    "legacy module declaration rejected"
    "expected '{'"
    (parseSurfaceProgram "module App::Core.")

testRejectsTrailingTopLevelStatementsAfterModuleBody :: IO ()
testRejectsTrailingTopLevelStatementsAfterModuleBody =
  assertLeftDiagnosticContains
    "trailing statement after module body"
    "after module declaration"
    (parseSurfaceProgram "module App::Core {\nx = 1.\n}\ny = 2.")

testRejectsModuleDeclarationAfterTopLevelStatement :: IO ()
testRejectsModuleDeclarationAfterTopLevelStatement =
  assertLeftDiagnosticContains
    "module declaration after top-level statement"
    "first top-level form"
    (parseSurfaceProgram "x = 1.\nmodule App::Core {\ny = 2.\n}")

testRejectsModuleDeclarationNestedInsideModuleBody :: IO ()
testRejectsModuleDeclarationNestedInsideModuleBody =
  assertLeftDiagnosticContains
    "module declaration nested inside module body"
    "top-level"
    (parseSurfaceProgram "module App::Core {\nmodule Inner::Thing {\ny = 1.\n}\n}")

testRejectsModuleDeclarationNestedInsideBlock :: IO ()
testRejectsModuleDeclarationNestedInsideBlock =
  assertLeftDiagnosticContains
    "module declaration nested inside block expression"
    "top-level"
    (parseSurfaceProgram "x = { module App::Core {\ny = 1.\n} y. }.")

testRejectsModuleMissingPath :: IO ()
testRejectsModuleMissingPath =
  assertLeftDiagnosticContains
    "module missing path error"
    "expected module path segment"
    (parseSurfaceProgram "module .")

testRejectsModuleTrailingSeparatorSpan :: IO ()
testRejectsModuleTrailingSeparatorSpan =
  assertLeftDiagnosticContains
    "module trailing separator span"
    "1:9"
    (parseSurfaceProgram "module A::.")

testRejectsImportEmptySymbolList :: IO ()
testRejectsImportEmptySymbolList =
  assertLeftDiagnosticContains
    "import empty symbol list error"
    "expected at least one import symbol"
    (parseSurfaceProgram "import Std::List ().")

testRejectsImportEmptySymbolListSpan :: IO ()
testRejectsImportEmptySymbolListSpan =
  assertLeftDiagnosticContains
    "import empty symbol list span"
    "1:19"
    (parseSurfaceProgram "import Std::List ().")

testRejectsImportDuplicateSymbols :: IO ()
testRejectsImportDuplicateSymbols =
  assertLeftDiagnosticContains
    "import duplicate symbol error"
    "duplicate import symbol 'map'"
    (parseSurfaceProgram "import Std::List (map, filter, map).")

testRejectsImportAliasWithSymbolList :: IO ()
testRejectsImportAliasWithSymbolList =
  assertLeftDiagnosticContains
    "import alias+symbol list error"
    "cannot combine import alias and symbol list"
    (parseSurfaceProgram "import Std::List as List (map).")

testRejectsImportSymbolListWithAlias :: IO ()
testRejectsImportSymbolListWithAlias =
  assertLeftDiagnosticContains
    "import symbol-list+alias error"
    "cannot combine import alias and symbol list"
    (parseSurfaceProgram "import Std::List (map) as List.")
