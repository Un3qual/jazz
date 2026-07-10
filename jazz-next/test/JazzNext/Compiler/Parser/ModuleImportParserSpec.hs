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
import JazzNext.Compiler.ModuleGraph
  ( CoreModule (coreModuleDeclaredExports),
    DeclaredModuleExports (..)
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
  ( lowerSurfaceExpr,
    lowerSurfaceModule
  )
import JazzNext.Compiler.Name (qualifiedName)
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
    ("parses populated module export list", testParsesModuleExportList),
    ("parses empty module export list", testParsesEmptyModuleExportList),
    ("lowers module export list into core metadata", testLowersModuleExportList),
    ("parses canonical brace-bodied module declaration boundary", testParsesCanonicalModuleDeclarationBoundary),
    ("parses module imports with stable indented spans", testParsesModuleImportsWithStableIndentedSpans),
    ("parses import statement bare dot", testParsesImportBare),
    ("parses import statement with alias", testParsesImportAlias),
    ("parses qualified alias lookup expression", testParsesQualifiedAliasLookup),
    ("parses abstraction keyword alias lookup expression", testParsesAbstractionKeywordAliasLookup),
    ("parses lowercase alias qualified lookup expression", testParsesLowercaseQualifiedAliasLookup),
    ("parses lowercase qualified lookup before alias import", testParsesLowercaseQualifiedAliasLookupBeforeImport),
    ("parses lowercase qualified lookup inside nested block", testParsesNestedLowercaseQualifiedAliasLookup),
    ("parses uppercase qualified alias member lookup", testParsesUppercaseQualifiedAliasMemberLookup),
    ("parses constructor-style signature when not an alias", testParsesConstructorStyleSignatureWhenNotAlias),
    ("parses compact signature when not an alias", testParsesCompactSignatureWhenNotAlias),
    ("parses compact signature before different binding when not an alias", testParsesCompactSignatureBeforeDifferentBindingWhenNotAlias),
    ("parses constructor-style unsupported signature when not an alias", testParsesConstructorStyleUnsupportedSignatureWhenNotAlias),
    ("parses compact type-variable signature before different binding when not an alias", testParsesCompactTypeVariableSignatureBeforeDifferentBindingWhenNotAlias),
    ("parses signature for binding sharing alias name", testParsesSignatureForBindingSharingAliasName),
    ("parses lowercase signature payload for binding sharing alias name", testParsesLowercaseSignaturePayloadForBindingSharingAliasName),
    ("parses lowercase signature payload when not an alias", testParsesLowercaseSignaturePayloadWhenNotAlias),
    ("parses import statement with symbol list", testParsesImportSymbolList),
    ("lowers module and import statements into core AST", testLowersModuleImportStatements),
    ("lowers qualified alias lookup expression into internal qualified name", testLowersQualifiedAliasLookup),
    ("rejects spaced qualified alias lookup inside binding expression", testRejectsSpacedQualifiedAliasLookupInBindingExpression),
    ("rejects qualified alias lookup with non-identifier member", testRejectsNonIdentifierQualifiedMember),
    ("rejects constructor qualified lookup with non-identifier member", testRejectsConstructorQualifiedNonIdentifierMember),
    ("rejects legacy dot-only module declaration syntax", testRejectsLegacyDotOnlyModuleDeclaration),
    ("rejects legacy equals-style module declaration syntax", testRejectsLegacyEqualsStyleModuleDeclaration),
    ("rejects legacy newline module declaration syntax", testRejectsLegacyNewlineModuleDeclaration),
    ("rejects trailing top-level statements after module body", testRejectsTrailingTopLevelStatementsAfterModuleBody),
    ("rejects module declaration after earlier top-level statement", testRejectsModuleDeclarationAfterTopLevelStatement),
    ("rejects module declaration nested inside module body", testRejectsModuleDeclarationNestedInsideModuleBody),
    ("rejects module declaration nested inside block expression", testRejectsModuleDeclarationNestedInsideBlock),
    ("rejects module statement with missing path", testRejectsModuleMissingPath),
    ("rejects module statement with trailing separator using separator span", testRejectsModuleTrailingSeparatorSpan),
    ("rejects duplicate module export", testRejectsDuplicateModuleExport),
    ("rejects trailing comma in module export list", testRejectsTrailingCommaInModuleExportList),
    ("rejects unclosed module export list", testRejectsUnclosedModuleExportList),
    ("rejects missing body after module export list", testRejectsMissingBodyAfterModuleExportList),
    ("rejects import statement with trailing separator using separator span", testRejectsImportTrailingSeparatorSpan),
    ("rejects import statement with empty symbol list", testRejectsImportEmptySymbolList),
    ("rejects import statement with empty symbol list using rparen span", testRejectsImportEmptySymbolListSpan),
    ("rejects import statement with duplicate symbols", testRejectsImportDuplicateSymbols),
    ("rejects import alias using reserved literal", testRejectsImportReservedLiteralAlias),
    ("rejects import statement with alias and symbol list together", testRejectsImportAliasWithSymbolList),
    ("rejects import statement with symbol list then alias", testRejectsImportSymbolListWithAlias)
  ]

testParsesModuleDeclaration :: IO ()
testParsesModuleDeclaration =
  assertEqual
    "module surface AST"
    ( Right
        ( SEBlock
            [ SSModule (SourceSpan 1 1) ["App", "Core"] Nothing,
              SSLet "x" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "module App::Core {\nx = 1.\n}")

testParsesModuleExportList :: IO ()
testParsesModuleExportList =
  assertEqual
    "module export list surface AST"
    ( Right
        ( SEBlock
            [ SSModule
                (SourceSpan 1 1)
                ["Lib", "Maybe"]
                (Just ["Maybe", "Just", "Nothing", "mapMaybe"]),
              SSLet "mapMaybe" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    ( parseSurfaceProgram
        "module Lib::Maybe (Maybe, Just, Nothing, mapMaybe) {\nmapMaybe = 1.\n}"
    )

testParsesEmptyModuleExportList :: IO ()
testParsesEmptyModuleExportList =
  assertEqual
    "empty module export list"
    ( Right
        ( SEBlock
            [ SSModule (SourceSpan 1 1) ["App", "Internal"] (Just []),
              SSLet "helper" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "module App::Internal () {\nhelper = 1.\n}")

testLowersModuleExportList :: IO ()
testLowersModuleExportList =
  assertRight
    "parse module export list"
    (parseSurfaceProgram "module Lib::Value (answer) {\nanswer = 1.\n}")
    ( \surfaceProgram ->
        assertEqual
          "lowered module export metadata"
          ( Right
              ( Just
                  ( DeclaredModuleExports
                      (SourceSpanIn "src/Lib/Value.jz" 1 1)
                      ["answer"]
                  )
              )
          )
          ( coreModuleDeclaredExports
              <$> lowerSurfaceModule "src/Lib/Value.jz" ["Lib", "Value"] surfaceProgram
          )
    )

testParsesCanonicalModuleDeclarationBoundary :: IO ()
testParsesCanonicalModuleDeclarationBoundary =
  assertEqual
    "canonical module boundary surface AST"
    ( Right
        ( SEBlock
            [ SSModule (SourceSpan 1 1) ["App", "Main"] Nothing,
              SSImport (SourceSpan 2 1) ["Lib", "Math"] (Just "Math") Nothing,
              SSLet "result" (SourceSpan 3 1) (SEQualifiedVar "Math" "answer")
            ]
        )
    )
    (parseSurfaceProgram "module App::Main {\nimport Lib::Math as Math.\nresult = Math::answer.\n}")

testParsesModuleImportsWithStableIndentedSpans :: IO ()
testParsesModuleImportsWithStableIndentedSpans =
  assertEqual
    "module import indented spans"
    ( Right
        ( SEBlock
            [ SSModule (SourceSpan 1 1) ["App", "Main"] Nothing,
              SSImport (SourceSpan 3 3) ["Lib", "Math"] (Just "Math") Nothing,
              SSImport (SourceSpan 4 3) ["Std", "List"] Nothing (Just ["map"]),
              SSLet "result" (SourceSpan 5 3) (SEQualifiedVar "Math" "answer")
            ]
        )
    )
    (parseSurfaceProgram "module App::Main {\n# keep comment line out of spans\n  import Lib::Math as Math.\n  import Std::List (map).\n  result = Math::answer.\n}")

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

testParsesAbstractionKeywordAliasLookup :: IO ()
testParsesAbstractionKeywordAliasLookup =
  assertEqual
    "abstraction keyword alias lookup surface AST"
    ( Right
        ( SEBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "class") Nothing,
              SSExpr (SourceSpan 2 1) (SEQualifiedVar "class" "subtract")
            ]
        )
    )
    (parseSurfaceProgram "import Lib::Math as class.\nclass::subtract.")

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

testParsesUppercaseQualifiedAliasMemberLookup :: IO ()
testParsesUppercaseQualifiedAliasMemberLookup =
  assertEqual
    "uppercase qualified alias member lookup surface AST"
    ( Right
        ( SEBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "Math") Nothing,
              SSExpr (SourceSpan 2 1) (SEQualifiedVar "Math" "Result")
            ]
        )
    )
    (parseSurfaceProgram "import Lib::Math as Math.\nMath::Result.")

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

testParsesCompactSignatureWhenNotAlias :: IO ()
testParsesCompactSignatureWhenNotAlias =
  assertEqual
    "compact signature surface AST"
    ( Right
        ( SEBlock
            [ SSSignature "value" (SourceSpan 1 1) (SurfaceSignatureType SurfaceTypeInt),
              SSLet "value" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "value::Int.\nvalue = 1.")

testParsesCompactSignatureBeforeDifferentBindingWhenNotAlias :: IO ()
testParsesCompactSignatureBeforeDifferentBindingWhenNotAlias =
  assertEqual
    "compact signature before different binding surface AST"
    ( Right
        ( SEBlock
            [ SSSignature "value" (SourceSpan 1 1) (SurfaceSignatureType SurfaceTypeInt),
              SSLet "other" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "value::Int.\nother = 1.")

testParsesConstructorStyleUnsupportedSignatureWhenNotAlias :: IO ()
testParsesConstructorStyleUnsupportedSignatureWhenNotAlias =
  assertEqual
    "constructor-style unsupported signature surface AST"
    ( Right
        ( SEBlock
            [ SSSignature "Result" (SourceSpan 1 1) (SurfaceUnsupportedSignature [SurfaceSignatureNameToken "a"]),
              SSLet "Result" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "Result :: a.\nResult = 1.")

testParsesCompactTypeVariableSignatureBeforeDifferentBindingWhenNotAlias :: IO ()
testParsesCompactTypeVariableSignatureBeforeDifferentBindingWhenNotAlias =
  assertEqual
    "compact type-variable signature before different binding surface AST"
    ( Right
        ( SEBlock
            [ SSSignature "Result" (SourceSpan 1 1) (SurfaceUnsupportedSignature [SurfaceSignatureNameToken "a"]),
              SSLet "other" (SourceSpan 2 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "Result::a.\nother = 1.")

testParsesSignatureForBindingSharingAliasName :: IO ()
testParsesSignatureForBindingSharingAliasName =
  assertEqual
    "alias-name binding signature surface AST"
    ( Right
        ( SEBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "math") Nothing,
              SSSignature "math" (SourceSpan 2 1) (SurfaceSignatureType SurfaceTypeInt),
              SSLet "math" (SourceSpan 3 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "import Lib::Math as math.\nmath :: Int.\nmath = 1.")

testParsesLowercaseSignaturePayloadForBindingSharingAliasName :: IO ()
testParsesLowercaseSignaturePayloadForBindingSharingAliasName =
  assertEqual
    "alias-name binding lowercase signature surface AST"
    ( Right
        ( SEBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "math") Nothing,
              SSSignature "math" (SourceSpan 2 1) (SurfaceUnsupportedSignature [SurfaceSignatureNameToken "a"]),
              SSLet "math" (SourceSpan 3 1) (SELit (SLInt 1))
            ]
        )
    )
    (parseSurfaceProgram "import Lib::Math as math.\nmath :: a.\nmath = 1.")

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
          SExpr (SourceSpan 2 1) (EVar (qualifiedName "Math" "subtract"))
        ]

testRejectsSpacedQualifiedAliasLookupInBindingExpression :: IO ()
testRejectsSpacedQualifiedAliasLookupInBindingExpression =
  assertLeftDiagnosticContains
    "spaced qualified alias lookup in binding expression"
    "expected '.' at"
    (parseSurfaceProgram "import Lib::Math as Math.\nmain = Math :: subtract.")

testRejectsNonIdentifierQualifiedMember :: IO ()
testRejectsNonIdentifierQualifiedMember =
  assertLeftDiagnosticContains
    "non-identifier qualified alias member"
    "expected member name after '::'"
    (parseSurfaceProgram "import Lib::Math as Math.\nMath::1.")

testRejectsConstructorQualifiedNonIdentifierMember :: IO ()
testRejectsConstructorQualifiedNonIdentifierMember =
  assertLeftDiagnosticContains
    "constructor qualified non-identifier member"
    "expected member name after '::'"
    (parseSurfaceProgram "Math::1.")

testRejectsLegacyDotOnlyModuleDeclaration :: IO ()
testRejectsLegacyDotOnlyModuleDeclaration =
  assertLeftDiagnosticContains
    "legacy module declaration rejected"
    "expected '{'"
    (parseSurfaceProgram "module App::Core.")

testRejectsLegacyEqualsStyleModuleDeclaration :: IO ()
testRejectsLegacyEqualsStyleModuleDeclaration =
  assertLeftDiagnosticContains
    "legacy equals-style module declaration rejected"
    "expected '{'"
    (parseSurfaceProgram "module App::Core = 1.")

testRejectsLegacyNewlineModuleDeclaration :: IO ()
testRejectsLegacyNewlineModuleDeclaration =
  assertLeftDiagnosticContains
    "legacy newline module declaration rejected"
    "expected '{'"
    (parseSurfaceProgram "module App::Core\nx = 1.")

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

testRejectsDuplicateModuleExport :: IO ()
testRejectsDuplicateModuleExport = do
  assertLeftDiagnosticContains
    "duplicate module export code"
    "E0001"
    (parseSurfaceProgram "module Lib::Value (answer, answer) {\nanswer = 1.\n}")
  assertLeftDiagnosticContains
    "duplicate module export message"
    "duplicate module export 'answer'"
    (parseSurfaceProgram "module Lib::Value (answer, answer) {\nanswer = 1.\n}")
  assertLeftDiagnosticContains
    "duplicate module export span"
    "1:28"
    (parseSurfaceProgram "module Lib::Value (answer, answer) {\nanswer = 1.\n}")

testRejectsTrailingCommaInModuleExportList :: IO ()
testRejectsTrailingCommaInModuleExportList = do
  assertLeftDiagnosticContains
    "trailing module export comma code"
    "E0001"
    (parseSurfaceProgram "module Lib::Value (answer,) {\nanswer = 1.\n}")
  assertLeftDiagnosticContains
    "trailing module export comma message"
    "expected module export name"
    (parseSurfaceProgram "module Lib::Value (answer,) {\nanswer = 1.\n}")

testRejectsUnclosedModuleExportList :: IO ()
testRejectsUnclosedModuleExportList =
  assertLeftDiagnosticContains
    "unclosed module export list"
    "expected ',' or ')'"
    (parseSurfaceProgram "module Lib::Value (answer {\nanswer = 1.\n}")

testRejectsMissingBodyAfterModuleExportList :: IO ()
testRejectsMissingBodyAfterModuleExportList =
  assertLeftDiagnosticContains
    "missing body after module export list"
    "expected '{'"
    (parseSurfaceProgram "module Lib::Value (answer).")

testRejectsImportTrailingSeparatorSpan :: IO ()
testRejectsImportTrailingSeparatorSpan =
  assertLeftDiagnosticContains
    "import trailing separator span"
    "1:9"
    (parseSurfaceProgram "import A::.")

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

testRejectsImportReservedLiteralAlias :: IO ()
testRejectsImportReservedLiteralAlias =
  assertLeftDiagnosticContains
    "import reserved alias error"
    "reserved literal 'True' cannot be used as an import alias"
    (parseSurfaceProgram "import Std::List as True.")

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
