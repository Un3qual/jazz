{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
  )
import Jazz.Compiler.ModuleExports
  ( LocatedModuleExportName (..),
    ModuleExportSelector (..),
    ModuleTypeConstructorSelector (..),
  )
import Jazz.Compiler.ModuleGraph
  ( CoreModule (coreModuleFacts),
    DeclaredModuleExports (..),
    DeclaredModuleFacts (declaredModuleExports),
  )
import Jazz.Compiler.ModuleIdentity
  ( ModuleIdentity,
    mkModulePath,
    mkSourceFile,
    moduleIdentity,
  )
import Jazz.Compiler.Name
  ( Identifier,
    NameNamespace (..),
    qualifiedName,
  )
import Jazz.Compiler.Parser
  ( parseSurfaceProgram,
  )
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr (..),
    SurfaceExprForm (..),
    SurfaceImplMethod (..),
    SurfaceLiteral (..),
    SurfaceStatement (..),
  )
import Jazz.Compiler.Parser.Lower
  ( lowerSurfaceExpr,
    lowerSurfaceModule,
  )
import Jazz.Compiler.TypeRepresentation
  ( SignaturePayload (..),
    SignatureType (..),
  )
import Jazz.TestCore
  ( assertLoweredCoreEqual,
    loweredBlock,
    loweredExpression,
    loweredImport,
    loweredModule,
    loweredVariable,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftDiagnosticContains,
    assertRight,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "ModuleImportParser" tests

tests :: [NamedTest]
tests =
  [ ("parses module declaration statement", testParsesModuleDeclaration),
    ("parses populated module export list", testParsesModuleExportList),
    ("parses namespace-aware module export list", testParsesNamespaceAwareModuleExportList),
    ("parses grouped type constructor exports", testParsesGroupedTypeConstructorExports),
    ("keeps remaining namespace prefix words contextual in module export lists", testParsesNamespacePrefixWordsAsBareExports),
    ("parses empty module export list", testParsesEmptyModuleExportList),
    ("lowers module export list into core metadata", testLowersModuleExportList),
    ("qualifies grouped module export spans during lowering", testLowersGroupedModuleExportList),
    ("parses canonical brace-bodied module declaration boundary", testParsesCanonicalModuleDeclarationBoundary),
    ("parses module imports with stable indented spans", testParsesModuleImportsWithStableIndentedSpans),
    ("parses import statement bare dot", testParsesImportBare),
    ("parses import statement with alias", testParsesImportAlias),
    ("parses qualified alias lookup expression", testParsesQualifiedAliasLookup),
    ("parses abstraction keyword alias lookup expression", testParsesAbstractionKeywordAliasLookup),
    ("parses lowercase alias qualified lookup expression", testParsesLowercaseQualifiedAliasLookup),
    ("parses lowercase qualified lookup before alias import", testParsesLowercaseQualifiedAliasLookupBeforeImport),
    ("parses lowercase qualified lookup inside nested block", testParsesNestedLowercaseQualifiedAliasLookup),
    ("parses nested lowercase qualified lookup before later alias import", testParsesNestedLowercaseQualifiedAliasLookupBeforeImport),
    ("parses uppercase qualified alias member lookup", testParsesUppercaseQualifiedAliasMemberLookup),
    ("parses constructor-style signature when not an alias", testParsesConstructorStyleSignatureWhenNotAlias),
    ("parses compact signature when not an alias", testParsesCompactSignatureWhenNotAlias),
    ("parses compact signature before different binding when not an alias", testParsesCompactSignatureBeforeDifferentBindingWhenNotAlias),
    ("parses constructor-style type-variable signature when not an alias", testParsesConstructorStyleTypeVariableSignatureWhenNotAlias),
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
    ("rejects legacy module declaration syntax", testRejectsLegacyModuleDeclarations),
    ("rejects trailing top-level statements after module body", testRejectsTrailingTopLevelStatementsAfterModuleBody),
    ("rejects module declaration after earlier top-level statement", testRejectsModuleDeclarationAfterTopLevelStatement),
    ("rejects module declaration nested inside module body", testRejectsModuleDeclarationNestedInsideModuleBody),
    ("rejects module declaration nested inside block expression", testRejectsModuleDeclarationNestedInsideBlock),
    ("rejects module statement with missing path", testRejectsModuleMissingPath),
    ("rejects module statement with trailing separator using separator span", testRejectsModuleTrailingSeparatorSpan),
    ("rejects duplicate module export", testRejectsDuplicateModuleExport),
    ("rejects duplicate namespace-aware module export", testRejectsDuplicateNamespaceAwareModuleExport),
    ("rejects empty grouped constructor export", testRejectsEmptyGroupedConstructorExport),
    ("rejects malformed all-constructor export", testRejectsMalformedAllConstructorExport),
    ("rejects missing grouped constructor comma", testRejectsMissingGroupedConstructorComma),
    ("rejects unclosed grouped constructor export", testRejectsUnclosedGroupedConstructorExport),
    ("rejects non-identifier grouped constructor export", testRejectsNonIdentifierGroupedConstructorExport),
    ("rejects duplicate grouped constructor export", testRejectsDuplicateGroupedConstructorExport),
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
        ( seBlock
            [ SSModule (SourceSpan 1 1) ["App", "Core"] Nothing,
              SSLet "x" (SourceSpan 2 1) (seLit (SLInt 1))
            ]
        )
    )
    ( parseNormalized
        """
        module App::Core {
        x = 1.
        }
        """
    )

testParsesModuleExportList :: IO ()
testParsesModuleExportList =
  assertEqual
    "module export list surface AST"
    ( Right
        ( seBlock
            [ SSModule
                (SourceSpan 1 1)
                ["Lib", "Maybe"]
                ( Just
                    [ ModuleExportSelector Nothing "Maybe",
                      ModuleExportSelector Nothing "Just",
                      ModuleExportSelector Nothing "Nothing",
                      ModuleExportSelector Nothing "mapMaybe"
                    ]
                ),
              SSLet "mapMaybe" (SourceSpan 2 1) (seLit (SLInt 1))
            ]
        )
    )
    ( parseNormalized
        """
        module Lib::Maybe (Maybe, Just, Nothing, mapMaybe) {
        mapMaybe = 1.
        }
        """
    )

testParsesNamespaceAwareModuleExportList :: IO ()
testParsesNamespaceAwareModuleExportList =
  assertEqual
    "namespace-aware module export list surface AST"
    ( Right
        ( seBlock
            [ SSModule
                (SourceSpan 1 1)
                ["Lib", "Box"]
                ( Just
                    [ ModuleTypeExportSelector "Box" (SourceSpan 1 23) AbstractType,
                      ModuleExportSelector (Just ConstructorNamespace) "Box",
                      ModuleExportSelector (Just ValueNamespace) "Box",
                      ModuleExportSelector (Just CapabilityNamespace) "Printable",
                      ModuleExportSelector Nothing "legacy"
                    ]
                ),
              SSLet "legacy" (SourceSpan 2 1) (seLit (SLInt 1))
            ]
        )
    )
    ( parseNormalized
        """
        module Lib::Box (type Box, constructor Box, value Box, class Printable, legacy) {
        legacy = 1.
        }
        """
    )

testParsesGroupedTypeConstructorExports :: IO ()
testParsesGroupedTypeConstructorExports =
  assertEqual
    "grouped type constructor exports"
    ( Right
        ( seBlock
            [ SSModule
                (SourceSpan 1 1)
                ["Lib", "Choice"]
                ( Just
                    [ ModuleTypeExportSelector "Hidden" (SourceSpan 1 26) AbstractType,
                      ModuleTypeExportSelector "Choice" (SourceSpan 1 39) (AllTypeConstructors (SourceSpan 1 46)),
                      ModuleTypeExportSelector
                        "Pair"
                        (SourceSpan 1 56)
                        ( SelectedTypeConstructors
                            ( LocatedModuleExportName "Pair" (SourceSpan 1 61)
                                :| [LocatedModuleExportName "Unit" (SourceSpan 1 67)]
                            )
                        )
                    ]
                )
            ]
        )
    )
    (parseNormalized "module Lib::Choice (type Hidden, type Choice(..), type Pair(Pair, Unit)) {}")

testParsesNamespacePrefixWordsAsBareExports :: IO ()
testParsesNamespacePrefixWordsAsBareExports =
  assertEqual
    "contextual namespace prefix words"
    ( Right
        ( seBlock
            [ SSModule
                (SourceSpan 1 1)
                ["Lib", "Keywords"]
                ( Just
                    [ ModuleExportSelector Nothing "constructor",
                      ModuleExportSelector Nothing "type",
                      ModuleExportSelector Nothing "class"
                    ]
                ),
              SSLet "answer" (SourceSpan 2 1) (seLit (SLInt 1))
            ]
        )
    )
    ( parseNormalized
        """
        module Lib::Keywords (constructor, type, class) {
        answer = 1.
        }
        """
    )

testParsesEmptyModuleExportList :: IO ()
testParsesEmptyModuleExportList =
  assertEqual
    "empty module export list"
    ( Right
        ( seBlock
            [ SSModule (SourceSpan 1 1) ["App", "Internal"] (Just []),
              SSLet "helper" (SourceSpan 2 1) (seLit (SLInt 1))
            ]
        )
    )
    ( parseNormalized
        """
        module App::Internal () {
        helper = 1.
        }
        """
    )

testLowersModuleExportList :: IO ()
testLowersModuleExportList =
  assertRight
    "parse module export list"
    ( parseNormalized
        """
        module Lib::Value (answer) {
        answer = 1.
        }
        """
    )
    ( \surfaceProgram ->
        assertEqual
          "lowered module export metadata"
          ( Right
              ( Just
                  ( DeclaredModuleExports
                      (SourceSpanIn "src/Lib/Value.jz" 1 1)
                      [ModuleExportSelector Nothing "answer"]
                  )
              )
          )
          ( declaredModuleExports . coreModuleFacts
              <$> lowerSurfaceModule
                (testModuleIdentity "src/Lib/Value.jz" ("Lib" :| ["Value"]))
                surfaceProgram
          )
    )

testLowersGroupedModuleExportList :: IO ()
testLowersGroupedModuleExportList =
  assertRight
    "parse grouped module export list"
    (parseNormalized "module Lib::Choice (type Choice(First, Second)) {}")
    ( \surfaceProgram ->
        assertEqual
          "qualified grouped module export spans"
          ( Right
              ( Just
                  ( DeclaredModuleExports
                      (SourceSpanIn "src/Lib/Choice.jz" 1 1)
                      [ ModuleTypeExportSelector
                          "Choice"
                          (SourceSpanIn "src/Lib/Choice.jz" 1 26)
                          ( SelectedTypeConstructors
                              ( LocatedModuleExportName "First" (SourceSpanIn "src/Lib/Choice.jz" 1 33)
                                  :| [LocatedModuleExportName "Second" (SourceSpanIn "src/Lib/Choice.jz" 1 40)]
                              )
                          )
                      ]
                  )
              )
          )
          ( declaredModuleExports . coreModuleFacts
              <$> lowerSurfaceModule
                (testModuleIdentity "src/Lib/Choice.jz" ("Lib" :| ["Choice"]))
                surfaceProgram
          )
    )

testModuleIdentity :: FilePath -> NonEmpty Identifier -> ModuleIdentity
testModuleIdentity sourcePath modulePath =
  moduleIdentity (mkModulePath modulePath) (mkSourceFile sourcePath)

testParsesCanonicalModuleDeclarationBoundary :: IO ()
testParsesCanonicalModuleDeclarationBoundary =
  assertEqual
    "canonical module boundary surface AST"
    ( Right
        ( seBlock
            [ SSModule (SourceSpan 1 1) ["App", "Main"] Nothing,
              SSImport (SourceSpan 2 1) ["Lib", "Math"] (Just "Math") Nothing,
              SSLet "result" (SourceSpan 3 1) (seQualifiedVar "Math" "answer")
            ]
        )
    )
    ( parseNormalized
        """
        module App::Main {
        import Lib::Math as Math.
        result = Math::answer.
        }
        """
    )

testParsesModuleImportsWithStableIndentedSpans :: IO ()
testParsesModuleImportsWithStableIndentedSpans =
  -- Explicit escapes are intentional: this case asserts exact whitespace or source spans.
  assertEqual
    "module import indented spans"
    ( Right
        ( seBlock
            [ SSModule (SourceSpan 1 1) ["App", "Main"] Nothing,
              SSImport (SourceSpan 3 3) ["Lib", "Math"] (Just "Math") Nothing,
              SSImport (SourceSpan 4 3) ["Std", "List"] Nothing (Just ["map"]),
              SSLet "result" (SourceSpan 5 3) (seQualifiedVar "Math" "answer")
            ]
        )
    )
    (parseNormalized "module App::Main {\n# keep comment line out of spans\n  import Lib::Math as Math.\n  import Std::List (map).\n  result = Math::answer.\n}")

testParsesImportBare :: IO ()
testParsesImportBare =
  assertEqual
    "import bare-dot surface AST"
    (Right (seBlock [SSImport (SourceSpan 1 1) ["A", "B"] Nothing Nothing]))
    (parseNormalized "import A::B.")

testParsesImportAlias :: IO ()
testParsesImportAlias =
  assertEqual
    "import alias surface AST"
    ( Right
        ( seBlock
            [ SSImport (SourceSpan 1 1) ["Std", "List"] (Just "List") Nothing,
              SSExpr (SourceSpan 2 1) (seVar "List")
            ]
        )
    )
    ( parseNormalized
        """
        import Std::List as List.
        List.
        """
    )

testParsesQualifiedAliasLookup :: IO ()
testParsesQualifiedAliasLookup =
  assertEqual
    "qualified alias lookup surface AST"
    ( Right
        ( seBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "Math") Nothing,
              SSExpr (SourceSpan 2 1) (seQualifiedVar "Math" "subtract")
            ]
        )
    )
    ( parseNormalized
        """
        import Lib::Math as Math.
        Math::subtract.
        """
    )

testParsesAbstractionKeywordAliasLookup :: IO ()
testParsesAbstractionKeywordAliasLookup =
  assertEqual
    "abstraction keyword alias lookup surface AST"
    ( Right
        ( seBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "class") Nothing,
              SSExpr (SourceSpan 2 1) (seQualifiedVar "class" "subtract")
            ]
        )
    )
    ( parseNormalized
        """
        import Lib::Math as class.
        class::subtract.
        """
    )

testParsesLowercaseQualifiedAliasLookup :: IO ()
testParsesLowercaseQualifiedAliasLookup =
  assertEqual
    "lowercase qualified alias lookup surface AST"
    ( Right
        ( seBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "math") Nothing,
              SSExpr (SourceSpan 2 1) (seQualifiedVar "math" "subtract")
            ]
        )
    )
    ( parseNormalized
        """
        import Lib::Math as math.
        math::subtract.
        """
    )

testParsesLowercaseQualifiedAliasLookupBeforeImport :: IO ()
testParsesLowercaseQualifiedAliasLookupBeforeImport =
  assertEqual
    "lowercase qualified alias lookup before import surface AST"
    ( Right
        ( seBlock
            [ SSExpr (SourceSpan 1 1) (seQualifiedVar "math" "subtract"),
              SSImport (SourceSpan 2 1) ["Lib", "Math"] (Just "math") Nothing
            ]
        )
    )
    ( parseNormalized
        """
        math::subtract.
        import Lib::Math as math.
        """
    )

testParsesNestedLowercaseQualifiedAliasLookup :: IO ()
testParsesNestedLowercaseQualifiedAliasLookup =
  assertEqual
    "nested lowercase qualified alias lookup surface AST"
    ( Right
        ( seBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "math") Nothing,
              SSLet
                "result"
                (SourceSpan 2 1)
                (seBlock [SSExpr (SourceSpan 3 3) (seQualifiedVar "math" "subtract")])
            ]
        )
    )
    ( parseNormalized
        """
        import Lib::Math as math.
        result = {
          math::subtract.
        }.
        """
    )

testParsesNestedLowercaseQualifiedAliasLookupBeforeImport :: IO ()
testParsesNestedLowercaseQualifiedAliasLookupBeforeImport =
  assertEqual
    "nested lowercase qualified alias lookup before import surface AST"
    ( Right
        ( seBlock
            [ SSLet
                "result"
                (SourceSpan 1 1)
                (seBlock [SSExpr (SourceSpan 2 3) (seQualifiedVar "math" "subtract")]),
              SSImport (SourceSpan 4 1) ["Lib", "Math"] (Just "math") Nothing
            ]
        )
    )
    ( parseNormalized
        """
        result = {
          math::subtract.
        }.
        import Lib::Math as math.
        """
    )

testParsesUppercaseQualifiedAliasMemberLookup :: IO ()
testParsesUppercaseQualifiedAliasMemberLookup =
  assertEqual
    "uppercase qualified alias member lookup surface AST"
    ( Right
        ( seBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "Math") Nothing,
              SSExpr (SourceSpan 2 1) (seQualifiedVar "Math" "Result")
            ]
        )
    )
    ( parseNormalized
        """
        import Lib::Math as Math.
        Math::Result.
        """
    )

testParsesConstructorStyleSignatureWhenNotAlias :: IO ()
testParsesConstructorStyleSignatureWhenNotAlias =
  assertEqual
    "constructor-style signature surface AST"
    ( Right
        ( seBlock
            [ SSSignature "Result" (SourceSpan 1 1) (SignatureType TypeInt),
              SSLet "Result" (SourceSpan 2 1) (seLit (SLInt 1))
            ]
        )
    )
    ( parseNormalized
        """
        Result :: Int.
        Result = 1.
        """
    )

testParsesCompactSignatureWhenNotAlias :: IO ()
testParsesCompactSignatureWhenNotAlias =
  assertEqual
    "compact signature surface AST"
    ( Right
        ( seBlock
            [ SSSignature "result" (SourceSpan 1 1) (SignatureType TypeInt),
              SSLet "result" (SourceSpan 2 1) (seLit (SLInt 1))
            ]
        )
    )
    ( parseNormalized
        """
        result::Int.
        result = 1.
        """
    )

testParsesCompactSignatureBeforeDifferentBindingWhenNotAlias :: IO ()
testParsesCompactSignatureBeforeDifferentBindingWhenNotAlias =
  assertEqual
    "compact signature before different binding surface AST"
    ( Right
        ( seBlock
            [ SSSignature "result" (SourceSpan 1 1) (SignatureType TypeInt),
              SSLet "other" (SourceSpan 2 1) (seLit (SLInt 1))
            ]
        )
    )
    ( parseNormalized
        """
        result::Int.
        other = 1.
        """
    )

testParsesConstructorStyleTypeVariableSignatureWhenNotAlias :: IO ()
testParsesConstructorStyleTypeVariableSignatureWhenNotAlias =
  assertEqual
    "constructor-style type-variable signature surface AST"
    ( Right
        ( seBlock
            [ SSSignature "Result" (SourceSpan 1 1) (SignatureType (TypeVariable "a")),
              SSLet "Result" (SourceSpan 2 1) (seLit (SLInt 1))
            ]
        )
    )
    ( parseNormalized
        """
        Result :: a.
        Result = 1.
        """
    )

testParsesCompactTypeVariableSignatureBeforeDifferentBindingWhenNotAlias :: IO ()
testParsesCompactTypeVariableSignatureBeforeDifferentBindingWhenNotAlias =
  assertEqual
    "compact type-variable signature before different binding surface AST"
    ( Right
        ( seBlock
            [ SSSignature "Result" (SourceSpan 1 1) (SignatureType (TypeVariable "a")),
              SSLet "other" (SourceSpan 2 1) (seLit (SLInt 1))
            ]
        )
    )
    ( parseNormalized
        """
        Result::a.
        other = 1.
        """
    )

testParsesSignatureForBindingSharingAliasName :: IO ()
testParsesSignatureForBindingSharingAliasName =
  assertEqual
    "alias-name binding signature surface AST"
    ( Right
        ( seBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "math") Nothing,
              SSSignature "math" (SourceSpan 2 1) (SignatureType TypeInt),
              SSLet "math" (SourceSpan 3 1) (seLit (SLInt 1))
            ]
        )
    )
    ( parseNormalized
        """
        import Lib::Math as math.
        math :: Int.
        math = 1.
        """
    )

testParsesLowercaseSignaturePayloadForBindingSharingAliasName :: IO ()
testParsesLowercaseSignaturePayloadForBindingSharingAliasName =
  assertEqual
    "alias-name binding lowercase signature surface AST"
    ( Right
        ( seBlock
            [ SSImport (SourceSpan 1 1) ["Lib", "Math"] (Just "math") Nothing,
              SSSignature "math" (SourceSpan 2 1) (SignatureType (TypeVariable "a")),
              SSLet "math" (SourceSpan 3 1) (seLit (SLInt 1))
            ]
        )
    )
    ( parseNormalized
        """
        import Lib::Math as math.
        math :: a.
        math = 1.
        """
    )

testParsesLowercaseSignaturePayloadWhenNotAlias :: IO ()
testParsesLowercaseSignaturePayloadWhenNotAlias =
  assertEqual
    "lowercase signature payload surface AST"
    ( Right
        ( seBlock
            [ SSSignature "result" (SourceSpan 1 1) (SignatureType (TypeVariable "a")),
              SSLet "result" (SourceSpan 2 1) (seLit (SLInt 1))
            ]
        )
    )
    ( parseNormalized
        """
        result :: a.
        result = 1.
        """
    )

testParsesImportSymbolList :: IO ()
testParsesImportSymbolList =
  assertEqual
    "import symbol-list surface AST"
    ( Right
        ( seBlock
            [ SSImport
                (SourceSpan 1 1)
                ["Std", "List"]
                Nothing
                (Just ["map", "filter"]),
              SSExpr (SourceSpan 2 1) (seVar "map")
            ]
        )
    )
    ( parseNormalized
        """
        import Std::List (map, filter).
        map.
        """
    )

testLowersModuleImportStatements :: IO ()
testLowersModuleImportStatements =
  assertRight
    "parse + lower module/import"
    ( parseNormalized
        """
        module App::Core {
        import Std::List (map).
        map.
        }
        """
    )
    (\surfaceProgram -> assertLoweredCoreEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredModule (SourceSpan 1 1) ["App", "Core"],
          loweredImport (SourceSpan 2 1) ["Std", "List"] Nothing (Just ["map"]),
          loweredExpression (SourceSpan 3 1) (loweredVariable "map")
        ]

testLowersQualifiedAliasLookup :: IO ()
testLowersQualifiedAliasLookup =
  assertRight
    "parse + lower qualified alias lookup"
    ( parseNormalized
        """
        import Lib::Math as Math.
        Math::subtract.
        """
    )
    (\surfaceProgram -> assertLoweredCoreEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      loweredBlock
        [ loweredImport (SourceSpan 1 1) ["Lib", "Math"] (Just "Math") Nothing,
          loweredExpression (SourceSpan 2 1) (loweredVariable (qualifiedName "Math" "subtract"))
        ]

testRejectsSpacedQualifiedAliasLookupInBindingExpression :: IO ()
testRejectsSpacedQualifiedAliasLookupInBindingExpression =
  assertLeftDiagnosticContains
    "spaced qualified alias lookup in binding expression"
    "2:13: expected '.'"
    ( parseNormalized
        """
        import Lib::Math as Math.
        main = Math :: subtract.
        """
    )

testRejectsNonIdentifierQualifiedMember :: IO ()
testRejectsNonIdentifierQualifiedMember =
  assertLeftDiagnosticContains
    "non-identifier qualified alias member"
    "expected member name after '::'"
    ( parseNormalized
        """
        import Lib::Math as Math.
        Math::1.
        """
    )

testRejectsConstructorQualifiedNonIdentifierMember :: IO ()
testRejectsConstructorQualifiedNonIdentifierMember =
  assertLeftDiagnosticContains
    "constructor qualified non-identifier member"
    "expected member name after '::'"
    (parseNormalized "Math::1.")

testRejectsLegacyModuleDeclarations :: IO ()
testRejectsLegacyModuleDeclarations =
  mapM_
    ( \(label, source) ->
        assertLeftDiagnosticContains
          label
          "expected '{'"
          (parseNormalized source)
    )
    [ ("legacy dot-only module declaration rejected", "module App::Core."),
      ("legacy equals-style module declaration rejected", "module App::Core = 1."),
      -- Explicit escapes are intentional: this case asserts exact whitespace or source spans.
      ("legacy newline module declaration rejected", "module App::Core\nx = 1.")
    ]

testRejectsTrailingTopLevelStatementsAfterModuleBody :: IO ()
testRejectsTrailingTopLevelStatementsAfterModuleBody =
  assertLeftDiagnosticContains
    "trailing statement after module body"
    "after module declaration"
    ( parseNormalized
        """
        module App::Core {
        x = 1.
        }
        y = 2.
        """
    )

testRejectsModuleDeclarationAfterTopLevelStatement :: IO ()
testRejectsModuleDeclarationAfterTopLevelStatement =
  assertLeftDiagnosticContains
    "module declaration after top-level statement"
    "first top-level form"
    ( parseNormalized
        """
        x = 1.
        module App::Core {
        y = 2.
        }
        """
    )

testRejectsModuleDeclarationNestedInsideModuleBody :: IO ()
testRejectsModuleDeclarationNestedInsideModuleBody =
  assertLeftDiagnosticContains
    "module declaration nested inside module body"
    "top-level"
    ( parseNormalized
        """
        module App::Core {
        module Inner::Thing {
        y = 1.
        }
        }
        """
    )

testRejectsModuleDeclarationNestedInsideBlock :: IO ()
testRejectsModuleDeclarationNestedInsideBlock =
  assertLeftDiagnosticContains
    "module declaration nested inside block expression"
    "top-level"
    ( parseNormalized
        """
        x = { module App::Core {
        y = 1.
        } y. }.
        """
    )

testRejectsModuleMissingPath :: IO ()
testRejectsModuleMissingPath =
  assertLeftDiagnosticContains
    "module missing path error"
    "expected module path segment"
    (parseNormalized "module .")

testRejectsModuleTrailingSeparatorSpan :: IO ()
testRejectsModuleTrailingSeparatorSpan =
  assertLeftDiagnosticContains
    "module trailing separator span"
    "1:9"
    (parseNormalized "module A::.")

testRejectsDuplicateModuleExport :: IO ()
testRejectsDuplicateModuleExport = do
  let result =
        parseNormalized
          """
          module Lib::Value (answer, answer) {
          answer = 1.
          }
          """
  assertLeftDiagnosticContains
    "duplicate module export code"
    "E0001"
    result
  assertLeftDiagnosticContains
    "duplicate module export message"
    "duplicate module export 'answer'"
    result
  assertLeftDiagnosticContains
    "duplicate module export span"
    "1:28"
    result

testRejectsDuplicateNamespaceAwareModuleExport :: IO ()
testRejectsDuplicateNamespaceAwareModuleExport = do
  assertRight
    "same-name different namespace module exports"
    ( parseNormalized
        """
        module Lib::Box (type Box, constructor Box) {
        data Box = Box Int.
        }
        """
    )
    (const (pure ()))
  assertLeftDiagnosticContains
    "duplicate namespace-aware module export"
    "duplicate module export type 'Box'"
    ( parseNormalized
        """
        module Lib::Box (type Box, type Box) {
        data Box = Box Int.
        }
        """
    )

testRejectsEmptyGroupedConstructorExport :: IO ()
testRejectsEmptyGroupedConstructorExport = do
  assertLeftDiagnosticContains
    "empty grouped constructor export code"
    "E0001"
    (parseNormalized "module Lib::Box (type Box()) {}")
  assertLeftDiagnosticContains
    "empty grouped constructor export span"
    "1:27"
    (parseNormalized "module Lib::Box (type Box()) {}")

testRejectsMalformedAllConstructorExport :: IO ()
testRejectsMalformedAllConstructorExport = do
  assertLeftDiagnosticContains
    "malformed all-constructor export code"
    "E0001"
    (parseNormalized "module Lib::Box (type Box(.)) {}")
  assertLeftDiagnosticContains
    "malformed all-constructor export span"
    "1:28"
    (parseNormalized "module Lib::Box (type Box(.)) {}")

testRejectsMissingGroupedConstructorComma :: IO ()
testRejectsMissingGroupedConstructorComma =
  assertLeftDiagnosticContains
    "missing grouped constructor comma"
    "1:31"
    (parseNormalized "module Lib::Box (type Box(One Two)) {}")

testRejectsUnclosedGroupedConstructorExport :: IO ()
testRejectsUnclosedGroupedConstructorExport =
  assertLeftDiagnosticContains
    "unclosed grouped constructor export"
    "expected ',' or ')'"
    (parseNormalized "module Lib::Box (type Box(One, Two) {")

testRejectsNonIdentifierGroupedConstructorExport :: IO ()
testRejectsNonIdentifierGroupedConstructorExport =
  assertLeftDiagnosticContains
    "non-identifier grouped constructor export"
    "1:27"
    (parseNormalized "module Lib::Box (type Box(1)) {}")

testRejectsDuplicateGroupedConstructorExport :: IO ()
testRejectsDuplicateGroupedConstructorExport = do
  assertLeftDiagnosticContains
    "duplicate grouped constructor export code"
    "E0001"
    (parseNormalized "module Lib::Box (type Box(One, One)) {}")
  assertLeftDiagnosticContains
    "duplicate grouped constructor export message"
    "duplicate constructor export 'One'"
    (parseNormalized "module Lib::Box (type Box(One, One)) {}")
  assertLeftDiagnosticContains
    "duplicate grouped constructor export span"
    "1:32"
    (parseNormalized "module Lib::Box (type Box(One, One)) {}")

testRejectsTrailingCommaInModuleExportList :: IO ()
testRejectsTrailingCommaInModuleExportList = do
  let result =
        parseNormalized
          """
          module Lib::Value (answer,) {
          answer = 1.
          }
          """
  assertLeftDiagnosticContains
    "trailing module export comma code"
    "E0001"
    result
  assertLeftDiagnosticContains
    "trailing module export comma message"
    "expected module export name"
    result

testRejectsUnclosedModuleExportList :: IO ()
testRejectsUnclosedModuleExportList =
  assertLeftDiagnosticContains
    "unclosed module export list"
    "expected ',' or ')'"
    ( parseNormalized
        """
        module Lib::Value (answer {
        answer = 1.
        }
        """
    )

testRejectsMissingBodyAfterModuleExportList :: IO ()
testRejectsMissingBodyAfterModuleExportList =
  assertLeftDiagnosticContains
    "missing body after module export list"
    "expected '{'"
    (parseNormalized "module Lib::Value (answer).")

testRejectsImportTrailingSeparatorSpan :: IO ()
testRejectsImportTrailingSeparatorSpan =
  assertLeftDiagnosticContains
    "import trailing separator span"
    "1:9"
    (parseNormalized "import A::.")

testRejectsImportEmptySymbolList :: IO ()
testRejectsImportEmptySymbolList =
  assertLeftDiagnosticContains
    "import empty symbol list error"
    "expected at least one import symbol"
    (parseNormalized "import Std::List ().")

testRejectsImportEmptySymbolListSpan :: IO ()
testRejectsImportEmptySymbolListSpan =
  assertLeftDiagnosticContains
    "import empty symbol list span"
    "1:19"
    (parseNormalized "import Std::List ().")

testRejectsImportDuplicateSymbols :: IO ()
testRejectsImportDuplicateSymbols =
  assertLeftDiagnosticContains
    "import duplicate symbol error"
    "duplicate import symbol 'map'"
    (parseNormalized "import Std::List (map, filter, map).")

testRejectsImportReservedLiteralAlias :: IO ()
testRejectsImportReservedLiteralAlias =
  assertLeftDiagnosticContains
    "import reserved alias error"
    "reserved literal 'True' cannot be used as an import alias"
    (parseNormalized "import Std::List as True.")

testRejectsImportAliasWithSymbolList :: IO ()
testRejectsImportAliasWithSymbolList =
  assertLeftDiagnosticContains
    "import alias+symbol list error"
    "cannot combine import alias and symbol list"
    (parseNormalized "import Std::List as List (map).")

testRejectsImportSymbolListWithAlias :: IO ()
testRejectsImportSymbolListWithAlias =
  assertLeftDiagnosticContains
    "import symbol-list+alias error"
    "cannot combine import alias and symbol list"
    (parseNormalized "import Std::List (map) as List.")

parseNormalized :: Text -> Either Diagnostic SurfaceExpr
parseNormalized = fmap normalizeSurfaceExpr . parseSurfaceProgram

normalizeSurfaceExpr :: SurfaceExpr -> SurfaceExpr
normalizeSurfaceExpr expression =
  SurfaceExpr
    fixtureSpan
    ( case surfaceExprForm expression of
        SEBlock statements -> SEBlock (map normalizeStatement statements)
        other -> other
    )

normalizeStatement :: SurfaceStatement -> SurfaceStatement
normalizeStatement statement =
  case statement of
    SSLet name statementSpan body -> SSLet name statementSpan (normalizeSurfaceExpr body)
    SSImpl statementSpan className arguments methods ->
      SSImpl
        statementSpan
        className
        arguments
        [SurfaceImplMethod name methodSpan (normalizeSurfaceExpr body) | SurfaceImplMethod name methodSpan body <- methods]
    SSExpr statementSpan body -> SSExpr statementSpan (normalizeSurfaceExpr body)
    other -> other

fixtureSpan :: SourceSpan
fixtureSpan = SourceSpan 1 1

seBlock :: [SurfaceStatement] -> SurfaceExpr
seBlock = SurfaceExpr fixtureSpan . SEBlock

seLit :: SurfaceLiteral -> SurfaceExpr
seLit = SurfaceExpr fixtureSpan . SELit

seQualifiedVar :: Identifier -> Identifier -> SurfaceExpr
seQualifiedVar qualifier member = SurfaceExpr fixtureSpan (SEQualifiedVar qualifier member)

seVar :: Identifier -> SurfaceExpr
seVar = SurfaceExpr fixtureSpan . SEVar
