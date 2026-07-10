{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
  )
import JazzNext.Compiler.Name
  ( mkIdentifier
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceDataConstructor (..),
    SurfaceDataConstructorArgument (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Lexer (Token)
import JazzNext.Compiler.Parser.Declaration
  ( parseDataStatementParser,
    parseImportStatementParser
  )
import JazzNext.Compiler.Parser.TestSupport
  ( lexSource
  )
import JazzNext.Compiler.Parser.TokenParser (runTokenParserPrefix)
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftDiagnosticContains,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "DeclarationParser" tests

tests :: [NamedTest]
tests =
  [ ("rejects import alias followed by symbol list", testRejectsImportAliasWithSymbolList),
    ("rejects import symbol list followed by alias", testRejectsImportSymbolListWithAlias),
    ("parses data constructors with named and grouped payloads", testParsesDataConstructors)
  ]

testRejectsImportAliasWithSymbolList :: IO ()
testRejectsImportAliasWithSymbolList = do
  tokens <- lexSource "import Lib::Math as Math (subtract)."
  assertLeftDiagnosticContains
    "import alias with symbol list"
    "cannot combine import alias and symbol list"
    (parseImportStatementTokens tokens)

testRejectsImportSymbolListWithAlias :: IO ()
testRejectsImportSymbolListWithAlias = do
  tokens <- lexSource "import Lib::Math (subtract) as Math."
  assertLeftDiagnosticContains
    "import symbol list with alias"
    "cannot combine import alias and symbol list"
    (parseImportStatementTokens tokens)

testParsesDataConstructors :: IO ()
testParsesDataConstructors = do
  tokens <- lexSource "data Maybe a = None | Some a | Pair (a, a) [a]."
  assertEqual
    "data declaration"
    ( Right
        ( SSData
            (SourceSpan 1 1)
            (mkIdentifier "Maybe")
            [mkIdentifier "a"]
            [ SurfaceDataConstructor (mkIdentifier "None") [],
              SurfaceDataConstructor
                (mkIdentifier "Some")
                [SurfaceDataConstructorArgumentName (mkIdentifier "a")],
              SurfaceDataConstructor
                (mkIdentifier "Pair")
                [ SurfaceDataConstructorArgumentOpaque,
                  SurfaceDataConstructorArgumentOpaque
                ]
            ],
          []
        )
    )
    (parseDataStatementTokens tokens)

parseImportStatementTokens :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseImportStatementTokens =
  runTokenParserPrefix "owned import declaration" parseImportStatementParser

parseDataStatementTokens :: [Token] -> Either Diagnostic (SurfaceStatement, [Token])
parseDataStatementTokens =
  runTokenParserPrefix "owned data declaration" parseDataStatementParser
