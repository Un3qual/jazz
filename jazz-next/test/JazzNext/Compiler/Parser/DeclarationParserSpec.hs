{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..),
    renderDiagnostic
  )
import JazzNext.Compiler.Identifier
  ( mkIdentifier
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceDataConstructor (..),
    SurfaceDataConstructorArgument (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Declaration
  ( parseDataStatementTokens,
    parseImportStatementTokens
  )
import JazzNext.Compiler.Parser.Lexer
  ( Token,
    tokenize
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftDiagnosticContains,
    failTest,
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

lexSource :: Text -> IO [Token]
lexSource source =
  case tokenize source of
    Right tokens -> pure tokens
    Left diagnostic -> failTest ("tokenize: expected Right, got " <> renderDiagnostic diagnostic)
