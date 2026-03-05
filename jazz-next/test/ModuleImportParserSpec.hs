{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import JazzNext.Compiler.AST
  ( Expr (..),
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
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftContains,
    assertRight,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "ModuleImportParser" tests

tests :: [NamedTest]
tests =
  [ ("parses module declaration statement", testParsesModuleDeclaration),
    ("parses import statement with alias", testParsesImportAlias),
    ("parses import statement with symbol list", testParsesImportSymbolList),
    ("lowers module and import statements into core AST", testLowersModuleImportStatements),
    ("rejects module statement with missing path", testRejectsModuleMissingPath),
    ("rejects module statement with trailing separator using separator span", testRejectsModuleTrailingSeparatorSpan),
    ("rejects import statement with empty symbol list", testRejectsImportEmptySymbolList),
    ("rejects import statement with alias and symbol list together", testRejectsImportAliasWithSymbolList)
  ]

testParsesModuleDeclaration :: IO ()
testParsesModuleDeclaration =
  assertEqual
    "module surface AST"
    ( Right
        ( SEScope
            [ SSModule (SourceSpan 1 1) ["App", "Core"],
              SSLet "x" (SourceSpan 2 1) (SEInt 1)
            ]
        )
    )
    (parseSurfaceProgram "module App::Core.\nx = 1.")

testParsesImportAlias :: IO ()
testParsesImportAlias =
  assertEqual
    "import alias surface AST"
    ( Right
        ( SEScope
            [ SSImport (SourceSpan 1 1) ["Std", "List"] (Just "List") Nothing,
              SSExpr (SourceSpan 2 1) (SEVar "List")
            ]
        )
    )
    (parseSurfaceProgram "import Std::List as List.\nList.")

testParsesImportSymbolList :: IO ()
testParsesImportSymbolList =
  assertEqual
    "import symbol-list surface AST"
    ( Right
        ( SEScope
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
    (parseSurfaceProgram "module App::Core.\nimport Std::List (map).\nmap.")
    (\surfaceProgram -> assertEqual "lowered AST" expectedProgram (lowerSurfaceExpr surfaceProgram))
  where
    expectedProgram =
      EScope
        [ SModule (SourceSpan 1 1) ["App", "Core"],
          SImport (SourceSpan 2 1) ["Std", "List"] Nothing (Just ["map"]),
          SExpr (SourceSpan 3 1) (EVar "map")
        ]

testRejectsModuleMissingPath :: IO ()
testRejectsModuleMissingPath =
  assertLeftContains
    "module missing path error"
    "expected module path segment"
    (parseSurfaceProgram "module .")

testRejectsModuleTrailingSeparatorSpan :: IO ()
testRejectsModuleTrailingSeparatorSpan =
  assertLeftContains
    "module trailing separator span"
    "1:9"
    (parseSurfaceProgram "module A::.")

testRejectsImportEmptySymbolList :: IO ()
testRejectsImportEmptySymbolList =
  assertLeftContains
    "import empty symbol list error"
    "expected at least one import symbol"
    (parseSurfaceProgram "import Std::List ().")

testRejectsImportAliasWithSymbolList :: IO ()
testRejectsImportAliasWithSymbolList =
  assertLeftContains
    "import alias+symbol list error"
    "cannot combine import alias and symbol list"
    (parseSurfaceProgram "import Std::List as List (map).")
