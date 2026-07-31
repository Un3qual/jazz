{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Semantics.PrimitiveSemantics.Shared
  ( assertCompileError,
    assertCompileErrorWithBundledPrelude,
    assertCompiles,
    assertCompilesWithBundledPrelude,
    mkProgram
  )
where

import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import Jazz.Compiler.BundledPrelude
  ( bundledPreludeSource
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import Jazz.Compiler.Driver
  ( compileErrors,
    compileSource,
    compileSourceWithPrelude
  )
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import Jazz.TestHarness
  ( assertEqual,
    assertSingleDiagnosticContains
  )

assertCompiles :: String -> IO ()
assertCompiles source = do
  result <- compileSource defaultWarningSettings (Text.pack source)
  assertEqual "compile errors" [] (compileErrors result)

assertCompilesWithBundledPrelude :: String -> IO ()
assertCompilesWithBundledPrelude source = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just bundledPreludeSource) (Text.pack source)
  assertEqual "compile errors" [] (compileErrors result)

assertCompileError :: String -> String -> String -> IO ()
assertCompileError source failureLabel errorCode = do
  result <- compileSource defaultWarningSettings (Text.pack source)
  assertSingleDiagnosticContains
    (Text.pack failureLabel)
    (Text.pack errorCode)
    (compileErrors result)

assertCompileErrorWithBundledPrelude :: String -> String -> String -> IO ()
assertCompileErrorWithBundledPrelude source failureLabel errorCode = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just bundledPreludeSource) (Text.pack source)
  assertSingleDiagnosticContains
    (Text.pack failureLabel)
    (Text.pack errorCode)
    (compileErrors result)

mkProgram :: Expr -> Expr
mkProgram expr =
  EBlock
    [ SExpr
        (SourceSpan 1 1)
        expr
    ]
