{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Semantics.PrimitiveSemantics.Shared
  ( assertCallableEqualityRejected,
    assertCallableEqualityRejectedWithBundledPrelude,
    assertCompileError,
    assertCompileErrorWithBundledPrelude,
    assertCompileErrorWithPrelude,
    assertCompiles,
    assertCompilesWithBundledPrelude,
    mkProgram
  )
where

import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.BundledPrelude
  ( bundledPreludeSource
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileSource,
    compileSourceWithPrelude
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings
  )
import JazzNext.TestHarness
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

assertCompileErrorWithPrelude :: String -> String -> String -> String -> IO ()
assertCompileErrorWithPrelude preludeSource source failureLabel errorCode = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just (Text.pack preludeSource)) (Text.pack source)
  assertSingleDiagnosticContains
    (Text.pack failureLabel)
    (Text.pack errorCode)
    (compileErrors result)

assertCallableEqualityRejected :: String -> Text.Text -> IO ()
assertCallableEqualityRejected failureLabel source = do
  result <- compileSource defaultWarningSettings source
  assertCallableEqualityDiagnostic failureLabel result

assertCallableEqualityRejectedWithBundledPrelude :: String -> Text.Text -> IO ()
assertCallableEqualityRejectedWithBundledPrelude failureLabel source = do
  result <- compileSourceWithPrelude defaultWarningSettings (Just bundledPreludeSource) source
  assertCallableEqualityDiagnostic failureLabel result

assertCallableEqualityDiagnostic :: String -> CompileResult -> IO ()
assertCallableEqualityDiagnostic failureLabel result = do
  assertSingleDiagnosticContains
    (Text.pack (failureLabel <> " code"))
    "E2004"
    (compileErrors result)
  assertSingleDiagnosticContains
    (Text.pack (failureLabel <> " callable text"))
    "callable values are not equality-supported"
    (compileErrors result)

mkProgram :: Expr -> Expr
mkProgram expr =
  EBlock
    [ SExpr
        (SourceSpan 1 1)
        expr
    ]
