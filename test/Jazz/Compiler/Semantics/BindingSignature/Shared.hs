{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Semantics.BindingSignature.Shared
  ( validSignatureProgram,
    separatedSignatureProgram,
    mismatchedSignatureProgram,
    useBeforeDefinitionProgram,
    nestedScopeProgram,
    selfRecursiveProgram,
    mutualRecursionProgram,
    threeNodeMutualRecursionProgram,
    nonRecursiveForwardReferenceProgram,
    retroactiveRebindingProgram,
    signatureTypeMismatchProgram,
    assertSourceOk,
    assertSourceOkWithoutPrelude,
    assertSourceErrorContains,
    assertSourceSingleErrorContains,
    assertSourceSingleErrorContainsWithoutPrelude,
    assertSourceSingleErrorCodeAndPrimarySpan,
    qualifiedEqSource,
    compileModuleSources,
    loweredProgram,
    resolvedProgram,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CorePhase (Lowered, Resolved),
    Expr (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Driver
  ( CompileResult,
    compileErrors,
    compileModuleGraphWithPrelude,
    compileSource,
    compileSourceWithPrelude,
  )
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleResolver (ModuleResolutionConfig (..), resolveStandaloneExprNames)
import Jazz.Compiler.Parser (parseSurfaceProgram)
import Jazz.Compiler.Parser.Lower (lowerSurfaceExpr)
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import Jazz.TestHarness
  ( assertContains,
    assertEqual,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    assertSingleDiagnosticPrimaryStart,
  )

validSignatureProgram :: Expr 'Lowered
validSignatureProgram = loweredProgram "x :: Int.\nx = 1.\nx."

separatedSignatureProgram :: Expr 'Lowered
separatedSignatureProgram = loweredProgram "x :: Int.\n1.\nx = 2."

mismatchedSignatureProgram :: Expr 'Lowered
mismatchedSignatureProgram = loweredProgram "x :: Int.\ny = 2."

useBeforeDefinitionProgram :: Expr 'Lowered
useBeforeDefinitionProgram = loweredProgram "x.\nx = 1."

nestedScopeProgram :: Expr 'Lowered
nestedScopeProgram = loweredProgram "x = 1.\nnested = { x. }."

selfRecursiveProgram :: Expr 'Lowered
selfRecursiveProgram = loweredProgram "f = f."

mutualRecursionProgram :: Expr 'Lowered
mutualRecursionProgram = loweredProgram "even = odd.\nodd = even.\neven."

threeNodeMutualRecursionProgram :: Expr 'Lowered
threeNodeMutualRecursionProgram = loweredProgram "a = b.\nb = c.\nc = a.\na."

nonRecursiveForwardReferenceProgram :: Expr 'Lowered
nonRecursiveForwardReferenceProgram = loweredProgram "x = y.\ny = 1.\nx."

retroactiveRebindingProgram :: Expr 'Lowered
retroactiveRebindingProgram = loweredProgram "x = y.\ny = 1.\ny = x.\nx."

signatureTypeMismatchProgram :: Expr 'Lowered
signatureTypeMismatchProgram = loweredProgram "x :: Int.\nx = True."

assertSourceOk :: Text.Text -> IO ()
assertSourceOk src = do
  result <- compileSource defaultWarningSettings src
  assertEqual "compile errors" [] (compileErrors result)

assertSourceOkWithoutPrelude :: Text.Text -> IO ()
assertSourceOkWithoutPrelude src = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing src
  assertEqual "compile errors" [] (compileErrors result)

assertSourceErrorContains :: Text.Text -> Text.Text -> IO ()
assertSourceErrorContains src needle = do
  result <- compileSource defaultWarningSettings src
  assertContains "source error" needle (Text.unlines (map renderDiagnostic (compileErrors result)))

assertSourceSingleErrorContains :: Text.Text -> Text.Text -> IO ()
assertSourceSingleErrorContains src needle = do
  result <- compileSource defaultWarningSettings src
  assertSingleDiagnosticContains "source error" needle (compileErrors result)

assertSourceSingleErrorContainsWithoutPrelude :: Text.Text -> Text.Text -> IO ()
assertSourceSingleErrorContainsWithoutPrelude src needle = do
  result <- compileSourceWithPrelude defaultWarningSettings Nothing src
  assertSingleDiagnosticContains "source error" needle (compileErrors result)

assertSourceSingleErrorCodeAndPrimarySpan :: Text.Text -> Text.Text -> SourceSpan -> IO ()
assertSourceSingleErrorCodeAndPrimarySpan src expectedCode expectedSpan = do
  result <- compileSource defaultWarningSettings src
  assertSingleDiagnosticCode "source error code" expectedCode (compileErrors result)
  assertSingleDiagnosticPrimaryStart "source error primary span" expectedSpan (compileErrors result)

qualifiedEqSource :: Text.Text
qualifiedEqSource =
  """
  class Equatable(a) {
  equals :: a -> a -> Bool.
  }.
  impl Equatable(Int) {
  equals = __kernel_equals.
  }.

  """

compileModuleSources :: [(FilePath, Text.Text)] -> IO CompileResult
compileModuleSources sources =
  compileModuleGraphWithPrelude
    defaultWarningSettings
    Nothing
    (ModuleResolutionConfig ["src"] ".jz")
    ["App"]
    (pure . (`Map.lookup` Map.fromList sources))

loweredProgram :: Text.Text -> Expr 'Lowered
loweredProgram source =
  case parseSurfaceProgram source of
    Left diagnostic -> error (Text.unpack (renderDiagnostic diagnostic))
    Right surface -> lowerSurfaceExpr surface

resolvedProgram :: Text.Text -> Expr 'Resolved
resolvedProgram = resolveStandaloneExprNames (exportInventory []) . loweredProgram
