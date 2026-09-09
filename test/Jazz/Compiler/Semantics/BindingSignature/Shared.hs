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
    importedQualifiedMethodFactsProgram,
    aliasOnlyImportedCapabilityFactsProgram,
    loweredProgram,
    resolvedProgram,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CorePhase (Lowered, Resolved),
    Expr (..),
    Statement (..),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Driver
  ( compileErrors,
    compileSource,
    compileSourceWithPrelude,
  )
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleResolver (resolveStandaloneExprNames)
import Jazz.Compiler.Parser (parseSurfaceProgram)
import Jazz.Compiler.Parser.Lower (lowerSurfaceExpr, reindexLoweredExpr)
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import Jazz.TestHarness
  ( assertContains,
    assertEqual,
    assertSingleDiagnosticCode,
    assertSingleDiagnosticContains,
    assertSingleDiagnosticPrimarySpan,
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
  assertSingleDiagnosticPrimarySpan "source error primary span" expectedSpan (compileErrors result)

qualifiedEqSource :: Text.Text
qualifiedEqSource =
  """
  class Eq(a) {
  equals :: a -> a -> Bool.
  }.
  impl Eq(Int) {
  equals = \\(left, right) -> left == right.
  }.

  """

importedQualifiedMethodFactsProgram :: Expr 'Lowered
importedQualifiedMethodFactsProgram =
  loweredPrograms
    [ "module Lib { class RemoteEq(a) { equals :: a -> a -> Bool. }. impl RemoteEq(Int) { equals = \\(left, right) -> left == right. }. }",
      "module App { import Lib. RemoteEq::equals 1 1. }"
    ]

aliasOnlyImportedCapabilityFactsProgram :: Expr 'Lowered
aliasOnlyImportedCapabilityFactsProgram =
  loweredPrograms
    [ "module Lib { class RemoteEq(a) { }. impl RemoteEq(Int) { }. }",
      "module App { import Lib as Lib. x :: @{RemoteEq(Int)}: Int. x = 1. }"
    ]

loweredProgram :: Text.Text -> Expr 'Lowered
loweredProgram source =
  case parseSurfaceProgram source of
    Left diagnostic -> error (Text.unpack (renderDiagnostic diagnostic))
    Right surface -> lowerSurfaceExpr surface

loweredPrograms :: [Text.Text] -> Expr 'Lowered
loweredPrograms = reindexLoweredExpr . mergeLoweredPrograms . map loweredProgram

mergeLoweredPrograms :: [Expr 'Lowered] -> Expr 'Lowered
mergeLoweredPrograms programs =
  case programs of
    [] -> error "expected at least one lowered source unit"
    EBlock node statements : remainingPrograms -> EBlock node (statements <> concatMap blockStatements remainingPrograms)
    expression : _ -> error ("expected lowered source-unit block, got " <> show expression)
  where
    blockStatements :: Expr 'Lowered -> [Statement 'Lowered]
    blockStatements (EBlock _ statements) = statements
    blockStatements expression = error ("expected lowered source-unit block, got " <> show expression)

resolvedProgram :: Text.Text -> Expr 'Resolved
resolvedProgram source =
  case resolveStandaloneExprNames (exportInventory []) (loweredProgram source) of
    Left diagnostics -> error (Text.unpack (Text.unlines (map renderDiagnostic (toList diagnostics))))
    Right expression -> expression
  where
    toList (diagnostic :| diagnostics) = diagnostic : diagnostics
