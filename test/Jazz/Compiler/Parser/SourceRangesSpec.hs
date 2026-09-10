{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (CoreNode (..), Expr (..), Statement (..))
import Jazz.Compiler.Diagnostics (SourceSpan (..), diagnosticPrimarySpan, diagnosticRelatedSpan, qualifySourceSpan, sourceSpanEnd, sourceSpanStart)
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleGraph (coreModuleExpr, coreModuleImports)
import Jazz.Compiler.ModuleIdentity (mkSourceFile, moduleIdentity, standaloneModulePath)
import Jazz.Compiler.ModuleResolver (resolveStandaloneExprNames)
import Jazz.Compiler.ModuleResolver.Imports (validateImportBindings)
import Jazz.Compiler.Parser (parseSurfaceProgram)
import Jazz.Compiler.Parser.AST (SurfaceExpr (..), SurfaceExprForm (..), SurfaceLambdaParameter (..), SurfacePattern (..), SurfacePatternForm (..), SurfacePatternLambdaClause (..), SurfaceStatement (..))
import Jazz.Compiler.Parser.Lexer (Token (..), tokenize)
import Jazz.Compiler.Parser.Lower (lowerSurfaceExpr, lowerSurfaceModule, reindexLoweredExpr)
import Jazz.Compiler.Parser.Pattern (parseCaseArmPatternTokens)
import Jazz.Compiler.Prelude (ResolvedPrelude (..), preparePrelude)
import Jazz.Compiler.TypeInference (inferExpressionDefault)
import Jazz.Compiler.TypeInference.Result (InferenceResult (inferredExpr))
import Jazz.TestHarness (assertEqual, assertRight, failTest, runTestSuite)

main :: IO ()
main =
  runTestSuite
    "SourceRanges"
    [ ("lexemes include Unicode and original escape spellings", lexemeExtent),
      ("multiline expressions retain nested delimiters and exclude trivia", nestedExtents),
      ("application and infix intermediate nodes have complete extents", applicationExtents),
      ("explicit type argument range survives lowering", typeApplicationExtent),
      ("nested patterns retain delimiters and constructor extents", patternExtents),
      ("lambda unit parameters and pattern clauses retain ranges", lambdaExtents),
      ("lowering reindexing and inference preserve expression ranges", lowerAndAnalyze),
      ("parser diagnostics expose the offending token range", parserDiagnosticRange),
      ("prelude bridge diagnostic unqualification retains both ranges", preludeDiagnosticRanges),
      ("import alias collision diagnostics retain both ranges", importDiagnosticRanges),
      ("qualification preserves endpoints and point projection is explicit", qualifyRanges)
    ]

lexemeExtent :: IO ()
lexemeExtent = assertRight "tokenize" (tokenize "αβ \"😀\\n\"\n\t42") $ \tokens ->
  assertEqual
    "lexeme ranges"
    [SourceRange 1 1 1 3, SourceRange 1 4 1 10, SourceRange 2 9 2 11]
    (map tokenSpan tokens)

expression :: Text -> (SurfaceExpr -> IO ()) -> IO ()
expression source check = assertRight "parse" (parseSurfaceProgram source) $ \program ->
  case surfaceExprForm program of
    SEBlock [SSExpr _ value] -> check value
    _ -> failTest ("expected single expression for " <> source <> ": " <> Text.pack (show program))

nestedExtents :: IO ()
nestedExtents = expression "[1,\n (2, 3)]   # ignored\n." $ \value -> do
  assertEqual "list range" (SourceRange 1 1 2 9) (surfaceExprSpan value)
  case surfaceExprForm value of
    SEList [one, tuple] -> do
      assertEqual "first element" (SourceRange 1 2 1 3) (surfaceExprSpan one)
      assertEqual "tuple includes delimiters" (SourceRange 2 2 2 8) (surfaceExprSpan tuple)
      case surfaceExprForm tuple of
        SETuple [two, three] ->
          assertEqual
            "tuple children"
            [SourceRange 2 3 2 4, SourceRange 2 6 2 7]
            (map surfaceExprSpan [two, three])
        _ -> failTest "expected pair"
    _ -> failTest "expected list"

applicationExtents :: IO ()
applicationExtents = do
  expression "f x y." $ \value -> case surfaceExprForm value of
    SEApply partial argument -> do
      assertEqual "whole application" (SourceRange 1 1 1 6) (surfaceExprSpan value)
      assertEqual "partial application" (SourceRange 1 1 1 4) (surfaceExprSpan partial)
      assertEqual "last argument" (SourceRange 1 5 1 6) (surfaceExprSpan argument)
    _ -> failTest "expected nested application"
  expression "1 + 2 * 3." $ \value -> case surfaceExprForm value of
    SEBinary "+" _ productExpr -> do
      assertEqual "sum" (SourceRange 1 1 1 10) (surfaceExprSpan value)
      assertEqual "product" (SourceRange 1 5 1 10) (surfaceExprSpan productExpr)
    _ -> failTest "expected sum"
  expression "f (x) y." $ \value -> case surfaceExprForm value of
    SEApply partial _ -> do
      assertEqual "grouped partial application" (SourceRange 1 1 1 6) (surfaceExprSpan partial)
      assertEqual "whole grouped application" (SourceRange 1 1 1 8) (surfaceExprSpan value)
    _ -> failTest "expected grouped application"
  expression "1 + (2)." $ \value -> case surfaceExprForm value of
    SEBinary _ _ right -> do
      assertEqual "infix includes consumed grouping" (SourceRange 1 1 1 8) (surfaceExprSpan value)
      assertEqual "grouped operand retains inner range" (SourceRange 1 6 1 7) (surfaceExprSpan right)
    _ -> failTest "expected grouped infix"
  expression "[漢::thing]." $ \value -> case surfaceExprForm value of
    SEList [member] -> assertEqual "adjacent wide identifier qualification" (SourceRange 1 2 1 11) (surfaceExprSpan member)
    _ -> failTest "expected qualified list member"
  expression "(123)." $ \value ->
    assertEqual "erased grouping retains inner range" (SourceRange 1 2 1 5) (surfaceExprSpan value)

patternExtents :: IO ()
patternExtents = assertRight "lex pattern" (tokenize "Pair (a, b) [x] -> body") $ \tokens ->
  assertRight "parse pattern" (parseCaseArmPatternTokens tokens) $ \(value, _) -> do
    assertEqual "constructor" (SourceRange 1 1 1 16) (surfacePatternSpan value)
    case surfacePatternForm value of
      SPConstructor _ [tuple, list] ->
        assertEqual
          "nested patterns"
          [SourceRange 1 6 1 12, SourceRange 1 13 1 16]
          (map surfacePatternSpan [tuple, list])
      _ -> failTest "expected constructor pattern"

lowerAndAnalyze :: IO ()
lowerAndAnalyze = assertRight "parse" (parseSurfaceProgram "[1,\n 2].") $ \surface -> do
  let lowered = lowerSurfaceExpr surface
      expected = [SourceRange 1 1 2 4, SourceRange 1 2 1 3, SourceRange 2 2 2 3]
      spans core = case core of
        EBlock _ [SExpr _ (EList node [ELit firstNode _, ELit secondNode _])] ->
          [coreNodeSpan node, coreNodeSpan firstNode, coreNodeSpan secondNode]
        _ -> []
  assertEqual "lowered" expected (spans lowered)
  assertEqual "reindexed" expected (spans (reindexLoweredExpr lowered))
  assertRight "resolve" (resolveStandaloneExprNames (exportInventory []) lowered) $ \resolved -> do
    inferred <- inferExpressionDefault resolved
    assertEqual "inferred" expected (spans (inferredExpr inferred))
  let identity = moduleIdentity standaloneModulePath (mkSourceFile "Main.jz")
  assertRight "lower qualified module" (lowerSurfaceModule identity surface) $ \coreModule -> do
    let qualified = map (qualifySourceSpan "Main.jz") expected
        moduleExpr = coreModuleExpr coreModule
    assertEqual "qualified lowering" qualified (spans moduleExpr)
    assertRight "resolve qualified module" (resolveStandaloneExprNames (exportInventory []) moduleExpr) $ \resolved -> do
      inferred <- inferExpressionDefault resolved
      assertEqual "qualified inference" qualified (spans (inferredExpr inferred))

qualifyRanges :: IO ()
qualifyRanges = do
  let range = SourceRange 2 3 4 5
  assertEqual "qualified range" (SourceRangeIn "Main.jz" 2 3 4 5) (qualifySourceSpan "Main.jz" range)
  assertEqual "total endpoint" (Just (4, 5)) (sourceSpanEnd range)
  assertEqual "legacy point" Nothing (sourceSpanEnd (SourceSpan 2 3))
  assertEqual "explicit projection" (SourceSpanIn "Main.jz" 2 3) (sourceSpanStart (qualifySourceSpan "Main.jz" range))
  assertEqual "structural equality retains extent" False (range == SourceSpan 2 3)

lambdaExtents :: IO ()
lambdaExtents = do
  expression "\\() -> 1." $ \value -> case surfaceExprForm value of
    SELambda (SurfaceLambdaPattern unit :| []) body -> do
      assertEqual "lambda" (SourceRange 1 1 1 9) (surfaceExprSpan value)
      assertEqual "unit parameter" (SourceRange 1 2 1 4) (surfacePatternSpan unit)
      assertEqual "body" (SourceRange 1 8 1 9) (surfaceExprSpan body)
    _ -> failTest "expected unit lambda"
  expression "\\ | (True) -> (1) | (False) -> 2." $ \value -> case surfaceExprForm value of
    SEPatternLambda (SurfacePatternLambdaClause firstSpan _ _ :| [SurfacePatternLambdaClause secondSpan _ _]) ->
      assertEqual "clause extents" [SourceRange 1 3 1 18, SourceRange 1 19 1 33] [firstSpan, secondSpan]
    _ -> failTest "expected pattern lambda"

parserDiagnosticRange :: IO ()
parserDiagnosticRange = case parseSurfaceProgram "[1,\n ] ." of
  Left diagnostic -> assertEqual "offending delimiter" (Just (SourceRange 2 2 2 3)) (diagnosticPrimarySpan diagnostic)
  Right _ -> failTest "expected missing list element diagnostic"

typeApplicationExtent :: IO ()
typeApplicationExtent = expression "f @Bool." $ \value -> do
  assertEqual "type application expression" (SourceRange 1 1 1 8) (surfaceExprSpan value)
  case (surfaceExprForm value, lowerSurfaceExpr value) of
    (SETypeApplication _ surfaceArgument _, ETypeApplication _ _ loweredArgument _) -> do
      assertEqual "type argument includes introducer" (SourceRange 1 3 1 8) surfaceArgument
      assertEqual "lowered type argument range" surfaceArgument loweredArgument
    _ -> failTest "expected explicit type application"

preludeDiagnosticRanges :: IO ()
preludeDiagnosticRanges =
  case preparePrelude (PreludeExplicit "__kernel_map = __kernel_map.\n__kernel_map = __kernel_map.") of
    Left diagnostic -> do
      assertEqual "prelude primary" (Just (SourceRange 2 1 2 13)) (diagnosticPrimarySpan diagnostic)
      assertEqual "prelude related" (Just (SourceRange 1 1 1 13)) (diagnosticRelatedSpan diagnostic)
    Right _ -> failTest "expected duplicate prelude bridge diagnostic"

importDiagnosticRanges :: IO ()
importDiagnosticRanges =
  assertRight "parse imports" (parseSurfaceProgram "import Lib::One as Same.\nimport Lib::Two as Same.") $ \surface ->
    assertRight "lower imports" (lowerSurfaceModule (moduleIdentity standaloneModulePath (mkSourceFile "Main.jz")) surface) $ \coreModule ->
      case validateImportBindings
        "Main.jz"
        standaloneModulePath
        (coreModuleImports coreModule)
        Set.empty
        Set.empty
        Set.empty
        Set.empty
        Map.empty
        Set.empty
        Set.empty
        Map.empty of
        Left diagnostic -> do
          assertEqual "import primary" (Just (SourceRange 2 1 2 7)) (diagnosticPrimarySpan diagnostic)
          assertEqual "import related" (Just (SourceRange 1 1 1 7)) (diagnosticRelatedSpan diagnostic)
        Right _ -> failTest "expected duplicate import alias diagnostic"
