{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Driver
  ( CompileResult (..),
    compileExpr,
    compileSource,
    compileSourceWithPrelude,
    RunResult (..),
    runExpr,
    runSource,
    runSourceWithPrelude
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr
  )
import JazzNext.Compiler.Diagnostics
  ( WarningRecord (..)
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )
import JazzNext.Compiler.Runtime
  ( evaluateRuntimeExpr,
    renderRuntimeValue
  )
import JazzNext.Compiler.TypeInference
  ( InferenceResult (..),
    inferExpression
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    isWarningError
  )

data CompileResult = CompileResult
  { compileWarnings :: [WarningRecord],
    compileErrors :: [Text],
    generatedJs :: Maybe Text
  }
  deriving (Eq, Show)

data RunResult = RunResult
  { runWarnings :: [WarningRecord],
    runCompileErrors :: [Text],
    runRuntimeErrors :: [Text],
    runOutput :: Maybe Text
  }
  deriving (Eq, Show)

-- Compiler driver flow for the current implementation slice:
-- analyze -> collect warnings/errors -> apply warning-as-error policy.
compileExpr :: WarningSettings -> Expr -> IO CompileResult
compileExpr settings expr = do
  (warnings, errors, _) <- analyzeWithWarnings settings expr
  let output =
        if null errors
          then
            -- This module is only responsible for warning/error control flow.
            -- JS generation remains a placeholder until codegen lands in jazz-next.
            Just "/* jazz-next codegen placeholder */"
          else Nothing
  pure
    CompileResult
      { compileWarnings = warnings,
        compileErrors = errors,
        generatedJs = output
      }

compileSource :: WarningSettings -> Text -> IO CompileResult
compileSource settings source =
  compileSourceWithPrelude settings Nothing source

compileSourceWithPrelude :: WarningSettings -> Maybe Text -> Text -> IO CompileResult
compileSourceWithPrelude settings preludeSource source =
  case parseAndLowerSource preludeSource source of
    Left parseErrorCode ->
      pure
        CompileResult
          { compileWarnings = [],
            compileErrors = [parseErrorCode],
            generatedJs = Nothing
          }
    Right loweredExpr ->
      compileExpr settings loweredExpr

runExpr :: WarningSettings -> Expr -> IO RunResult
runExpr settings expr = do
  (warnings, compileErrors, canonicalExpr) <- analyzeWithWarnings settings expr
  if not (null compileErrors)
    then
      pure
        RunResult
          { runWarnings = warnings,
            runCompileErrors = compileErrors,
            runRuntimeErrors = [],
            runOutput = Nothing
          }
    else
      case evaluateRuntimeExpr canonicalExpr of
        Left runtimeError ->
          pure
            RunResult
              { runWarnings = warnings,
                runCompileErrors = [],
                runRuntimeErrors = [runtimeError],
                runOutput = Nothing
              }
        Right runtimeValue ->
          pure
            RunResult
              { runWarnings = warnings,
                runCompileErrors = [],
                runRuntimeErrors = [],
                runOutput = fmap renderRuntimeValue runtimeValue
              }

runSource :: WarningSettings -> Text -> IO RunResult
runSource settings source =
  runSourceWithPrelude settings Nothing source

runSourceWithPrelude :: WarningSettings -> Maybe Text -> Text -> IO RunResult
runSourceWithPrelude settings preludeSource source =
  case parseAndLowerSource preludeSource source of
    Left parseErrorCode ->
      pure
        RunResult
          { runWarnings = [],
            runCompileErrors = [parseErrorCode],
            runRuntimeErrors = [],
            runOutput = Nothing
          }
    Right loweredExpr ->
      runExpr settings loweredExpr

analyzeWithWarnings :: WarningSettings -> Expr -> IO ([WarningRecord], [Text], Expr)
analyzeWithWarnings settings expr = do
  inference <- inferExpression settings expr
  let warnings = filterWarningsForPromotion settings (inferredWarnings inference)
      promotedWarnings = filter (isPromoted settings) warnings
      promotedWarningErrors = map warningToError promotedWarnings
      errors = inferredErrors inference ++ promotedWarningErrors
  pure (warnings, errors, inferredExpr inference)

filterWarningsForPromotion :: WarningSettings -> [WarningRecord] -> [WarningRecord]
-- Placeholder hook for future category-level filtering.
filterWarningsForPromotion _ = id

isPromoted :: WarningSettings -> WarningRecord -> Bool
isPromoted settings warning = isWarningError settings (warningCategory warning)

warningToError :: WarningRecord -> Text
warningToError warning =
  warningCodeText warning
    <> ": "
    <> warningMessage warning

parseAndLowerSource :: Maybe Text -> Text -> Either Text Expr
parseAndLowerSource preludeSource source = do
  validatePrelude preludeSource
  surfaceProgram <- parseSurfaceWithErrorCode (composeSource preludeSource source)
  pure (lowerSurfaceExpr surfaceProgram)

validatePrelude :: Maybe Text -> Either Text ()
validatePrelude preludeSource =
  case preludeSource of
    Nothing -> Right ()
    Just preludeText ->
      case parseSurfaceProgram preludeText of
        Left parseError ->
          Left ("E0002: prelude parse error: " <> parseError)
        Right _ ->
          Right ()

composeSource :: Maybe Text -> Text -> Text
composeSource preludeSource source =
  case preludeSource of
    Nothing -> source
    Just preludeText ->
      if Text.null source
        then preludeText
        else preludeText <> "\n" <> source

parseSurfaceWithErrorCode :: Text -> Either Text SurfaceExpr
parseSurfaceWithErrorCode source =
  case parseSurfaceProgram source of
    Left parseError ->
      Left ("E0001: parse error: " <> parseError)
    Right surfaceProgram ->
      Right surfaceProgram
