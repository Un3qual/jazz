{-# LANGUAGE OverloadedStrings #-}

-- | Explicit prelude preparation boundary shared by standalone and module flows.
module JazzNext.Compiler.Prelude
  ( PreparedPrelude (..),
    ResolvedPrelude (..),
    preparePrelude,
    resolvedExplicitPrelude
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.AST
  ( DataConstructor (..),
    Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    prependDiagnosticSummary,
    setDiagnosticErrorCode
  )
import JazzNext.Compiler.DiagnosticCatalog
  ( ErrorCode (..)
  )
import JazzNext.Compiler.Name (Name)
import JazzNext.Compiler.Parser (parseSurfaceProgram)
import JazzNext.Compiler.Parser.Lower (lowerSurfaceExpr)
import JazzNext.Compiler.PreludeContract (validatePreludeKernelBridges)
import JazzNext.Compiler.SourceProgram (scopeStatements)

data ResolvedPrelude
  = PreludeAbsent
  | PreludeBundled Text
  | PreludeExplicit Text
  deriving (Eq, Show)

data PreparedPrelude = PreparedPrelude
  { preparedPreludeExpr :: Maybe Expr,
    preparedPreludeHiddenStatementIndices :: Set Int,
    preparedPreludeVisibleValues :: Set Name,
    preparedPreludeVisibleClasses :: Set Name,
    preparedPreludeBuiltinMode :: BuiltinResolutionMode
  }
  deriving (Eq, Show)

preparePrelude :: ResolvedPrelude -> Either Diagnostic PreparedPrelude
preparePrelude resolvedPrelude =
  case resolvedPrelude of
    PreludeAbsent ->
      Right
        PreparedPrelude
          { preparedPreludeExpr = Nothing,
            preparedPreludeHiddenStatementIndices = Set.empty,
            preparedPreludeVisibleValues = Set.empty,
            preparedPreludeVisibleClasses = Set.empty,
            preparedPreludeBuiltinMode = ResolveKernelOnly
          }
    PreludeBundled source -> prepare True source
    PreludeExplicit source -> prepare False source
  where
    prepare hidden source = do
      loweredPrelude <- validateAndLowerPrelude source
      let statements = scopeStatements loweredPrelude
      pure
        PreparedPrelude
          { preparedPreludeExpr = Just loweredPrelude,
            preparedPreludeHiddenStatementIndices =
              if hidden
                then Set.fromList [0 .. length statements - 1]
                else Set.empty,
            preparedPreludeVisibleValues = collectPreludeValues loweredPrelude,
            preparedPreludeVisibleClasses = collectPreludeClasses loweredPrelude,
            preparedPreludeBuiltinMode = ResolveKernelOnly
          }

resolvedExplicitPrelude :: Maybe Text -> ResolvedPrelude
resolvedExplicitPrelude maybePrelude =
  case maybePrelude of
    Nothing -> PreludeAbsent
    Just preludeText -> PreludeExplicit preludeText

validateAndLowerPrelude :: Text -> Either Diagnostic Expr
validateAndLowerPrelude preludeText =
  case parseSurfaceProgram preludeText of
    Left parseError ->
      Left (setDiagnosticErrorCode E0002 (prependDiagnosticSummary "prelude parse error: " parseError))
    Right preludeSurfaceExpr ->
      let loweredPrelude = lowerSurfaceExpr preludeSurfaceExpr
       in case validatePreludeKernelBridges loweredPrelude of
            [] -> Right loweredPrelude
            firstValidationError : _ -> Left firstValidationError

collectPreludeValues :: Expr -> Set Name
collectPreludeValues expression =
  case expression of
    EBlock statements -> Set.fromList (concatMap statementValues statements)
    _ -> Set.empty
  where
    statementValues statement =
      case statement of
        SLet name _ _ -> [name]
        SData _ typeName _ constructors -> typeName : [name | DataConstructor name _ <- constructors]
        _ -> []

collectPreludeClasses :: Expr -> Set Name
collectPreludeClasses expression =
  case expression of
    EBlock statements ->
      Set.fromList
        [ className
          | SClass _ className _ _ <- statements
        ]
    _ -> Set.empty
