{-# LANGUAGE OverloadedStrings #-}

-- | Explicit prelude preparation boundary shared by standalone and module flows.
module Jazz.Compiler.Prelude
  ( PreparedPrelude (..),
    ResolvedPrelude (..),
    preparePrelude,
    resolvedExplicitPrelude
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( DataConstructor (..),
    Expr (..),
    Statement (..)
  )
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly))
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    prependDiagnosticSummary,
    setDiagnosticErrorCode
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..)
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventory
  )
import Jazz.Compiler.Name
  ( NameNamespace (..),
    renderName
  )
import Jazz.Compiler.Parser (parseSurfaceProgram)
import Jazz.Compiler.Parser.Lower (lowerSurfaceExpr)
import Jazz.Compiler.PreludeContract (validatePreludeKernelBridges)
import Jazz.Compiler.SourceProgram (scopeStatements)

data ResolvedPrelude
  = PreludeAbsent
  | PreludeBundled Text
  | PreludeExplicit Text
  deriving (Eq, Show)

data PreparedPrelude = PreparedPrelude
  { preparedPreludeExpr :: Maybe Expr,
    preparedPreludeHiddenStatementIndices :: Set Int,
    preparedPreludeVisibleExports :: ModuleExportInventory,
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
            preparedPreludeVisibleExports = exportInventory [],
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
            preparedPreludeVisibleExports = collectPreludeExports loweredPrelude,
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

collectPreludeExports :: Expr -> ModuleExportInventory
collectPreludeExports expression =
  exportInventory $
    case expression of
      EBlock statements -> concatMap statementExports statements
      _ -> []
  where
    statementExports statement =
      case statement of
        SLet name _ _ ->
          [ModuleExport ValueNamespace (renderName name)]
        SData _ typeName _ constructors ->
          ModuleExport TypeNamespace (renderName typeName)
            : [ ModuleExport ConstructorNamespace (renderName name)
                | DataConstructor name _ <- constructors
              ]
        SClass _ className _ _ ->
          [ModuleExport CapabilityNamespace (renderName className)]
        _ -> []
