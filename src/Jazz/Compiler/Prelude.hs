{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Explicit prelude preparation boundary shared by standalone and module flows.
module Jazz.Compiler.Prelude
  ( PreparedPrelude (..),
    ResolvedPrelude (..),
    preparedPreludeExpr,
    preparePrelude,
    resolvedExplicitPrelude,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CorePhase (..),
    DataConstructor (..),
    Expr (..),
    Statement (..),
  )
import Jazz.Compiler.BundledPrelude (bundledPreludeIdentity)
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic (..),
    DiagnosticLabel (..),
    SourceSpan (..),
    prependDiagnosticSummary,
    setDiagnosticErrorCode,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    exportInventory,
  )
import Jazz.Compiler.ModuleGraph
  ( CoreModule (..),
    PreludeArtifact (..),
    coreModuleExpr,
  )
import Jazz.Compiler.ModuleIdentity
  ( ModuleIdentity,
    mkSourceFile,
    moduleIdentity,
    preludeModulePath,
  )
import Jazz.Compiler.Name
  ( NameNamespace (..),
    renderName,
  )
import Jazz.Compiler.Parser (parseSurfaceProgram)
import Jazz.Compiler.Parser.Lower (lowerSurfaceModule)
import Jazz.Compiler.PreludeContract (validatePreludeKernelBridges)

data ResolvedPrelude
  = PreludeAbsent
  | PreludeBundled Text
  | PreludeExplicit Text
  deriving (Eq, Show)

data PreparedPrelude = PreparedPrelude
  { preparedPreludeArtifact :: PreludeArtifact 'Lowered,
    preparedPreludeHiddenStatementIndices :: Set Int,
    preparedPreludeVisibleExports :: ModuleExportInventory
  }
  deriving (Eq, Show)

preparedPreludeExpr :: PreparedPrelude -> Maybe (Expr 'Lowered)
preparedPreludeExpr = fmap coreModuleExpr . preludeModule . preparedPreludeArtifact

preparePrelude :: ResolvedPrelude -> Either Diagnostic PreparedPrelude
preparePrelude resolvedPrelude =
  case resolvedPrelude of
    PreludeAbsent ->
      Right
        PreparedPrelude
          { preparedPreludeArtifact = preludeArtifact absentPreludeIdentity Nothing,
            preparedPreludeHiddenStatementIndices = Set.empty,
            preparedPreludeVisibleExports = exportInventory []
          }
    PreludeBundled source -> prepare bundledPreludeIdentity True source
    PreludeExplicit source -> prepare explicitPreludeIdentity False source
  where
    prepare identity hidden source = do
      loweredPrelude <- validateAndLowerPrelude identity source
      let statements = coreModuleStatements loweredPrelude
      pure
        PreparedPrelude
          { preparedPreludeArtifact = preludeArtifact identity (Just loweredPrelude),
            preparedPreludeHiddenStatementIndices =
              if hidden
                then Set.fromList [0 .. length statements - 1]
                else Set.empty,
            preparedPreludeVisibleExports = collectPreludeExports loweredPrelude
          }

    preludeArtifact identity maybeModule =
      PreludeArtifact
        { preludeIdentity = identity,
          preludeModule = maybeModule
        }

resolvedExplicitPrelude :: Maybe Text -> ResolvedPrelude
resolvedExplicitPrelude maybePrelude =
  case maybePrelude of
    Nothing -> PreludeAbsent
    Just preludeText -> PreludeExplicit preludeText

absentPreludeIdentity :: ModuleIdentity
absentPreludeIdentity = syntheticPreludeIdentity "<absent-prelude>"

explicitPreludeIdentity :: ModuleIdentity
explicitPreludeIdentity = syntheticPreludeIdentity "<explicit-prelude>"

syntheticPreludeIdentity :: FilePath -> ModuleIdentity
syntheticPreludeIdentity sourcePath =
  moduleIdentity
    preludeModulePath
    (mkSourceFile sourcePath)

validateAndLowerPrelude :: ModuleIdentity -> Text -> Either Diagnostic (CoreModule 'Lowered)
validateAndLowerPrelude identity preludeText =
  case parseSurfaceProgram preludeText of
    Left parseError ->
      Left (setDiagnosticErrorCode E0002 (prependDiagnosticSummary "prelude parse error: " parseError))
    Right preludeSurfaceExpr -> do
      loweredPrelude <- lowerSurfaceModule identity preludeSurfaceExpr
      case map unqualifyDiagnosticSpans (validatePreludeKernelBridges (coreModuleExpr loweredPrelude)) of
        [] -> Right loweredPrelude
        firstValidationError : _ -> Left firstValidationError

unqualifyDiagnosticSpans :: Diagnostic -> Diagnostic
unqualifyDiagnosticSpans diagnostic =
  diagnostic
    { diagnosticPrimaryLabel = unqualifyLabel <$> diagnosticPrimaryLabel diagnostic,
      diagnosticSecondaryLabels = map unqualifyLabel (diagnosticSecondaryLabels diagnostic)
    }
  where
    unqualifyLabel label = label {labelSpan = unqualifySpan (labelSpan label)}
    unqualifySpan spanValue = SourceSpan (spanLine spanValue) (spanColumn spanValue)

collectPreludeExports :: CoreModule 'Lowered -> ModuleExportInventory
collectPreludeExports coreModule =
  exportInventory (concatMap statementExports (coreModuleStatements coreModule))
  where
    statementExports :: Statement 'Lowered -> [ModuleExport]
    statementExports statement =
      case statement of
        SLet _ name _ ->
          [ModuleExport ValueNamespace (renderName name)]
        SData _ typeName _ constructors ->
          ModuleExport TypeNamespace (renderName typeName)
            : [ ModuleExport ConstructorNamespace (renderName name)
              | DataConstructor _ name _ <- constructors
              ]
        SClass _ className _ _ ->
          [ModuleExport CapabilityNamespace (renderName className)]
        _ -> []
