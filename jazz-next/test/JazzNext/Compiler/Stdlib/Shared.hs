{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Stdlib.Shared
  ( runStdlibFixture,
    runStdlibPrivateProbe,
    runStdlibSource,
    runStdlibSourceObserved,
  )
where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.BundledPrelude
  ( loadBundledPreludeSource,
  )
import JazzNext.Compiler.DiagnosticCatalog
  ( ErrorCode (E4001),
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (CompilationOrigin),
    mkErrorDiagnostic,
  )
import JazzNext.Compiler.Driver
  ( RunResult,
    buildCompiledProgram,
    runModuleGraph,
    runModuleGraphObserved,
  )
import JazzNext.Compiler.ModuleGraph
  ( ResolvedModule (resolvedModulePath),
  )
import JazzNext.Compiler.ModuleInterface
  ( CompiledModule (..),
    CompiledPrelude (..),
    CompiledProgram (..),
    ModuleInterface (interfaceRuntimeHints),
    compiledProgramErrors,
  )
import JazzNext.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
  )
import JazzNext.Compiler.Name
  ( IdentifierLike (identifierText),
    Name (..),
    NameNamespace (..),
    ResolvedNameOrigin (..),
    mkQualifiedIdentifier,
    sourceName,
  )
import JazzNext.Compiler.Prelude
  ( ResolvedPrelude (PreludeBundled),
  )
import JazzNext.Compiler.Runtime
  ( ModuleEvaluationMode (..),
    RuntimeCell,
    RuntimeEnv,
    ScopeResult (..),
    evaluateModuleScope,
    renderRuntimeValue,
  )
import JazzNext.Compiler.Runtime.Observation
  ( RuntimeObservationRequest,
  )
import JazzNext.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import JazzNext.Compiler.SourceProgram
  ( parseAndLowerStandaloneSource,
    scopeStatements,
  )
import JazzNext.Repository.SourceLayout
  ( JazzSourceRole (StandardLibrarySource),
  )
import JazzNext.TestSource
  ( readCheckedInJazzModuleSource,
    readCheckedInJazzTestFixture,
  )

runStdlibFixture :: [Text] -> FilePath -> IO RunResult
runStdlibFixture modulePath fixturePath = do
  source <- readCheckedInJazzTestFixture fixturePath
  runStdlibSource modulePath source

runStdlibPrivateProbe :: [Text] -> Text -> IO (Either Diagnostic (Maybe Text))
runStdlibPrivateProbe targetModulePath probeSource = do
  bundledPreludeSource <- loadBundledPreludeSource
  compiledResult <-
    buildCompiledProgram
      defaultWarningSettings
      (PreludeBundled bundledPreludeSource)
      resolverConfig
      targetModulePath
      (readCheckedInJazzModuleSource StandardLibrarySource)
  pure $ do
    compiledProgram <- compiledResult
    case compiledProgramErrors compiledProgram of
      firstError : _ -> Left firstError
      [] -> evaluateCompiledPrivateProbe targetModulePath probeSource compiledProgram

runStdlibSource :: [Text] -> Text -> IO RunResult
runStdlibSource modulePath entrySource =
  runModuleGraph
    defaultWarningSettings
    resolverConfig
    modulePath
    lookupSource
  where
    entryPath = "src/" <> modulePathFile modulePath <> ".jz"

    lookupSource path
      | path == entryPath = pure (Just entrySource)
      | otherwise = readCheckedInJazzModuleSource StandardLibrarySource path

runStdlibSourceObserved :: RuntimeObservationRequest -> [Text] -> Text -> IO RunResult
runStdlibSourceObserved observationRequest modulePath entrySource =
  runModuleGraphObserved
    observationRequest
    defaultWarningSettings
    resolverConfig
    modulePath
    lookupSource
  where
    entryPath = "src/" <> modulePathFile modulePath <> ".jz"

    lookupSource path
      | path == entryPath = pure (Just entrySource)
      | otherwise = readCheckedInJazzModuleSource StandardLibrarySource path

resolverConfig :: ModuleResolutionConfig
resolverConfig =
  ModuleResolutionConfig
    { moduleRoots = ["src"],
      moduleExtension = ".jz"
    }

modulePathFile :: [Text] -> FilePath
modulePathFile =
  foldr1 (\segment suffix -> segment <> "/" <> suffix) . map Text.unpack

evaluateCompiledPrivateProbe :: [Text] -> Text -> CompiledProgram -> Either Diagnostic (Maybe Text)
evaluateCompiledPrivateProbe targetModulePath probeSource compiledProgram = do
  ambientEnvironment <- evaluateTestPrelude (compiledProgramPrelude compiledProgram)
  targetScope <- evaluateModules ambientEnvironment Nothing (compiledProgramModules compiledProgram)
  case targetScope of
    Nothing -> Left (privateProbeDiagnostic targetModulePath)
    Just (compiledModule, environment) -> do
      probeExpression <- parseAndLowerStandaloneSource probeSource
      probeResult <-
        evaluateModuleScope
          (Just targetModulePath)
          EvaluateEntryModule
          (compiledPreludeBuiltinMode (compiledProgramPrelude compiledProgram))
          (interfaceRuntimeHints (compiledModuleInterface compiledModule))
          (withSourceAliases environment)
          (scopeStatements probeExpression)
      pure (renderRuntimeValue <$> scopeResultValue probeResult)
  where
    evaluateModules _ targetScope [] = Right targetScope
    evaluateModules availableEnvironment targetScope (compiledModule : rest) = do
      let modulePath = resolvedModulePath (compiledResolvedModule compiledModule)
          evaluationMode = if modulePath == targetModulePath then EvaluateEntryModule else EvaluateDependencyModule
      scopeResult <-
        evaluateModuleScope
          (Just modulePath)
          evaluationMode
          (compiledPreludeBuiltinMode (compiledProgramPrelude compiledProgram))
          (interfaceRuntimeHints (compiledModuleInterface compiledModule))
          availableEnvironment
          (scopeStatements (compiledModuleExpr compiledModule))
      let fullEnvironment = scopeResultEnvironment scopeResult
          publishedEnvironment = publishTestScope (ImportedModule modulePath) fullEnvironment
          nextAvailableEnvironment = Map.union publishedEnvironment availableEnvironment
          nextTargetScope =
            if modulePath == targetModulePath
              then Just (compiledModule, fullEnvironment)
              else targetScope
      evaluateModules nextAvailableEnvironment nextTargetScope rest

evaluateTestPrelude :: CompiledPrelude -> Either Diagnostic RuntimeEnv
evaluateTestPrelude compiledPrelude =
  case compiledPreludeExpr compiledPrelude of
    Nothing -> Right Map.empty
    Just expression -> do
      scopeResult <-
        evaluateModuleScope
          (Just [])
          EvaluateDependencyModule
          (compiledPreludeBuiltinMode compiledPrelude)
          (compiledPreludeRuntimeHints compiledPrelude)
          Map.empty
          (scopeStatements expression)
      pure (publishTestScope AmbientPrelude (scopeResultEnvironment scopeResult))

publishTestScope :: ResolvedNameOrigin -> RuntimeEnv -> RuntimeEnv
publishTestScope origin = Map.fromList . concatMap publishCell . Map.toList
  where
    publishCell :: (Name, RuntimeCell) -> [(Name, RuntimeCell)]
    publishCell (name, cell) =
      case name of
        SourceName identifier ->
          [ (ResolvedName origin namespace identifier, cell)
            | namespace <- [ValueNamespace, ConstructorNamespace, TypeNamespace, CapabilityNamespace]
          ]
        QualifiedName qualifier member ->
          [ ( ResolvedName
                origin
                ValueNamespace
                (mkQualifiedIdentifier (identifierText qualifier) (identifierText member)),
              cell
            )
          ]
        ResolvedName CurrentModule namespace identifier ->
          [(ResolvedName origin namespace identifier, cell)]
        ResolvedName AmbientPrelude namespace identifier
          | origin == AmbientPrelude -> [(ResolvedName AmbientPrelude namespace identifier, cell)]
        _ -> []

withSourceAliases :: RuntimeEnv -> RuntimeEnv
withSourceAliases environment = Map.union aliases environment
  where
    aliases =
      Map.fromList
        [ (sourceName identifier, cell)
          | (ResolvedName CurrentModule _ identifier, cell) <- Map.toList environment
        ]

privateProbeDiagnostic :: [Text] -> Diagnostic
privateProbeDiagnostic modulePath =
  mkErrorDiagnostic
    E4001
    CompilationOrigin
    ("private stdlib probe could not retain module scope '" <> Text.intercalate "::" modulePath <> "'")
