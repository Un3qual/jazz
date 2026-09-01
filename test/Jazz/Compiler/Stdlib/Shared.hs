{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Stdlib.Shared
  ( assertStdlibConstructorPrivate,
    assertSuccessfulStdlibOutput,
    runStdlibFixtureExpecting,
    runStdlibPrivateProbeValue,
    runStdlibSource,
    runStdlibSourceObserved,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST (CorePhase (..))
import Jazz.Compiler.BundledPrelude
  ( loadBundledPreludeSource,
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (E4001),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (CompilationOrigin),
    mkErrorDiagnostic,
  )
import Jazz.Compiler.Diagnostics.Render
  ( renderDiagnostic,
  )
import Jazz.Compiler.Driver
  ( RunResult,
    buildAnalyzedProgram,
    runCompileErrors,
    runModuleGraph,
    runModuleGraphObserved,
    runOutput,
    runRuntimeErrors,
  )
import Jazz.Compiler.ModuleCompiler
  ( analyzedProgramErrors,
  )
import Jazz.Compiler.ModuleExports (exportInventory)
import Jazz.Compiler.ModuleGraph
  ( CoreProgram,
    PreludeArtifact (..),
    coreModuleExpr,
    coreModulePath,
    coreProgramModules,
    coreProgramPrelude,
  )
import Jazz.Compiler.ModuleIdentity (ModulePath, mkModulePath, modulePathTextSegments)
import Jazz.Compiler.ModuleResolver
  ( ModuleResolutionConfig (..),
    resolveStandaloneExprNames,
  )
import Jazz.Compiler.Name
  ( Name (..),
    ResolvedName,
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    mkIdentifier,
  )
import Jazz.Compiler.Prelude
  ( ResolvedPrelude (PreludeBundled),
  )
import Jazz.Compiler.Runtime
  ( ModuleEvaluationMode (..),
    RuntimeCell,
    RuntimeEnv,
    RuntimeValue,
    ScopeResult (..),
    evaluateModuleScope,
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeObservationRequest,
  )
import Jazz.Compiler.RuntimeHints (projectRuntimeHints)
import Jazz.Compiler.SourceProgram
  ( parseAndLowerStandaloneSource,
    scopeStatements,
  )
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import Jazz.Repository.SourceLayout
  ( JazzSourceRole (StandardLibrarySource),
  )
import Jazz.TestHarness
  ( assertContains,
    assertEqual,
    failTest,
  )
import Jazz.TestSource
  ( readCheckedInJazzModuleSource,
    readCheckedInJazzTestFixture,
  )

runStdlibFixture :: [Text] -> FilePath -> IO RunResult
runStdlibFixture modulePath fixturePath = do
  source <- readCheckedInJazzTestFixture fixturePath
  runStdlibSource modulePath source

runStdlibFixtureExpecting :: [Text] -> FilePath -> Text -> IO ()
runStdlibFixtureExpecting modulePath fixturePath expectedOutput = do
  result <- runStdlibFixture modulePath fixturePath
  assertSuccessfulStdlibOutput expectedOutput result

assertSuccessfulStdlibOutput :: Text -> RunResult -> IO ()
assertSuccessfulStdlibOutput expectedOutput result = do
  assertEqual "compile errors" [] (runCompileErrors result)
  assertEqual "runtime errors" [] (runRuntimeErrors result)
  assertEqual "runtime output" (Just expectedOutput) (runOutput result)

assertStdlibConstructorPrivate :: [Text] -> Text -> Text -> IO ()
assertStdlibConstructorPrivate modulePath constructorName source = do
  result <- runStdlibSource modulePath source
  case runCompileErrors result of
    [] -> failTest (constructorName <> " constructor was unexpectedly public")
    diagnostics ->
      assertContains
        (constructorName <> " private-constructor diagnostic")
        ("unbound variable '" <> constructorName <> "'")
        (Text.unlines (map renderDiagnostic diagnostics))

runStdlibPrivateProbeValue :: [Text] -> Text -> IO (Either Diagnostic (Maybe RuntimeValue))
runStdlibPrivateProbeValue targetModulePath probeSource = do
  bundledPreludeSource <- loadBundledPreludeSource
  analyzedResult <-
    buildAnalyzedProgram
      defaultWarningSettings
      (PreludeBundled bundledPreludeSource)
      resolverConfig
      targetModulePath
      (readCheckedInJazzModuleSource StandardLibrarySource)
  pure $ do
    (resolvedProgram, _, maybeAnalyzedProgram) <- analyzedResult
    analyzedProgram <- maybe (Left (privateProbeDiagnostic targetModulePath)) Right maybeAnalyzedProgram
    case analyzedProgramErrors analyzedProgram of
      firstError : _ -> Left firstError
      [] -> evaluateAnalyzedPrivateProbeValue targetModulePath probeSource resolvedProgram analyzedProgram

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

evaluateAnalyzedPrivateProbeValue :: [Text] -> Text -> CoreProgram 'Resolved -> CoreProgram 'Analyzed -> Either Diagnostic (Maybe RuntimeValue)
evaluateAnalyzedPrivateProbeValue targetModulePath probeSource resolvedProgram analyzedProgram = do
  ambientEnvironment <-
    evaluateTestPrelude
      (coreProgramPrelude resolvedProgram)
  targetScope <-
    evaluateModules
      ambientEnvironment
      Nothing
      (NonEmpty.toList (coreProgramModules resolvedProgram))
  case targetScope of
    Nothing -> Left (privateProbeDiagnostic targetModulePath)
    Just environment -> do
      loweredProbe <- parseAndLowerStandaloneSource probeSource
      probeExpression <-
        case resolveStandaloneExprNames
          (preludeBuiltinMode (coreProgramPrelude resolvedProgram))
          (exportInventory [])
          loweredProbe of
          Left diagnostics -> Left (NonEmpty.head diagnostics)
          Right resolvedProbe -> Right resolvedProbe
      probeResult <-
        evaluateModuleScope
          (Just targetModulePath)
          EvaluateEntryModule
          (preludeBuiltinMode (coreProgramPrelude resolvedProgram))
          runtimeHints
          environment
          (scopeStatements probeExpression)
      pure (scopeResultValue probeResult)
  where
    runtimeHints = projectRuntimeHints analyzedProgram
    targetNominalPath = nominalModulePath targetModulePath
    evaluateModules _ targetScope [] = Right targetScope
    evaluateModules availableEnvironment targetScope (resolvedModule : rest) = do
      let modulePath = coreModulePath resolvedModule
          evaluationMode = if modulePath == targetNominalPath then EvaluateEntryModule else EvaluateDependencyModule
      scopeResult <-
        evaluateModuleScope
          (Just (NonEmpty.toList (modulePathTextSegments modulePath)))
          evaluationMode
          (preludeBuiltinMode (coreProgramPrelude resolvedProgram))
          runtimeHints
          availableEnvironment
          (scopeStatements (coreModuleExpr resolvedModule))
      let fullEnvironment = scopeResultEnvironment scopeResult
          publishedEnvironment = publishTestScope (ImportedModule modulePath) fullEnvironment
          nextAvailableEnvironment = Map.union publishedEnvironment availableEnvironment
          nextTargetScope =
            if modulePath == targetNominalPath
              then Just fullEnvironment
              else targetScope
      evaluateModules nextAvailableEnvironment nextTargetScope rest
    evaluateTestPrelude resolvedPrelude =
      case preludeModule resolvedPrelude of
        Nothing -> Right Map.empty
        Just resolvedModule -> do
          scopeResult <-
            evaluateModuleScope
              (Just (NonEmpty.toList (modulePathTextSegments (coreModulePath resolvedModule))))
              EvaluateDependencyModule
              (preludeBuiltinMode resolvedPrelude)
              runtimeHints
              Map.empty
              (scopeStatements (coreModuleExpr resolvedModule))
          pure (publishTestScope AmbientPrelude (scopeResultEnvironment scopeResult))

publishTestScope :: ResolvedNameOrigin -> RuntimeEnv -> RuntimeEnv
publishTestScope origin = Map.fromList . concatMap publishCell . Map.toList
  where
    publishCell :: (ResolvedName, RuntimeCell) -> [(ResolvedName, RuntimeCell)]
    publishCell (name, cell) =
      case name of
        UserName (ResolvedUserName CurrentModule namespace identifier) ->
          [(UserName (ResolvedUserName origin namespace identifier), cell)]
        UserName (ResolvedUserName AmbientPrelude namespace identifier)
          | origin == AmbientPrelude ->
              [(UserName (ResolvedUserName AmbientPrelude namespace identifier), cell)]
        _ -> []

nominalModulePath :: [Text] -> ModulePath
nominalModulePath path =
  case NonEmpty.nonEmpty path of
    Just segments -> mkModulePath (fmap mkIdentifier segments)
    Nothing -> error "stdlib fixture module path cannot be empty"

privateProbeDiagnostic :: [Text] -> Diagnostic
privateProbeDiagnostic modulePath =
  mkErrorDiagnostic
    E4001
    CompilationOrigin
    ("private stdlib probe could not retain module scope '" <> Text.intercalate "::" modulePath <> "'")
