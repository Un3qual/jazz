{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compile resolved modules once against explicit dependency interfaces.
module Jazz.Compiler.ModuleCompiler
  ( analyzeProgram,
    analyzedProgramDiagnostics,
    analyzedProgramErrors,
  )
where

import Control.Monad (foldM)
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Jazz.Compiler.AST (CorePhase (..))
import Jazz.Compiler.Diagnostics (Diagnostic, isErrorDiagnostic)
import Jazz.Compiler.ModuleAnalysis
  ( ImportedInterface,
    analyzeModule,
    dependencyImportInterface,
    importWholeInterface,
    moduleBinderInventory,
    moduleEvidenceCandidates,
  )
import Jazz.Compiler.ModuleGraph
  ( CoreProgram,
    PreludeArtifact,
    coreModuleFacts,
    coreModuleImports,
    coreModulePath,
    coreProgramEntry,
    coreProgramModules,
    coreProgramPrelude,
    mkCoreProgram,
  )
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleIdentity (SourceUnitOwner (..))
import Jazz.Compiler.ModuleInterface (CompileInputs (..))
import Jazz.Compiler.Name (ResolvedNameOrigin (AmbientPrelude, ImportedModule))
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))

analyzedProgramDiagnostics :: CoreProgram 'Analyzed -> [Diagnostic]
analyzedProgramDiagnostics program =
  preludeDiagnostics <> foldMap moduleDiagnostics (coreProgramModules program)
  where
    preludeDiagnostics = maybe [] moduleDiagnostics (ModuleGraph.preludeModule (coreProgramPrelude program))
    moduleDiagnostics = ModuleGraph.analyzedModuleDiagnostics . coreModuleFacts

analyzedProgramErrors :: CoreProgram 'Analyzed -> [Diagnostic]
analyzedProgramErrors = filter isErrorDiagnostic . analyzedProgramDiagnostics

analyzeProgram :: CompileInputs -> CoreProgram 'Resolved -> IO ([Diagnostic], Maybe (CoreProgram 'Analyzed))
analyzeProgram inputs resolvedProgram =
  {-# SCC "jazz-stage:runtime-preparation" #-}
  do
    (preludeDiagnostics, maybePrelude, ambientInterface) <- analyzePrelude inputs (coreProgramPrelude resolvedProgram)
    (maybeModules, _, moduleDiagnostics) <-
      if any isErrorDiagnostic preludeDiagnostics
        then pure (Seq.empty, Map.empty, Seq.empty)
        else
          foldM
            (analyzeDependency ambientInterface)
            (Seq.empty, Map.empty, Seq.empty)
            (NonEmpty.toList (coreProgramModules resolvedProgram))
    let diagnostics = preludeDiagnostics <> toList moduleDiagnostics
    if any isErrorDiagnostic diagnostics
      then pure (diagnostics, Nothing)
      else case (maybePrelude, traverse id (toList maybeModules)) of
        (Just analyzedPrelude, Just (firstModule : remainingModules)) ->
          case mkCoreProgram analyzedPrelude (coreProgramEntry resolvedProgram) (firstModule NonEmpty.:| remainingModules) of
            Left failures -> fail ("analyzed program violated preserved graph invariants: " <> show failures)
            Right analyzed -> pure (diagnostics, Just analyzed)
        _ -> fail "successful analyzed program lost a prelude or module artifact"
  where
    analyzeDependency _ accumulated@(_, dependenciesByPath, _) resolvedModule
      | any ((`Map.notMember` dependenciesByPath) . ModuleGraph.importedModule) (coreModuleImports resolvedModule) =
          pure accumulated
    analyzeDependency ambientInterface (modules, dependenciesByPath, diagnostics) resolvedModule = do
      let explicitImports =
            foldMap
              (uncurry dependencyImportInterface)
              [ (importDecl, dependency)
              | importDecl <- coreModuleImports resolvedModule,
                Just dependency <- [Map.lookup (ModuleGraph.importedModule importDecl) dependenciesByPath]
              ]
          importedInterface = ambientInterface <> explicitImports
          modulePath = coreModulePath resolvedModule
      (inference, maybeAnalyzedModule) <-
        analyzeModule inputs NamedSourceUnit Set.empty importedInterface resolvedModule
      let dependency analyzedModule =
            ( ModuleGraph.resolvedModuleExports (coreModuleFacts resolvedModule),
              importWholeInterface
                (ImportedModule modulePath)
                (moduleBinderInventory analyzedModule)
                (moduleEvidenceCandidates NamedSourceUnit resolvedModule)
                (inferredModuleInterface inference)
                <> explicitImports
            )
      pure
        ( modules Seq.|> maybeAnalyzedModule,
          maybe dependenciesByPath (\analyzedModule -> Map.insert modulePath (dependency analyzedModule) dependenciesByPath) maybeAnalyzedModule,
          diagnostics <> Seq.fromList (inferredDiagnostics inference)
        )

analyzePrelude :: CompileInputs -> PreludeArtifact 'Resolved -> IO ([Diagnostic], Maybe (PreludeArtifact 'Analyzed), ImportedInterface)
analyzePrelude inputs prelude =
  case ModuleGraph.preludeModule prelude of
    Nothing ->
      pure
        ( [],
          Just (ModuleGraph.PreludeArtifact (ModuleGraph.preludeIdentity prelude) Nothing),
          mempty
        )
    Just resolvedPreludeModule -> do
      (inference, maybeAnalyzedModule) <-
        analyzeModule inputs PreludeSourceUnit (compileInputPreludeHiddenStatementIndices inputs) mempty resolvedPreludeModule
      let diagnostics = inferredDiagnostics inference
          maybeAnalyzedPrelude =
            (\analyzedModule -> ModuleGraph.PreludeArtifact (ModuleGraph.preludeIdentity prelude) (Just analyzedModule))
              <$> maybeAnalyzedModule
          ambientInterface =
            importWholeInterface
              AmbientPrelude
              (maybe Map.empty moduleBinderInventory maybeAnalyzedModule)
              (moduleEvidenceCandidates PreludeSourceUnit resolvedPreludeModule)
              (inferredModuleInterface inference)
      pure
        ( diagnostics,
          if any isErrorDiagnostic diagnostics then Nothing else maybeAnalyzedPrelude,
          ambientInterface
        )
