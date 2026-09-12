{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Compile resolved modules once against explicit dependency interfaces.
module Jazz.Compiler.ModuleCompiler
  ( analyzeProgram,
  )
where

import Control.Monad (foldM)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Jazz.Compiler.AST (CoreNode (coreNodeFacts, coreNodeSpan), CorePhase (..), DataConstructor (..), Statement (..))
import Jazz.Compiler.Analyzer.UnusedBindings (referencedScopeBindingIds)
import Jazz.Compiler.BundledPrelude (bundledPreludeIdentity)
import Jazz.Compiler.CoreIdentity (ResolvedNodeFacts (..), ResolvedReference (..))
import Jazz.Compiler.DiagnosticCatalog (WarningCategory (SameScopeRebinding))
import Jazz.Compiler.Diagnostics (CompilationDiagnostics (..), Diagnostic, compilationDiagnostics, diagnosticWarningCategory, isErrorDiagnostic, mkSameScopeRebindingWarning, promoteDiagnostic)
import Jazz.Compiler.ModuleAnalysis
  ( ImportedInterface,
    analyzeModule,
    dependencyImportInterface,
    importWholeInterface,
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
    isStandaloneSourceModule,
    mkCoreProgram,
    orderedProgramDiagnostics,
  )
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleIdentity (SourceUnitOwner (..))
import Jazz.Compiler.ModuleInterface (CompileInputs (..))
import Jazz.Compiler.Name (ResolvedNameOrigin (AmbientPrelude), identifierText)
import Jazz.Compiler.TypeInference.Result (InferenceResult (..))
import Jazz.Compiler.WarningConfig (isWarningEnabled, isWarningError)

analyzeProgram :: CompileInputs -> CoreProgram 'Resolved -> IO ([Diagnostic], Maybe (CoreProgram 'Analyzed))
analyzeProgram inputs resolvedProgram =
  {-# SCC "jazz-stage:runtime-preparation" #-}
  do
    (preludeDiagnostics, maybePrelude, ambientInterface) <- analyzePrelude (inputs {compileInputExternalUses = sourcePreludeUses}) (coreProgramPrelude resolvedProgram)
    (maybeModules, _, moduleDiagnostics) <-
      if any isErrorDiagnostic (compilationDiagnostics preludeDiagnostics) && not (any isStandaloneSourceModule (coreProgramModules resolvedProgram))
        then pure (Seq.empty, Map.empty, Seq.empty)
        else
          foldM
            (analyzeDependency ambientInterface)
            (Seq.empty, Map.empty, Seq.empty)
            (NonEmpty.toList (coreProgramModules resolvedProgram))
    let diagnostics = orderedProgramDiagnostics resolvedProgram (preludeDiagnostics : toList moduleDiagnostics)
    if any isErrorDiagnostic diagnostics
      then pure (diagnostics, Nothing)
      else case (maybePrelude, traverse id (toList maybeModules)) of
        (Just analyzedPrelude, Just (firstModule : remainingModules)) ->
          case mkCoreProgram analyzedPrelude (coreProgramEntry resolvedProgram) (firstModule NonEmpty.:| remainingModules) of
            Left failures -> fail ("analyzed program violated preserved graph invariants: " <> show failures)
            Right analyzed -> pure (diagnostics, Just analyzed)
        _ -> fail "successful analyzed program lost a prelude or module artifact"
  where
    sourcePreludeUses = Set.unions [referencedScopeBindingIds (ModuleGraph.coreModuleStatements source) | source <- NonEmpty.toList (coreProgramModules resolvedProgram), isStandaloneSourceModule source]
    analyzeDependency _ accumulated@(_, dependenciesByPath, _) resolvedModule
      | any ((`Map.notMember` dependenciesByPath) . ModuleGraph.importedModule) (coreModuleImports resolvedModule) =
          pure accumulated
    analyzeDependency ambientInterface (modules, dependenciesByPath, diagnostics) resolvedModule = do
      let importedInterface =
            ambientInterface
              <> foldMap
                (uncurry (dependencyImportInterface (ModuleGraph.resolvedModuleImportScope (coreModuleFacts resolvedModule))))
                [ (ModuleGraph.importedModule importDecl, dependency)
                | importDecl <- nubOrdOn ModuleGraph.importedModule (coreModuleImports resolvedModule),
                  Just dependency <- [Map.lookup (ModuleGraph.importedModule importDecl) dependenciesByPath]
                ]
          modulePath = coreModulePath resolvedModule
          owner = const (resolvedNodeOwner (coreNodeFacts (ModuleGraph.coreModuleBodyNode resolvedModule)))
      (inference, maybeAnalyzedModule) <-
        analyzeModule inputs owner False importedInterface resolvedModule
      let sourceDiagnostics = addPreludeRebindingWarnings resolvedModule (inferredDiagnosticGroups inference)
          withDiagnostics analyzed = analyzed {ModuleGraph.coreModuleFacts = (coreModuleFacts analyzed) {ModuleGraph.analyzedModuleDiagnosticGroups = sourceDiagnostics}}
          dependency = inferredModuleInterface inference
      pure
        ( modules Seq.|> fmap withDiagnostics maybeAnalyzedModule,
          maybe dependenciesByPath (\_ -> Map.insert modulePath dependency dependenciesByPath) maybeAnalyzedModule,
          diagnostics Seq.|> sourceDiagnostics
        )

    -- Explicit source preludes previously shared the root lexical scope. Their
    -- declaration identities retain that warning relationship across artifacts.
    addPreludeRebindingWarnings :: ModuleGraph.CoreModule 'Resolved -> CompilationDiagnostics -> CompilationDiagnostics
    addPreludeRebindingWarnings resolvedModule diagnostics
      | not (isStandaloneSourceModule resolvedModule)
          || ModuleGraph.preludeIdentity (coreProgramPrelude resolvedProgram) == bundledPreludeIdentity
          || not (isWarningEnabled settings SameScopeRebinding) =
          diagnostics
      | otherwise = diagnostics {compilationWarnings = compilationWarnings diagnostics <> map promoteWarning extraWarnings}
      where
        settings = compileInputWarningSettings inputs
        promoteWarning warning = case diagnosticWarningCategory warning of
          Just category | isWarningError settings category -> promoteDiagnostic warning
          _ -> warning
        extraWarnings =
          [ mkSameScopeRebindingWarning (identifierText name) (coreNodeSpan node) previousSpan
          | SLet node name _ <- ModuleGraph.coreModuleStatements resolvedModule,
            Just (LexicalReference previous) <- [resolvedNodeShadowedReference (coreNodeFacts node)],
            Just previousSpan <- [Map.lookup previous preludeBindingSpans]
          ]
    preludeBindingSpans =
      Map.fromList
        [ (binder, coreNodeSpan node)
        | prelude <- maybe [] (pure . ModuleGraph.coreModuleStatements) (ModuleGraph.preludeModule (coreProgramPrelude resolvedProgram)),
          node <- concatMap declarationNodes prelude,
          Just binder <- [resolvedNodeBinder (coreNodeFacts node)]
        ]
    declarationNodes (SLet node _ _) = [node]
    declarationNodes (SData _ _ _ constructors) = [node | DataConstructor node _ _ <- constructors]
    declarationNodes _ = []

analyzePrelude :: CompileInputs -> PreludeArtifact 'Resolved -> IO (CompilationDiagnostics, Maybe (PreludeArtifact 'Analyzed), ImportedInterface)
analyzePrelude inputs prelude =
  case ModuleGraph.preludeModule prelude of
    Nothing ->
      pure
        ( mempty,
          Just (ModuleGraph.PreludeArtifact (ModuleGraph.preludeIdentity prelude) Nothing),
          mempty
        )
    Just resolvedPreludeModule -> do
      (inference, maybeAnalyzedModule) <-
        analyzeModule inputs PreludeSourceUnit (ModuleGraph.preludeIdentity prelude == bundledPreludeIdentity) mempty resolvedPreludeModule
      let diagnostics = inferredDiagnosticGroups inference
          maybeAnalyzedPrelude =
            (\analyzedModule -> ModuleGraph.PreludeArtifact (ModuleGraph.preludeIdentity prelude) (Just analyzedModule))
              <$> maybeAnalyzedModule
          ambientInterface =
            importWholeInterface
              AmbientPrelude
              (inferredModuleInterface inference)
      pure
        ( diagnostics,
          if any isErrorDiagnostic (compilationDiagnostics diagnostics) then Nothing else maybeAnalyzedPrelude,
          ambientInterface
        )
