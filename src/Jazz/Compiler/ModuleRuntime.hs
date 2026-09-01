{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Evaluate a successful compiled program once in dependency order.
module Jazz.Compiler.ModuleRuntime
  ( RuntimeExport (..),
    RuntimeModule (..),
    RuntimeProgram (..),
    evaluateCompiledProgram,
    evaluateCompiledProgramObserved,
    evaluateCompiledProgramWithHost,
    evaluateCompiledProgramWithHostObserved,
    lookupRuntimeModule,
  )
where

import Control.Monad.Trans.Except
  ( ExceptT (..),
    runExceptT,
  )
import Data.Foldable (toList)
import Data.Functor.Identity (runIdentity)
import Data.List (find)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CorePhase (..),
    Expr (EBlock),
    Statement,
  )
import Jazz.Compiler.CapabilityFacts (splitQualifiedMethodKey)
import Jazz.Compiler.Diagnostics (Diagnostic)
import Jazz.Compiler.ModuleCompiler
  ( CompiledModule,
    CompiledProgram,
    compiledModuleExportInventory,
    compiledModuleExpr,
    compiledModuleImports,
    compiledModuleInterface,
    compiledModulePath,
    compiledProgramEntryPath,
    compiledProgramModules,
    compiledProgramPrelude,
    compiledProgramPreludePath,
    firstCompiledProgramError,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (..),
    ModuleExportInventory,
    ModuleImportMode (..),
    exportInventoryEntries,
    exportNamesInNamespace,
    inventoryHasExport,
    visibleImportInventory,
  )
import Jazz.Compiler.ModuleGraph (ImportExposure (..), ModuleImport)
import qualified Jazz.Compiler.ModuleGraph as ModuleGraph
import Jazz.Compiler.ModuleIdentity (ModulePath, modulePathTextSegments)
import Jazz.Compiler.ModuleInterface
  ( CompiledPrelude (..),
    ModuleInterface (..),
    moduleInterfaceExportInventory,
  )
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (..),
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    identifierText,
    mkIdentifier,
    renderName,
    resolvedAmbientName,
  )
import Jazz.Compiler.Runtime
  ( ModuleEvaluationMode (..),
    RuntimeCell,
    RuntimeEnv,
    RuntimeHostEvaluationT,
    RuntimeValue,
    ScopeResult (..),
    evaluateModuleScope,
    evaluateModuleScopeWithRequiredEvaluationHostControl,
    runRuntimeHostEvaluation,
    runRuntimeHostEvaluationWithObservation,
    runtimeExprRequiresHost,
  )
import Jazz.Compiler.Runtime.Observation
  ( RuntimeObservationRequest (..),
    RuntimeObservationResult (..),
    finishRuntimeObservationResult,
  )
import Jazz.Compiler.Runtime.Outcome
  ( RuntimeControl,
    RuntimeOutcome (..),
    diagnosticResultOutcome,
    runtimeControlOutcome,
    runtimeOutcomeAsDiagnosticResult,
  )
import Jazz.Compiler.RuntimeHost
  ( RuntimeHost,
    disabledRuntimeHost,
  )

-- | Runtime-facing exports keep capability methods structurally distinct from
-- ordinary values instead of encoding their owner in a value-name string.
data RuntimeExport
  = RuntimeBindingExport ModuleExport
  | RuntimeCapabilityMethodExport
      { runtimeExportCapabilityName :: Text,
        runtimeExportMethodName :: Text
      }
  deriving (Eq, Ord, Show)

data RuntimeModule = RuntimeModule
  { runtimeModulePath :: [Text],
    runtimeModuleExports :: Map RuntimeExport RuntimeCell
  }

data RuntimeProgram = RuntimeProgram
  { runtimeProgramModules :: [RuntimeModule],
    runtimeProgramOutput :: Maybe RuntimeValue
  }

data RuntimeModuleAccumulator = RuntimeModuleAccumulator
  { accumulatedRuntimeModules :: !(Seq.Seq RuntimeModule),
    accumulatedRuntimeModulesByPath :: !(Map ModulePath RuntimeModule)
  }

data PreparedModuleEvaluation = PreparedModuleEvaluation
  { preparedModulePath :: ModulePath,
    preparedModuleEvaluationMode :: ModuleEvaluationMode,
    preparedModuleImportedEnvironment :: RuntimeEnv
  }

lookupRuntimeModule :: [Text] -> RuntimeProgram -> Maybe RuntimeModule
lookupRuntimeModule modulePath =
  find ((== modulePath) . runtimeModulePath) . runtimeProgramModules

evaluateCompiledProgram :: CompiledProgram -> Either Diagnostic RuntimeProgram
evaluateCompiledProgram =
  runtimeOutcomeAsDiagnosticResult
    . runtimeObservationOutcome
    . evaluateCompiledProgramObserved RuntimeObservationDisabled

evaluateCompiledProgramObserved :: RuntimeObservationRequest -> CompiledProgram -> RuntimeObservationResult RuntimeProgram
evaluateCompiledProgramObserved observationRequest compiledProgram =
  runIdentity
    (evaluateCompiledProgramWithHostObserved observationRequest disabledRuntimeHost compiledProgram)

evaluateCompiledProgramPureUnchecked :: CompiledProgram -> Either Diagnostic RuntimeProgram
evaluateCompiledProgramPureUnchecked compiledProgram = do
  ambientEnv <- evaluatePrelude (compiledProgramPreludePath compiledProgram) (compiledProgramPrelude compiledProgram)
  evaluateModules compiledModulesByPath ambientEnv emptyRuntimeModuleAccumulator Nothing (compiledProgramModules compiledProgram)
  where
    entryPath = compiledProgramEntryPath compiledProgram
    compiledModulesByPath = buildCompiledModulePathIndex compiledProgram

    evaluateModules compiledModules ambientEnv runtimeModules output remainingModules =
      case remainingModules of
        [] ->
          Right
            (finishRuntimeProgram runtimeModules output)
        compiledModule : rest -> do
          let preparedModule =
                prepareModuleEvaluation entryPath compiledModules ambientEnv runtimeModules compiledModule
          scopeResult <-
            evaluateModuleScope
              (Just (modulePathTexts (preparedModulePath preparedModule)))
              (preparedModuleEvaluationMode preparedModule)
              (compiledPreludeBuiltinMode (compiledProgramPrelude compiledProgram))
              (interfaceRuntimeHints (compiledModuleInterface compiledModule))
              (preparedModuleImportedEnvironment preparedModule)
              (scopeStatements (compiledModuleExpr compiledModule))
          let (nextRuntimeModules, nextOutput) =
                completeModuleEvaluation preparedModule compiledModule scopeResult runtimeModules output
          evaluateModules compiledModules ambientEnv nextRuntimeModules nextOutput rest

evaluatePrelude :: ModulePath -> CompiledPrelude -> Either Diagnostic RuntimeEnv
evaluatePrelude preludePath compiledPrelude =
  case compiledPreludeExpr compiledPrelude of
    Nothing -> Right Map.empty
    Just expression -> do
      scopeResult <-
        evaluateModuleScope
          (Just (modulePathTexts preludePath))
          EvaluateDependencyModule
          (compiledPreludeBuiltinMode compiledPrelude)
          (compiledPreludeRuntimeHints compiledPrelude)
          Map.empty
          (scopeStatements expression)
      pure
        ( publishEnvironment
            AmbientPrelude
            (moduleInterfaceExportInventory (compiledPreludeInterface compiledPrelude))
            (compiledPreludeInterface compiledPrelude)
            (scopeResultEnvironment scopeResult)
        )

evaluateCompiledProgramWithHost ::
  (Monad m) =>
  RuntimeHost m ->
  CompiledProgram ->
  m (Either Diagnostic RuntimeProgram)
evaluateCompiledProgramWithHost host compiledProgram =
  runtimeOutcomeAsDiagnosticResult . runtimeObservationOutcome
    <$> evaluateCompiledProgramWithHostObserved RuntimeObservationDisabled host compiledProgram

evaluateCompiledProgramWithHostObserved ::
  (Monad m) =>
  RuntimeObservationRequest ->
  RuntimeHost m ->
  CompiledProgram ->
  m (RuntimeObservationResult RuntimeProgram)
evaluateCompiledProgramWithHostObserved observationRequest host compiledProgram =
  {-# SCC "jazz-stage:evaluation" #-}
  case firstCompiledProgramError compiledProgram of
    Just firstError -> pure (RuntimeObservationResult (RuntimeOutcomeFailed firstError) Nothing)
    Nothing ->
      case observationRequest of
        RuntimeObservationDisabled -> do
          outcome <- evaluateCompiledProgramWithHostUnobserved host compiledProgram
          pure (RuntimeObservationResult outcome Nothing)
        _ -> do
          (outcome, observationState) <-
            runRuntimeHostEvaluationWithObservation observationRequest host $ \evaluationHost ->
              evaluateCompiledProgramWithEvaluationHostUnchecked evaluationHost compiledProgram
          pure (finishRuntimeObservationResult (runtimeControlOutcome outcome) observationState)

evaluateCompiledProgramWithHostUnobserved ::
  (Monad m) =>
  RuntimeHost m ->
  CompiledProgram ->
  m (RuntimeOutcome RuntimeProgram)
evaluateCompiledProgramWithHostUnobserved host compiledProgram =
  if compiledProgramRequiresHost compiledProgram
    then
      runtimeControlOutcome
        <$> runRuntimeHostEvaluation
          host
          ( \evaluationHost ->
              evaluateCompiledProgramWithEvaluationHostUnchecked evaluationHost compiledProgram
          )
    else pure (diagnosticResultOutcome (evaluateCompiledProgramPureUnchecked compiledProgram))

evaluateCompiledProgramWithEvaluationHostUnchecked ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  CompiledProgram ->
  RuntimeHostEvaluationT m (Either RuntimeControl RuntimeProgram)
evaluateCompiledProgramWithEvaluationHostUnchecked evaluationHost compiledProgram =
  runExceptT $ do
    ambientEnv <-
      ExceptT
        ( evaluatePreludeWithEvaluationHost
            evaluationHost
            (compiledProgramPreludePath compiledProgram)
            (compiledProgramPrelude compiledProgram)
        )
    evaluateModules compiledModulesByPath ambientEnv emptyRuntimeModuleAccumulator Nothing (compiledProgramModules compiledProgram)
  where
    entryPath = compiledProgramEntryPath compiledProgram
    compiledModulesByPath = buildCompiledModulePathIndex compiledProgram

    evaluateModules compiledModules ambientEnv runtimeModules output remainingModules =
      case remainingModules of
        [] ->
          pure
            (finishRuntimeProgram runtimeModules output)
        compiledModule : rest -> do
          let preparedModule =
                prepareModuleEvaluation entryPath compiledModules ambientEnv runtimeModules compiledModule
          scopeResult <-
            ExceptT
              ( evaluateModuleScopeWithRequiredEvaluationHostControl
                  evaluationHost
                  (Just (modulePathTexts (preparedModulePath preparedModule)))
                  (preparedModuleEvaluationMode preparedModule)
                  (compiledPreludeBuiltinMode (compiledProgramPrelude compiledProgram))
                  (interfaceRuntimeHints (compiledModuleInterface compiledModule))
                  (preparedModuleImportedEnvironment preparedModule)
                  (scopeStatements (compiledModuleExpr compiledModule))
              )
          let (nextRuntimeModules, nextOutput) =
                completeModuleEvaluation preparedModule compiledModule scopeResult runtimeModules output
          evaluateModules compiledModules ambientEnv nextRuntimeModules nextOutput rest

prepareModuleEvaluation ::
  ModulePath ->
  Map ModulePath CompiledModule ->
  RuntimeEnv ->
  RuntimeModuleAccumulator ->
  CompiledModule ->
  PreparedModuleEvaluation
prepareModuleEvaluation entryPath compiledModules ambientEnv runtimeModules compiledModule =
  PreparedModuleEvaluation
    { preparedModulePath = modulePath,
      preparedModuleEvaluationMode =
        if modulePath == entryPath
          then EvaluateEntryModule
          else EvaluateDependencyModule,
      preparedModuleImportedEnvironment =
        foldr
          (importRuntimeModule compiledModules (accumulatedRuntimeModulesByPath runtimeModules))
          ambientEnv
          (compiledModuleImports compiledModule)
    }
  where
    modulePath = compiledModulePath compiledModule

completeModuleEvaluation ::
  PreparedModuleEvaluation ->
  CompiledModule ->
  ScopeResult ->
  RuntimeModuleAccumulator ->
  Maybe RuntimeValue ->
  (RuntimeModuleAccumulator, Maybe RuntimeValue)
completeModuleEvaluation preparedModule compiledModule scopeResult runtimeModules output =
  ( accumulateRuntimeModule (preparedModulePath preparedModule) runtimeModule runtimeModules,
    case preparedModuleEvaluationMode preparedModule of
      EvaluateEntryModule -> scopeResultValue scopeResult
      EvaluateDependencyModule -> output
  )
  where
    runtimeModule =
      RuntimeModule
        { runtimeModulePath = modulePathTexts (preparedModulePath preparedModule),
          runtimeModuleExports =
            publishExports
              CurrentModule
              (compiledModuleExportInventory compiledModule)
              (compiledModuleInterface compiledModule)
              (scopeResultEnvironment scopeResult)
        }

compiledProgramRequiresHost :: CompiledProgram -> Bool
compiledProgramRequiresHost compiledProgram =
  maybe False runtimeExprRequiresHost (compiledPreludeExpr (compiledProgramPrelude compiledProgram))
    || any (runtimeExprRequiresHost . compiledModuleExpr) (compiledProgramModules compiledProgram)

evaluatePreludeWithEvaluationHost ::
  (Monad m) =>
  RuntimeHost (RuntimeHostEvaluationT m) ->
  ModulePath ->
  CompiledPrelude ->
  RuntimeHostEvaluationT m (Either RuntimeControl RuntimeEnv)
evaluatePreludeWithEvaluationHost host preludePath compiledPrelude =
  case compiledPreludeExpr compiledPrelude of
    Nothing -> pure (Right Map.empty)
    Just expression -> do
      scopeResult <-
        evaluateModuleScopeWithRequiredEvaluationHostControl
          host
          (Just (modulePathTexts preludePath))
          EvaluateDependencyModule
          (compiledPreludeBuiltinMode compiledPrelude)
          (compiledPreludeRuntimeHints compiledPrelude)
          Map.empty
          (scopeStatements expression)
      pure $
        fmap
          ( \result ->
              publishEnvironment
                AmbientPrelude
                (moduleInterfaceExportInventory (compiledPreludeInterface compiledPrelude))
                (compiledPreludeInterface compiledPrelude)
                (scopeResultEnvironment result)
          )
          scopeResult

importRuntimeModule :: Map ModulePath CompiledModule -> Map ModulePath RuntimeModule -> ModuleImport 'Resolved -> RuntimeEnv -> RuntimeEnv
importRuntimeModule compiledModules runtimeModules importDecl env =
  case (Map.lookup dependencyPath compiledModules, Map.lookup dependencyPath runtimeModules) of
    (Just compiledDependency, Just runtimeDependency) ->
      let publicInventory =
            compiledModuleExportInventory compiledDependency
          selectedExports =
            [ (runtimeExport, cell)
            | (runtimeExport, cell) <- Map.toList (runtimeModuleExports runtimeDependency),
              runtimeExportSelected importDecl publicInventory runtimeExport
            ]
          insertExport (runtimeExport, cell) =
            Map.insert
              ( UserName
                  ( ResolvedUserName
                      dependencyOrigin
                      (runtimeExportNamespace runtimeExport)
                      (mkIdentifier (runtimeExportName runtimeExport))
                  )
              )
              cell
       in foldr insertExport env selectedExports
    _ -> env
  where
    dependencyPath = ModuleGraph.importedModule importDecl
    dependencyOrigin = ImportedModule dependencyPath

emptyRuntimeModuleAccumulator :: RuntimeModuleAccumulator
emptyRuntimeModuleAccumulator = RuntimeModuleAccumulator Seq.empty Map.empty

accumulateRuntimeModule :: ModulePath -> RuntimeModule -> RuntimeModuleAccumulator -> RuntimeModuleAccumulator
accumulateRuntimeModule modulePath runtimeModule runtimeModules =
  RuntimeModuleAccumulator
    { accumulatedRuntimeModules = accumulatedRuntimeModules runtimeModules Seq.|> runtimeModule,
      accumulatedRuntimeModulesByPath =
        Map.insertWith
          (\_ firstRuntimeModule -> firstRuntimeModule)
          modulePath
          runtimeModule
          (accumulatedRuntimeModulesByPath runtimeModules)
    }

finishRuntimeProgram :: RuntimeModuleAccumulator -> Maybe RuntimeValue -> RuntimeProgram
finishRuntimeProgram runtimeModules output =
  RuntimeProgram
    { runtimeProgramModules = toList (accumulatedRuntimeModules runtimeModules),
      runtimeProgramOutput = output
    }

buildCompiledModulePathIndex :: CompiledProgram -> Map ModulePath CompiledModule
buildCompiledModulePathIndex =
  Map.fromListWith (\_ firstCompiledModule -> firstCompiledModule)
    . map
      (\compiledModule -> (compiledModulePath compiledModule, compiledModule))
    . compiledProgramModules

modulePathTexts :: ModulePath -> [Text]
modulePathTexts = NonEmpty.toList . modulePathTextSegments

publishEnvironment :: ResolvedNameOrigin -> ModuleExportInventory -> ModuleInterface -> RuntimeEnv -> RuntimeEnv
publishEnvironment origin publicInventory moduleInterface env =
  let renderedLookupIndex = buildRenderedLookupIndex env
   in Map.fromList
        [ (UserName (ResolvedUserName origin (runtimeExportNamespace runtimeExport) (mkIdentifier (runtimeExportName runtimeExport))), cell)
        | runtimeExport <- interfaceExports publicInventory moduleInterface,
          Just cell <- [lookupExportCell origin runtimeExport env renderedLookupIndex]
        ]

publishExports :: ResolvedNameOrigin -> ModuleExportInventory -> ModuleInterface -> RuntimeEnv -> Map RuntimeExport RuntimeCell
publishExports origin publicInventory moduleInterface env =
  let renderedLookupIndex = buildRenderedLookupIndex env
   in Map.fromList
        [ (runtimeExport, cell)
        | runtimeExport <- interfaceExports publicInventory moduleInterface,
          Just cell <- [lookupExportCell origin runtimeExport env renderedLookupIndex]
        ]

interfaceExports :: ModuleExportInventory -> ModuleInterface -> [RuntimeExport]
interfaceExports publicInventory moduleInterface =
  [ RuntimeBindingExport export
  | export <- Set.toList (exportInventoryEntries publicInventory),
    moduleExportNamespace export `elem` [ValueNamespace, ConstructorNamespace]
  ]
    <> [ RuntimeCapabilityMethodExport className methodName
       | methodKey <- Map.keys (interfaceClassMethods moduleInterface),
         Just (className, methodName) <- [splitQualifiedMethodKey methodKey],
         Set.member className publicClassNames
       ]
  where
    publicClassNames = exportNamesInNamespace CapabilityNamespace publicInventory

runtimeExportSelected :: ModuleImport 'Resolved -> ModuleExportInventory -> RuntimeExport -> Bool
runtimeExportSelected importDecl publicInventory runtimeExport =
  case ModuleGraph.importExposure importDecl of
    ImportAllUnqualified -> selectedBy UnqualifiedImport Nothing True
    ImportOnlyUnqualified symbolNames -> selectedBy UnqualifiedImport (Just (map identifierText (NonEmpty.toList symbolNames))) True
    ImportQualifiedOnly -> selectedBy QualifiedAliasImport Nothing False
  where
    selectedBy importMode symbolNames includeCapabilityMethods =
      case runtimeExport of
        RuntimeCapabilityMethodExport className _ ->
          includeCapabilityMethods
            && Set.member className selectedClassNames
        RuntimeBindingExport moduleExport ->
          inventoryHasExport moduleExport selectedInventory
      where
        selectedInventory =
          visibleImportInventory
            importMode
            symbolNames
            publicInventory
        selectedClassNames =
          exportNamesInNamespace CapabilityNamespace selectedInventory

runtimeExportName :: RuntimeExport -> Text
runtimeExportName runtimeExport =
  case runtimeExport of
    RuntimeBindingExport moduleExport -> moduleExportName moduleExport
    RuntimeCapabilityMethodExport className methodName -> className <> "::" <> methodName

runtimeExportNamespace :: RuntimeExport -> NameNamespace
runtimeExportNamespace runtimeExport =
  case runtimeExport of
    RuntimeBindingExport moduleExport -> moduleExportNamespace moduleExport
    RuntimeCapabilityMethodExport {} -> ValueNamespace

type RenderedLookupIndex = Map (NameNamespace, Text) RuntimeCell

lookupExportCell :: ResolvedNameOrigin -> RuntimeExport -> RuntimeEnv -> RenderedLookupIndex -> Maybe RuntimeCell
lookupExportCell origin runtimeExport env renderedLookupIndex =
  case Map.lookup expectedName env of
    Just cell -> Just cell
    Nothing -> lookupRendered runtimeExport renderedLookupIndex
  where
    exportName = runtimeExportName runtimeExport
    expectedName =
      case origin of
        AmbientPrelude -> resolvedAmbientName (runtimeExportNamespace runtimeExport) (mkIdentifier exportName)
        _ -> UserName (ResolvedUserName origin (runtimeExportNamespace runtimeExport) (mkIdentifier exportName))

buildRenderedLookupIndex :: RuntimeEnv -> RenderedLookupIndex
buildRenderedLookupIndex =
  foldr indexName Map.empty . Map.toList
  where
    indexName (name, cell) index =
      foldr
        (\key -> Map.insert key cell)
        index
        [ (namespace, renderedName)
        | namespace <- matchingNamespaces name,
          renderedName <- Set.toList (Set.fromList [renderName name, identifierText name])
        ]
    matchingNamespaces name =
      case name of
        UserName (ResolvedUserName _ namespace _) -> [namespace]
        _ -> [ValueNamespace, ConstructorNamespace, TypeNamespace, CapabilityNamespace]

lookupRendered :: RuntimeExport -> RenderedLookupIndex -> Maybe RuntimeCell
lookupRendered runtimeExport renderedLookupIndex =
  Map.lookup
    (runtimeExportNamespace runtimeExport, runtimeExportName runtimeExport)
    renderedLookupIndex

scopeStatements :: Expr 'Resolved -> [Statement 'Resolved]
scopeStatements expression =
  case expression of
    EBlock _ statements -> statements
    _ -> []
