{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Semantic analysis for the current compiler slice. This pass keeps the core
-- AST shape intact while enforcing scope visibility, signature adjacency,
-- capability constraints, purity, and rebinding rules.
module Jazz.Compiler.Analyzer
  ( AnalysisBinding (..),
    AnalysisInputs (..),
    Expr (..),
    Statement (..),
    AnalysisResult (..),
    analyzeProgramWithHiddenStatements,
    analyzeProgramWithInputs,
    analyzeProgramWithInputsAndPreparedScope,
    analyzeProgram,
    analyzeRebindingWarnings,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Monoid (Endo (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNode (coreNodeSpan),
    CorePhase (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Pattern (..),
    SignatureType,
    Statement (..),
  )
import Jazz.Compiler.Analyzer.UnusedBindings
  ( collectUnusedBindingWarnings,
  )
import Jazz.Compiler.BuiltinCatalog
  ( isKernelBuiltinSymbolName,
    kernelBuiltinNames,
  )
import Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact,
    concreteImplFact,
    renderConcreteImplFact,
    splitQualifiedMethodKey,
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
    WarningCategory (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    SourceSpan (..),
    appendDiagnosticSecondaryLabel,
    diagnosticWarningCategory,
    mkErrorDiagnostic,
    mkSameScopeRebindingWarning,
    mkWarningDiagnostic,
    promoteDiagnostic,
    setDiagnosticPrimaryLabel,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject,
    sortWarnings,
  )
import Jazz.Compiler.Name
  ( NameNamespace (ValueNamespace),
    ResolvedName,
    identifierPurity,
    identifierText,
    mkIdentifier,
    resolvedAmbientName,
    resolvedValueScopeName,
  )
import Jazz.Compiler.Pattern
  ( patternBinderNames,
  )
import Jazz.Compiler.Purity
  ( Purity (..),
  )
import Jazz.Compiler.RecursiveBindings
  ( PreparedRecursiveScope,
    prepareResolvedScope,
    preparedRecursiveScopeFactsForOuterBindings,
    preparedRecursiveScopeStatements,
    recursiveScopeGroups,
  )
import Jazz.Compiler.WarningConfig
  ( WarningSettings,
    isWarningEnabled,
    isWarningError,
  )

-- | Analyzer output keeps the original expression plus the warnings/errors
-- discovered while walking it.
data AnalysisResult = AnalysisResult
  { analyzedExpr :: Expr 'Resolved,
    analysisDiagnostics :: [Diagnostic]
  }
  deriving (Eq, Show)

data AnalysisBinding = AnalysisBinding
  { analysisBindingSpan :: Maybe SourceSpan,
    analysisBindingIsHiddenPrelude :: Bool
  }
  deriving (Eq, Show)

data AnalysisInputs = AnalysisInputs
  { analysisWarningSettings :: WarningSettings,
    analysisImportedValues :: Map ResolvedName AnalysisBinding,
    analysisForwardFunctions :: Map Int (ResolvedName, AnalysisBinding),
    analysisImportedClasses :: Set ResolvedName,
    analysisModulePath :: Maybe [Text]
  }
  deriving (Eq, Show)

-- | Describes the purity and location context surrounding the expression
-- currently being analyzed.
data AnalysisContext = AnalysisContext
  { contextLabel :: Text,
    contextAllowsImpureCalls :: Bool,
    contextPrimarySpan :: Maybe SourceSpan,
    contextSubject :: Maybe Text,
    contextLambdaSpan :: Maybe SourceSpan
  }

-- | Binding metadata retained in visibility maps so diagnostics can decide
-- whether a binding should surface source locations to users.
data VisibleBinding = VisibleBinding
  { visibleBindingSpan :: SourceSpan,
    visibleBindingIsHiddenPrelude :: Bool
  }

-- Entry point for the current analyzer slice:
-- - unbound variable diagnostics
-- - signature adjacency/name diagnostics
-- - optional same-scope rebinding warnings
-- - recursive-group visibility for self/mutual recursion
analyzeProgram :: WarningSettings -> Expr 'Resolved -> IO AnalysisResult
analyzeProgram =
  analyzeProgramWithHiddenStatements Set.empty

-- | Analyzer entrypoint used by prelude/module flows. Hidden statement indices
-- suppress synthetic-source locations while preserving the same semantic walk
-- used for ordinary user code.
analyzeProgramWithHiddenStatements ::
  Set Int ->
  WarningSettings ->
  Expr 'Resolved ->
  IO AnalysisResult
analyzeProgramWithHiddenStatements hiddenStatementIndices settings expr =
  analyzeProgramWithInputs
    AnalysisInputs
      { analysisWarningSettings = settings,
        analysisImportedValues = Map.empty,
        analysisForwardFunctions = Map.empty,
        analysisImportedClasses = Set.empty,
        analysisModulePath = Nothing
      }
    hiddenStatementIndices
    expr

analyzeProgramWithInputs :: AnalysisInputs -> Set Int -> Expr 'Resolved -> IO AnalysisResult
analyzeProgramWithInputs inputs hiddenStatementIndices expr =
  {-# SCC "jazz-stage:static-analysis" #-}
  analyzeProgramWithInputsAndDiagnostics inputs expr collectedDiagnostics
  where
    collectedDiagnostics =
      case expr of
        EBlock node statements ->
          collectScopeDiagnostics hiddenStatementIndices settings importedBindings forwardBindings importedClasses topLevelContext (prepareResolvedScope node statements)
        _ ->
          collectExprDiagnostics settings importedBindings importedClasses topLevelContext expr
    settings = analysisWarningSettings inputs
    importedBindings = analysisVisibleBindings inputs
    forwardBindings = analysisVisibleForwardBindings inputs
    importedClasses = Set.map identifierText (analysisImportedClasses inputs)

analyzeProgramWithInputsAndPreparedScope ::
  AnalysisInputs ->
  Set Int ->
  Expr 'Resolved ->
  PreparedRecursiveScope 'Resolved ->
  IO AnalysisResult
analyzeProgramWithInputsAndPreparedScope inputs hiddenStatementIndices expr preparedScope =
  {-# SCC "jazz-stage:static-analysis" #-}
  let expectedOuterBindingNames =
        Set.union
          (Map.keysSet (analysisImportedValues inputs))
          (Set.map (resolvedAmbientName ValueNamespace . mkIdentifier) kernelBuiltinNames)
      analysisScope = preparedAnalysisScope expectedOuterBindingNames preparedScope
      collectedDiagnostics =
        collectScopeDiagnosticsWithPreparedScope
          analysisScope
          hiddenStatementIndices
          (analysisWarningSettings inputs)
          (analysisVisibleBindings inputs)
          (analysisVisibleForwardBindings inputs)
          (Set.map identifierText (analysisImportedClasses inputs))
          topLevelContext
   in analysisScope `seq`
        expr `seq`
          analyzeProgramWithInputsAndDiagnostics inputs expr collectedDiagnostics

data PreparedAnalysisScope = PreparedAnalysisScope ![Statement 'Resolved] !(Map Int [Int])

preparedAnalysisScope :: Set ResolvedName -> PreparedRecursiveScope 'Resolved -> PreparedAnalysisScope
preparedAnalysisScope expectedOuterBindingNames preparedScope =
  PreparedAnalysisScope
    (preparedRecursiveScopeStatements preparedScope)
    (recursiveScopeGroups recursiveScopeFactsValue)
  where
    recursiveScopeFactsValue =
      preparedRecursiveScopeFactsForOuterBindings expectedOuterBindingNames preparedScope

analyzeProgramWithInputsAndDiagnostics :: AnalysisInputs -> Expr 'Resolved -> CollectedDiagnostics -> IO AnalysisResult
analyzeProgramWithInputsAndDiagnostics inputs expr collectedDiagnostics =
  let (warnings, errors) = materializeDiagnostics collectedDiagnostics
      diagnostics =
        map (applyWarningPolicy settings) (sortWarnings warnings <> errors)
      result =
        AnalysisResult
          { analyzedExpr = expr,
            analysisDiagnostics = diagnostics
          }
   in pure result
  where
    settings = analysisWarningSettings inputs

analysisVisibleBindings :: AnalysisInputs -> Map ResolvedName VisibleBinding
analysisVisibleBindings =
  Map.mapKeys resolvedValueScopeName
    . Map.map analysisBindingToVisibleBinding
    . analysisImportedValues

analysisVisibleForwardBindings :: AnalysisInputs -> Map Int (ResolvedName, VisibleBinding)
analysisVisibleForwardBindings inputs =
  Map.map
    (\(name, binding) -> (resolvedValueScopeName name, analysisBindingToVisibleBinding binding))
    (analysisForwardFunctions inputs)

analysisBindingToVisibleBinding :: AnalysisBinding -> VisibleBinding
analysisBindingToVisibleBinding binding =
  VisibleBinding
    { visibleBindingSpan = maybe (SourceSpan 0 0) id (analysisBindingSpan binding),
      visibleBindingIsHiddenPrelude =
        analysisBindingIsHiddenPrelude binding || analysisBindingSpan binding == Nothing
    }

-- | Append-efficient diagnostic streams. Builders compose in source order and
-- are materialized only at the public analyzer boundary.
newtype CollectedDiagnostics = CollectedDiagnostics (Endo [Diagnostic], Endo [Diagnostic])
  deriving newtype (Semigroup, Monoid)

diagnosticsFromLists :: [Diagnostic] -> [Diagnostic] -> CollectedDiagnostics
diagnosticsFromLists warnings errors =
  CollectedDiagnostics (Endo (warnings ++), Endo (errors ++))

warningDiagnostics :: [Diagnostic] -> CollectedDiagnostics
warningDiagnostics warnings = diagnosticsFromLists warnings []

errorDiagnostics :: [Diagnostic] -> CollectedDiagnostics
errorDiagnostics errors = diagnosticsFromLists [] errors

materializeDiagnostics :: CollectedDiagnostics -> ([Diagnostic], [Diagnostic])
materializeDiagnostics (CollectedDiagnostics (warnings, errors)) =
  (appEndo warnings [], appEndo errors [])

analyzeRebindingWarnings :: WarningSettings -> Expr 'Resolved -> IO [Diagnostic]
analyzeRebindingWarnings settings expr =
  filter (isJust . diagnosticWarningCategory) . analysisDiagnostics
    <$> analyzeProgram settings expr

applyWarningPolicy :: WarningSettings -> Diagnostic -> Diagnostic
applyWarningPolicy settings diagnostic =
  case diagnosticWarningCategory diagnostic of
    Just category
      | isWarningError settings category -> promoteDiagnostic diagnostic
    _ -> diagnostic

collectExprDiagnostics ::
  WarningSettings ->
  Map ResolvedName VisibleBinding ->
  Set Text ->
  AnalysisContext ->
  Expr 'Resolved ->
  CollectedDiagnostics
collectExprDiagnostics settings visibleBindings visibleClassNames context expr =
  case expr of
    ELit _ _ -> mempty
    EVar _ name ->
      case Map.lookup (resolvedValueScopeName name) visibleBindings of
        Just _ -> mempty
        Nothing
          | isKernelBuiltinSymbolName nameText -> mempty
          | qualifiedMethodClassIsVisible visibleClassNames nameText -> mempty
          | otherwise -> errorDiagnostics [mkUnboundVariableError nameText]
      where
        nameText = identifierText name
    ELambda _ parameterName bodyExpr ->
      let lambdaBindings =
            Map.insert
              (resolvedValueScopeName parameterName)
              lambdaVisibleBinding
              visibleBindings
          shadowingWarnings =
            case lambdaShadowingSpan context of
              Nothing -> []
              Just primarySpan ->
                collectOuterScopeShadowingWarnings
                  settings
                  parameterName
                  primarySpan
                  visibleBindings
          bodyDiagnostics =
            collectExprDiagnostics settings lambdaBindings visibleClassNames context bodyExpr
       in warningDiagnostics shadowingWarnings <> bodyDiagnostics
    EOperatorValue _ _ -> mempty
    EList _ elements ->
      collectExprListDiagnostics settings visibleBindings visibleClassNames context elements
    ETuple _ elements ->
      collectExprListDiagnostics settings visibleBindings visibleClassNames context elements
    EApply _ functionExpr argumentExpr ->
      let functionDiagnostics =
            collectExprDiagnostics settings visibleBindings visibleClassNames context functionExpr
          argumentDiagnostics =
            collectExprDiagnostics settings visibleBindings visibleClassNames context argumentExpr
          purityErrors =
            case directCallCalleeName functionExpr of
              Just calleeName
                | shouldRejectImpureCall visibleBindings visibleClassNames context calleeName ->
                    [ mkImpureCallInPureContextError
                        context
                        calleeName
                        (Map.lookup (resolvedValueScopeName calleeName) visibleBindings >>= visibleBindingDiagnosticSpan)
                    ]
              _ -> []
       in functionDiagnostics <> argumentDiagnostics <> errorDiagnostics purityErrors
    ETypeApplication _ functionExpr _ _ ->
      collectExprDiagnostics settings visibleBindings visibleClassNames context functionExpr
    EIf _ conditionExpr thenExpr elseExpr ->
      let conditionDiagnostics =
            collectExprDiagnostics settings visibleBindings visibleClassNames context conditionExpr
          thenDiagnostics =
            collectExprDiagnostics settings visibleBindings visibleClassNames context thenExpr
          elseDiagnostics =
            collectExprDiagnostics settings visibleBindings visibleClassNames context elseExpr
       in conditionDiagnostics <> thenDiagnostics <> elseDiagnostics
    EPatternCase _ scrutineeExpr caseArms ->
      let scrutineeDiagnostics =
            collectExprDiagnostics settings visibleBindings visibleClassNames context scrutineeExpr
          armDiagnostics (CaseArm _ pattern guardExpr bodyExpr) =
            let armBindings = extendBindingsWithPattern pattern visibleBindings
                guardDiagnostics =
                  maybe
                    mempty
                    ( collectExprDiagnostics
                        settings
                        armBindings
                        visibleClassNames
                        context
                    )
                    guardExpr
                bodyDiagnostics =
                  collectExprDiagnostics
                    settings
                    armBindings
                    visibleClassNames
                    context
                    bodyExpr
             in guardDiagnostics <> bodyDiagnostics
       in scrutineeDiagnostics <> foldMap armDiagnostics caseArms
    EBinary _ _ leftExpr rightExpr ->
      let leftDiagnostics =
            collectExprDiagnostics settings visibleBindings visibleClassNames context leftExpr
          rightDiagnostics =
            collectExprDiagnostics settings visibleBindings visibleClassNames context rightExpr
       in leftDiagnostics <> rightDiagnostics
    ESectionLeft _ leftExpr _ ->
      collectExprDiagnostics settings visibleBindings visibleClassNames context leftExpr
    ESectionRight _ _ rightExpr ->
      collectExprDiagnostics settings visibleBindings visibleClassNames context rightExpr
    EBlock node statements -> collectScopeDiagnostics Set.empty settings visibleBindings Map.empty visibleClassNames context (prepareResolvedScope node statements)

collectExprListDiagnostics ::
  WarningSettings ->
  Map ResolvedName VisibleBinding ->
  Set Text ->
  AnalysisContext ->
  [Expr 'Resolved] ->
  CollectedDiagnostics
collectExprListDiagnostics settings visibleBindings visibleClassNames context elements =
  foldMap
    (collectExprDiagnostics settings visibleBindings visibleClassNames context)
    elements

-- | Walk a block scope in declaration order, enforcing signature adjacency,
-- rebinding policy, and recursive-peer visibility at the same time.
collectScopeDiagnostics ::
  Set Int ->
  WarningSettings ->
  Map ResolvedName VisibleBinding ->
  Map Int (ResolvedName, VisibleBinding) ->
  Set Text ->
  AnalysisContext ->
  PreparedRecursiveScope 'Resolved ->
  CollectedDiagnostics
collectScopeDiagnostics hiddenStatementIndices settings outerScope forwardBindings outerClassNames context preparedScope =
  collectScopeDiagnosticsWithPreparedScope
    ( preparedAnalysisScope
        outerBindingNames
        preparedScope
    )
    hiddenStatementIndices
    settings
    outerScope
    forwardBindings
    outerClassNames
    context
  where
    outerBindingNames =
      Set.union
        (Map.keysSet outerScope)
        (Set.map (resolvedAmbientName ValueNamespace . mkIdentifier) kernelBuiltinNames)

collectScopeDiagnosticsWithPreparedScope ::
  PreparedAnalysisScope ->
  Set Int ->
  WarningSettings ->
  Map ResolvedName VisibleBinding ->
  Map Int (ResolvedName, VisibleBinding) ->
  Set Text ->
  AnalysisContext ->
  CollectedDiagnostics
collectScopeDiagnosticsWithPreparedScope (PreparedAnalysisScope statements rawRecursiveGroupsByStatement) hiddenStatementIndices settings outerScope forwardBindings outerClassNames context =
  flushPendingSignature finalPendingSignature finalDiagnostics
  where
    indexedStatements = zip [0 ..] statements
    moduleBaselineClassDeclarations = collectModuleBaselineClassDeclarations indexedStatements
    moduleClassDeclarationsByPath = collectModuleClassDeclarations indexedStatements

    -- Build recursion groups from local binding dependencies so mutually recursive
    -- bindings can reference each other independent of declaration order.
    recursiveGroupsByStatement =
      Map.map Set.fromList rawRecursiveGroupsByStatement
    bindingDeclarationsByStatement = collectBindingDeclarations indexedStatements
    unusedBindingWarningsByStatement =
      collectUnusedBindingWarnings
        settings
        hiddenStatementIndices
        indexedStatements

    -- Diagnostics use source-ordered builders for O(1) append.
    -- `pendingSignature` tracks exactly one immediately-preceding signature that
    -- must be consumed by the next binding.
    (_, _, _, _, finalPendingSignature, finalDiagnostics) =
      foldl' step (Map.empty, Map.empty, Set.empty, Map.empty, Nothing, mempty) indexedStatements

    step ::
      (Map ResolvedName VisibleBinding, Map Text SourceSpan, Set Text, Map ConcreteImplFact SourceSpan, Maybe PendingSignature, CollectedDiagnostics) ->
      (Int, Statement 'Resolved) ->
      (Map ResolvedName VisibleBinding, Map Text SourceSpan, Set Text, Map ConcreteImplFact SourceSpan, Maybe PendingSignature, CollectedDiagnostics)
    step (scopeBindings, classDeclarations, importedClassNames, implDeclarations, pendingSignature, diagnostics) (statementIndex, statement) =
      case statement of
        SExpr exprNode expr ->
          -- Any signature followed by a non-binding is invalid by contract.
          let exprSpan = coreNodeSpan exprNode
              diagnosticsWithPending = flushPendingSignature pendingSignature diagnostics
              visible = currentVisibleBindings scopeBindings
              exprDiagnostics =
                collectExprDiagnostics
                  settings
                  visible
                  (currentVisibleClassNames classDeclarations importedClassNames)
                  (contextForExpressionStatement exprSpan context)
                  expr
           in ( scopeBindings,
                classDeclarations,
                importedClassNames,
                implDeclarations,
                Nothing,
                diagnosticsWithPending <> exprDiagnostics
              )
        SModule {} ->
          let diagnosticsWithPending = flushPendingSignature pendingSignature diagnostics
           in ( scopeBindings,
                moduleBaselineClassDeclarations,
                Set.empty,
                Map.empty,
                Nothing,
                diagnosticsWithPending
              )
        SImport _ modulePath maybeAlias maybeSymbolNames ->
          let diagnosticsWithPending = flushPendingSignature pendingSignature diagnostics
              nextImportedClassNames =
                Set.union
                  importedClassNames
                  (visibleImportedClassNames modulePath maybeAlias maybeSymbolNames)
           in ( scopeBindings,
                classDeclarations,
                nextImportedClassNames,
                implDeclarations,
                Nothing,
                diagnosticsWithPending
              )
        SClass classNode capabilityName _parameters methods ->
          let classSpan = coreNodeSpan classNode
              diagnosticsWithPending = flushPendingSignature pendingSignature diagnostics
              classNameText = identifierText capabilityName
              (nextClassDeclarations, classErrors) =
                case Map.lookup classNameText classDeclarations of
                  Just previousSpan ->
                    ( classDeclarations,
                      [mkDuplicateClassDeclarationError classNameText classSpan (Just previousSpan)]
                    )
                  Nothing
                    | Set.member classNameText (Set.union importedClassNames outerClassNames) ->
                        ( classDeclarations,
                          [mkDuplicateClassDeclarationError classNameText classSpan Nothing]
                        )
                  Nothing ->
                    (Map.insert classNameText classSpan classDeclarations, [])
              methodErrors = duplicateClassMethodErrors classNameText methods
           in ( scopeBindings,
                nextClassDeclarations,
                importedClassNames,
                implDeclarations,
                Nothing,
                diagnosticsWithPending <> errorDiagnostics (classErrors ++ methodErrors)
              )
        SImpl implNode capabilityName arguments methods ->
          let implSpan = coreNodeSpan implNode
              diagnosticsWithPending = flushPendingSignature pendingSignature diagnostics
              visible = currentVisibleBindings scopeBindings
              (nextImplDeclarations, implErrors) =
                case concreteImplFact capabilityName arguments of
                  Nothing ->
                    (implDeclarations, [])
                  Just implFact ->
                    case Map.lookup implFact implDeclarations of
                      Just previousSpan ->
                        ( implDeclarations,
                          [mkDuplicateImplDeclarationError (renderConcreteImplFact implFact) implSpan previousSpan]
                        )
                      Nothing ->
                        (Map.insert implFact implSpan implDeclarations, [])
              methodErrors = duplicateImplMethodErrors capabilityName arguments methods
              methodBodyDiagnostics =
                collectImplMethodDiagnostics
                  settings
                  visible
                  (currentVisibleClassNames classDeclarations importedClassNames)
                  methods
           in ( scopeBindings,
                classDeclarations,
                importedClassNames,
                nextImplDeclarations,
                Nothing,
                diagnosticsWithPending
                  <> errorDiagnostics (implErrors ++ methodErrors)
                  <> methodBodyDiagnostics
              )
        SData _ _ _ constructors ->
          let diagnosticsWithPending = flushPendingSignature pendingSignature diagnostics
              constructorWarnings =
                collectDataConstructorRebindingWarnings
                  settings
                  hiddenStatementIndices
                  statementIndex
                  constructors
                  scopeBindings
           in ( registerDataConstructors
                  hiddenStatementIndices
                  statementIndex
                  constructors
                  scopeBindings,
                classDeclarations,
                importedClassNames,
                implDeclarations,
                Nothing,
                diagnosticsWithPending <> warningDiagnostics constructorWarnings
              )
        SSignature signatureNode signatureName _signatureText ->
          -- Signature payload text is carried forward for future type parsing.
          -- This pass only enforces placement/name coherence.
          let signatureSpan = coreNodeSpan signatureNode
              diagnosticsWithPending = flushPendingSignature pendingSignature diagnostics
           in ( scopeBindings,
                classDeclarations,
                importedClassNames,
                implDeclarations,
                Just (PendingSignature (identifierText signatureName) signatureSpan),
                diagnosticsWithPending
              )
        SLet bindingNode bindingName valueExpr ->
          -- Bindings consume a pending signature if names match. Rebinding
          -- stays semantically valid but may emit an optional warning.
          let bindingSpan = coreNodeSpan bindingNode
              bindingNameText = identifierText bindingName
              bindingScopeName = resolvedValueScopeName bindingName
              errorsFromSignature =
                case pendingSignature of
                  Nothing -> []
                  Just (PendingSignature signatureName signatureDeclSpan)
                    | signatureName == bindingNameText -> []
                    | otherwise ->
                        [ mkMismatchedSignatureError
                            signatureName
                            signatureDeclSpan
                            bindingNameText
                            bindingSpan
                        ]
              rebindingWarning =
                case Map.lookup bindingScopeName scopeBindings of
                  Just previousBinding
                    | isWarningEnabled settings SameScopeRebinding,
                      not (visibleBindingIsHiddenPrelude previousBinding) ->
                        [ mkSameScopeRebindingWarning
                            bindingNameText
                            bindingSpan
                            (visibleBindingSpan previousBinding)
                        ]
                  _ -> []
              shadowingWarning =
                case Map.lookup bindingScopeName scopeBindings of
                  Just _ -> []
                  Nothing ->
                    collectOuterScopeShadowingWarnings
                      settings
                      bindingName
                      bindingSpan
                      outerScope
              nextScope =
                Map.insert
                  bindingScopeName
                  (mkVisibleBinding hiddenStatementIndices statementIndex bindingSpan)
                  scopeBindings
              visible =
                -- Recursive peer names in the same SCC are visible while
                -- analyzing the binding body.
                withForwardFunctionBindings
                  statementIndex
                  ( withRecursivePeerBindings
                      statementIndex
                      (currentVisibleBindings nextScope)
                  )
              bindingContext = contextForBinding bindingName
              valueDiagnostics =
                collectExprDiagnostics
                  settings
                  visible
                  (currentVisibleClassNames classDeclarations importedClassNames)
                  (bindingContext bindingSpan)
                  valueExpr
              unusedWarnings =
                Map.findWithDefault [] statementIndex unusedBindingWarningsByStatement
              bindingDiagnostics =
                diagnostics
                  <> errorDiagnostics errorsFromSignature
                  <> valueDiagnostics
                  <> warningDiagnostics rebindingWarning
                  <> warningDiagnostics shadowingWarning
                  <> warningDiagnostics unusedWarnings
           in ( nextScope,
                classDeclarations,
                importedClassNames,
                implDeclarations,
                Nothing,
                bindingDiagnostics
              )

    currentVisibleBindings :: Map ResolvedName VisibleBinding -> Map ResolvedName VisibleBinding
    -- Local scope is left-biased so inner declarations shadow outer bindings.
    currentVisibleBindings scopeBindings = scopeBindings `Map.union` outerScope

    currentVisibleClassNames :: Map Text SourceSpan -> Set Text -> Set Text
    currentVisibleClassNames classDeclarations importedClassNames =
      Map.keysSet classDeclarations `Set.union` importedClassNames `Set.union` outerClassNames

    collectModuleBaselineClassDeclarations :: [(Int, Statement 'Resolved)] -> Map Text SourceSpan
    collectModuleBaselineClassDeclarations indexedScopeStatements =
      case [statementIndex | (statementIndex, SModule {}) <- indexedScopeStatements] of
        [] -> Map.empty
        firstModuleStatementIndex : _ ->
          Map.fromList
            [ (identifierText className, coreNodeSpan classNode)
            | (statementIndex, SClass classNode className _ _) <- indexedScopeStatements,
              statementIndex < firstModuleStatementIndex
            ]

    collectModuleClassDeclarations :: [(Int, Statement 'Resolved)] -> Map [Text] (Map Text SourceSpan)
    collectModuleClassDeclarations =
      snd . foldl' collectModuleClassDeclaration (Nothing, Map.empty)
      where
        collectModuleClassDeclaration (currentModulePath, declarationsByPath) (_, statement) =
          case statement of
            SModule _ modulePath ->
              (Just modulePath, declarationsByPath)
            SClass classNode className _ _ ->
              case currentModulePath of
                Just modulePath ->
                  ( currentModulePath,
                    Map.insertWith
                      Map.union
                      modulePath
                      (Map.singleton (identifierText className) (coreNodeSpan classNode))
                      declarationsByPath
                  )
                Nothing ->
                  (currentModulePath, declarationsByPath)
            _ ->
              (currentModulePath, declarationsByPath)

    visibleImportedClassNames :: [Text] -> Maybe Text -> Maybe [Text] -> Set Text
    visibleImportedClassNames modulePath maybeAlias maybeSymbolNames =
      case Map.lookup modulePath moduleClassDeclarationsByPath of
        Nothing -> Set.empty
        Just importedClassDeclarations ->
          case maybeAlias of
            Just _ -> Set.empty
            Nothing ->
              case maybeSymbolNames of
                Nothing -> Map.keysSet importedClassDeclarations
                Just symbolNames ->
                  Set.intersection
                    (Map.keysSet importedClassDeclarations)
                    (Set.fromList symbolNames)

    withRecursivePeerBindings ::
      Int ->
      Map ResolvedName VisibleBinding ->
      Map ResolvedName VisibleBinding
    withRecursivePeerBindings statementIndex visibleNow =
      let peers =
            Set.delete
              statementIndex
              (Map.findWithDefault Set.empty statementIndex recursiveGroupsByStatement)
          peerEntries =
            Map.fromList
              [ (resolvedValueScopeName peerName, mkVisibleBinding hiddenStatementIndices peerStatementIndex peerSpan)
              | peerStatementIndex <- Set.toList peers,
                Just (peerName, peerSpan) <- [Map.lookup peerStatementIndex bindingDeclarationsByStatement],
                -- Do not override currently visible names (for example due to
                -- local rebinding) when adding recursive peers.
                Map.notMember (resolvedValueScopeName peerName) visibleNow
              ]
       in visibleNow `Map.union` peerEntries

    withForwardFunctionBindings ::
      Int ->
      Map ResolvedName VisibleBinding ->
      Map ResolvedName VisibleBinding
    withForwardFunctionBindings statementIndex visibleNow =
      case Map.lookup statementIndex forwardBindings of
        Nothing -> visibleNow
        Just _ ->
          foldl'
            (\visibleAcc (_, (name, binding)) -> Map.insertWith (\_ existing -> existing) (resolvedValueScopeName name) binding visibleAcc)
            visibleNow
            (filter ((> statementIndex) . fst) (Map.toAscList forwardBindings))

-- | Signature bookkeeping is intentionally small: only one immediately
-- preceding signature may be waiting for a matching binding.
data PendingSignature = PendingSignature
  { pendingSignatureName :: Text,
    pendingSignatureSpan :: SourceSpan
  }

-- | Signatures must be consumed by the next binding; reaching any other
-- statement turns the pending signature into a diagnostic.
flushPendingSignature :: Maybe PendingSignature -> CollectedDiagnostics -> CollectedDiagnostics
flushPendingSignature pending diagnostics =
  case pending of
    Nothing -> diagnostics
    Just pendingSignature ->
      diagnostics <> errorDiagnostics [mkMissingBindingForSignatureError pendingSignature]

mkUnboundVariableError :: Text -> Diagnostic
mkUnboundVariableError variableName =
  setDiagnosticSubject variableName $
    mkErrorDiagnostic E1001 CompilationOrigin ("unbound variable '" <> variableName <> "'")

qualifiedMethodClassIsVisible :: Set Text -> Text -> Bool
qualifiedMethodClassIsVisible visibleClassNames nameText =
  case splitQualifiedMethodKey nameText of
    Just (capabilityName, _) -> Set.member capabilityName visibleClassNames
    Nothing -> False

mkMissingBindingForSignatureError :: PendingSignature -> Diagnostic
mkMissingBindingForSignatureError pendingSignature =
  setDiagnosticSubject
    (pendingSignatureName pendingSignature)
    ( setDiagnosticPrimarySpan
        (pendingSignatureSpan pendingSignature)
        ( mkErrorDiagnostic
            E1002
            CompilationOrigin
            ( "signature for '"
                <> pendingSignatureName pendingSignature
                <> "' must be immediately followed by a matching binding"
            )
        )
    )

mkMismatchedSignatureError :: Text -> SourceSpan -> Text -> SourceSpan -> Diagnostic
mkMismatchedSignatureError signatureName signatureSpan bindingName bindingSpan =
  setDiagnosticSubject
    signatureName
    ( setDiagnosticRelatedSpan
        bindingSpan
        ( setDiagnosticPrimarySpan
            signatureSpan
            ( mkErrorDiagnostic
                E1003
                CompilationOrigin
                ( "signature for '"
                    <> signatureName
                    <> "' must annotate the next binding with the same name; found '"
                    <> bindingName
                    <> "'"
                )
            )
        )
    )

mkDuplicateClassDeclarationError :: Text -> SourceSpan -> Maybe SourceSpan -> Diagnostic
mkDuplicateClassDeclarationError className classSpan maybePreviousSpan =
  setDiagnosticSubject className $
    maybe id setDiagnosticRelatedSpan maybePreviousSpan $
      setDiagnosticPrimarySpan classSpan $
        mkErrorDiagnostic E1004 CompilationOrigin ("duplicate class declaration '" <> className <> "'")

duplicateClassMethodErrors :: Text -> [ClassMethodSignature 'Resolved] -> [Diagnostic]
duplicateClassMethodErrors className methods =
  reverse errorsRev
  where
    (_, errorsRev) = foldl' step (Map.empty, []) methods
    step (seenMethods, acc) (ClassMethodSignature methodNode methodName _) =
      let methodSpan = coreNodeSpan methodNode
          methodNameText = identifierText methodName
       in case Map.lookup methodNameText seenMethods of
            Just previousSpan ->
              ( seenMethods,
                mkDuplicateClassMethodError className methodNameText methodSpan previousSpan : acc
              )
            Nothing ->
              (Map.insert methodNameText methodSpan seenMethods, acc)

mkDuplicateClassMethodError :: Text -> Text -> SourceSpan -> SourceSpan -> Diagnostic
mkDuplicateClassMethodError className methodName methodSpan previousSpan =
  setDiagnosticSubject (className <> "." <> methodName) $
    setDiagnosticRelatedSpan previousSpan $
      setDiagnosticPrimarySpan
        methodSpan
        (mkErrorDiagnostic E1006 CompilationOrigin ("duplicate method signature '" <> methodName <> "' in class '" <> className <> "'"))

duplicateImplMethodErrors :: ResolvedName -> [SignatureType 'Resolved] -> [ImplMethod 'Resolved] -> [Diagnostic]
duplicateImplMethodErrors capabilityName arguments methods =
  reverse errorsRev
  where
    implLabel =
      case concreteImplFact capabilityName arguments of
        Just implFact -> renderConcreteImplFact implFact
        Nothing -> identifierText capabilityName
    (_, errorsRev) = foldl' step (Map.empty, []) methods
    step (seenMethods, acc) (ImplMethod methodNode methodName _) =
      let methodSpan = coreNodeSpan methodNode
          methodNameText = identifierText methodName
       in case Map.lookup methodNameText seenMethods of
            Just previousSpan ->
              ( seenMethods,
                mkDuplicateImplMethodError implLabel methodNameText methodSpan previousSpan : acc
              )
            Nothing ->
              (Map.insert methodNameText methodSpan seenMethods, acc)

mkDuplicateImplMethodError :: Text -> Text -> SourceSpan -> SourceSpan -> Diagnostic
mkDuplicateImplMethodError implLabel methodName methodSpan previousSpan =
  setDiagnosticSubject (implLabel <> "." <> methodName) $
    setDiagnosticRelatedSpan previousSpan $
      setDiagnosticPrimarySpan
        methodSpan
        (mkErrorDiagnostic E1007 CompilationOrigin ("duplicate method binding '" <> methodName <> "' in impl '" <> implLabel <> "'"))

collectImplMethodDiagnostics ::
  WarningSettings ->
  Map ResolvedName VisibleBinding ->
  Set Text ->
  [ImplMethod 'Resolved] ->
  CollectedDiagnostics
collectImplMethodDiagnostics settings visibleBindings visibleClassNames methods =
  foldMap collectMethodDiagnostics methods
  where
    collectMethodDiagnostics (ImplMethod methodNode methodName methodExpr) =
      collectExprDiagnostics
        settings
        visibleBindings
        visibleClassNames
        (contextForImplMethod methodName (coreNodeSpan methodNode))
        methodExpr

mkDuplicateImplDeclarationError :: Text -> SourceSpan -> SourceSpan -> Diagnostic
mkDuplicateImplDeclarationError implFactKey implSpan previousSpan =
  setDiagnosticSubject implFactKey $
    setDiagnosticRelatedSpan previousSpan $
      setDiagnosticPrimarySpan
        implSpan
        (mkErrorDiagnostic E1005 CompilationOrigin ("duplicate impl declaration for '" <> implFactKey <> "'"))

topLevelContext :: AnalysisContext
topLevelContext =
  -- Top-level expression statements stay permissive so program-entry
  -- expression calls like `print! ...` remain valid in stub-v1 purity mode.
  AnalysisContext
    { contextLabel = "top-level expression",
      contextAllowsImpureCalls = True,
      contextPrimarySpan = Nothing,
      contextSubject = Nothing,
      contextLambdaSpan = Nothing
    }

-- | Create the purity/diagnostic context that should apply while checking the
-- body of a specific binding.
contextForBinding :: ResolvedName -> SourceSpan -> AnalysisContext
contextForBinding bindingName bindingSpan =
  AnalysisContext
    { contextLabel = "binding '" <> identifierText bindingName <> "'",
      contextAllowsImpureCalls = identifierPurity bindingName == Impure,
      contextPrimarySpan = Just bindingSpan,
      contextSubject = Just (identifierText bindingName),
      contextLambdaSpan = Just bindingSpan
    }

contextForImplMethod :: ResolvedName -> SourceSpan -> AnalysisContext
contextForImplMethod methodName methodSpan =
  AnalysisContext
    { contextLabel = "impl method '" <> identifierText methodName <> "'",
      contextAllowsImpureCalls = identifierPurity methodName == Impure,
      contextPrimarySpan = Just methodSpan,
      contextSubject = Just (identifierText methodName),
      contextLambdaSpan = Just methodSpan
    }

contextForExpressionStatement :: SourceSpan -> AnalysisContext -> AnalysisContext
contextForExpressionStatement statementSpan context =
  context {contextLambdaSpan = Just statementSpan}

-- | Purity is name-based in this compiler slice; reject only when the current
-- context is pure and the callee is known either locally or through builtins.
shouldRejectImpureCall ::
  Map ResolvedName VisibleBinding ->
  Set Text ->
  AnalysisContext ->
  ResolvedName ->
  Bool
shouldRejectImpureCall visibleBindings visibleClassNames context calleeName =
  not (contextAllowsImpureCalls context)
    && isKnownImpureCallee
  where
    calleeNameText = identifierText calleeName
    isKnownImpureCallee =
      identifierPurity calleeName == Impure
        && ( Map.member (resolvedValueScopeName calleeName) visibleBindings
               || isKernelBuiltinSymbolName calleeNameText
               || qualifiedMethodClassIsVisible visibleClassNames calleeNameText
           )

directCallCalleeName :: Expr 'Resolved -> Maybe ResolvedName
directCallCalleeName expr =
  case expr of
    EVar _ calleeName -> Just calleeName
    ETypeApplication _ functionExpr _ _ -> directCallCalleeName functionExpr
    _ -> Nothing

mkImpureCallInPureContextError ::
  AnalysisContext ->
  ResolvedName ->
  Maybe SourceSpan ->
  Diagnostic
mkImpureCallInPureContextError context calleeName maybeCalleeSpan =
  withMaybe
    (contextSubject context)
    setDiagnosticSubject
    ( withMaybe
        (contextPrimarySpan context)
        setDiagnosticPrimarySpan
        ( withMaybe
            maybeCalleeSpan
            setDiagnosticRelatedSpan
            ( mkErrorDiagnostic
                E1010
                CompilationOrigin
                ( contextLabel context
                    <> " cannot call impure callee '"
                    <> identifierText calleeName
                    <> "'"
                )
            )
        )
    )

withMaybe :: Maybe a -> (a -> b -> b) -> b -> b
withMaybe maybeValue setter value =
  case maybeValue of
    Nothing -> value
    Just presentValue -> setter presentValue value

collectBindingDeclarations ::
  [(Int, Statement 'Resolved)] ->
  Map Int (ResolvedName, SourceSpan)
collectBindingDeclarations =
  foldl' collect Map.empty
  where
    collect declarations (statementIndex, statement) =
      case statement of
        SLet node name _ ->
          Map.insert statementIndex (name, coreNodeSpan node) declarations
        _ -> declarations

-- | Tag bindings that came from hidden prelude statements so user-facing
-- diagnostics can avoid pointing at synthetic source positions.
mkVisibleBinding :: Set Int -> Int -> SourceSpan -> VisibleBinding
mkVisibleBinding hiddenStatementIndices statementIndex spanValue =
  VisibleBinding
    { visibleBindingSpan = spanValue,
      visibleBindingIsHiddenPrelude = statementIndex `Set.member` hiddenStatementIndices
    }

-- | Data constructors join the value namespace for analyzer visibility and
-- same-scope rebinding checks.
registerDataConstructors ::
  Set Int ->
  Int ->
  [DataConstructor 'Resolved] ->
  Map ResolvedName VisibleBinding ->
  Map ResolvedName VisibleBinding
registerDataConstructors hiddenStatementIndices statementIndex constructors bindings =
  foldl' register bindings constructors
  where
    register bindingsAcc (DataConstructor constructorNode constructorName _) =
      Map.insert
        (resolvedValueScopeName constructorName)
        (mkVisibleBinding hiddenStatementIndices statementIndex (coreNodeSpan constructorNode))
        bindingsAcc

collectDataConstructorRebindingWarnings ::
  WarningSettings ->
  Set Int ->
  Int ->
  [DataConstructor 'Resolved] ->
  Map ResolvedName VisibleBinding ->
  [Diagnostic]
collectDataConstructorRebindingWarnings
  settings
  hiddenStatementIndices
  statementIndex
  constructors
  bindings
    | not (isWarningEnabled settings SameScopeRebinding) = []
    | otherwise =
        reverse warningsRev
    where
      (_, warningsRev) = foldl' collect (bindings, []) constructors

      collect (bindingsAcc, warningsAcc) (DataConstructor constructorNode constructorName _) =
        let constructorSpan = coreNodeSpan constructorNode
            constructorBinding = mkVisibleBinding hiddenStatementIndices statementIndex constructorSpan
            constructorNameText = identifierText constructorName
            constructorValueName = resolvedValueScopeName constructorName
            warning =
              case Map.lookup constructorValueName bindingsAcc of
                Just previousBinding
                  | not (visibleBindingIsHiddenPrelude previousBinding) ->
                      [ mkSameScopeRebindingWarning
                          constructorNameText
                          constructorSpan
                          (visibleBindingSpan previousBinding)
                      ]
                _ -> []
         in ( Map.insert constructorValueName constructorBinding bindingsAcc,
              warning ++ warningsAcc
            )

visibleBindingDiagnosticSpan :: VisibleBinding -> Maybe SourceSpan
visibleBindingDiagnosticSpan visibleBinding =
  if visibleBindingIsHiddenPrelude visibleBinding
    then Nothing
    else Just (visibleBindingSpan visibleBinding)

collectOuterScopeShadowingWarnings ::
  WarningSettings ->
  ResolvedName ->
  SourceSpan ->
  Map ResolvedName VisibleBinding ->
  [Diagnostic]
collectOuterScopeShadowingWarnings settings bindingName primarySpan outerScope
  | not (isWarningEnabled settings ShadowingOuterScope) = []
  | otherwise =
      case Map.lookup (resolvedValueScopeName bindingName) outerScope of
        Just previousBinding
          | not (visibleBindingIsHiddenPrelude previousBinding) ->
              [ mkOuterScopeShadowingWarning
                  (identifierText bindingName)
                  primarySpan
                  (visibleBindingDiagnosticSpan previousBinding)
              ]
        _ -> []

mkOuterScopeShadowingWarning :: Text -> SourceSpan -> Maybe SourceSpan -> Diagnostic
mkOuterScopeShadowingWarning variableName primarySpan previousSpan =
  maybe id (\spanValue -> appendDiagnosticSecondaryLabel spanValue "previous") previousSpan $
    setDiagnosticPrimaryLabel primarySpan "warning emitted here" $
      setDiagnosticSubject variableName $
        mkWarningDiagnostic
          ShadowingOuterScope
          CompilationOrigin
          ( "outer-scope shadowing: '"
              <> variableName
              <> "' shadows a visible binding from an outer scope"
          )

lambdaShadowingSpan :: AnalysisContext -> Maybe SourceSpan
lambdaShadowingSpan context =
  case contextLambdaSpan context of
    Just spanValue -> Just spanValue
    Nothing -> contextPrimarySpan context

lambdaVisibleBinding :: VisibleBinding
lambdaVisibleBinding =
  VisibleBinding
    { visibleBindingSpan = SourceSpan 0 0,
      visibleBindingIsHiddenPrelude = True
    }

extendBindingsWithPattern :: Pattern 'Resolved -> Map ResolvedName VisibleBinding -> Map ResolvedName VisibleBinding
extendBindingsWithPattern pattern bindings =
  Set.foldl'
    (\bindingsAcc binderName -> Map.insert (resolvedValueScopeName binderName) patternVisibleBinding bindingsAcc)
    bindings
    (patternBinderNames pattern)

patternVisibleBinding :: VisibleBinding
patternVisibleBinding =
  VisibleBinding
    { visibleBindingSpan = SourceSpan 0 0,
      visibleBindingIsHiddenPrelude = True
    }
