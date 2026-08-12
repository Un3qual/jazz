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
    analyzeProgramWithBuiltinsAndHiddenStatements,
    analyzeProgramWithInputs,
    analyzeProgramWithInputsAndPreparedScope,
    analyzeProgramWithBuiltins,
    analyzeProgram,
    analyzeRebindingWarningsWithBuiltins,
    analyzeRebindingWarnings
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    SignatureType (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Pattern (..),
    Statement (..)
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    builtinNamesInMode,
    isBuiltinSymbolNameInMode
  )
import Jazz.Compiler.Analyzer.UnusedBindings
  ( collectUnusedBindingWarnings
  )
import Jazz.Compiler.CapabilityFacts
  ( concreteImplFactKey,
    splitQualifiedMethodKey
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    SourceSpan (..),
    appendDiagnosticSecondaryLabel,
    diagnosticWarningCategory,
    mkErrorDiagnostic,
    mkWarningDiagnostic,
    mkSameScopeRebindingWarning,
    promoteDiagnostic,
    setDiagnosticPrimaryLabel,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject,
    sortWarnings
  )
import Jazz.Compiler.Name
  ( Name,
    identifierPurity,
    identifierText,
    mkIdentifier,
    sourceName
  )
import Jazz.Compiler.Pattern
  ( patternBinderNames
  )
import Jazz.Compiler.RecursiveBindings
  ( PreparedRecursiveScope,
    prepareRecursiveScope,
    preparedRecursiveScopeGroups,
    preparedRecursiveScopeStatements,
  )
import Jazz.Compiler.Purity
  ( Purity (..)
  )
import Jazz.Compiler.WarningConfig
  ( WarningSettings,
    isWarningEnabled,
    isWarningError
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
    WarningCategory (..)
  )

-- | Analyzer output keeps the original expression plus the warnings/errors
-- discovered while walking it.
data AnalysisResult = AnalysisResult
  { analyzedExpr :: Expr,
    analysisDiagnostics :: [Diagnostic]
  }
  deriving (Eq, Show)

data AnalysisBinding = AnalysisBinding
  { analysisBindingSpan :: Maybe SourceSpan,
    analysisBindingIsHiddenPrelude :: Bool
  }
  deriving (Eq, Show)

data AnalysisInputs = AnalysisInputs
  { analysisBuiltinMode :: BuiltinResolutionMode,
    analysisWarningSettings :: WarningSettings,
    analysisImportedValues :: Map Name AnalysisBinding,
    analysisForwardFunctions :: Map Int (Name, AnalysisBinding),
    analysisImportedClasses :: Set Name,
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
analyzeProgram :: WarningSettings -> Expr -> IO AnalysisResult
analyzeProgram = analyzeProgramWithBuiltins ResolveKernelOnly

analyzeProgramWithBuiltins :: BuiltinResolutionMode -> WarningSettings -> Expr -> IO AnalysisResult
analyzeProgramWithBuiltins builtinMode =
  analyzeProgramWithBuiltinsAndHiddenStatements builtinMode Set.empty

-- | Analyzer entrypoint used by prelude/module flows. Hidden statement indices
-- suppress synthetic-source locations while preserving the same semantic walk
-- used for ordinary user code.
analyzeProgramWithBuiltinsAndHiddenStatements ::
  BuiltinResolutionMode ->
  Set Int ->
  WarningSettings ->
  Expr ->
  IO AnalysisResult
analyzeProgramWithBuiltinsAndHiddenStatements builtinMode hiddenStatementIndices settings expr =
  analyzeProgramWithInputs
    AnalysisInputs
      { analysisBuiltinMode = builtinMode,
        analysisWarningSettings = settings,
        analysisImportedValues = Map.empty,
        analysisForwardFunctions = Map.empty,
        analysisImportedClasses = Set.empty,
        analysisModulePath = Nothing
      }
    hiddenStatementIndices
    expr

analyzeProgramWithInputs :: AnalysisInputs -> Set Int -> Expr -> IO AnalysisResult
analyzeProgramWithInputs inputs hiddenStatementIndices expr =
  analyzeProgramWithInputsAndTarget inputs hiddenStatementIndices (AnalyzeExpression expr)

analyzeProgramWithInputsAndPreparedScope :: AnalysisInputs -> Set Int -> PreparedRecursiveScope -> IO AnalysisResult
analyzeProgramWithInputsAndPreparedScope inputs hiddenStatementIndices preparedScope =
  let analysisScope = preparedAnalysisScope preparedScope
   in analysisScope `seq`
        analyzeProgramWithInputsAndTarget inputs hiddenStatementIndices (AnalyzePreparedScope analysisScope)

data PreparedAnalysisScope = PreparedAnalysisScope ![Statement] !(Map Int [Int])

preparedAnalysisScope :: PreparedRecursiveScope -> PreparedAnalysisScope
preparedAnalysisScope preparedScope =
  PreparedAnalysisScope
    (preparedRecursiveScopeStatements preparedScope)
    (preparedRecursiveScopeGroups preparedScope)

data AnalysisTarget
  = AnalyzeExpression Expr
  | AnalyzePreparedScope PreparedAnalysisScope

analyzeProgramWithInputsAndTarget :: AnalysisInputs -> Set Int -> AnalysisTarget -> IO AnalysisResult
analyzeProgramWithInputsAndTarget inputs hiddenStatementIndices target =
  {-# SCC "jazz-stage:static-analysis" #-}
  let (expr, collectedDiagnostics) =
        case target of
          AnalyzePreparedScope analysisScope ->
            ( preparedScopeExpr analysisScope,
              collectScopeDiagnosticsWithPreparedScope analysisScope builtinMode hiddenStatementIndices settings importedBindings forwardBindings importedClasses topLevelContext
            )
          AnalyzeExpression expression@(EBlock statements) ->
            ( expression,
              collectScopeDiagnostics builtinMode hiddenStatementIndices settings importedBindings forwardBindings importedClasses topLevelContext statements
            )
          AnalyzeExpression expression ->
            ( expression,
              collectExprDiagnostics builtinMode settings importedBindings importedClasses topLevelContext expression
            )
      (warnings, errors) = materializeDiagnostics collectedDiagnostics
      diagnostics =
        map (applyWarningPolicy settings) (sortWarnings warnings <> errors)
      result =
        AnalysisResult
          { analyzedExpr = expr,
            analysisDiagnostics = diagnostics
          }
   in case target of
        AnalyzePreparedScope _ -> expr `seq` pure result
        AnalyzeExpression _ -> pure result
  where
    builtinMode = analysisBuiltinMode inputs
    settings = analysisWarningSettings inputs
    importedBindings = Map.map analysisBindingToVisibleBinding (analysisImportedValues inputs)
    forwardBindings =
      Map.map
        (\(name, binding) -> (name, analysisBindingToVisibleBinding binding))
        (analysisForwardFunctions inputs)
    importedClasses = Set.map identifierText (analysisImportedClasses inputs)

preparedScopeExpr :: PreparedAnalysisScope -> Expr
preparedScopeExpr (PreparedAnalysisScope statements _) = EBlock statements

analysisBindingToVisibleBinding :: AnalysisBinding -> VisibleBinding
analysisBindingToVisibleBinding binding =
  VisibleBinding
    { visibleBindingSpan = maybe (SourceSpan 0 0) id (analysisBindingSpan binding),
      visibleBindingIsHiddenPrelude =
        analysisBindingIsHiddenPrelude binding || analysisBindingSpan binding == Nothing
    }

-- | Append-efficient diagnostic streams. Builders compose in source order and
-- are materialized only at the public analyzer boundary.
data CollectedDiagnostics = CollectedDiagnostics
  { collectedWarningsBuilder :: [Diagnostic] -> [Diagnostic],
    collectedErrorsBuilder :: [Diagnostic] -> [Diagnostic]
  }

instance Semigroup CollectedDiagnostics where
  CollectedDiagnostics leftWarnings leftErrors <> CollectedDiagnostics rightWarnings rightErrors =
    CollectedDiagnostics
      (leftWarnings . rightWarnings)
      (leftErrors . rightErrors)

instance Monoid CollectedDiagnostics where
  mempty = CollectedDiagnostics id id

diagnosticsFromLists :: [Diagnostic] -> [Diagnostic] -> CollectedDiagnostics
diagnosticsFromLists warnings errors =
  CollectedDiagnostics (warnings ++) (errors ++)

warningDiagnostics :: [Diagnostic] -> CollectedDiagnostics
warningDiagnostics warnings = diagnosticsFromLists warnings []

errorDiagnostics :: [Diagnostic] -> CollectedDiagnostics
errorDiagnostics errors = diagnosticsFromLists [] errors

materializeDiagnostics :: CollectedDiagnostics -> ([Diagnostic], [Diagnostic])
materializeDiagnostics diagnostics =
  ( collectedWarningsBuilder diagnostics [],
    collectedErrorsBuilder diagnostics []
  )

analyzeRebindingWarnings :: WarningSettings -> Expr -> IO [Diagnostic]
analyzeRebindingWarnings = analyzeRebindingWarningsWithBuiltins ResolveKernelOnly

analyzeRebindingWarningsWithBuiltins :: BuiltinResolutionMode -> WarningSettings -> Expr -> IO [Diagnostic]
analyzeRebindingWarningsWithBuiltins builtinMode settings expr =
  filter (isJust . diagnosticWarningCategory) . analysisDiagnostics
    <$> analyzeProgramWithBuiltins builtinMode settings expr

applyWarningPolicy :: WarningSettings -> Diagnostic -> Diagnostic
applyWarningPolicy settings diagnostic =
  case diagnosticWarningCategory diagnostic of
    Just category
      | isWarningError settings category -> promoteDiagnostic diagnostic
    _ -> diagnostic

collectExprDiagnostics ::
  BuiltinResolutionMode ->
  WarningSettings ->
  Map Name VisibleBinding ->
  Set Text ->
  AnalysisContext ->
  Expr ->
  CollectedDiagnostics
collectExprDiagnostics builtinMode settings visibleBindings visibleClassNames context expr =
  case expr of
    ELit _ -> mempty
    EVar name ->
      case Map.lookup name visibleBindings of
        Just _ -> mempty
        Nothing
          | isBuiltinSymbolNameInMode builtinMode nameText -> mempty
          | qualifiedMethodClassIsVisible visibleClassNames nameText -> mempty
          | otherwise -> errorDiagnostics [mkUnboundVariableError nameText]
      where
        nameText = identifierText name
    ELambda parameterName bodyExpr ->
      let lambdaBindings =
            Map.insert
              parameterName
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
            collectExprDiagnostics builtinMode settings lambdaBindings visibleClassNames context bodyExpr
       in warningDiagnostics shadowingWarnings <> bodyDiagnostics
    EOperatorValue _ -> mempty
    EList elements ->
      collectExprListDiagnostics builtinMode settings visibleBindings visibleClassNames context elements
    ETuple elements ->
      collectExprListDiagnostics builtinMode settings visibleBindings visibleClassNames context elements
    EApply functionExpr argumentExpr ->
      let functionDiagnostics =
            collectExprDiagnostics builtinMode settings visibleBindings visibleClassNames context functionExpr
          argumentDiagnostics =
            collectExprDiagnostics builtinMode settings visibleBindings visibleClassNames context argumentExpr
          purityErrors =
            case directCallCalleeName functionExpr of
              Just calleeName
                | shouldRejectImpureCall builtinMode visibleBindings visibleClassNames context calleeName ->
                    [ mkImpureCallInPureContextError
                        context
                        calleeName
                        (Map.lookup calleeName visibleBindings >>= visibleBindingDiagnosticSpan)
                    ]
              _ -> []
       in functionDiagnostics <> argumentDiagnostics <> errorDiagnostics purityErrors
    ETypeApplication functionExpr _ _ ->
      collectExprDiagnostics builtinMode settings visibleBindings visibleClassNames context functionExpr
    EIf conditionExpr thenExpr elseExpr ->
      let conditionDiagnostics =
            collectExprDiagnostics builtinMode settings visibleBindings visibleClassNames context conditionExpr
          thenDiagnostics =
            collectExprDiagnostics builtinMode settings visibleBindings visibleClassNames context thenExpr
          elseDiagnostics =
            collectExprDiagnostics builtinMode settings visibleBindings visibleClassNames context elseExpr
       in conditionDiagnostics <> thenDiagnostics <> elseDiagnostics
    EPatternCase scrutineeExpr caseArms ->
      let scrutineeDiagnostics =
            collectExprDiagnostics builtinMode settings visibleBindings visibleClassNames context scrutineeExpr
          armDiagnostics (CaseArm pattern guardExpr bodyExpr) =
            let armBindings = extendBindingsWithPattern pattern visibleBindings
                guardDiagnostics =
                  maybe
                    mempty
                    ( collectExprDiagnostics
                        builtinMode
                        settings
                        armBindings
                        visibleClassNames
                        context
                    )
                    guardExpr
                bodyDiagnostics =
                  collectExprDiagnostics
                    builtinMode
                    settings
                    armBindings
                    visibleClassNames
                    context
                    bodyExpr
             in guardDiagnostics <> bodyDiagnostics
       in scrutineeDiagnostics <> foldMap armDiagnostics caseArms
    EBinary _ leftExpr rightExpr ->
      let leftDiagnostics =
            collectExprDiagnostics builtinMode settings visibleBindings visibleClassNames context leftExpr
          rightDiagnostics =
            collectExprDiagnostics builtinMode settings visibleBindings visibleClassNames context rightExpr
       in leftDiagnostics <> rightDiagnostics
    ESectionLeft leftExpr _ ->
      collectExprDiagnostics builtinMode settings visibleBindings visibleClassNames context leftExpr
    ESectionRight _ rightExpr ->
      collectExprDiagnostics builtinMode settings visibleBindings visibleClassNames context rightExpr
    EBlock statements -> collectScopeDiagnostics builtinMode Set.empty settings visibleBindings Map.empty visibleClassNames context statements

collectExprListDiagnostics ::
  BuiltinResolutionMode ->
  WarningSettings ->
  Map Name VisibleBinding ->
  Set Text ->
  AnalysisContext ->
  [Expr] ->
  CollectedDiagnostics
collectExprListDiagnostics builtinMode settings visibleBindings visibleClassNames context elements =
  foldMap
    (collectExprDiagnostics builtinMode settings visibleBindings visibleClassNames context)
    elements

-- | Walk a block scope in declaration order, enforcing signature adjacency,
-- rebinding policy, and recursive-peer visibility at the same time.
collectScopeDiagnostics ::
  BuiltinResolutionMode ->
  Set Int ->
  WarningSettings ->
  Map Name VisibleBinding ->
  Map Int (Name, VisibleBinding) ->
  Set Text ->
  AnalysisContext ->
  [Statement] ->
  CollectedDiagnostics
collectScopeDiagnostics builtinMode hiddenStatementIndices settings outerScope forwardBindings outerClassNames context statements =
  collectScopeDiagnosticsWithPreparedScope
    ( preparedAnalysisScope
        ( prepareRecursiveScope
            (Set.union (Map.keysSet outerScope) (Set.map (sourceName . mkIdentifier) (builtinNamesInMode builtinMode)))
            statements
        )
    )
    builtinMode
    hiddenStatementIndices
    settings
    outerScope
    forwardBindings
    outerClassNames
    context

collectScopeDiagnosticsWithPreparedScope ::
  PreparedAnalysisScope ->
  BuiltinResolutionMode ->
  Set Int ->
  WarningSettings ->
  Map Name VisibleBinding ->
  Map Int (Name, VisibleBinding) ->
  Set Text ->
  AnalysisContext ->
  CollectedDiagnostics
collectScopeDiagnosticsWithPreparedScope (PreparedAnalysisScope statements rawRecursiveGroupsByStatement) builtinMode hiddenStatementIndices settings outerScope forwardBindings outerClassNames context =
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
        recursiveGroupsByStatement
        indexedStatements

    -- Diagnostics use source-ordered builders for O(1) append.
    -- `pendingSignature` tracks exactly one immediately-preceding signature that
    -- must be consumed by the next binding.
    (_, _, _, _, finalPendingSignature, finalDiagnostics) =
      foldl' step (Map.empty, Map.empty, Set.empty, Map.empty, Nothing, mempty) indexedStatements

    step ::
      (Map Name VisibleBinding, Map Text SourceSpan, Set Text, Map Text SourceSpan, Maybe PendingSignature, CollectedDiagnostics) ->
      (Int, Statement) ->
      (Map Name VisibleBinding, Map Text SourceSpan, Set Text, Map Text SourceSpan, Maybe PendingSignature, CollectedDiagnostics)
    step (scopeBindings, classDeclarations, importedClassNames, implDeclarations, pendingSignature, diagnostics) (statementIndex, statement) =
      case statement of
        SExpr exprSpan expr ->
          -- Any signature followed by a non-binding is invalid by contract.
          let diagnosticsWithPending = flushPendingSignature pendingSignature diagnostics
              visible = currentVisibleBindings scopeBindings
              exprDiagnostics =
                collectExprDiagnostics
                  builtinMode
                  settings
                  visible
                  (currentVisibleClassNames classDeclarations importedClassNames)
                  (contextForExpressionStatement exprSpan context)
                  expr
           in
            ( scopeBindings,
              classDeclarations,
              importedClassNames,
              implDeclarations,
              Nothing,
              diagnosticsWithPending <> exprDiagnostics
            )
        SModule {} ->
          let diagnosticsWithPending = flushPendingSignature pendingSignature diagnostics
           in
            ( scopeBindings,
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
           in
            ( scopeBindings,
              classDeclarations,
              nextImportedClassNames,
              implDeclarations,
              Nothing,
              diagnosticsWithPending
            )
        SClass classSpan capabilityName _parameters methods ->
          let diagnosticsWithPending = flushPendingSignature pendingSignature diagnostics
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
           in
            ( scopeBindings,
              nextClassDeclarations,
              importedClassNames,
              implDeclarations,
              Nothing,
              diagnosticsWithPending <> errorDiagnostics (classErrors ++ methodErrors)
            )
        SImpl implSpan capabilityName arguments methods ->
          let diagnosticsWithPending = flushPendingSignature pendingSignature diagnostics
              visible = currentVisibleBindings scopeBindings
              (nextImplDeclarations, implErrors) =
                case concreteImplFactKey capabilityName arguments of
                  Nothing ->
                    (implDeclarations, [])
                  Just implFactKey ->
                    case Map.lookup implFactKey implDeclarations of
                      Just previousSpan ->
                        ( implDeclarations,
                          [mkDuplicateImplDeclarationError implFactKey implSpan previousSpan]
                        )
                      Nothing ->
                        (Map.insert implFactKey implSpan implDeclarations, [])
              methodErrors = duplicateImplMethodErrors capabilityName arguments methods
              methodBodyDiagnostics =
                collectImplMethodDiagnostics
                  builtinMode
                  settings
                  visible
                  (currentVisibleClassNames classDeclarations importedClassNames)
                  methods
           in
            ( scopeBindings,
              classDeclarations,
              importedClassNames,
              nextImplDeclarations,
              Nothing,
              diagnosticsWithPending
                <> errorDiagnostics (implErrors ++ methodErrors)
                <> methodBodyDiagnostics
            )
        SData spanValue _ _ constructors ->
          let diagnosticsWithPending = flushPendingSignature pendingSignature diagnostics
              constructorWarnings =
                collectDataConstructorRebindingWarnings
                  settings
                  hiddenStatementIndices
                  statementIndex
                  spanValue
                  constructors
                  scopeBindings
           in
            ( registerDataConstructors
                hiddenStatementIndices
                statementIndex
                spanValue
                constructors
                scopeBindings,
              classDeclarations,
              importedClassNames,
              implDeclarations,
              Nothing,
              diagnosticsWithPending <> warningDiagnostics constructorWarnings
            )
        SSignature signatureName signatureSpan _signatureText ->
          -- Signature payload text is carried forward for future type parsing.
          -- This pass only enforces placement/name coherence.
          let diagnosticsWithPending = flushPendingSignature pendingSignature diagnostics
           in
            ( scopeBindings,
              classDeclarations,
              importedClassNames,
              implDeclarations,
              Just (PendingSignature (identifierText signatureName) signatureSpan),
              diagnosticsWithPending
            )
        SLet bindingName bindingSpan valueExpr ->
          -- Bindings consume a pending signature if names match. Rebinding
          -- stays semantically valid but may emit an optional warning.
          let bindingNameText = identifierText bindingName
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
                case Map.lookup bindingName scopeBindings of
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
                case Map.lookup bindingName scopeBindings of
                  Just _ -> []
                  Nothing ->
                    collectOuterScopeShadowingWarnings
                      settings
                      bindingName
                      bindingSpan
                      outerScope
              nextScope =
                Map.insert
                  bindingName
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
                  builtinMode
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
           in
            ( nextScope,
              classDeclarations,
              importedClassNames,
              implDeclarations,
              Nothing,
              bindingDiagnostics
            )

    currentVisibleBindings :: Map Name VisibleBinding -> Map Name VisibleBinding
    -- Local scope is left-biased so inner declarations shadow outer bindings.
    currentVisibleBindings scopeBindings = scopeBindings `Map.union` outerScope

    currentVisibleClassNames :: Map Text SourceSpan -> Set Text -> Set Text
    currentVisibleClassNames classDeclarations importedClassNames =
      Map.keysSet classDeclarations `Set.union` importedClassNames `Set.union` outerClassNames

    collectModuleBaselineClassDeclarations :: [(Int, Statement)] -> Map Text SourceSpan
    collectModuleBaselineClassDeclarations indexedScopeStatements =
      case [statementIndex | (statementIndex, SModule {}) <- indexedScopeStatements] of
        [] -> Map.empty
        firstModuleStatementIndex : _ ->
          Map.fromList
            [ (identifierText className, classSpan)
              | (statementIndex, SClass classSpan className _ _) <- indexedScopeStatements,
                statementIndex < firstModuleStatementIndex
            ]

    collectModuleClassDeclarations :: [(Int, Statement)] -> Map [Text] (Map Text SourceSpan)
    collectModuleClassDeclarations =
      snd . foldl' collectModuleClassDeclaration (Nothing, Map.empty)
      where
        collectModuleClassDeclaration (currentModulePath, declarationsByPath) (_, statement) =
          case statement of
            SModule _ modulePath ->
              (Just modulePath, declarationsByPath)
            SClass classSpan className _ _ ->
              case currentModulePath of
                Just modulePath ->
                  ( currentModulePath,
                    Map.insertWith
                      Map.union
                      modulePath
                      (Map.singleton (identifierText className) classSpan)
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
      Map Name VisibleBinding ->
      Map Name VisibleBinding
    withRecursivePeerBindings statementIndex visibleNow =
      let peers =
            Set.delete
              statementIndex
              (Map.findWithDefault Set.empty statementIndex recursiveGroupsByStatement)
          peerEntries =
            Map.fromList
              [ (peerName, mkVisibleBinding hiddenStatementIndices peerStatementIndex peerSpan)
                | peerStatementIndex <- Set.toList peers,
                  Just (peerName, peerSpan) <- [Map.lookup peerStatementIndex bindingDeclarationsByStatement],
                  -- Do not override currently visible names (for example due to
                  -- local rebinding) when adding recursive peers.
                  Map.notMember peerName visibleNow
              ]
       in visibleNow `Map.union` peerEntries

    withForwardFunctionBindings ::
      Int ->
      Map Name VisibleBinding ->
      Map Name VisibleBinding
    withForwardFunctionBindings statementIndex visibleNow =
      case Map.lookup statementIndex forwardBindings of
        Nothing -> visibleNow
        Just _ ->
          foldl'
            (\visibleAcc (_, (name, binding)) -> Map.insertWith (\_ existing -> existing) name binding visibleAcc)
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
            E1002 CompilationOrigin
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
                E1003 CompilationOrigin
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

duplicateClassMethodErrors :: Text -> [ClassMethodSignature] -> [Diagnostic]
duplicateClassMethodErrors className methods =
  reverse errorsRev
  where
    (_, errorsRev) = foldl' step (Map.empty, []) methods
    step (seenMethods, acc) (ClassMethodSignature methodName methodSpan _) =
      let methodNameText = identifierText methodName
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

duplicateImplMethodErrors :: Name -> [SignatureType] -> [ImplMethod] -> [Diagnostic]
duplicateImplMethodErrors capabilityName arguments methods =
  reverse errorsRev
  where
    implLabel =
      case concreteImplFactKey capabilityName arguments of
        Just implFactKey -> implFactKey
        Nothing -> identifierText capabilityName
    (_, errorsRev) = foldl' step (Map.empty, []) methods
    step (seenMethods, acc) (ImplMethod methodName methodSpan _) =
      let methodNameText = identifierText methodName
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
  BuiltinResolutionMode ->
  WarningSettings ->
  Map Name VisibleBinding ->
  Set Text ->
  [ImplMethod] ->
  CollectedDiagnostics
collectImplMethodDiagnostics builtinMode settings visibleBindings visibleClassNames methods =
  foldMap collectMethodDiagnostics methods
  where
    collectMethodDiagnostics (ImplMethod methodName methodSpan methodExpr) =
      collectExprDiagnostics
        builtinMode
        settings
        visibleBindings
        visibleClassNames
        (contextForImplMethod methodName methodSpan)
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
contextForBinding :: Name -> SourceSpan -> AnalysisContext
contextForBinding bindingName bindingSpan =
  AnalysisContext
    { contextLabel = "binding '" <> identifierText bindingName <> "'",
      contextAllowsImpureCalls = identifierPurity bindingName == Impure,
      contextPrimarySpan = Just bindingSpan,
      contextSubject = Just (identifierText bindingName),
      contextLambdaSpan = Just bindingSpan
    }

contextForImplMethod :: Name -> SourceSpan -> AnalysisContext
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
  BuiltinResolutionMode ->
  Map Name VisibleBinding ->
  Set Text ->
  AnalysisContext ->
  Name ->
  Bool
shouldRejectImpureCall builtinMode visibleBindings visibleClassNames context calleeName =
  not (contextAllowsImpureCalls context)
    && isKnownImpureCallee
  where
    calleeNameText = identifierText calleeName
    isKnownImpureCallee =
      identifierPurity calleeName == Impure
        && ( Map.member calleeName visibleBindings
               || isBuiltinSymbolNameInMode builtinMode calleeNameText
               || qualifiedMethodClassIsVisible visibleClassNames calleeNameText
           )

directCallCalleeName :: Expr -> Maybe Name
directCallCalleeName expr =
  case expr of
    EVar calleeName -> Just calleeName
    ETypeApplication functionExpr _ _ -> directCallCalleeName functionExpr
    _ -> Nothing

mkImpureCallInPureContextError ::
  AnalysisContext ->
  Name ->
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
                E1010 CompilationOrigin
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
  [(Int, Statement)] ->
  Map Int (Name, SourceSpan)
collectBindingDeclarations =
  foldl' collect Map.empty
  where
    collect declarations (statementIndex, statement) =
      case statement of
        SLet name spanValue _ ->
          Map.insert statementIndex (name, spanValue) declarations
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
  SourceSpan ->
  [DataConstructor] ->
  Map Name VisibleBinding ->
  Map Name VisibleBinding
registerDataConstructors hiddenStatementIndices statementIndex spanValue constructors bindings =
  foldl' register bindings constructors
  where
    constructorBinding = mkVisibleBinding hiddenStatementIndices statementIndex spanValue
    register bindingsAcc (DataConstructor constructorName _) =
      Map.insert constructorName constructorBinding bindingsAcc

collectDataConstructorRebindingWarnings ::
  WarningSettings ->
  Set Int ->
  Int ->
  SourceSpan ->
  [DataConstructor] ->
  Map Name VisibleBinding ->
  [Diagnostic]
collectDataConstructorRebindingWarnings
  settings
  hiddenStatementIndices
  statementIndex
  spanValue
  constructors
  bindings
  | not (isWarningEnabled settings SameScopeRebinding) = []
  | otherwise =
      reverse warningsRev
  where
    constructorBinding = mkVisibleBinding hiddenStatementIndices statementIndex spanValue
    (_, warningsRev) = foldl' collect (bindings, []) constructors

    collect (bindingsAcc, warningsAcc) (DataConstructor constructorName _) =
      let constructorNameText = identifierText constructorName
          warning =
            case Map.lookup constructorName bindingsAcc of
              Just previousBinding
                | not (visibleBindingIsHiddenPrelude previousBinding) ->
                    [ mkSameScopeRebindingWarning
                        constructorNameText
                        spanValue
                        (visibleBindingSpan previousBinding)
                    ]
              _ -> []
       in
        ( Map.insert constructorName constructorBinding bindingsAcc,
          warning ++ warningsAcc
        )

visibleBindingDiagnosticSpan :: VisibleBinding -> Maybe SourceSpan
visibleBindingDiagnosticSpan visibleBinding =
  if visibleBindingIsHiddenPrelude visibleBinding
    then Nothing
    else Just (visibleBindingSpan visibleBinding)

collectOuterScopeShadowingWarnings ::
  WarningSettings ->
  Name ->
  SourceSpan ->
  Map Name VisibleBinding ->
  [Diagnostic]
collectOuterScopeShadowingWarnings settings bindingName primarySpan outerScope
  | not (isWarningEnabled settings ShadowingOuterScope) = []
  | otherwise =
      case Map.lookup bindingName outerScope of
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
extendBindingsWithPattern :: Pattern -> Map Name VisibleBinding -> Map Name VisibleBinding
extendBindingsWithPattern pattern bindings =
  Set.foldl'
    (\bindingsAcc binderName -> Map.insert binderName patternVisibleBinding bindingsAcc)
    bindings
    (patternBinderNames pattern)

patternVisibleBinding :: VisibleBinding
patternVisibleBinding =
  VisibleBinding
    { visibleBindingSpan = SourceSpan 0 0,
      visibleBindingIsHiddenPrelude = True
    }
