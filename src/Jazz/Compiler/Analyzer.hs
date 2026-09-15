{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    CoreNode (coreNodeFacts, coreNodeSpan),
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
  )
import Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact,
    concreteImplFact,
    renderConcreteImplFact,
    splitQualifiedMethodKey,
  )
import Jazz.Compiler.CoreIdentity (CoreBinderId, resolvedOperatorSpelling)
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
    setDiagnosticPrimaryLabel,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject,
    sortWarnings,
  )
import Jazz.Compiler.Name
  ( ResolvedName,
    identifierPurity,
    identifierText,
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
    preparedRecursiveScopeGroups,
    preparedRecursiveScopeStatements,
  )
import Jazz.Compiler.WarningConfig
  ( WarningSettings,
    applyWarningPolicy,
    isWarningEnabled,
  )

-- | Analyzer output keeps the original expression plus the warnings/errors
-- discovered while walking it.
data AnalysisResult = AnalysisResult
  { analysisResolvedExpr :: Expr 'Resolved,
    analysisDiagnostics :: [Diagnostic]
  }

data AnalysisBinding = AnalysisBinding
  { analysisBindingSpan :: Maybe SourceSpan,
    analysisBindingIsHiddenPrelude :: Bool
  }
  deriving (Eq, Show)

data AnalysisInputs = AnalysisInputs
  { analysisWarningSettings :: WarningSettings,
    analysisExternalUses :: Set CoreBinderId,
    analysisImportedValues :: Map ResolvedName AnalysisBinding,
    analysisImportedClasses :: Set ResolvedName
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
analyzeProgram settings expr =
  analyzeProgramWithInputs
    AnalysisInputs
      { analysisWarningSettings = settings,
        analysisExternalUses = Set.empty,
        analysisImportedValues = Map.empty,
        analysisImportedClasses = Set.empty
      }
    False
    expr

analyzeProgramWithInputs :: AnalysisInputs -> Bool -> Expr 'Resolved -> IO AnalysisResult
analyzeProgramWithInputs inputs hideRootBindings expr =
  {-# SCC "jazz-stage:static-analysis" #-}
  analyzeProgramWithInputsAndDiagnostics inputs expr collectedDiagnostics
  where
    collectedDiagnostics =
      case expr of
        EBlock node statements ->
          either (const mempty) (collectScopeDiagnostics (analysisExternalUses inputs) hideRootBindings settings importedBindings importedClasses topLevelContext) (prepareResolvedScope node statements)
        _ ->
          collectExprDiagnostics settings importedBindings importedClasses topLevelContext expr
    settings = analysisWarningSettings inputs
    importedBindings = analysisVisibleBindings inputs
    importedClasses = Set.map identifierText (analysisImportedClasses inputs)

analyzeProgramWithInputsAndPreparedScope ::
  AnalysisInputs ->
  Bool ->
  Expr 'Resolved ->
  PreparedRecursiveScope 'Resolved ->
  IO AnalysisResult
analyzeProgramWithInputsAndPreparedScope inputs hideRootBindings expr preparedScope =
  {-# SCC "jazz-stage:static-analysis" #-}
  let analysisScope = preparedAnalysisScope preparedScope
      collectedDiagnostics =
        collectScopeDiagnosticsWithPreparedScope
          analysisScope
          (analysisExternalUses inputs)
          hideRootBindings
          (analysisWarningSettings inputs)
          (analysisVisibleBindings inputs)
          (Set.map identifierText (analysisImportedClasses inputs))
          topLevelContext
   in analysisScope `seq`
        expr `seq`
          analyzeProgramWithInputsAndDiagnostics inputs expr collectedDiagnostics

data PreparedAnalysisScope = PreparedAnalysisScope ![Statement 'Resolved] !(Map Int [Int])

preparedAnalysisScope :: PreparedRecursiveScope 'Resolved -> PreparedAnalysisScope
preparedAnalysisScope preparedScope =
  PreparedAnalysisScope
    (preparedRecursiveScopeStatements preparedScope)
    (preparedRecursiveScopeGroups preparedScope)

analyzeProgramWithInputsAndDiagnostics :: AnalysisInputs -> Expr 'Resolved -> CollectedDiagnostics -> IO AnalysisResult
analyzeProgramWithInputsAndDiagnostics inputs expr collectedDiagnostics =
  let (warnings, errors) = materializeDiagnostics collectedDiagnostics
      diagnostics =
        map (applyWarningPolicy settings) (sortWarnings warnings <> errors)
      result =
        AnalysisResult
          { analysisResolvedExpr = expr,
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
    EVar node _ | isJust (resolvedOperatorSpelling (coreNodeFacts node)) -> mempty
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
              parameterVisibleBinding
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
    EList _ elements -> foldMap collect elements
    ETuple _ elements -> foldMap collect elements
    EApply _ functionExpr argumentExpr ->
      let purityErrors =
            case directCallCalleeName functionExpr of
              Just calleeName
                | shouldRejectImpureCall visibleBindings visibleClassNames context calleeName ->
                    [ mkImpureCallInPureContextError
                        context
                        calleeName
                        (Map.lookup (resolvedValueScopeName calleeName) visibleBindings >>= visibleBindingDiagnosticSpan)
                    ]
              _ -> []
       in collect functionExpr <> collect argumentExpr <> errorDiagnostics purityErrors
    ETypeApplication _ functionExpr _ _ -> collect functionExpr
    EIf _ conditionExpr thenExpr elseExpr ->
      collect conditionExpr <> collect thenExpr <> collect elseExpr
    EPatternCase _ scrutineeExpr caseArms ->
      let armDiagnostics (CaseArm _ pattern guardExpr bodyExpr) =
            let collectArm = collectExprDiagnostics settings (extendBindingsWithPattern pattern visibleBindings) visibleClassNames context
             in foldMap collectArm guardExpr <> collectArm bodyExpr
       in collect scrutineeExpr <> foldMap armDiagnostics caseArms
    EBinary _ _ leftExpr rightExpr -> collect leftExpr <> collect rightExpr
    ESectionLeft _ leftExpr _ -> collect leftExpr
    ESectionRight _ _ rightExpr -> collect rightExpr
    EBlock node statements -> either (const mempty) (collectScopeDiagnostics Set.empty False settings visibleBindings visibleClassNames context) (prepareResolvedScope node statements)
  where
    collect = collectExprDiagnostics settings visibleBindings visibleClassNames context

-- | State carried between adjacent declarations. Each statement updates only
-- the scope facts it owns; diagnostics keep their source-ordered builders.
data AnalysisScope = AnalysisScope
  { scopeBindings :: Map ResolvedName VisibleBinding,
    classDeclarations :: Map Text SourceSpan,
    implDeclarations :: Map ConcreteImplFact SourceSpan,
    pendingSignature :: Maybe PendingSignature,
    scopeDiagnostics :: CollectedDiagnostics
  }

-- | Walk a block scope in declaration order, enforcing signature adjacency,
-- rebinding policy, and recursive-peer visibility at the same time.
collectScopeDiagnostics ::
  Set CoreBinderId ->
  Bool ->
  WarningSettings ->
  Map ResolvedName VisibleBinding ->
  Set Text ->
  AnalysisContext ->
  PreparedRecursiveScope 'Resolved ->
  CollectedDiagnostics
collectScopeDiagnostics externalUses hideRootBindings settings outerScope outerClassNames context preparedScope =
  collectScopeDiagnosticsWithPreparedScope
    (preparedAnalysisScope preparedScope)
    externalUses
    hideRootBindings
    settings
    outerScope
    outerClassNames
    context

collectScopeDiagnosticsWithPreparedScope ::
  PreparedAnalysisScope ->
  Set CoreBinderId ->
  Bool ->
  WarningSettings ->
  Map ResolvedName VisibleBinding ->
  Set Text ->
  AnalysisContext ->
  CollectedDiagnostics
collectScopeDiagnosticsWithPreparedScope (PreparedAnalysisScope statements rawRecursiveGroupsByStatement) externalUses hideRootBindings settings outerScope outerClassNames context =
  flushPendingSignature (pendingSignature finalScope) (scopeDiagnostics finalScope)
  where
    indexedStatements = zip [0 ..] statements

    -- Build recursion groups from local binding dependencies so mutually recursive
    -- bindings can reference each other independent of declaration order.
    recursiveGroupsByStatement =
      Map.map Set.fromList rawRecursiveGroupsByStatement
    bindingDeclarationsByStatement = collectBindingDeclarations indexedStatements
    unusedBindingWarningsByStatement =
      collectUnusedBindingWarnings
        settings
        hideRootBindings
        externalUses
        indexedStatements

    finalScope =
      foldl' step (AnalysisScope Map.empty Map.empty Map.empty Nothing mempty) indexedStatements

    step :: AnalysisScope -> (Int, Statement 'Resolved) -> AnalysisScope
    step
      current@AnalysisScope
        { scopeBindings,
          classDeclarations,
          implDeclarations,
          pendingSignature,
          scopeDiagnostics = diagnostics
        }
      (statementIndex, statement) =
        case statement of
          SExpr exprNode expr ->
            next
              { scopeDiagnostics =
                  scopeDiagnostics next
                    <> collectExprDiagnostics
                      settings
                      visible
                      visibleClasses
                      (contextForExpressionStatement (coreNodeSpan exprNode) context)
                      expr
              }
          SModule {} -> next
          SImport {} -> next
          SClass classNode capabilityName _parameters methods _ defaults ->
            let classSpan = coreNodeSpan classNode
                classNameText = identifierText capabilityName
                (nextClassDeclarations, classErrors) =
                  case Map.lookup classNameText classDeclarations of
                    Just previousSpan ->
                      ( classDeclarations,
                        [mkDuplicateClassDeclarationError classNameText classSpan (Just previousSpan)]
                      )
                    Nothing
                      | Set.member classNameText outerClassNames ->
                          ( classDeclarations,
                            [mkDuplicateClassDeclarationError classNameText classSpan Nothing]
                          )
                    Nothing ->
                      (Map.insert classNameText classSpan classDeclarations, [])
                methodBindings = foldl' (\bindings (ClassMethodSignature node name _) -> Map.insert (resolvedValueScopeName name) (VisibleBinding (coreNodeSpan node) hideRootBindings) bindings) scopeBindings methods
                methodErrors = duplicateClassMethodErrors classNameText methods <> methodCollisions statementIndex methods
                defaultDiagnostics = collectImplMethodDiagnostics settings (currentVisibleBindings methodBindings) (Set.insert classNameText visibleClasses) defaults
             in next
                  { classDeclarations = nextClassDeclarations,
                    scopeBindings = methodBindings,
                    scopeDiagnostics = scopeDiagnostics next <> errorDiagnostics (classErrors ++ methodErrors) <> defaultDiagnostics
                  }
          SImpl implNode capabilityName arguments methods _ ->
            let implSpan = coreNodeSpan implNode
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
                    visibleClasses
                    methods
             in next
                  { implDeclarations = nextImplDeclarations,
                    scopeDiagnostics = scopeDiagnostics next <> errorDiagnostics (implErrors ++ methodErrors) <> methodBodyDiagnostics
                  }
          SData _ _ _ constructors ->
            let register (currentBindings, currentWarnings) (DataConstructor node name _) =
                  let (registered, rebinding) = registerBinding settings hideRootBindings name (coreNodeSpan node) currentBindings
                   in (registered, currentWarnings <> warningDiagnostics rebinding)
                (bindings, warnings) = foldl' register (scopeBindings, mempty) constructors
             in next {scopeBindings = bindings, scopeDiagnostics = scopeDiagnostics next <> warnings}
          SSignature signatureNode signatureName _ ->
            next {pendingSignature = Just (PendingSignature (identifierText signatureName) (coreNodeSpan signatureNode))}
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
                (nextScope, rebindingWarning) =
                  registerBinding settings hideRootBindings bindingName bindingSpan scopeBindings
                shadowingWarning =
                  case Map.lookup bindingScopeName scopeBindings of
                    Just _ -> []
                    Nothing ->
                      collectOuterScopeShadowingWarnings
                        settings
                        bindingName
                        bindingSpan
                        outerScope
                bindingVisible =
                  -- Recursive peer names in the same SCC are visible while
                  -- analyzing the binding body.
                  withRecursivePeerBindings
                    statementIndex
                    (currentVisibleBindings nextScope)
                bindingContext = contextForBinding bindingName
                valueDiagnostics =
                  collectExprDiagnostics
                    settings
                    bindingVisible
                    visibleClasses
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
             in current {scopeBindings = nextScope, pendingSignature = Nothing, scopeDiagnostics = bindingDiagnostics}
        where
          visible = currentVisibleBindings scopeBindings
          visibleClasses = currentVisibleClassNames classDeclarations
          -- Every non-binding ends signature adjacency, including a new signature.
          next = current {pendingSignature = Nothing, scopeDiagnostics = flushPendingSignature pendingSignature diagnostics}

    methodCollisions index methods =
      [ setDiagnosticRelatedSpan
          previousSpan
          ( setDiagnosticPrimarySpan
              (coreNodeSpan node)
              (mkErrorDiagnostic E1007 CompilationOrigin ("duplicate value declaration '" <> identifierText name <> "'"))
          )
      | ClassMethodSignature node name _ <- methods,
        (otherIndex, other) <- indexedStatements,
        (otherName, previousSpan) <- case other of
          SLet otherNode otherName _ -> [(otherName, coreNodeSpan otherNode)]
          SClass _ _ _ otherMethods _ _ | otherIndex < index -> [(otherName, coreNodeSpan otherNode) | ClassMethodSignature otherNode otherName _ <- otherMethods]
          SData _ _ _ constructors -> [(otherName, coreNodeSpan otherNode) | DataConstructor otherNode otherName _ <- constructors]
          _ -> [],
        identifierText otherName == identifierText name
      ]

    currentVisibleBindings :: Map ResolvedName VisibleBinding -> Map ResolvedName VisibleBinding
    -- Local scope is left-biased so inner declarations shadow outer bindings.
    currentVisibleBindings scopeBindings = scopeBindings `Map.union` outerScope

    currentVisibleClassNames :: Map Text SourceSpan -> Set Text
    currentVisibleClassNames classDeclarations =
      Map.keysSet classDeclarations `Set.union` outerClassNames

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
              [ (resolvedValueScopeName peerName, mkVisibleBinding hideRootBindings peerSpan)
              | peerStatementIndex <- Set.toList peers,
                Just (peerName, peerSpan) <- [Map.lookup peerStatementIndex bindingDeclarationsByStatement],
                -- Do not override currently visible names (for example due to
                -- local rebinding) when adding recursive peers.
                Map.notMember (resolvedValueScopeName peerName) visibleNow
              ]
       in visibleNow `Map.union` peerEntries

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
  duplicateMethodErrors
    (mkDuplicateClassMethodError className)
    [(name, coreNodeSpan node) | ClassMethodSignature node name _ <- methods]

-- Report every repeated name against its first declaration, in source order.
duplicateMethodErrors :: (Text -> SourceSpan -> SourceSpan -> Diagnostic) -> [(ResolvedName, SourceSpan)] -> [Diagnostic]
duplicateMethodErrors diagnostic = reverse . snd . foldl' step (Map.empty, [])
  where
    step (seen, errors) (name, spanValue) =
      let text = identifierText name
       in case Map.lookup text seen of
            Just previous -> (seen, diagnostic text spanValue previous : errors)
            Nothing -> (Map.insert text spanValue seen, errors)

mkDuplicateClassMethodError :: Text -> Text -> SourceSpan -> SourceSpan -> Diagnostic
mkDuplicateClassMethodError className methodName methodSpan previousSpan =
  setDiagnosticSubject (className <> "." <> methodName) $
    setDiagnosticRelatedSpan previousSpan $
      setDiagnosticPrimarySpan
        methodSpan
        (mkErrorDiagnostic E1006 CompilationOrigin ("duplicate method signature '" <> methodName <> "' in class '" <> className <> "'"))

duplicateImplMethodErrors :: ResolvedName -> [SignatureType 'Resolved] -> [ImplMethod 'Resolved] -> [Diagnostic]
duplicateImplMethodErrors capabilityName arguments methods =
  duplicateMethodErrors
    (mkDuplicateImplMethodError implLabel)
    [(name, coreNodeSpan node) | ImplMethod node name _ <- methods]
  where
    implLabel = maybe (identifierText capabilityName) renderConcreteImplFact (concreteImplFact capabilityName arguments)

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
contextForBinding = contextForDeclaration "binding"

contextForImplMethod :: ResolvedName -> SourceSpan -> AnalysisContext
contextForImplMethod = contextForDeclaration "impl method"

contextForDeclaration :: Text -> ResolvedName -> SourceSpan -> AnalysisContext
contextForDeclaration label bindingName bindingSpan =
  AnalysisContext
    { contextLabel = label <> " '" <> identifierText bindingName <> "'",
      contextAllowsImpureCalls = identifierPurity bindingName == Impure,
      contextPrimarySpan = Just bindingSpan,
      contextSubject = Just (identifierText bindingName),
      contextLambdaSpan = Just bindingSpan
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
  maybe id setDiagnosticSubject (contextSubject context) $
    maybe id setDiagnosticPrimarySpan (contextPrimarySpan context) $
      maybe id setDiagnosticRelatedSpan maybeCalleeSpan $
        mkErrorDiagnostic
          E1010
          CompilationOrigin
          ( contextLabel context
              <> " cannot call impure callee '"
              <> identifierText calleeName
              <> "'"
          )

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

-- | The prelude artifact chooses whether its root bindings expose locations.
mkVisibleBinding :: Bool -> SourceSpan -> VisibleBinding
mkVisibleBinding hideRootBindings spanValue =
  VisibleBinding
    { visibleBindingSpan = spanValue,
      visibleBindingIsHiddenPrelude = hideRootBindings
    }

-- | Let bindings and data constructors enter the same value namespace and
-- share the same rebinding policy, including hidden-prelude suppression.
registerBinding :: WarningSettings -> Bool -> ResolvedName -> SourceSpan -> Map ResolvedName VisibleBinding -> (Map ResolvedName VisibleBinding, [Diagnostic])
registerBinding settings hideRootBindings name spanValue bindings =
  (Map.insert key (mkVisibleBinding hideRootBindings spanValue) bindings, warnings)
  where
    key = resolvedValueScopeName name
    warnings = case Map.lookup key bindings of
      Just previous
        | isWarningEnabled settings SameScopeRebinding,
          not (visibleBindingIsHiddenPrelude previous) ->
            [mkSameScopeRebindingWarning (identifierText name) spanValue (visibleBindingSpan previous)]
      _ -> []

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

parameterVisibleBinding :: VisibleBinding
parameterVisibleBinding =
  VisibleBinding
    { visibleBindingSpan = SourceSpan 0 0,
      visibleBindingIsHiddenPrelude = True
    }

extendBindingsWithPattern :: Pattern 'Resolved -> Map ResolvedName VisibleBinding -> Map ResolvedName VisibleBinding
extendBindingsWithPattern pattern bindings =
  Set.foldl'
    (\bindingsAcc binderName -> Map.insert (resolvedValueScopeName binderName) parameterVisibleBinding bindingsAcc)
    bindings
    (patternBinderNames pattern)
