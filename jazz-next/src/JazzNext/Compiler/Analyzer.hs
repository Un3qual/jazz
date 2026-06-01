{-# LANGUAGE OverloadedStrings #-}

-- | Semantic analysis for the current compiler slice. This pass keeps the core
-- AST shape intact while enforcing scope visibility, signature adjacency, and
-- stub-v1 purity/rebinding rules.
module JazzNext.Compiler.Analyzer
  ( Expr (..),
    Statement (..),
    AnalysisResult (..),
    analyzeProgramWithBuiltinsAndHiddenStatements,
    analyzeProgramWithBuiltins,
    analyzeProgram,
    analyzeRebindingWarningsWithBuiltins,
    analyzeRebindingWarnings
  ) where

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    ConstraintSignatureType (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    Pattern (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    builtinNamesInMode,
    isBuiltinSymbolNameInMode
  )
import JazzNext.Compiler.CapabilityFacts
  ( concreteImplFactKey
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    WarningRecord (..),
    mkDiagnostic,
    mkSameScopeRebindingWarning,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject,
    sortWarnings
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierPurity,
    identifierText
  )
import JazzNext.Compiler.RecursiveBindings
  ( freeVarsExprWithBound,
    inferRecursiveGroupsOrdered
  )
import JazzNext.Compiler.Purity
  ( Purity (..)
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    isWarningEnabled
  )
import JazzNext.Compiler.Warnings
  ( WarningCategory (..),
    warningCode
  )

-- | Analyzer output keeps the original expression plus the warnings/errors
-- discovered while walking it.
data AnalysisResult = AnalysisResult
  { analyzedExpr :: Expr,
    analysisWarnings :: [WarningRecord],
    analysisErrors :: [Diagnostic]
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
  let (warnings, errors) =
        case expr of
          EBlock statements ->
            collectScopeDiagnostics builtinMode hiddenStatementIndices settings Map.empty topLevelContext statements
          _ ->
            collectExprDiagnostics builtinMode settings Map.empty topLevelContext expr
   in
    pure
      AnalysisResult
        { analyzedExpr = expr,
          analysisWarnings = sortWarnings warnings,
          analysisErrors = errors
        }

analyzeRebindingWarnings :: WarningSettings -> Expr -> IO [WarningRecord]
analyzeRebindingWarnings = analyzeRebindingWarningsWithBuiltins ResolveKernelOnly

analyzeRebindingWarningsWithBuiltins :: BuiltinResolutionMode -> WarningSettings -> Expr -> IO [WarningRecord]
analyzeRebindingWarningsWithBuiltins builtinMode settings expr =
  analysisWarnings <$> analyzeProgramWithBuiltins builtinMode settings expr

collectExprDiagnostics ::
  BuiltinResolutionMode ->
  WarningSettings ->
  Map Text VisibleBinding ->
  AnalysisContext ->
  Expr ->
  ([WarningRecord], [Diagnostic])
collectExprDiagnostics builtinMode settings visibleBindings context expr =
  case expr of
    ELit _ -> ([], [])
    EVar name ->
      case Map.lookup nameText visibleBindings of
        Just _ -> ([], [])
        Nothing
          | isBuiltinSymbolNameInMode builtinMode nameText -> ([], [])
          | otherwise -> ([], [mkUnboundVariableError nameText])
      where
        nameText = identifierText name
    ELambda parameterName bodyExpr ->
      let lambdaBindings =
            Map.insert
              parameterNameText
              lambdaVisibleBinding
              visibleBindings
          shadowingWarnings =
            case lambdaShadowingSpan context of
              Nothing -> []
              Just primarySpan ->
                collectOuterScopeShadowingWarnings
                  settings
                  parameterNameText
                  primarySpan
                  visibleBindings
          (bodyWarnings, bodyErrors) =
            collectExprDiagnostics builtinMode settings lambdaBindings context bodyExpr
       in (shadowingWarnings ++ bodyWarnings, bodyErrors)
      where
        parameterNameText = identifierText parameterName
    EOperatorValue _ -> ([], [])
    EList elements ->
      collectExprListDiagnostics builtinMode settings visibleBindings context elements
    ETuple elements ->
      collectExprListDiagnostics builtinMode settings visibleBindings context elements
    EApply functionExpr argumentExpr ->
      let (functionWarnings, functionErrors) =
            collectExprDiagnostics builtinMode settings visibleBindings context functionExpr
          (argumentWarnings, argumentErrors) =
            collectExprDiagnostics builtinMode settings visibleBindings context argumentExpr
          purityErrors =
            case functionExpr of
              EVar calleeName
                | shouldRejectImpureCall builtinMode visibleBindings context calleeName ->
                    [ mkImpureCallInPureContextError
                        context
                        calleeName
                        (Map.lookup (identifierText calleeName) visibleBindings >>= visibleBindingDiagnosticSpan)
                    ]
              _ -> []
       in
        ( functionWarnings ++ argumentWarnings,
          functionErrors ++ argumentErrors ++ purityErrors
        )
    EIf conditionExpr thenExpr elseExpr ->
      collectExprDiagnostics builtinMode settings visibleBindings context (ECase conditionExpr thenExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr ->
      let (conditionWarnings, conditionErrors) =
            collectExprDiagnostics builtinMode settings visibleBindings context conditionExpr
          (thenWarnings, thenErrors) =
            collectExprDiagnostics builtinMode settings visibleBindings context thenExpr
          (elseWarnings, elseErrors) =
            collectExprDiagnostics builtinMode settings visibleBindings context elseExpr
       in
        ( conditionWarnings ++ thenWarnings ++ elseWarnings,
          conditionErrors ++ thenErrors ++ elseErrors
        )
    EPatternCase scrutineeExpr caseArms ->
      let (scrutineeWarnings, scrutineeErrors) =
            collectExprDiagnostics builtinMode settings visibleBindings context scrutineeExpr
          armResults =
            map
              ( \(CaseArm pattern bodyExpr) ->
                  collectExprDiagnostics
                    builtinMode
                    settings
                    (extendBindingsWithPattern pattern visibleBindings)
                    context
                    bodyExpr
              )
              caseArms
       in
        ( scrutineeWarnings ++ concatMap fst armResults,
          scrutineeErrors ++ concatMap snd armResults
        )
    EBinary _ leftExpr rightExpr ->
      let (leftWarnings, leftErrors) =
            collectExprDiagnostics builtinMode settings visibleBindings context leftExpr
          (rightWarnings, rightErrors) =
            collectExprDiagnostics builtinMode settings visibleBindings context rightExpr
       in
        (leftWarnings ++ rightWarnings, leftErrors ++ rightErrors)
    ESectionLeft leftExpr _ ->
      collectExprDiagnostics builtinMode settings visibleBindings context leftExpr
    ESectionRight _ rightExpr ->
      collectExprDiagnostics builtinMode settings visibleBindings context rightExpr
    EBlock statements -> collectScopeDiagnostics builtinMode Set.empty settings visibleBindings context statements

collectExprListDiagnostics ::
  BuiltinResolutionMode ->
  WarningSettings ->
  Map Text VisibleBinding ->
  AnalysisContext ->
  [Expr] ->
  ([WarningRecord], [Diagnostic])
collectExprListDiagnostics builtinMode settings visibleBindings context elements =
  let (warningsRev, errorsRev) =
        foldl'
          step
          ([], [])
          elements
   in (concat (reverse warningsRev), concat (reverse errorsRev))
  where
    step (warningsRev, errorsRev) element =
      let (elementWarnings, elementErrors) =
            collectExprDiagnostics builtinMode settings visibleBindings context element
       in
        (elementWarnings : warningsRev, elementErrors : errorsRev)

-- | Walk a block scope in declaration order, enforcing signature adjacency,
-- rebinding policy, and recursive-peer visibility at the same time.
collectScopeDiagnostics ::
  BuiltinResolutionMode ->
  Set Int ->
  WarningSettings ->
  Map Text VisibleBinding ->
  AnalysisContext ->
  [Statement] ->
  ([WarningRecord], [Diagnostic])
collectScopeDiagnostics builtinMode hiddenStatementIndices settings outerScope context statements =
  (reverse finalWarningsRev, reverse errorsWithFinalPending)
  where
    indexedStatements = zip [0 ..] statements

    -- Build recursion groups from local binding dependencies so mutually recursive
    -- bindings can reference each other independent of declaration order.
    recursiveGroupsByStatement =
      Map.map
        Set.fromList
        ( inferRecursiveGroupsOrdered
            (Set.union (Map.keysSet outerScope) (builtinNamesInMode builtinMode))
            indexedStatements
        )
    bindingDeclarationsByStatement = collectBindingDeclarations indexedStatements
    unusedBindingWarningsByStatement =
      collectUnusedBindingWarnings
        settings
        hiddenStatementIndices
        indexedStatements

    -- Internal accumulators are built in reverse for O(1) append.
    -- `pendingSignature` tracks exactly one immediately-preceding signature that
    -- must be consumed by the next binding.
    (_, _, _, finalPendingSignature, finalWarningsRev, finalErrorsRev) =
      foldl' step (Map.empty, Map.empty, Map.empty, Nothing, [], []) indexedStatements
    errorsWithFinalPending = flushPendingSignature finalPendingSignature finalErrorsRev

    step ::
      (Map Text VisibleBinding, Map Text SourceSpan, Map Text SourceSpan, Maybe PendingSignature, [WarningRecord], [Diagnostic]) ->
      (Int, Statement) ->
      (Map Text VisibleBinding, Map Text SourceSpan, Map Text SourceSpan, Maybe PendingSignature, [WarningRecord], [Diagnostic])
    step (scopeBindings, classDeclarations, implDeclarations, pendingSignature, warningsRev, errorsRev) (statementIndex, statement) =
      case statement of
        SExpr exprSpan expr ->
          -- Any signature followed by a non-binding is invalid by contract.
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
              visible = currentVisibleBindings scopeBindings
              (exprWarnings, exprErrors) =
                collectExprDiagnostics
                  builtinMode
                  settings
                  visible
                  (contextForExpressionStatement exprSpan context)
                  expr
           in
            ( scopeBindings,
              classDeclarations,
              implDeclarations,
              Nothing,
              appendWarnings warningsRev exprWarnings,
              appendErrors errorsWithPending exprErrors
            )
        SModule {} ->
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
           in
            ( scopeBindings,
              Map.empty,
              Map.empty,
              Nothing,
              warningsRev,
              errorsWithPending
            )
        SImport {} ->
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
           in
            ( scopeBindings,
              classDeclarations,
              implDeclarations,
              Nothing,
              warningsRev,
              errorsWithPending
            )
        SClass classSpan capabilityName _parameters methods ->
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
              classNameText = identifierText capabilityName
              (nextClassDeclarations, classErrors) =
                case Map.lookup classNameText classDeclarations of
                  Just previousSpan ->
                    ( classDeclarations,
                      [mkDuplicateClassDeclarationError classNameText classSpan previousSpan]
                    )
                  Nothing ->
                    (Map.insert classNameText classSpan classDeclarations, [])
              methodErrors = duplicateClassMethodErrors classNameText methods
           in
            ( scopeBindings,
              nextClassDeclarations,
              implDeclarations,
              Nothing,
              warningsRev,
              appendErrors errorsWithPending (classErrors ++ methodErrors)
            )
        SImpl implSpan capabilityName arguments methods ->
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
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
              (methodWarnings, methodBodyErrors) =
                collectImplMethodDiagnostics builtinMode settings visible methods
           in
            ( scopeBindings,
              classDeclarations,
              nextImplDeclarations,
              Nothing,
              appendWarnings warningsRev methodWarnings,
              appendErrors errorsWithPending (implErrors ++ methodErrors ++ methodBodyErrors)
            )
        SData spanValue _ _ constructors ->
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
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
              implDeclarations,
              Nothing,
              appendWarnings warningsRev constructorWarnings,
              errorsWithPending
            )
        SSignature signatureName signatureSpan _signatureText ->
          -- Signature payload text is carried forward for future type parsing.
          -- This pass only enforces placement/name coherence.
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
           in
            ( scopeBindings,
              classDeclarations,
              implDeclarations,
              Just (PendingSignature (identifierText signatureName) signatureSpan),
              warningsRev,
              errorsWithPending
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
                case Map.lookup bindingNameText scopeBindings of
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
                case Map.lookup bindingNameText scopeBindings of
                  Just _ -> []
                  Nothing ->
                    collectOuterScopeShadowingWarnings
                      settings
                      bindingNameText
                      bindingSpan
                      outerScope
              nextScope =
                Map.insert
                  bindingNameText
                  (mkVisibleBinding hiddenStatementIndices statementIndex bindingSpan)
                  scopeBindings
              visible =
                -- Recursive peer names in the same SCC are visible while
                -- analyzing the binding body.
                withRecursivePeerBindings
                  statementIndex
                  (currentVisibleBindings nextScope)
              bindingContext = contextForBinding bindingName
              (valueWarnings, valueErrors) =
                collectExprDiagnostics builtinMode settings visible (bindingContext bindingSpan) valueExpr
              warningsWithValue = appendWarnings warningsRev valueWarnings
              errorsWithValue =
                appendErrors (appendErrors errorsRev errorsFromSignature) valueErrors
              warningsWithRebinding = appendWarnings warningsWithValue rebindingWarning
              warningsWithShadowing = appendWarnings warningsWithRebinding shadowingWarning
              unusedWarnings =
                Map.findWithDefault [] statementIndex unusedBindingWarningsByStatement
           in
            ( nextScope,
              classDeclarations,
              implDeclarations,
              Nothing,
              appendWarnings warningsWithShadowing unusedWarnings,
              errorsWithValue
            )

    currentVisibleBindings :: Map Text VisibleBinding -> Map Text VisibleBinding
    -- Local scope is left-biased so inner declarations shadow outer bindings.
    currentVisibleBindings scopeBindings = scopeBindings `Map.union` outerScope

    withRecursivePeerBindings ::
      Int ->
      Map Text VisibleBinding ->
      Map Text VisibleBinding
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

    appendWarnings :: [WarningRecord] -> [WarningRecord] -> [WarningRecord]
    appendWarnings = foldl' (flip (:))

    appendErrors :: [Diagnostic] -> [Diagnostic] -> [Diagnostic]
    appendErrors = foldl' (flip (:))

-- | Signature bookkeeping is intentionally small: only one immediately
-- preceding signature may be waiting for a matching binding.
data PendingSignature = PendingSignature
  { pendingSignatureName :: Text,
    pendingSignatureSpan :: SourceSpan
  }

-- | Signatures must be consumed by the next binding; reaching any other
-- statement turns the pending signature into a diagnostic.
flushPendingSignature :: Maybe PendingSignature -> [Diagnostic] -> [Diagnostic]
flushPendingSignature pending errorsRev =
  case pending of
    Nothing -> errorsRev
    Just pendingSignature ->
      appendError errorsRev (mkMissingBindingForSignatureError pendingSignature)
  where
    appendError rev errorText = errorText : rev

mkUnboundVariableError :: Text -> Diagnostic
mkUnboundVariableError variableName =
  setDiagnosticSubject variableName $
    mkDiagnostic "E1001" ("unbound variable '" <> variableName <> "'")

mkMissingBindingForSignatureError :: PendingSignature -> Diagnostic
mkMissingBindingForSignatureError pendingSignature =
  setDiagnosticSubject
    (pendingSignatureName pendingSignature)
    ( setDiagnosticPrimarySpan
        (pendingSignatureSpan pendingSignature)
        ( mkDiagnostic
            "E1002"
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
            ( mkDiagnostic
                "E1003"
                ( "signature for '"
                    <> signatureName
                    <> "' must annotate the next binding with the same name; found '"
                    <> bindingName
                    <> "'"
                )
            )
        )
    )

mkDuplicateClassDeclarationError :: Text -> SourceSpan -> SourceSpan -> Diagnostic
mkDuplicateClassDeclarationError className classSpan previousSpan =
  setDiagnosticSubject className $
    setDiagnosticRelatedSpan previousSpan $
      setDiagnosticPrimarySpan
        classSpan
        (mkDiagnostic "E1004" ("duplicate class declaration '" <> className <> "'"))

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
        (mkDiagnostic "E1006" ("duplicate method signature '" <> methodName <> "' in class '" <> className <> "'"))

duplicateImplMethodErrors :: Identifier -> [ConstraintSignatureType] -> [ImplMethod] -> [Diagnostic]
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
        (mkDiagnostic "E1007" ("duplicate method binding '" <> methodName <> "' in impl '" <> implLabel <> "'"))

collectImplMethodDiagnostics ::
  BuiltinResolutionMode ->
  WarningSettings ->
  Map Text VisibleBinding ->
  [ImplMethod] ->
  ([WarningRecord], [Diagnostic])
collectImplMethodDiagnostics builtinMode settings visibleBindings methods =
  foldl' step ([], []) methods
  where
    step (warningsAcc, errorsAcc) (ImplMethod methodName methodSpan methodExpr) =
      let (methodWarnings, methodErrors) =
            collectExprDiagnostics
              builtinMode
              settings
              visibleBindings
              (contextForImplMethod methodName methodSpan)
              methodExpr
       in (warningsAcc ++ methodWarnings, errorsAcc ++ methodErrors)

mkDuplicateImplDeclarationError :: Text -> SourceSpan -> SourceSpan -> Diagnostic
mkDuplicateImplDeclarationError implFactKey implSpan previousSpan =
  setDiagnosticSubject implFactKey $
    setDiagnosticRelatedSpan previousSpan $
      setDiagnosticPrimarySpan
        implSpan
        (mkDiagnostic "E1005" ("duplicate impl declaration for '" <> implFactKey <> "'"))

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
contextForBinding :: Identifier -> SourceSpan -> AnalysisContext
contextForBinding bindingName bindingSpan =
  AnalysisContext
    { contextLabel = "binding '" <> identifierText bindingName <> "'",
      contextAllowsImpureCalls = identifierPurity bindingName == Impure,
      contextPrimarySpan = Just bindingSpan,
      contextSubject = Just (identifierText bindingName),
      contextLambdaSpan = Just bindingSpan
    }

contextForImplMethod :: Identifier -> SourceSpan -> AnalysisContext
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
  Map Text VisibleBinding ->
  AnalysisContext ->
  Identifier ->
  Bool
shouldRejectImpureCall builtinMode visibleBindings context calleeName =
  not (contextAllowsImpureCalls context)
    && isKnownImpureCallee
  where
    calleeNameText = identifierText calleeName
    isKnownImpureCallee =
      identifierPurity calleeName == Impure
        && (Map.member calleeNameText visibleBindings || isBuiltinSymbolNameInMode builtinMode calleeNameText)

mkImpureCallInPureContextError ::
  AnalysisContext ->
  Identifier ->
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
            ( mkDiagnostic
                "E1010"
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
  Map Int (Text, SourceSpan)
collectBindingDeclarations =
  foldl' collect Map.empty
  where
    collect declarations (statementIndex, statement) =
      case statement of
        SLet name spanValue _ ->
          Map.insert statementIndex (identifierText name, spanValue) declarations
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
  Map Text VisibleBinding ->
  Map Text VisibleBinding
registerDataConstructors hiddenStatementIndices statementIndex spanValue constructors bindings =
  foldl' register bindings constructors
  where
    constructorBinding = mkVisibleBinding hiddenStatementIndices statementIndex spanValue
    register bindingsAcc (DataConstructor constructorName _) =
      Map.insert (identifierText constructorName) constructorBinding bindingsAcc

collectDataConstructorRebindingWarnings ::
  WarningSettings ->
  Set Int ->
  Int ->
  SourceSpan ->
  [DataConstructor] ->
  Map Text VisibleBinding ->
  [WarningRecord]
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
            case Map.lookup constructorNameText bindingsAcc of
              Just previousBinding
                | not (visibleBindingIsHiddenPrelude previousBinding) ->
                    [ mkSameScopeRebindingWarning
                        constructorNameText
                        spanValue
                        (visibleBindingSpan previousBinding)
                    ]
              _ -> []
       in
        ( Map.insert constructorNameText constructorBinding bindingsAcc,
          warning ++ warningsAcc
        )

collectUnusedBindingWarnings ::
  WarningSettings ->
  Set Int ->
  [(Int, Statement)] ->
  Map Int [WarningRecord]
collectUnusedBindingWarnings settings hiddenStatementIndices indexedStatements
  | not (isWarningEnabled settings UnusedBinding) = Map.empty
  | otherwise =
      Map.fromList
        [ (statementIndex, [mkUnusedBindingWarning bindingNameText bindingSpan])
          | (statementIndex, SLet bindingName bindingSpan _) <- indexedStatements,
            statementIndex `Set.notMember` hiddenStatementIndices,
            let bindingNameText = identifierText bindingName,
            not (isBindingUsedByOtherStatements statementIndex),
            not (shouldSuppressRebindingDuplicate statementIndex)
        ]
  where
    (usedBindingStatementIndices, rebindingStatementIndices) =
      collectUnusedBindingUseState hiddenStatementIndices indexedStatements

    isBindingUsedByOtherStatements bindingStatementIndex =
      Set.member bindingStatementIndex usedBindingStatementIndices

    shouldSuppressRebindingDuplicate statementIndex =
      isWarningEnabled settings SameScopeRebinding
        && Set.member statementIndex rebindingStatementIndices

collectUnusedBindingUseState ::
  Set Int ->
  [(Int, Statement)] ->
  (Set Int, Set Int)
collectUnusedBindingUseState hiddenStatementIndices indexedStatements =
  let (_, _, usedStatementIndices, rebindingStatementIndices) =
        foldl' step (Map.empty, Set.empty, Set.empty, Set.empty) indexedStatements
   in (usedStatementIndices, rebindingStatementIndices)
  where
    step
      (activeBindings, activeRebindingNames, usedStatementIndices, rebindingStatementIndices)
      (statementIndex, statement)
      | statementIndex `Set.member` hiddenStatementIndices =
          (activeBindings, activeRebindingNames, usedStatementIndices, rebindingStatementIndices)
      | otherwise =
          let referenceNames = statementReferenceNames statement
              usedWithStatementReferences =
                Set.foldl'
                  (markReferencedBinding activeBindings)
                  usedStatementIndices
                  referenceNames
           in
            case statement of
              SLet bindingName _ _ ->
                let bindingNameText = identifierText bindingName
                    rebindingStatementIndices' =
                      if Set.member bindingNameText activeRebindingNames
                        then Set.insert statementIndex rebindingStatementIndices
                        else rebindingStatementIndices
                 in
                  ( Map.insert bindingNameText statementIndex activeBindings,
                    Set.insert bindingNameText activeRebindingNames,
                    usedWithStatementReferences,
                    rebindingStatementIndices'
                  )
              SData _ _ _ constructors ->
                ( foldl' removeConstructor activeBindings constructors,
                  foldl' registerConstructor activeRebindingNames constructors,
                  usedWithStatementReferences,
                  rebindingStatementIndices
                )
              _ ->
                ( activeBindings,
                  activeRebindingNames,
                  usedWithStatementReferences,
                  rebindingStatementIndices
                )

    statementReferenceNames statement =
      case statement of
        SLet bindingName _ valueExpr ->
          Set.delete
            (identifierText bindingName)
            (freeVarsExprWithBound Set.empty valueExpr)
        SExpr _ expr ->
          freeVarsExprWithBound Set.empty expr
        SImpl _ _ _ methods ->
          Set.unions
            [ freeVarsExprWithBound Set.empty methodExpr
              | ImplMethod _ _ methodExpr <- methods
            ]
        _ -> Set.empty

    markReferencedBinding activeBindings usedStatementIndices referenceName =
      case Map.lookup referenceName activeBindings of
        Nothing -> usedStatementIndices
        Just bindingStatementIndex ->
          Set.insert bindingStatementIndex usedStatementIndices

    removeConstructor activeBindings (DataConstructor constructorName _) =
      Map.delete (identifierText constructorName) activeBindings

    registerConstructor activeRebindingNames (DataConstructor constructorName _) =
      Set.insert (identifierText constructorName) activeRebindingNames

visibleBindingDiagnosticSpan :: VisibleBinding -> Maybe SourceSpan
visibleBindingDiagnosticSpan visibleBinding =
  if visibleBindingIsHiddenPrelude visibleBinding
    then Nothing
    else Just (visibleBindingSpan visibleBinding)

collectOuterScopeShadowingWarnings ::
  WarningSettings ->
  Text ->
  SourceSpan ->
  Map Text VisibleBinding ->
  [WarningRecord]
collectOuterScopeShadowingWarnings settings bindingName primarySpan outerScope
  | not (isWarningEnabled settings ShadowingOuterScope) = []
  | otherwise =
      case Map.lookup bindingName outerScope of
        Just previousBinding
          | not (visibleBindingIsHiddenPrelude previousBinding) ->
              [ mkOuterScopeShadowingWarning
                  bindingName
                  primarySpan
                  (visibleBindingDiagnosticSpan previousBinding)
              ]
        _ -> []

mkOuterScopeShadowingWarning :: Text -> SourceSpan -> Maybe SourceSpan -> WarningRecord
mkOuterScopeShadowingWarning variableName primarySpan previousSpan =
  WarningRecord
    { warningCategory = ShadowingOuterScope,
      warningCodeText = warningCode ShadowingOuterScope,
      warningVariableName = variableName,
      warningPrimarySpan = primarySpan,
      warningPreviousSpan = previousSpan,
      warningMessage =
        "outer-scope shadowing: '"
          <> variableName
          <> "' shadows a visible binding from an outer scope"
    }

mkUnusedBindingWarning :: Text -> SourceSpan -> WarningRecord
mkUnusedBindingWarning variableName primarySpan =
  WarningRecord
    { warningCategory = UnusedBinding,
      warningCodeText = warningCode UnusedBinding,
      warningVariableName = variableName,
      warningPrimarySpan = primarySpan,
      warningPreviousSpan = Nothing,
      warningMessage =
        "unused binding: '"
          <> variableName
          <> "' is never referenced in this lexical block"
    }

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

extendBindingsWithPattern :: Pattern -> Map Text VisibleBinding -> Map Text VisibleBinding
extendBindingsWithPattern pattern bindings =
  case pattern of
    PVariable name ->
      Map.insert (identifierText name) patternVisibleBinding bindings
    PWildcard -> bindings
    PLiteral {} -> bindings
    PConstructor _ patterns ->
      foldl' (flip extendBindingsWithPattern) bindings patterns
    PList patterns ->
      foldl' (flip extendBindingsWithPattern) bindings patterns
    PConsList headPattern tailPattern ->
      extendBindingsWithPattern tailPattern (extendBindingsWithPattern headPattern bindings)
    PTuple patterns ->
      foldl' (flip extendBindingsWithPattern) bindings patterns
    PAs name pattern ->
      extendBindingsWithPattern
        pattern
        (Map.insert (identifierText name) patternVisibleBinding bindings)

patternVisibleBinding :: VisibleBinding
patternVisibleBinding =
  VisibleBinding
    { visibleBindingSpan = SourceSpan 0 0,
      visibleBindingIsHiddenPrelude = True
    }
