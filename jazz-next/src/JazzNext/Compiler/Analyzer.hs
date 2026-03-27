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
    Expr (..),
    Literal (..),
    Pattern (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    builtinNamesInMode,
    isBuiltinSymbolNameInMode
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    SourceSpan (..),
    WarningRecord,
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
    freeVarsScopeWithBound,
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
  ( WarningCategory (..)
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
    contextSubject :: Maybe Text
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
              (identifierText parameterName)
              lambdaVisibleBinding
              visibleBindings
       in collectExprDiagnostics builtinMode settings lambdaBindings context bodyExpr
    EOperatorValue _ -> ([], [])
    EList elements ->
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

    -- Internal accumulators are built in reverse for O(1) append.
    -- `pendingSignature` tracks exactly one immediately-preceding signature that
    -- must be consumed by the next binding.
    (_, finalPendingSignature, finalWarningsRev, finalErrorsRev) =
      foldl' step (Map.empty, Nothing, [], []) indexedStatements
    errorsWithFinalPending = flushPendingSignature finalPendingSignature finalErrorsRev

    step ::
      (Map Text VisibleBinding, Maybe PendingSignature, [WarningRecord], [Diagnostic]) ->
      (Int, Statement) ->
      (Map Text VisibleBinding, Maybe PendingSignature, [WarningRecord], [Diagnostic])
    step (scopeBindings, pendingSignature, warningsRev, errorsRev) (statementIndex, statement) =
      case statement of
        SExpr _ expr ->
          -- Any signature followed by a non-binding is invalid by contract.
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
              visible = currentVisibleBindings scopeBindings
              (exprWarnings, exprErrors) = collectExprDiagnostics builtinMode settings visible context expr
           in
            ( scopeBindings,
              Nothing,
              appendWarnings warningsRev exprWarnings,
              appendErrors errorsWithPending exprErrors
            )
        SModule {} ->
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
           in
            ( scopeBindings,
              Nothing,
              warningsRev,
              errorsWithPending
            )
        SImport {} ->
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
           in
            ( scopeBindings,
              Nothing,
              warningsRev,
              errorsWithPending
            )
        SSignature signatureName signatureSpan _signatureText ->
          -- Signature payload text is carried forward for future type parsing.
          -- This pass only enforces placement/name coherence.
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
           in
            ( scopeBindings,
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
           in
            ( nextScope,
              Nothing,
              appendWarnings warningsWithValue rebindingWarning,
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

topLevelContext :: AnalysisContext
topLevelContext =
  -- Top-level expression statements stay permissive so program-entry
  -- expression calls like `print! ...` remain valid in stub-v1 purity mode.
  AnalysisContext
    { contextLabel = "top-level expression",
      contextAllowsImpureCalls = True,
      contextPrimarySpan = Nothing,
      contextSubject = Nothing
    }

-- | Create the purity/diagnostic context that should apply while checking the
-- body of a specific binding.
contextForBinding :: Identifier -> SourceSpan -> AnalysisContext
contextForBinding bindingName bindingSpan =
  AnalysisContext
    { contextLabel = "binding '" <> identifierText bindingName <> "'",
      contextAllowsImpureCalls = identifierPurity bindingName == Impure,
      contextPrimarySpan = Just bindingSpan,
      contextSubject = Just (identifierText bindingName)
    }

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

visibleBindingDiagnosticSpan :: VisibleBinding -> Maybe SourceSpan
visibleBindingDiagnosticSpan visibleBinding =
  if visibleBindingIsHiddenPrelude visibleBinding
    then Nothing
    else Just (visibleBindingSpan visibleBinding)

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

patternVisibleBinding :: VisibleBinding
patternVisibleBinding =
  VisibleBinding
    { visibleBindingSpan = SourceSpan 0 0,
      visibleBindingIsHiddenPrelude = True
    }
