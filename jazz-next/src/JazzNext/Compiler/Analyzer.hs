{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Analyzer
  ( Expr (..),
    Statement (..),
    AnalysisResult (..),
    analyzeProgram,
    analyzeRebindingWarnings
  ) where

import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan,
    WarningRecord,
    mkSameScopeRebindingWarning,
    spanColumn,
    spanLine,
    sortWarnings
  )
import JazzNext.Compiler.WarningConfig
  ( WarningSettings,
    isWarningEnabled
  )
import JazzNext.Compiler.Warnings
  ( WarningCategory (..)
  )

data AnalysisResult = AnalysisResult
  { analyzedExpr :: Expr,
    analysisWarnings :: [WarningRecord],
    analysisErrors :: [Text]
  }
  deriving (Eq, Show)

-- Entry point for the current analyzer slice:
-- - unbound variable diagnostics
-- - signature adjacency/name diagnostics
-- - optional same-scope rebinding warnings
-- - recursive-group visibility for self/mutual recursion
analyzeProgram :: WarningSettings -> Expr -> IO AnalysisResult
analyzeProgram settings expr =
  let (warnings, errors) = collectExprDiagnostics settings Map.empty expr
   in
    pure
      AnalysisResult
        { analyzedExpr = expr,
          analysisWarnings = sortWarnings warnings,
          analysisErrors = errors
        }

analyzeRebindingWarnings :: WarningSettings -> Expr -> IO [WarningRecord]
analyzeRebindingWarnings settings expr = analysisWarnings <$> analyzeProgram settings expr

collectExprDiagnostics ::
  WarningSettings ->
  Map Text SourceSpan ->
  Expr ->
  ([WarningRecord], [Text])
collectExprDiagnostics settings visibleBindings expr =
  case expr of
    EInt _ -> ([], [])
    EBool _ -> ([], [])
    EVar name ->
      case Map.lookup name visibleBindings of
        Just _ -> ([], [])
        Nothing -> ([], [mkUnboundVariableError name])
    EIf conditionExpr thenExpr elseExpr ->
      let (conditionWarnings, conditionErrors) =
            collectExprDiagnostics settings visibleBindings conditionExpr
          (thenWarnings, thenErrors) =
            collectExprDiagnostics settings visibleBindings thenExpr
          (elseWarnings, elseErrors) =
            collectExprDiagnostics settings visibleBindings elseExpr
       in
        ( conditionWarnings ++ thenWarnings ++ elseWarnings,
          conditionErrors ++ thenErrors ++ elseErrors
        )
    EBinary _ leftExpr rightExpr ->
      let (leftWarnings, leftErrors) =
            collectExprDiagnostics settings visibleBindings leftExpr
          (rightWarnings, rightErrors) =
            collectExprDiagnostics settings visibleBindings rightExpr
       in
        (leftWarnings ++ rightWarnings, leftErrors ++ rightErrors)
    ESectionLeft leftExpr _ ->
      collectExprDiagnostics settings visibleBindings leftExpr
    ESectionRight _ rightExpr ->
      collectExprDiagnostics settings visibleBindings rightExpr
    EScope statements -> collectScopeDiagnostics settings visibleBindings statements

collectScopeDiagnostics ::
  WarningSettings ->
  Map Text SourceSpan ->
  [Statement] ->
  ([WarningRecord], [Text])
collectScopeDiagnostics settings outerScope statements =
  (reverse finalWarningsRev, reverse errorsWithFinalPending)
  where
    indexedStatements = zip [0 ..] statements

    -- Build recursion groups from local binding dependencies so mutually recursive
    -- bindings can reference each other independent of declaration order.
    recursiveGroupsByStatement = inferRecursiveGroups outerScope indexedStatements
    bindingDeclarationsByStatement = collectBindingDeclarations indexedStatements

    -- Internal accumulators are built in reverse for O(1) append.
    -- `pendingSignature` tracks exactly one immediately-preceding signature that
    -- must be consumed by the next binding.
    (_, finalPendingSignature, finalWarningsRev, finalErrorsRev) =
      foldl' step (Map.empty, Nothing, [], []) indexedStatements
    errorsWithFinalPending = flushPendingSignature finalPendingSignature finalErrorsRev

    step ::
      (Map Text SourceSpan, Maybe PendingSignature, [WarningRecord], [Text]) ->
      (Int, Statement) ->
      (Map Text SourceSpan, Maybe PendingSignature, [WarningRecord], [Text])
    step (scopeBindings, pendingSignature, warningsRev, errorsRev) (statementIndex, statement) =
      case statement of
        SExpr _ expr ->
          -- Any signature followed by a non-binding is invalid by contract.
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
              visible = currentVisibleBindings scopeBindings
              (exprWarnings, exprErrors) = collectExprDiagnostics settings visible expr
           in
            ( scopeBindings,
              Nothing,
              appendWarnings warningsRev exprWarnings,
              appendErrors errorsWithPending exprErrors
            )
        SSignature signatureName signatureSpan _signatureText ->
          -- Signature payload text is carried forward for future type parsing.
          -- This pass only enforces placement/name coherence.
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
           in
            ( scopeBindings,
              Just (PendingSignature signatureName signatureSpan),
              warningsRev,
              errorsWithPending
            )
        SLet bindingName bindingSpan valueExpr ->
          -- Bindings consume a pending signature if names match. Rebinding
          -- stays semantically valid but may emit an optional warning.
          let errorsFromSignature =
                case pendingSignature of
                  Nothing -> []
                  Just (PendingSignature signatureName signatureDeclSpan)
                    | signatureName == bindingName -> []
                    | otherwise ->
                        [ mkMismatchedSignatureError
                            signatureName
                            signatureDeclSpan
                            bindingName
                        ]
              rebindingWarning =
                case Map.lookup bindingName scopeBindings of
                  Just previousSpan
                    | isWarningEnabled settings SameScopeRebinding ->
                        [mkSameScopeRebindingWarning bindingName bindingSpan previousSpan]
                  _ -> []
              nextScope = Map.insert bindingName bindingSpan scopeBindings
              visible =
                -- Recursive peer names in the same SCC are visible while
                -- analyzing the binding body.
                withRecursivePeerBindings
                  statementIndex
                  (currentVisibleBindings nextScope)
              (valueWarnings, valueErrors) = collectExprDiagnostics settings visible valueExpr
              warningsWithValue = appendWarnings warningsRev valueWarnings
              errorsWithValue =
                appendErrors (appendErrors errorsRev errorsFromSignature) valueErrors
           in
            ( nextScope,
              Nothing,
              appendWarnings warningsWithValue rebindingWarning,
              errorsWithValue
            )

    currentVisibleBindings :: Map Text SourceSpan -> Map Text SourceSpan
    -- Local scope is left-biased so inner declarations shadow outer bindings.
    currentVisibleBindings scopeBindings = scopeBindings `Map.union` outerScope

    withRecursivePeerBindings ::
      Int ->
      Map Text SourceSpan ->
      Map Text SourceSpan
    withRecursivePeerBindings statementIndex visibleNow =
      let peers =
            Set.delete
              statementIndex
              (Map.findWithDefault Set.empty statementIndex recursiveGroupsByStatement)
          peerEntries =
            Map.fromList
              [ (peerName, peerSpan)
                | peerStatementIndex <- Set.toList peers,
                  Just (peerName, peerSpan) <- [Map.lookup peerStatementIndex bindingDeclarationsByStatement],
                  -- Do not override currently visible names (for example due to
                  -- local rebinding) when adding recursive peers.
                  Map.notMember peerName visibleNow
              ]
       in visibleNow `Map.union` peerEntries

    appendWarnings :: [WarningRecord] -> [WarningRecord] -> [WarningRecord]
    appendWarnings = foldl' (flip (:))

    appendErrors :: [Text] -> [Text] -> [Text]
    appendErrors = foldl' (flip (:))

data PendingSignature = PendingSignature
  { pendingSignatureName :: Text,
    pendingSignatureSpan :: SourceSpan
  }

flushPendingSignature :: Maybe PendingSignature -> [Text] -> [Text]
flushPendingSignature pending errorsRev =
  case pending of
    Nothing -> errorsRev
    Just pendingSignature ->
      appendError errorsRev (mkMissingBindingForSignatureError pendingSignature)
  where
    appendError rev errorText = errorText : rev

mkUnboundVariableError :: Text -> Text
mkUnboundVariableError variableName =
  "E1001: unbound variable '" <> variableName <> "'"

mkMissingBindingForSignatureError :: PendingSignature -> Text
mkMissingBindingForSignatureError pendingSignature =
  "E1002: signature for '"
    <> pendingSignatureName pendingSignature
    <> "' at "
    <> renderSpan (pendingSignatureSpan pendingSignature)
    <> " must be immediately followed by a matching binding"

mkMismatchedSignatureError :: Text -> SourceSpan -> Text -> Text
mkMismatchedSignatureError signatureName signatureSpan bindingName =
  "E1003: signature for '"
    <> signatureName
    <> "' at "
    <> renderSpan signatureSpan
    <> " must annotate the next binding with the same name; found '"
    <> bindingName
    <> "'"

renderSpan :: SourceSpan -> Text
renderSpan spanValue = Text.pack (show (spanLine spanValue)) <> ":" <> Text.pack (show (spanColumn spanValue))

inferRecursiveGroups ::
  Map Text SourceSpan ->
  [(Int, Statement)] ->
  Map Int (Set Int)
inferRecursiveGroups outerScope indexedStatements =
  Map.fromList
    [ (statementIndex, Set.fromList componentStatements)
      | component <- stronglyConnComp graphNodes,
        let componentStatements = componentStatementIndices component,
        isRecursiveComponent component,
        statementIndex <- componentStatements
    ]
  where
    -- Track only binding statements; signatures/expr statements do not form
    -- recursion nodes.
    declarationInfo =
      [ (statementIndex, bindingName, valueExpr)
        | (statementIndex, SLet bindingName _ valueExpr) <- indexedStatements
      ]
    declarationStatementsByName =
      foldl'
        collectDeclaration
        Map.empty
        declarationInfo
    outerBindingNames = Map.keysSet outerScope
    baseDependencies =
      Map.fromList
        [ (statementIndex, Set.empty)
          | (statementIndex, _, _) <- declarationInfo
        ]
    dependenciesByStatement =
      foldl' addBindingDependencies baseDependencies declarationInfo
    graphNodes =
      [ (statementIndex, statementIndex, Set.toList dependencies)
        | (statementIndex, dependencies) <- Map.toList dependenciesByStatement
      ]

    collectDeclaration ::
      Map Text [Int] ->
      (Int, Text, Expr) ->
      Map Text [Int]
    collectDeclaration declarationsByName (statementIndex, bindingName, _) =
      Map.insertWith (\new old -> old ++ new) bindingName [statementIndex] declarationsByName

    addBindingDependencies ::
      Map Int (Set Int) ->
      (Int, Text, Expr) ->
      Map Int (Set Int)
    addBindingDependencies dependencies (statementIndex, bindingName, valueExpr) =
      let localDependencyNames =
            Set.filter
              (`Map.member` declarationStatementsByName)
              (freeVarsExprWithBound (Set.singleton bindingName) valueExpr)
          resolvedDependencies =
            Set.fromList
              [ dependencyStatementIndex
                | dependencyName <- Set.toList localDependencyNames,
                  Just dependencyStatementIndex <-
                    [ resolveDependencyStatement statementIndex dependencyName ]
              ]
       in
        Map.insert statementIndex resolvedDependencies dependencies

    resolveDependencyStatement :: Int -> Text -> Maybe Int
    resolveDependencyStatement statementIndex dependencyName =
      case Map.lookup dependencyName declarationStatementsByName of
        Nothing -> Nothing
        Just declarationStatements ->
          -- For a given dependency name:
          -- 1) prefer the nearest earlier declaration (rebinding snapshot),
          -- 2) if none and name exists in outer scope, keep it as outer reference,
          -- 3) otherwise use the first later declaration (forward edge).
          case closestPriorDeclaration declarationStatements of
            Just prior -> Just prior
            Nothing
              | Set.member dependencyName outerBindingNames -> Nothing
              | otherwise -> closestFutureDeclaration declarationStatements
      where
        closestPriorDeclaration declarations =
          case filter (< statementIndex) declarations of
            [] -> Nothing
            priorDeclarations -> Just (last priorDeclarations)

        closestFutureDeclaration declarations =
          case filter (> statementIndex) declarations of
            [] -> Nothing
            firstFuture : _ -> Just firstFuture

    componentStatementIndices :: SCC Int -> [Int]
    componentStatementIndices component =
      case component of
        AcyclicSCC statementIndex -> [statementIndex]
        CyclicSCC statementIndices -> statementIndices

    isRecursiveComponent :: SCC Int -> Bool
    isRecursiveComponent component =
      case component of
        CyclicSCC _ -> True
        AcyclicSCC statementIndex ->
          Set.member
            statementIndex
            (Map.findWithDefault Set.empty statementIndex dependenciesByStatement)

collectBindingDeclarations ::
  [(Int, Statement)] ->
  Map Int (Text, SourceSpan)
collectBindingDeclarations =
  foldl' collect Map.empty
  where
    collect declarations (statementIndex, statement) =
      case statement of
        SLet name spanValue _ -> Map.insert statementIndex (name, spanValue) declarations
        _ -> declarations

freeVarsExprWithBound :: Set Text -> Expr -> Set Text
freeVarsExprWithBound bound expr =
  case expr of
    EInt _ -> Set.empty
    EBool _ -> Set.empty
    EVar name
      | Set.member name bound -> Set.empty
      | otherwise -> Set.singleton name
    EIf conditionExpr thenExpr elseExpr ->
      Set.unions
        [ freeVarsExprWithBound bound conditionExpr,
          freeVarsExprWithBound bound thenExpr,
          freeVarsExprWithBound bound elseExpr
        ]
    EBinary _ leftExpr rightExpr ->
      Set.union
        (freeVarsExprWithBound bound leftExpr)
        (freeVarsExprWithBound bound rightExpr)
    ESectionLeft leftExpr _ ->
      freeVarsExprWithBound bound leftExpr
    ESectionRight _ rightExpr ->
      freeVarsExprWithBound bound rightExpr
    EScope statements -> freeVarsScopeWithBound bound statements

freeVarsScopeWithBound :: Set Text -> [Statement] -> Set Text
freeVarsScopeWithBound initialBound statements =
  snd (foldl' step (initialBound, Set.empty) statements)
  where
    step :: (Set Text, Set Text) -> Statement -> (Set Text, Set Text)
    step (boundNames, freeNames) statement =
      case statement of
        SSignature _ _ _ -> (boundNames, freeNames)
        SExpr _ expr ->
          ( boundNames,
            Set.union freeNames (freeVarsExprWithBound boundNames expr)
          )
        SLet bindingName _ valueExpr ->
          -- Bindings are visible in their own RHS for self-recursion analysis.
          let boundWithSelf = Set.insert bindingName boundNames
           in
            ( boundWithSelf,
              Set.union freeNames (freeVarsExprWithBound boundWithSelf valueExpr)
            )
