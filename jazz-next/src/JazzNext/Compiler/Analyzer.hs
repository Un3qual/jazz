module JazzNext.Compiler.Analyzer
  ( Expr (..),
    Statement (..),
    AnalysisResult (..),
    analyzeProgram,
    analyzeRebindingWarnings
  ) where

import qualified Data.Map.Strict as Map
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
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

data Expr
  = EInt Int
  | EVar String
  | EScope [Statement]
  deriving (Eq, Show)

data Statement
  = SLet String SourceSpan Expr
  | SSignature String SourceSpan String
  | SExpr Expr
  deriving (Eq, Show)

data AnalysisResult = AnalysisResult
  { analyzedExpr :: Expr,
    analysisWarnings :: [WarningRecord],
    analysisErrors :: [String]
  }
  deriving (Eq, Show)

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
  Map String SourceSpan ->
  Expr ->
  ([WarningRecord], [String])
collectExprDiagnostics settings visibleBindings expr =
  case expr of
    EInt _ -> ([], [])
    EVar name ->
      case Map.lookup name visibleBindings of
        Just _ -> ([], [])
        Nothing -> ([], [mkUnboundVariableError name])
    EScope statements -> collectScopeDiagnostics settings visibleBindings statements

collectScopeDiagnostics ::
  WarningSettings ->
  Map String SourceSpan ->
  [Statement] ->
  ([WarningRecord], [String])
collectScopeDiagnostics settings outerScope statements =
  (reverse finalWarningsRev, reverse errorsWithFinalPending)
  where
    -- Build recursion groups from local binding dependencies so mutually recursive
    -- bindings can reference each other independent of declaration order.
    recursiveGroupsByBinding = inferRecursiveGroups statements
    firstBindingSpans = collectFirstBindingSpans statements

    (_, finalPendingSignature, finalWarningsRev, finalErrorsRev) =
      foldl' step (Map.empty, Nothing, [], []) statements
    errorsWithFinalPending = flushPendingSignature finalPendingSignature finalErrorsRev

    step ::
      (Map String SourceSpan, Maybe PendingSignature, [WarningRecord], [String]) ->
      Statement ->
      (Map String SourceSpan, Maybe PendingSignature, [WarningRecord], [String])
    step (scopeBindings, pendingSignature, warningsRev, errorsRev) statement =
      case statement of
        SExpr expr ->
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
          let errorsWithPending = flushPendingSignature pendingSignature errorsRev
           in
            ( scopeBindings,
              Just (PendingSignature signatureName signatureSpan),
              warningsRev,
              errorsWithPending
            )
        SLet bindingName bindingSpan valueExpr ->
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
                withRecursivePeerBindings
                  bindingName
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

    currentVisibleBindings :: Map String SourceSpan -> Map String SourceSpan
    currentVisibleBindings scopeBindings = scopeBindings `Map.union` outerScope

    withRecursivePeerBindings ::
      String ->
      Map String SourceSpan ->
      Map String SourceSpan
    withRecursivePeerBindings bindingName visibleNow =
      let peers =
            Set.delete
              bindingName
              (Map.findWithDefault Set.empty bindingName recursiveGroupsByBinding)
          peerEntries =
            Map.fromList
              [ (peerName, peerSpan)
                | peerName <- Set.toList peers,
                  Map.notMember peerName visibleNow,
                  Just peerSpan <- [Map.lookup peerName firstBindingSpans]
              ]
       in visibleNow `Map.union` peerEntries

    appendWarnings :: [WarningRecord] -> [WarningRecord] -> [WarningRecord]
    appendWarnings = foldl' (flip (:))

    appendErrors :: [String] -> [String] -> [String]
    appendErrors = foldl' (flip (:))

data PendingSignature = PendingSignature
  { pendingSignatureName :: String,
    pendingSignatureSpan :: SourceSpan
  }

flushPendingSignature :: Maybe PendingSignature -> [String] -> [String]
flushPendingSignature pending errorsRev =
  case pending of
    Nothing -> errorsRev
    Just pendingSignature ->
      appendError errorsRev (mkMissingBindingForSignatureError pendingSignature)
  where
    appendError rev errorText = errorText : rev

mkUnboundVariableError :: String -> String
mkUnboundVariableError variableName =
  "E1001: unbound variable '" ++ variableName ++ "'"

mkMissingBindingForSignatureError :: PendingSignature -> String
mkMissingBindingForSignatureError pendingSignature =
  "E1002: signature for '"
    ++ pendingSignatureName pendingSignature
    ++ "' at "
    ++ renderSpan (pendingSignatureSpan pendingSignature)
    ++ " must be immediately followed by a matching binding"

mkMismatchedSignatureError :: String -> SourceSpan -> String -> String
mkMismatchedSignatureError signatureName signatureSpan bindingName =
  "E1003: signature for '"
    ++ signatureName
    ++ "' at "
    ++ renderSpan signatureSpan
    ++ " must annotate the next binding with the same name; found '"
    ++ bindingName
    ++ "'"

renderSpan :: SourceSpan -> String
renderSpan spanValue = show (spanLine spanValue) ++ ":" ++ show (spanColumn spanValue)

inferRecursiveGroups :: [Statement] -> Map String (Set String)
inferRecursiveGroups statements =
  Map.fromList
    [ (name, Set.fromList names)
      | component <- stronglyConnComp graphNodes,
        let names = componentNames component,
        isRecursiveComponent component,
        name <- names
    ]
  where
    localBindingNames = Set.fromList [name | SLet name _ _ <- statements]
    baseDependencies =
      Map.fromSet (const Set.empty) localBindingNames
    dependenciesByName =
      foldl' addBindingDependencies baseDependencies statements
    graphNodes =
      [ (name, name, Set.toList dependencies)
        | (name, dependencies) <- Map.toList dependenciesByName
      ]

    addBindingDependencies ::
      Map String (Set String) ->
      Statement ->
      Map String (Set String)
    addBindingDependencies dependencies statement =
      case statement of
        SLet bindingName _ valueExpr ->
          let localDependencies =
                Set.intersection
                  localBindingNames
                  (freeVarsExprWithBound (Set.singleton bindingName) valueExpr)
           in
            Map.insertWith Set.union bindingName localDependencies dependencies
        _ -> dependencies

    componentNames :: SCC String -> [String]
    componentNames component =
      case component of
        AcyclicSCC name -> [name]
        CyclicSCC names -> names

    isRecursiveComponent :: SCC String -> Bool
    isRecursiveComponent component =
      case component of
        CyclicSCC _ -> True
        AcyclicSCC name ->
          Set.member name (Map.findWithDefault Set.empty name dependenciesByName)

collectFirstBindingSpans :: [Statement] -> Map String SourceSpan
collectFirstBindingSpans =
  foldl' collect Map.empty
  where
    collect spans statement =
      case statement of
        SLet name spanValue _ ->
          Map.insertWith (\_existing original -> original) name spanValue spans
        _ -> spans

freeVarsExprWithBound :: Set String -> Expr -> Set String
freeVarsExprWithBound bound expr =
  case expr of
    EInt _ -> Set.empty
    EVar name
      | Set.member name bound -> Set.empty
      | otherwise -> Set.singleton name
    EScope statements -> freeVarsScopeWithBound bound statements

freeVarsScopeWithBound :: Set String -> [Statement] -> Set String
freeVarsScopeWithBound initialBound statements =
  snd (foldl' step (initialBound, Set.empty) statements)
  where
    step :: (Set String, Set String) -> Statement -> (Set String, Set String)
    step (boundNames, freeNames) statement =
      case statement of
        SSignature _ _ _ -> (boundNames, freeNames)
        SExpr expr ->
          ( boundNames,
            Set.union freeNames (freeVarsExprWithBound boundNames expr)
          )
        SLet bindingName _ valueExpr ->
          let boundWithSelf = Set.insert bindingName boundNames
           in
            ( boundWithSelf,
              Set.union freeNames (freeVarsExprWithBound boundWithSelf valueExpr)
            )
