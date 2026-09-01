{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Shared recursive-binding graph and free-variable helpers used by analyzer,
-- type inference, and runtime.
module Jazz.Compiler.RecursiveBindings
  ( LambdaCaptureHints,
    PreparedRecursiveScope,
    RecursiveScopeFacts,
    buildRecursiveScopeFacts,
    closureCaptureCandidatesWithBound,
    collectBindingNames,
    collectLambdaCaptureHints,
    emptyLambdaCaptureHints,
    freeVarsExprWithBound,
    freeVarsScopeWithBound,
    exprContainsFunctionBranch,
    inferRecursiveGroupsOrdered,
    inferSelfReferencedBindings,
    inferSelfRecursiveBindings,
    lambdaCaptureHintsChild,
    lookupLambdaCapturedNames,
    lookupLambdaCapturedNamesOrdered,
    prepareRecursiveScope,
    preparedRecursiveScopeBindingNames,
    preparedRecursiveScopeFactsForOuterBindings,
    preparedRecursiveScopeGroups,
    preparedRecursiveScopeOuterBindingNames,
    preparedRecursiveScopeStatements,
    recursiveScopeBindingNames,
    recursiveScopeGroups,
  )
where

import Data.Graph
  ( SCC (..),
    stronglyConnComp,
  )
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    CoreNameAt,
    CoreNode,
    CorePhaseNames (coreOperatorBindingName),
    Expr (..),
    Statement (..),
  )
import Jazz.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol,
  )
import Jazz.Compiler.Pattern
  ( extendBoundWithPattern,
    patternBinderNames,
  )
import Jazz.Compiler.StableSet
  ( StableSet,
    stableSetDelete,
    stableSetDifference,
    stableSetFromSet,
    stableSetMembershipSet,
    stableSetOrderedList,
    stableSetSingleton,
  )

collectBindingNames :: [(Int, Statement phase)] -> Map Int (CoreNameAt phase)
collectBindingNames =
  foldl' step Map.empty
  where
    step bindingNames (statementIndex, statement) =
      case statement of
        SLet _ bindingName _ ->
          Map.insert statementIndex bindingName bindingNames
        _ -> bindingNames

-- | Immutable local recursion facts for one exact statement scope and outer
-- visibility projection. The product deliberately retains names and integer
-- indices only; callers continue to own the statement AST.
data RecursiveScopeFacts phase = RecursiveScopeFacts
  { recursiveScopeBindingNames :: Map Int (CoreNameAt phase),
    recursiveScopeGroups :: Map Int [Int]
  }

buildRecursiveScopeFacts :: (CorePhaseNames phase) => Set (CoreNameAt phase) -> [(Int, Statement phase)] -> RecursiveScopeFacts phase
buildRecursiveScopeFacts outerBindingNames indexedStatements =
  RecursiveScopeFacts
    { recursiveScopeBindingNames = collectBindingNames indexedStatements,
      recursiveScopeGroups = inferRecursiveGroupsOrderedInternal outerBindingNames indexedStatements
    }

-- | One statement scope paired with the outer visibility projection and
-- recursive facts from which it was derived. The constructor stays private so
-- consumers cannot cross-pair any of the three.
data PreparedRecursiveScope phase = PreparedRecursiveScope ![Statement phase] !(Set (CoreNameAt phase)) !(RecursiveScopeFacts phase)

prepareRecursiveScope :: (CorePhaseNames phase) => Set (CoreNameAt phase) -> [Statement phase] -> PreparedRecursiveScope phase
prepareRecursiveScope outerBindingNames statements =
  PreparedRecursiveScope
    statements
    outerBindingNames
    (buildRecursiveScopeFacts outerBindingNames (zip [0 ..] statements))

preparedRecursiveScopeStatements :: PreparedRecursiveScope phase -> [Statement phase]
preparedRecursiveScopeStatements (PreparedRecursiveScope statements _ _) = statements

preparedRecursiveScopeOuterBindingNames :: PreparedRecursiveScope phase -> Set (CoreNameAt phase)
preparedRecursiveScopeOuterBindingNames (PreparedRecursiveScope _ outerBindingNames _) =
  outerBindingNames

-- | Reuse the owned facts when the consumer has the same outer visibility.
-- A prepared scope crossing a compiler boundary with different imports or
-- builtin visibility is repaired from its retained statements rather than
-- silently applying recursion facts derived for another environment.
preparedRecursiveScopeFactsForOuterBindings ::
  (CorePhaseNames phase) =>
  Set (CoreNameAt phase) ->
  PreparedRecursiveScope phase ->
  RecursiveScopeFacts phase
preparedRecursiveScopeFactsForOuterBindings
  expectedOuterBindingNames
  (PreparedRecursiveScope statements preparedOuterBindingNames recursiveScopeFactsValue)
    | expectedOuterBindingNames == preparedOuterBindingNames = recursiveScopeFactsValue
    | otherwise =
        buildRecursiveScopeFacts expectedOuterBindingNames (zip [0 ..] statements)

preparedRecursiveScopeBindingNames :: PreparedRecursiveScope phase -> Map Int (CoreNameAt phase)
preparedRecursiveScopeBindingNames (PreparedRecursiveScope _ _ recursiveScopeFactsValue) =
  recursiveScopeBindingNames recursiveScopeFactsValue

preparedRecursiveScopeGroups :: PreparedRecursiveScope phase -> Map Int [Int]
preparedRecursiveScopeGroups (PreparedRecursiveScope _ _ recursiveScopeFactsValue) =
  recursiveScopeGroups recursiveScopeFactsValue

-- | Free-variable facts arranged in the same child-index shape as the lambda
-- AST. The plan deliberately retains neither lambda bodies nor parameters, so
-- runtime lookup cannot fall back to structural expression equality.
data LambdaCaptureHint phase = LambdaCaptureHint (StableSet (CoreNameAt phase)) (LambdaCaptureHints phase)

data LambdaCaptureHints phase = LambdaCaptureHints
  { lambdaCaptureHintAtRoot :: Maybe (LambdaCaptureHint phase),
    lambdaCaptureChildHints :: IntMap (LambdaCaptureHints phase)
  }

emptyLambdaCaptureHints :: LambdaCaptureHints phase
emptyLambdaCaptureHints = LambdaCaptureHints Nothing IntMap.empty

lambdaCaptureHintsChild :: Int -> LambdaCaptureHints phase -> LambdaCaptureHints phase
lambdaCaptureHintsChild childIndex =
  IntMap.findWithDefault emptyLambdaCaptureHints childIndex . lambdaCaptureChildHints

collectLambdaCaptureHints :: (CorePhaseNames phase) => Expr phase -> LambdaCaptureHints phase
collectLambdaCaptureHints = snd . analyzeLambdaCaptures

analyzeLambdaCaptures :: (CorePhaseNames phase) => Expr phase -> (StableSet (CoreNameAt phase), LambdaCaptureHints phase)
analyzeLambdaCaptures expr =
  case expr of
    ELit _ _ -> emptyCaptureAnalysis
    EVar _ name -> (stableSetSingleton name, emptyLambdaCaptureHints)
    ELambda _ parameterName bodyExpr ->
      let (bodyFreeNames, bodyHints) = analyzeLambdaCaptures bodyExpr
          capturedNames = stableSetDelete parameterName bodyFreeNames
       in ( capturedNames,
            LambdaCaptureHints
              (Just (LambdaCaptureHint capturedNames bodyHints))
              IntMap.empty
          )
    EOperatorValue node operatorSymbol ->
      (stableSetFromSet (operatorBindingFreeVar node Set.empty operatorSymbol), emptyLambdaCaptureHints)
    EList _ elements -> analyzeLambdaChildren elements
    ETuple _ elements -> analyzeLambdaChildren elements
    EApply _ functionExpr argumentExpr ->
      analyzeLambdaChildren [functionExpr, argumentExpr]
    ETypeApplication _ functionExpr _ _ ->
      analyzeLambdaChildren [functionExpr]
    EIf _ conditionExpr thenExpr elseExpr ->
      analyzeLambdaChildren [conditionExpr, thenExpr, elseExpr]
    EPatternCase _ scrutineeExpr caseArms ->
      analyzeLambdaPatternCase scrutineeExpr caseArms
    EBinary node operatorSymbol leftExpr rightExpr ->
      let (freeNames, hints) = analyzeLambdaChildren [leftExpr, rightExpr]
       in (stableSetFromSet (operatorBindingFreeVar node Set.empty operatorSymbol) <> freeNames, hints)
    ESectionLeft node leftExpr operatorSymbol ->
      let (freeNames, hints) = analyzeLambdaChildren [leftExpr]
       in (stableSetFromSet (operatorBindingFreeVar node Set.empty operatorSymbol) <> freeNames, hints)
    ESectionRight node operatorSymbol rightExpr ->
      let (freeNames, hints) = analyzeLambdaChildren [rightExpr]
       in (stableSetFromSet (operatorBindingFreeVar node Set.empty operatorSymbol) <> freeNames, hints)
    EBlock _ statements ->
      analyzeLambdaScope statements

emptyCaptureAnalysis :: (CorePhaseNames phase) => (StableSet (CoreNameAt phase), LambdaCaptureHints phase)
emptyCaptureAnalysis = (mempty, emptyLambdaCaptureHints)

analyzeLambdaChildren :: (CorePhaseNames phase) => [Expr phase] -> (StableSet (CoreNameAt phase), LambdaCaptureHints phase)
analyzeLambdaChildren expressions =
  ( mconcat freeNames,
    LambdaCaptureHints Nothing (IntMap.fromList childHints)
  )
  where
    analyses = map analyzeLambdaCaptures expressions
    freeNames = map fst analyses
    childHints =
      [ (childIndex, hints)
      | (childIndex, (_, hints)) <- zip [0 ..] analyses,
        not (lambdaCaptureHintsAreEmpty hints)
      ]

analyzeLambdaPatternCase :: (CorePhaseNames phase) => Expr phase -> [CaseArm phase] -> (StableSet (CoreNameAt phase), LambdaCaptureHints phase)
analyzeLambdaPatternCase scrutineeExpr caseArms =
  foldl' analyzeArm initialAnalysis (zip [0 ..] caseArms)
  where
    (scrutineeFreeNames, scrutineeHints) = analyzeLambdaCaptures scrutineeExpr
    initialAnalysis =
      ( scrutineeFreeNames,
        insertLambdaChildHint 0 scrutineeHints emptyLambdaCaptureHints
      )

    analyzeArm (freeNames, hints) (armIndex, CaseArm _ pattern guardExpr bodyExpr) =
      ( mconcat
          [ freeNames,
            stableSetDifference guardFreeNames boundNames,
            stableSetDifference bodyFreeNames boundNames
          ],
        insertLambdaChildHint
          bodyChildIndex
          bodyHints
          (insertLambdaChildHint guardChildIndex guardHints hints)
      )
      where
        boundNames = patternBinderNames pattern
        (guardFreeNames, guardHints) =
          maybe emptyCaptureAnalysis analyzeLambdaCaptures guardExpr
        (bodyFreeNames, bodyHints) = analyzeLambdaCaptures bodyExpr
        guardChildIndex = 1 + (2 * armIndex)
        bodyChildIndex = guardChildIndex + 1

analyzeLambdaScope :: (CorePhaseNames phase) => [Statement phase] -> (StableSet (CoreNameAt phase), LambdaCaptureHints phase)
analyzeLambdaScope statements =
  (freeNames, LambdaCaptureHints Nothing childHints)
  where
    (_, freeNames, childHints) =
      foldl' analyzeStatement (Set.empty, mempty, IntMap.empty) (zip [0 ..] statements)

    analyzeStatement (boundNames, accumulatedFreeNames, accumulatedHints) (statementIndex, statement) =
      case statement of
        SLet _ bindingName valueExpr ->
          analyzeValue (Set.insert bindingName boundNames) valueExpr
        SExpr _ valueExpr ->
          analyzeValue boundNames valueExpr
        SSignature {} -> unchanged
        SData {} -> unchanged
        SClass {} -> unchanged
        SImpl {} -> unchanged
        SModule {} -> unchanged
        SImport {} -> unchanged
      where
        unchanged = (boundNames, accumulatedFreeNames, accumulatedHints)
        analyzeValue nextBoundNames valueExpr =
          let (valueFreeNames, valueHints) = analyzeLambdaCaptures valueExpr
           in ( nextBoundNames,
                accumulatedFreeNames <> stableSetDifference valueFreeNames boundNames,
                insertLambdaChildHintMap statementIndex valueHints accumulatedHints
              )

insertLambdaChildHint :: Int -> LambdaCaptureHints phase -> LambdaCaptureHints phase -> LambdaCaptureHints phase
insertLambdaChildHint childIndex childHints hints =
  hints
    { lambdaCaptureChildHints =
        insertLambdaChildHintMap childIndex childHints (lambdaCaptureChildHints hints)
    }

insertLambdaChildHintMap :: Int -> LambdaCaptureHints phase -> IntMap (LambdaCaptureHints phase) -> IntMap (LambdaCaptureHints phase)
insertLambdaChildHintMap childIndex childHints hints
  | lambdaCaptureHintsAreEmpty childHints = hints
  | otherwise = IntMap.insert childIndex childHints hints

lambdaCaptureHintsAreEmpty :: LambdaCaptureHints phase -> Bool
lambdaCaptureHintsAreEmpty (LambdaCaptureHints Nothing childHints) = IntMap.null childHints
lambdaCaptureHintsAreEmpty _ = False

lookupLambdaCapturedNames :: LambdaCaptureHints phase -> Maybe (Set (CoreNameAt phase), LambdaCaptureHints phase)
lookupLambdaCapturedNames hints =
  case lambdaCaptureHintAtRoot hints of
    Just (LambdaCaptureHint capturedNames nestedHints) -> Just (stableSetMembershipSet capturedNames, nestedHints)
    Nothing -> Nothing

lookupLambdaCapturedNamesOrdered :: LambdaCaptureHints phase -> Maybe ([CoreNameAt phase], LambdaCaptureHints phase)
lookupLambdaCapturedNamesOrdered hints =
  case lambdaCaptureHintAtRoot hints of
    Just (LambdaCaptureHint capturedNames nestedHints) -> Just (stableSetOrderedList capturedNames, nestedHints)
    Nothing -> Nothing

freeVarsExprWithBound :: (CorePhaseNames phase) => Set (CoreNameAt phase) -> Expr phase -> Set (CoreNameAt phase)
freeVarsExprWithBound = freeVarsExprWithVisibleBindings Set.empty

freeVarsExprWithVisibleBindings :: (CorePhaseNames phase) => Set (CoreNameAt phase) -> Set (CoreNameAt phase) -> Expr phase -> Set (CoreNameAt phase)
freeVarsExprWithVisibleBindings visibleBindingNames =
  freeVarsExprUsing (freeVarsScopeWithVisibleBindings visibleBindingNames)

-- | Names that may need to come from the environment when a closure is
-- created. Unlike recursive-binding analysis, an ordinary binding is not in
-- scope in its own initializer: a same-name reference snapshots a previously
-- visible value. Recursive cells supplied by scope evaluation are harmless
-- candidates here because restricting an environment drops absent names.
closureCaptureCandidatesWithBound :: (CorePhaseNames phase) => Set (CoreNameAt phase) -> Expr phase -> Set (CoreNameAt phase)
closureCaptureCandidatesWithBound =
  freeVarsExprUsing closureCaptureCandidatesScopeWithBound

freeVarsExprUsing :: (CorePhaseNames phase) => (Set (CoreNameAt phase) -> [Statement phase] -> Set (CoreNameAt phase)) -> Set (CoreNameAt phase) -> Expr phase -> Set (CoreNameAt phase)
freeVarsExprUsing scopeFreeVars bound expr =
  case expr of
    ELit _ _ -> Set.empty
    EVar _ name
      | Set.member name bound -> Set.empty
      | otherwise -> Set.singleton name
    ELambda _ parameterName bodyExpr ->
      freeVarsExprUsing
        scopeFreeVars
        (Set.insert parameterName bound)
        bodyExpr
    EOperatorValue node operatorSymbol ->
      operatorBindingFreeVar node bound operatorSymbol
    EList _ elements ->
      Set.unions (map (freeVarsExprUsing scopeFreeVars bound) elements)
    ETuple _ elements ->
      Set.unions (map (freeVarsExprUsing scopeFreeVars bound) elements)
    EApply _ functionExpr argumentExpr ->
      Set.union
        (freeVarsExprUsing scopeFreeVars bound functionExpr)
        (freeVarsExprUsing scopeFreeVars bound argumentExpr)
    ETypeApplication _ functionExpr _ _ ->
      freeVarsExprUsing scopeFreeVars bound functionExpr
    EIf _ conditionExpr thenExpr elseExpr ->
      Set.unions
        [ freeVarsExprUsing scopeFreeVars bound conditionExpr,
          freeVarsExprUsing scopeFreeVars bound thenExpr,
          freeVarsExprUsing scopeFreeVars bound elseExpr
        ]
    EPatternCase _ scrutineeExpr caseArms ->
      Set.unions
        ( freeVarsExprUsing scopeFreeVars bound scrutineeExpr
            : [ Set.union
                  (maybe Set.empty (freeVarsExprUsing scopeFreeVars armBound) guardExpr)
                  (freeVarsExprUsing scopeFreeVars armBound bodyExpr)
              | CaseArm _ pattern guardExpr bodyExpr <- caseArms,
                let armBound = extendBoundWithPattern pattern bound
              ]
        )
    EBinary node operatorSymbol leftExpr rightExpr ->
      Set.unions
        [ operatorBindingFreeVar node bound operatorSymbol,
          freeVarsExprUsing scopeFreeVars bound leftExpr,
          freeVarsExprUsing scopeFreeVars bound rightExpr
        ]
    ESectionLeft node leftExpr operatorSymbol ->
      Set.union
        (operatorBindingFreeVar node bound operatorSymbol)
        (freeVarsExprUsing scopeFreeVars bound leftExpr)
    ESectionRight node operatorSymbol rightExpr ->
      Set.union
        (operatorBindingFreeVar node bound operatorSymbol)
        (freeVarsExprUsing scopeFreeVars bound rightExpr)
    EBlock _ statements ->
      scopeFreeVars bound statements

operatorBindingFreeVar :: forall phase sort. (CorePhaseNames phase) => CoreNode phase sort -> Set (CoreNameAt phase) -> Text -> Set (CoreNameAt phase)
operatorBindingFreeVar _ bound operatorSymbol
  | isBuiltinOperatorSymbol operatorSymbol = Set.empty
  | Set.member bindingName bound = Set.empty
  | otherwise = Set.singleton bindingName
  where
    bindingName = coreOperatorBindingName (Proxy :: Proxy phase) operatorSymbol

closureCaptureCandidatesScopeWithBound :: (CorePhaseNames phase) => Set (CoreNameAt phase) -> [Statement phase] -> Set (CoreNameAt phase)
closureCaptureCandidatesScopeWithBound initialBound statements =
  snd (foldl' step (initialBound, Set.empty) statements)
  where
    step (boundNames, captureCandidates) statement =
      case statement of
        SSignature {} -> (boundNames, captureCandidates)
        SModule {} -> (boundNames, captureCandidates)
        SImport {} -> (boundNames, captureCandidates)
        SClass {} -> (boundNames, captureCandidates)
        SImpl {} -> (boundNames, captureCandidates)
        SData {} -> (boundNames, captureCandidates)
        SExpr _ expr ->
          ( boundNames,
            Set.union
              captureCandidates
              (closureCaptureCandidatesWithBound boundNames expr)
          )
        SLet _ bindingName valueExpr ->
          ( Set.insert bindingName boundNames,
            Set.union
              captureCandidates
              (closureCaptureCandidatesWithBound boundNames valueExpr)
          )

freeVarsScopeWithBound :: (CorePhaseNames phase) => Set (CoreNameAt phase) -> [Statement phase] -> Set (CoreNameAt phase)
freeVarsScopeWithBound = freeVarsScopeWithVisibleBindings Set.empty

freeVarsScopeWithVisibleBindings :: (CorePhaseNames phase) => Set (CoreNameAt phase) -> Set (CoreNameAt phase) -> [Statement phase] -> Set (CoreNameAt phase)
freeVarsScopeWithVisibleBindings visibleBindingNames initialBound statements =
  snd (foldl' step (initialBound, Set.empty) indexedStatements)
  where
    indexedStatements = zip [0 ..] statements
    recursiveScopeFactsValue =
      buildRecursiveScopeFacts
        (Set.union visibleBindingNames initialBound)
        indexedStatements
    recursiveGroupsByStatement = recursiveScopeGroups recursiveScopeFactsValue
    bindingNamesByStatement = recursiveScopeBindingNames recursiveScopeFactsValue

    recursiveGroupMemberNames statementIndex =
      Set.fromList
        [ peerName
        | peerIndex <- Map.findWithDefault [] statementIndex recursiveGroupsByStatement,
          Just peerName <- [Map.lookup peerIndex bindingNamesByStatement]
        ]

    step (boundNames, freeNames) (statementIndex, statement) =
      case statement of
        SSignature {} -> (boundNames, freeNames)
        SModule {} -> (boundNames, freeNames)
        SImport {} -> (boundNames, freeNames)
        SClass {} -> (boundNames, freeNames)
        SImpl {} -> (boundNames, freeNames)
        SData {} -> (boundNames, freeNames)
        SExpr _ expr ->
          ( boundNames,
            Set.union
              freeNames
              (freeVarsExprWithVisibleBindings visibleBindingNames boundNames expr)
          )
        SLet _ bindingName valueExpr ->
          let boundWithSelf = Set.insert bindingName boundNames
              rhsBoundNames = Set.union boundNames (recursiveGroupMemberNames statementIndex)
           in ( boundWithSelf,
                Set.union
                  freeNames
                  (freeVarsExprWithVisibleBindings visibleBindingNames rhsBoundNames valueExpr)
              )

inferRecursiveGroupsOrdered :: (CorePhaseNames phase) => Set (CoreNameAt phase) -> [(Int, Statement phase)] -> Map Int [Int]
inferRecursiveGroupsOrdered outerBindingNames =
  recursiveScopeGroups . buildRecursiveScopeFacts outerBindingNames

inferRecursiveGroupsOrderedInternal :: (CorePhaseNames phase) => Set (CoreNameAt phase) -> [(Int, Statement phase)] -> Map Int [Int]
inferRecursiveGroupsOrderedInternal outerBindingNames indexedStatements =
  Map.fromList
    [ (statementIndex, componentStatements)
    | component <- stronglyConnComp graphNodes,
      let componentStatements = componentStatementIndices component,
      isRecursiveComponent component,
      statementIndex <- componentStatements
    ]
  where
    declarationInfo =
      [ (statementIndex, bindingName, valueExpr)
      | (statementIndex, SLet _ bindingName valueExpr) <- indexedStatements
      ]
    firstDeclarationStatementByName =
      foldl' collectFirstDeclaration Map.empty declarationInfo
    baseDependencies =
      Map.fromList
        [ (statementIndex, Set.empty)
        | (statementIndex, _, _) <- declarationInfo
        ]
    (_, _, dependenciesByStatement) =
      foldl'
        addBindingDependencies
        (outerBindingNames, Map.empty, baseDependencies)
        declarationInfo
    graphNodes =
      [ (statementIndex, statementIndex, Set.toList dependencies)
      | (statementIndex, dependencies) <- Map.toList dependenciesByStatement
      ]

    collectFirstDeclaration firstDeclarations (statementIndex, bindingNameText, _) =
      Map.insertWith (\_ firstDeclaration -> firstDeclaration) bindingNameText statementIndex firstDeclarations

    addBindingDependencies (visibleBindingNames, latestDeclarationByName, dependencies) (statementIndex, bindingNameText, valueExpr) =
      let localDependencyNames =
            Set.filter
              (`Map.member` firstDeclarationStatementByName)
              ( freeVarsExprWithVisibleBindings
                  visibleBindingNames
                  Set.empty
                  valueExpr
              )
          resolvedDependencies =
            Set.fromList
              [ dependencyStatementIndex
              | dependencyName <- Set.toList localDependencyNames,
                Just dependencyStatementIndex <-
                  [resolveDependencyStatement latestDeclarationByName statementIndex bindingNameText valueExpr dependencyName]
              ]
       in ( Set.insert bindingNameText visibleBindingNames,
            Map.insert bindingNameText statementIndex latestDeclarationByName,
            Map.insert statementIndex resolvedDependencies dependencies
          )

    resolveDependencyStatement latestDeclarationByName statementIndex bindingNameText valueExpr dependencyName =
      -- Rebindings snapshot the nearest earlier declaration, which the
      -- source-order fold keeps directly. If there is no prior local binding,
      -- fall back to an outer binding before creating a forward edge to the
      -- first local declaration. Same-name references become self-edges for
      -- alias-shaped wrappers and callable-producing initializers, matching
      -- the cells owned during evaluation. Eager scalar self-use stays on the
      -- existing non-recursive path instead of forcing itself into an SCC.
      case Map.lookup dependencyName latestDeclarationByName of
        Just priorDeclaration -> Just priorDeclaration
        Nothing
          | Set.member dependencyName outerBindingNames -> Nothing
          | dependencyName == bindingNameText ->
              if selfReferenceOwnsRecursiveCell bindingNameText valueExpr
                then Just statementIndex
                else Nothing
          | otherwise -> Map.lookup dependencyName firstDeclarationStatementByName

    componentStatementIndices component =
      let memberIndices =
            case component of
              AcyclicSCC componentIndex -> Set.singleton componentIndex
              CyclicSCC indices -> Set.fromList indices
       in -- SCC traversal order is not the declaration order consumed by later
          -- phases, so re-project members through the original statement list.
          [ statementIndex
          | (statementIndex, _) <- indexedStatements,
            Set.member statementIndex memberIndices
          ]

    isRecursiveComponent component =
      case component of
        CyclicSCC _ -> True
        AcyclicSCC statementIndex ->
          Set.member
            statementIndex
            (Map.findWithDefault Set.empty statementIndex dependenciesByStatement)

inferSelfRecursiveBindings :: (CorePhaseNames phase) => Set (CoreNameAt phase) -> (Expr phase -> Bool) -> [(Int, Statement phase)] -> Set Int
inferSelfRecursiveBindings outerBindingNames predicate =
  foldl' step Set.empty
  where
    step recursiveStatements (statementIndex, statement) =
      case statement of
        SLet _ bindingName valueExpr
          | predicate valueExpr,
            selfReferenceOwnsRecursiveCellWith predicate bindingName valueExpr,
            Set.member
              bindingName
              (freeVarsExprWithBound outerBindingNames valueExpr) ->
              Set.insert statementIndex recursiveStatements
        _ -> recursiveStatements

-- | Bindings whose own unresolved definition is referenced by their RHS.
-- Runtime recursion applies a stricter cell-ownership predicate; inference only
-- needs this syntactic set so every occurrence shares the definition's one
-- prepared type variable.
inferSelfReferencedBindings :: (CorePhaseNames phase) => Set (CoreNameAt phase) -> [(Int, Statement phase)] -> Set Int
inferSelfReferencedBindings outerBindingNames =
  foldl' step Set.empty
  where
    step selfReferences (statementIndex, statement) =
      case statement of
        SLet _ bindingName valueExpr
          | Set.member bindingName (freeVarsExprWithBound outerBindingNames valueExpr) ->
              Set.insert statementIndex selfReferences
        _ -> selfReferences

newtype ScopeBindingIdentity = ScopeBindingIdentity [Int]
  deriving (Eq, Ord)

data ScopeBindingExpr phase
  = ScopeBindingExpr
      ScopeBindingIdentity
      (CoreNameAt phase)
      (Expr phase)
      [ScopeBindingExpr phase]
      (Set (CoreNameAt phase))
      [Int]

data ScopeStatementContext phase
  = ScopeStatementContext (Statement phase) [ScopeBindingExpr phase] [Int]

scopeStatementContexts ::
  (CorePhaseNames phase) =>
  [Int] -> Set (CoreNameAt phase) -> [ScopeBindingExpr phase] -> [Statement phase] -> [ScopeStatementContext phase]
scopeStatementContexts scopePath bindingBoundNames initialVisibleBindings statements = contexts
  where
    indexedStatements = zip [0 ..] statements
    contexts = buildContexts initialVisibleBindings indexedStatements
    outerBindingNames =
      Set.union
        bindingBoundNames
        ( Set.fromList
            [ bindingName
            | ScopeBindingExpr _ bindingName _ _ _ _ <- initialVisibleBindings
            ]
        )
    recursiveGroupsByStatement =
      inferRecursiveGroupsOrdered outerBindingNames indexedStatements
    -- This is an intentional lazy knot: bindingByStatement depends on each
    -- definition environment, which in turn reads visibleBindingsByStatement.
    -- Keep ScopeBindingExpr fields and this Data.Map usage lazy.
    bindingByStatement =
      Map.fromList
        [ ( statementIndex,
            ScopeBindingExpr
              (ScopeBindingIdentity (statementPath statementIndex))
              bindingName
              valueExpr
              (definitionBindings statementIndex)
              bindingBoundNames
              (statementPath statementIndex)
          )
        | (statementIndex, SLet _ bindingName valueExpr) <- indexedStatements
        ]
    visibleBindingsByStatement =
      Map.fromList
        [ (statementIndex, visibleBindings)
        | (statementIndex, ScopeStatementContext _ visibleBindings _) <- zip [0 ..] contexts
        ]

    statementPath statementIndex = scopePath <> [statementIndex]

    definitionBindings statementIndex =
      Map.findWithDefault initialVisibleBindings statementIndex visibleBindingsByStatement
        <> bindingsAt
          [ peerIndex
          | peerIndex <- Map.findWithDefault [] statementIndex recursiveGroupsByStatement,
            peerIndex > statementIndex
          ]

    bindingsAt statementIndices =
      [ binding
      | statementIndex <- statementIndices,
        Just binding <- [Map.lookup statementIndex bindingByStatement]
      ]

    buildContexts _ [] = []
    buildContexts visibleBindings ((statementIndex, statement) : rest) =
      let nextVisibleBindings =
            case Map.lookup statementIndex bindingByStatement of
              Just binding -> binding : visibleBindings
              Nothing -> visibleBindings
       in ScopeStatementContext statement visibleBindings (statementPath statementIndex)
            : buildContexts nextVisibleBindings rest

lookupScopeBinding :: (CorePhaseNames phase) => CoreNameAt phase -> [ScopeBindingExpr phase] -> Maybe (ScopeBindingExpr phase)
lookupScopeBinding requestedName =
  find (\(ScopeBindingExpr _ bindingName _ _ _ _) -> bindingName == requestedName)

-- Keep callable-shape recognition beside canonical recursive ownership so
-- nested and top-level scopes agree on lambda self recursion.
exprContainsFunctionBranch :: (CorePhaseNames phase) => Expr phase -> Bool
exprContainsFunctionBranch =
  go [] Set.empty [] Set.empty
  where
    go expressionPath boundNames scopeBindings visitedBindings expr =
      case expr of
        EVar _ bindingName
          | Set.member bindingName boundNames -> False
          | otherwise ->
              case lookupScopeBinding bindingName scopeBindings of
                Just (ScopeBindingExpr identity _ bindingExpr priorBindings bindingBoundNames bindingPath)
                  | Set.notMember identity visitedBindings ->
                      go
                        bindingPath
                        bindingBoundNames
                        priorBindings
                        (Set.insert identity visitedBindings)
                        bindingExpr
                _ -> False
        ELambda {} -> True
        ETypeApplication _ functionExpr _ _ ->
          go (expressionPath <> [0]) boundNames scopeBindings visitedBindings functionExpr
        EIf _ _ thenExpr elseExpr ->
          go (expressionPath <> [1]) boundNames scopeBindings visitedBindings thenExpr
            || go (expressionPath <> [2]) boundNames scopeBindings visitedBindings elseExpr
        EPatternCase _ _ caseArms ->
          any
            ( \(armIndex, CaseArm _ pattern _ bodyExpr) ->
                go
                  (expressionPath <> [1, armIndex])
                  (extendBoundWithPattern pattern boundNames)
                  scopeBindings
                  visitedBindings
                  bodyExpr
            )
            (zip [0 ..] caseArms)
        EBlock _ statements ->
          case reverse (scopeStatementContexts expressionPath boundNames scopeBindings statements) of
            ScopeStatementContext (SExpr _ terminalExpr) terminalBindings terminalPath : _ ->
              go terminalPath boundNames terminalBindings visitedBindings terminalExpr
            _ -> False
        _ -> False

selfReferenceOwnsRecursiveCell :: (CorePhaseNames phase) => CoreNameAt phase -> Expr phase -> Bool
selfReferenceOwnsRecursiveCell =
  selfReferenceOwnsRecursiveCellWith exprContainsFunctionBranch

selfReferenceOwnsRecursiveCellWith :: (CorePhaseNames phase) => (Expr phase -> Bool) -> CoreNameAt phase -> Expr phase -> Bool
selfReferenceOwnsRecursiveCellWith containsFunctionBranch bindingName candidateExpr =
  (hasAliasPath && not hasEagerPath)
    || (containsFunctionBranch candidateExpr && not hasCallableDisqualifyingEagerPath)
  where
    (hasAliasPath, hasEagerPath, hasCallableDisqualifyingEagerPath) =
      aliasSummary [] Set.empty [] Set.empty candidateExpr

    noSummary = (False, False, False)

    combineSummaries
      (leftAliasPath, leftNonAliasPath, leftCallableDisqualifyingPath)
      (rightAliasPath, rightNonAliasPath, rightCallableDisqualifyingPath) =
        ( leftAliasPath || rightAliasPath,
          leftNonAliasPath || rightNonAliasPath,
          leftCallableDisqualifyingPath || rightCallableDisqualifyingPath
        )

    -- A guard selects which callable case-arm body owns the binding. Keep it
    -- eager for alias-only classification, but do not confuse that selection
    -- with an unrelated eager statement before a callable result.
    allowCallablePatternGuard (guardAliasPath, guardEagerPath, _) =
      (guardAliasPath, guardEagerPath, False)

    aliasSummary expressionPath boundNames scopeBindings visitedBindings expr =
      case expr of
        EVar _ name ->
          if Set.member name boundNames
            then noSummary
            else case lookupScopeBinding name scopeBindings of
              Just (ScopeBindingExpr identity _ bindingExpr priorBindings bindingBoundNames bindingPath)
                | Set.notMember identity visitedBindings ->
                    aliasSummary
                      bindingPath
                      bindingBoundNames
                      priorBindings
                      (Set.insert identity visitedBindings)
                      bindingExpr
              Just _ -> noSummary
              Nothing ->
                if name == bindingName
                  then (True, False, False)
                  else noSummary
        EOperatorValue node operatorSymbol
          | not (isBuiltinOperatorSymbol operatorSymbol),
            Set.member bindingName (operatorBindingFreeVar node Set.empty operatorSymbol) ->
              (True, False, False)
        EOperatorValue {} -> noSummary
        ETypeApplication _ functionExpr _ _ ->
          aliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings functionExpr
        EIf _ conditionExpr thenExpr elseExpr ->
          foldl'
            combineSummaries
            (nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings conditionExpr)
            [ aliasSummary (expressionPath <> [1]) boundNames scopeBindings visitedBindings thenExpr,
              aliasSummary (expressionPath <> [2]) boundNames scopeBindings visitedBindings elseExpr
            ]
        EPatternCase _ scrutineeExpr caseArms ->
          foldl'
            combineSummaries
            (nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings scrutineeExpr)
            [ combineSummaries
                ( maybe
                    noSummary
                    ( allowCallablePatternGuard
                        . nonAliasSummary
                          (expressionPath <> [1, armIndex, 0])
                          armBoundNames
                          scopeBindings
                          visitedBindings
                    )
                    guardExpr
                )
                ( aliasSummary
                    (expressionPath <> [1, armIndex, 1])
                    armBoundNames
                    scopeBindings
                    visitedBindings
                    bodyExpr
                )
            | (armIndex, CaseArm _ pattern guardExpr bodyExpr) <- zip [0 ..] caseArms,
              let armBoundNames = extendBoundWithPattern pattern boundNames
            ]
        EBlock _ blockStatements ->
          let contexts = scopeStatementContexts expressionPath boundNames scopeBindings blockStatements
              (eagerStatements, terminalSummary) =
                case reverse contexts of
                  ScopeStatementContext (SExpr _ terminalExpr) terminalBindings terminalPath : reversedLeadingStatements ->
                    ( reverse reversedLeadingStatements,
                      aliasSummary terminalPath boundNames terminalBindings visitedBindings terminalExpr
                    )
                  _ ->
                    (contexts, noSummary)
              eagerBindingSummary =
                foldl'
                  combineSummaries
                  noSummary
                  [ summary
                  | ScopeStatementContext statement statementBindings statementPath <- eagerStatements,
                    summary <-
                      case statement of
                        SLet _ _ valueExpr ->
                          [nonAliasSummary statementPath boundNames statementBindings Set.empty valueExpr]
                        SExpr _ statementExpr ->
                          [nonAliasSummary statementPath boundNames statementBindings Set.empty statementExpr]
                        _ -> []
                  ]
           in combineSummaries terminalSummary eagerBindingSummary
        _ -> nonAliasSummary expressionPath boundNames scopeBindings visitedBindings expr

    nonAliasSummary expressionPath boundNames scopeBindings visitedBindings expr =
      case expr of
        ELit {} -> noSummary
        EVar _ name ->
          nonAliasReferenceSummary boundNames scopeBindings visitedBindings name
        ELambda {} -> noSummary
        EOperatorValue node operatorSymbol ->
          nonAliasOperatorSummary node boundNames scopeBindings visitedBindings operatorSymbol
        EList _ elements ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasSummary (expressionPath <> [elementIndex]) boundNames scopeBindings visitedBindings element
            | (elementIndex, element) <- zip [0 ..] elements
            ]
        ETuple _ elements ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasSummary (expressionPath <> [elementIndex]) boundNames scopeBindings visitedBindings element
            | (elementIndex, element) <- zip [0 ..] elements
            ]
        EApply _ functionExpr argumentExpr ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings functionExpr,
              nonAliasSummary (expressionPath <> [1]) boundNames scopeBindings visitedBindings argumentExpr
            ]
        ETypeApplication _ functionExpr _ _ ->
          nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings functionExpr
        EIf _ conditionExpr thenExpr elseExpr ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings conditionExpr,
              nonAliasSummary (expressionPath <> [1]) boundNames scopeBindings visitedBindings thenExpr,
              nonAliasSummary (expressionPath <> [2]) boundNames scopeBindings visitedBindings elseExpr
            ]
        EPatternCase _ scrutineeExpr caseArms ->
          foldl'
            combineSummaries
            (nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings scrutineeExpr)
            [ combineSummaries
                ( maybe
                    noSummary
                    (nonAliasSummary (expressionPath <> [1, armIndex, 0]) armBoundNames scopeBindings visitedBindings)
                    guardExpr
                )
                ( nonAliasSummary
                    (expressionPath <> [1, armIndex, 1])
                    armBoundNames
                    scopeBindings
                    visitedBindings
                    bodyExpr
                )
            | (armIndex, CaseArm _ pattern guardExpr bodyExpr) <- zip [0 ..] caseArms,
              let armBoundNames = extendBoundWithPattern pattern boundNames
            ]
        EBinary node operatorSymbol leftExpr rightExpr ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasOperatorSummary node boundNames scopeBindings visitedBindings operatorSymbol,
              nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings leftExpr,
              nonAliasSummary (expressionPath <> [1]) boundNames scopeBindings visitedBindings rightExpr
            ]
        ESectionLeft node leftExpr operatorSymbol ->
          combineSummaries
            (nonAliasOperatorSummary node boundNames scopeBindings visitedBindings operatorSymbol)
            (nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings leftExpr)
        ESectionRight node operatorSymbol rightExpr ->
          combineSummaries
            (nonAliasOperatorSummary node boundNames scopeBindings visitedBindings operatorSymbol)
            (nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings rightExpr)
        EBlock _ blockStatements ->
          foldl'
            combineSummaries
            noSummary
            [ summary
            | ScopeStatementContext statement statementBindings statementPath <-
                scopeStatementContexts expressionPath boundNames scopeBindings blockStatements,
              summary <-
                case statement of
                  SLet _ _ valueExpr ->
                    [nonAliasSummary statementPath boundNames statementBindings Set.empty valueExpr]
                  SExpr _ statementExpr ->
                    [nonAliasSummary statementPath boundNames statementBindings Set.empty statementExpr]
                  _ -> []
            ]

    nonAliasOperatorSummary node boundNames scopeBindings visitedBindings operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol = noSummary
      | otherwise =
          case Set.toList (operatorBindingFreeVar node Set.empty operatorSymbol) of
            [binding] ->
              nonAliasReferenceSummary boundNames scopeBindings visitedBindings binding
            _ -> noSummary

    nonAliasReferenceSummary boundNames scopeBindings visitedBindings name
      | Set.member name boundNames = noSummary
      | otherwise =
          case lookupScopeBinding name scopeBindings of
            Just (ScopeBindingExpr identity _ bindingExpr priorBindings bindingBoundNames bindingPath)
              | Set.notMember identity visitedBindings ->
                  nonAliasSummary
                    bindingPath
                    bindingBoundNames
                    priorBindings
                    (Set.insert identity visitedBindings)
                    bindingExpr
            Just _ -> noSummary
            Nothing
              | name == bindingName -> (False, True, True)
              | otherwise -> noSummary
