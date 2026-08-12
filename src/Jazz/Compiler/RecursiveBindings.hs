{-# LANGUAGE OverloadedStrings #-}

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
    inferSelfRecursiveBindings,
    lambdaCaptureHintsChild,
    lookupLambdaCapturedNames,
    prepareRecursiveScope,
    preparedRecursiveScopeBindingNames,
    preparedRecursiveScopeFactsForOuterBindings,
    preparedRecursiveScopeGroups,
    preparedRecursiveScopeOuterBindingNames,
    preparedRecursiveScopeStatements,
    recursiveScopeBindingNames,
    recursiveScopeGroups
  ) where

import Data.Graph
  ( SCC (..),
    stronglyConnComp
  )
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    Statement (..)
  )
import Jazz.Compiler.Name
  ( Name,
    operatorBindingName
  )
import Jazz.Compiler.Pattern
  ( extendBoundWithPattern,
    patternBinderNames
  )
import Jazz.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol
  )

collectBindingNames :: [(Int, Statement)] -> Map Int Name
collectBindingNames =
  foldl' step Map.empty
  where
    step bindingNames (statementIndex, statement) =
      case statement of
        SLet bindingName _ _ ->
          Map.insert statementIndex bindingName bindingNames
        _ -> bindingNames

-- | Immutable local recursion facts for one exact statement scope and outer
-- visibility projection. The product deliberately retains names and integer
-- indices only; callers continue to own the statement AST.
data RecursiveScopeFacts = RecursiveScopeFacts
  { recursiveScopeBindingNames :: Map Int Name,
    recursiveScopeGroups :: Map Int [Int]
  }

buildRecursiveScopeFacts :: Set Name -> [(Int, Statement)] -> RecursiveScopeFacts
buildRecursiveScopeFacts outerBindingNames indexedStatements =
  RecursiveScopeFacts
    { recursiveScopeBindingNames = collectBindingNames indexedStatements,
      recursiveScopeGroups = inferRecursiveGroupsOrderedInternal outerBindingNames indexedStatements
    }

-- | One statement scope paired with the outer visibility projection and
-- recursive facts from which it was derived. The constructor stays private so
-- consumers cannot cross-pair any of the three.
data PreparedRecursiveScope = PreparedRecursiveScope ![Statement] !(Set Name) !RecursiveScopeFacts

prepareRecursiveScope :: Set Name -> [Statement] -> PreparedRecursiveScope
prepareRecursiveScope outerBindingNames statements =
  PreparedRecursiveScope
    statements
    outerBindingNames
    (buildRecursiveScopeFacts outerBindingNames (zip [0 ..] statements))

preparedRecursiveScopeStatements :: PreparedRecursiveScope -> [Statement]
preparedRecursiveScopeStatements (PreparedRecursiveScope statements _ _) = statements

preparedRecursiveScopeOuterBindingNames :: PreparedRecursiveScope -> Set Name
preparedRecursiveScopeOuterBindingNames (PreparedRecursiveScope _ outerBindingNames _) =
  outerBindingNames

-- | Reuse the owned facts when the consumer has the same outer visibility.
-- A prepared scope crossing a compiler boundary with different imports or
-- builtin visibility is repaired from its retained statements rather than
-- silently applying recursion facts derived for another environment.
preparedRecursiveScopeFactsForOuterBindings ::
  Set Name ->
  PreparedRecursiveScope ->
  RecursiveScopeFacts
preparedRecursiveScopeFactsForOuterBindings
  expectedOuterBindingNames
  (PreparedRecursiveScope statements preparedOuterBindingNames recursiveScopeFactsValue)
  | expectedOuterBindingNames == preparedOuterBindingNames = recursiveScopeFactsValue
  | otherwise =
      buildRecursiveScopeFacts expectedOuterBindingNames (zip [0 ..] statements)

preparedRecursiveScopeBindingNames :: PreparedRecursiveScope -> Map Int Name
preparedRecursiveScopeBindingNames (PreparedRecursiveScope _ _ recursiveScopeFactsValue) =
  recursiveScopeBindingNames recursiveScopeFactsValue

preparedRecursiveScopeGroups :: PreparedRecursiveScope -> Map Int [Int]
preparedRecursiveScopeGroups (PreparedRecursiveScope _ _ recursiveScopeFactsValue) =
  recursiveScopeGroups recursiveScopeFactsValue

-- | Free-variable facts arranged in the same child-index shape as the lambda
-- AST. The plan deliberately retains neither lambda bodies nor parameters, so
-- runtime lookup cannot fall back to structural expression equality.
data LambdaCaptureHint = LambdaCaptureHint (Set Name) LambdaCaptureHints

data LambdaCaptureHints = LambdaCaptureHints
  { lambdaCaptureHintAtRoot :: Maybe LambdaCaptureHint,
    lambdaCaptureChildHints :: IntMap LambdaCaptureHints
  }

emptyLambdaCaptureHints :: LambdaCaptureHints
emptyLambdaCaptureHints = LambdaCaptureHints Nothing IntMap.empty

lambdaCaptureHintsChild :: Int -> LambdaCaptureHints -> LambdaCaptureHints
lambdaCaptureHintsChild childIndex =
  IntMap.findWithDefault emptyLambdaCaptureHints childIndex . lambdaCaptureChildHints

collectLambdaCaptureHints :: Expr -> LambdaCaptureHints
collectLambdaCaptureHints = snd . analyzeLambdaCaptures

analyzeLambdaCaptures :: Expr -> (Set Name, LambdaCaptureHints)
analyzeLambdaCaptures expr =
  case expr of
    ELit _ -> emptyCaptureAnalysis
    EVar name -> (Set.singleton name, emptyLambdaCaptureHints)
    ELambda parameterName bodyExpr ->
      let (bodyFreeNames, bodyHints) = analyzeLambdaCaptures bodyExpr
          capturedNames = Set.delete parameterName bodyFreeNames
       in ( capturedNames,
            LambdaCaptureHints
              (Just (LambdaCaptureHint capturedNames bodyHints))
              IntMap.empty
          )
    EOperatorValue operatorSymbol ->
      (operatorBindingFreeVar Set.empty operatorSymbol, emptyLambdaCaptureHints)
    EList elements -> analyzeLambdaChildren elements
    ETuple elements -> analyzeLambdaChildren elements
    EApply functionExpr argumentExpr ->
      analyzeLambdaChildren [functionExpr, argumentExpr]
    ETypeApplication functionExpr _ _ ->
      analyzeLambdaChildren [functionExpr]
    EIf conditionExpr thenExpr elseExpr ->
      analyzeLambdaChildren [conditionExpr, thenExpr, elseExpr]
    EPatternCase scrutineeExpr caseArms ->
      analyzeLambdaPatternCase scrutineeExpr caseArms
    EBinary operatorSymbol leftExpr rightExpr ->
      let (freeNames, hints) = analyzeLambdaChildren [leftExpr, rightExpr]
       in (Set.union (operatorBindingFreeVar Set.empty operatorSymbol) freeNames, hints)
    ESectionLeft leftExpr operatorSymbol ->
      let (freeNames, hints) = analyzeLambdaChildren [leftExpr]
       in (Set.union (operatorBindingFreeVar Set.empty operatorSymbol) freeNames, hints)
    ESectionRight operatorSymbol rightExpr ->
      let (freeNames, hints) = analyzeLambdaChildren [rightExpr]
       in (Set.union (operatorBindingFreeVar Set.empty operatorSymbol) freeNames, hints)
    EBlock statements ->
      analyzeLambdaScope statements

emptyCaptureAnalysis :: (Set Name, LambdaCaptureHints)
emptyCaptureAnalysis = (Set.empty, emptyLambdaCaptureHints)

analyzeLambdaChildren :: [Expr] -> (Set Name, LambdaCaptureHints)
analyzeLambdaChildren expressions =
  ( Set.unions freeNames,
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

analyzeLambdaPatternCase :: Expr -> [CaseArm] -> (Set Name, LambdaCaptureHints)
analyzeLambdaPatternCase scrutineeExpr caseArms =
  foldl' analyzeArm initialAnalysis (zip [0 ..] caseArms)
  where
    (scrutineeFreeNames, scrutineeHints) = analyzeLambdaCaptures scrutineeExpr
    initialAnalysis =
      ( scrutineeFreeNames,
        insertLambdaChildHint 0 scrutineeHints emptyLambdaCaptureHints
      )

    analyzeArm (freeNames, hints) (armIndex, CaseArm pattern guardExpr bodyExpr) =
      ( Set.unions
          [ freeNames,
            Set.difference guardFreeNames boundNames,
            Set.difference bodyFreeNames boundNames
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

analyzeLambdaScope :: [Statement] -> (Set Name, LambdaCaptureHints)
analyzeLambdaScope statements =
  (freeNames, LambdaCaptureHints Nothing childHints)
  where
    (_, freeNames, childHints) =
      foldl' analyzeStatement (Set.empty, Set.empty, IntMap.empty) (zip [0 ..] statements)

    analyzeStatement (boundNames, accumulatedFreeNames, accumulatedHints) (statementIndex, statement) =
      case statement of
        SLet bindingName _ valueExpr ->
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
                Set.union accumulatedFreeNames (Set.difference valueFreeNames boundNames),
                insertLambdaChildHintMap statementIndex valueHints accumulatedHints
              )

insertLambdaChildHint :: Int -> LambdaCaptureHints -> LambdaCaptureHints -> LambdaCaptureHints
insertLambdaChildHint childIndex childHints hints =
  hints
    { lambdaCaptureChildHints =
        insertLambdaChildHintMap childIndex childHints (lambdaCaptureChildHints hints)
    }

insertLambdaChildHintMap :: Int -> LambdaCaptureHints -> IntMap LambdaCaptureHints -> IntMap LambdaCaptureHints
insertLambdaChildHintMap childIndex childHints hints
  | lambdaCaptureHintsAreEmpty childHints = hints
  | otherwise = IntMap.insert childIndex childHints hints

lambdaCaptureHintsAreEmpty :: LambdaCaptureHints -> Bool
lambdaCaptureHintsAreEmpty (LambdaCaptureHints Nothing childHints) = IntMap.null childHints
lambdaCaptureHintsAreEmpty _ = False

lookupLambdaCapturedNames :: LambdaCaptureHints -> Maybe (Set Name, LambdaCaptureHints)
lookupLambdaCapturedNames hints =
  case lambdaCaptureHintAtRoot hints of
    Just (LambdaCaptureHint capturedNames nestedHints) -> Just (capturedNames, nestedHints)
    Nothing -> Nothing

freeVarsExprWithBound :: Set Name -> Expr -> Set Name
freeVarsExprWithBound = freeVarsExprWithVisibleBindings Set.empty

freeVarsExprWithVisibleBindings :: Set Name -> Set Name -> Expr -> Set Name
freeVarsExprWithVisibleBindings visibleBindingNames =
  freeVarsExprUsing (freeVarsScopeWithVisibleBindings visibleBindingNames)

-- | Names that may need to come from the environment when a closure is
-- created. Unlike recursive-binding analysis, an ordinary binding is not in
-- scope in its own initializer: a same-name reference snapshots a previously
-- visible value. Recursive cells supplied by scope evaluation are harmless
-- candidates here because restricting an environment drops absent names.
closureCaptureCandidatesWithBound :: Set Name -> Expr -> Set Name
closureCaptureCandidatesWithBound =
  freeVarsExprUsing closureCaptureCandidatesScopeWithBound

freeVarsExprUsing :: (Set Name -> [Statement] -> Set Name) -> Set Name -> Expr -> Set Name
freeVarsExprUsing scopeFreeVars bound expr =
  case expr of
    ELit _ -> Set.empty
    EVar name
      | Set.member name bound -> Set.empty
      | otherwise -> Set.singleton name
    ELambda parameterName bodyExpr ->
      freeVarsExprUsing scopeFreeVars
        (Set.insert parameterName bound)
        bodyExpr
    EOperatorValue operatorSymbol ->
      operatorBindingFreeVar bound operatorSymbol
    EList elements ->
      Set.unions (map (freeVarsExprUsing scopeFreeVars bound) elements)
    ETuple elements ->
      Set.unions (map (freeVarsExprUsing scopeFreeVars bound) elements)
    EApply functionExpr argumentExpr ->
      Set.union
        (freeVarsExprUsing scopeFreeVars bound functionExpr)
        (freeVarsExprUsing scopeFreeVars bound argumentExpr)
    ETypeApplication functionExpr _ _ ->
      freeVarsExprUsing scopeFreeVars bound functionExpr
    EIf conditionExpr thenExpr elseExpr ->
      Set.unions
        [ freeVarsExprUsing scopeFreeVars bound conditionExpr,
          freeVarsExprUsing scopeFreeVars bound thenExpr,
          freeVarsExprUsing scopeFreeVars bound elseExpr
        ]
    EPatternCase scrutineeExpr caseArms ->
      Set.unions
        ( freeVarsExprUsing scopeFreeVars bound scrutineeExpr :
          [ Set.union
              (maybe Set.empty (freeVarsExprUsing scopeFreeVars armBound) guardExpr)
              (freeVarsExprUsing scopeFreeVars armBound bodyExpr)
          | CaseArm pattern guardExpr bodyExpr <- caseArms,
            let armBound = extendBoundWithPattern pattern bound
          ]
        )
    EBinary operatorSymbol leftExpr rightExpr ->
      Set.unions
        [ operatorBindingFreeVar bound operatorSymbol,
          freeVarsExprUsing scopeFreeVars bound leftExpr,
          freeVarsExprUsing scopeFreeVars bound rightExpr
        ]
    ESectionLeft leftExpr operatorSymbol ->
      Set.union
        (operatorBindingFreeVar bound operatorSymbol)
        (freeVarsExprUsing scopeFreeVars bound leftExpr)
    ESectionRight operatorSymbol rightExpr ->
      Set.union
        (operatorBindingFreeVar bound operatorSymbol)
        (freeVarsExprUsing scopeFreeVars bound rightExpr)
    EBlock statements ->
      scopeFreeVars bound statements

operatorBindingFreeVar :: Set Name -> Text -> Set Name
operatorBindingFreeVar bound operatorSymbol
  | isBuiltinOperatorSymbol operatorSymbol = Set.empty
  | Set.member bindingName bound = Set.empty
  | otherwise = Set.singleton bindingName
  where
    bindingName = operatorBindingName operatorSymbol

closureCaptureCandidatesScopeWithBound :: Set Name -> [Statement] -> Set Name
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
        SLet bindingName _ valueExpr ->
          ( Set.insert bindingName boundNames,
            Set.union
              captureCandidates
              (closureCaptureCandidatesWithBound boundNames valueExpr)
          )

freeVarsScopeWithBound :: Set Name -> [Statement] -> Set Name
freeVarsScopeWithBound = freeVarsScopeWithVisibleBindings Set.empty

freeVarsScopeWithVisibleBindings :: Set Name -> Set Name -> [Statement] -> Set Name
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
        SLet bindingName _ valueExpr ->
          let boundWithSelf = Set.insert bindingName boundNames
              rhsBoundNames = Set.union boundNames (recursiveGroupMemberNames statementIndex)
           in
            ( boundWithSelf,
              Set.union
                freeNames
                (freeVarsExprWithVisibleBindings visibleBindingNames rhsBoundNames valueExpr)
            )

inferRecursiveGroupsOrdered :: Set Name -> [(Int, Statement)] -> Map Int [Int]
inferRecursiveGroupsOrdered outerBindingNames =
  recursiveScopeGroups . buildRecursiveScopeFacts outerBindingNames

inferRecursiveGroupsOrderedInternal :: Set Name -> [(Int, Statement)] -> Map Int [Int]
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
        | (statementIndex, SLet bindingName _ valueExpr) <- indexedStatements
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
       in
        -- SCC traversal order is not the declaration order consumed by later
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

inferSelfRecursiveBindings :: Set Name -> (Expr -> Bool) -> [(Int, Statement)] -> Set Int
inferSelfRecursiveBindings outerBindingNames predicate =
  foldl' step Set.empty
  where
    step recursiveStatements (statementIndex, statement) =
      case statement of
        SLet bindingName _ valueExpr
          | predicate valueExpr,
            selfReferenceOwnsRecursiveCellWith predicate bindingName valueExpr,
            Set.member
              bindingName
              (freeVarsExprWithBound outerBindingNames valueExpr) ->
              Set.insert statementIndex recursiveStatements
        _ -> recursiveStatements

newtype ScopeBindingIdentity = ScopeBindingIdentity [Int]
  deriving (Eq, Ord)

data ScopeBindingExpr =
  ScopeBindingExpr
    ScopeBindingIdentity
    Name
    Expr
    [ScopeBindingExpr]
    (Set Name)
    [Int]

data ScopeStatementContext =
  ScopeStatementContext Statement [ScopeBindingExpr] [Int]

scopeStatementContexts :: [Int] -> Set Name -> [ScopeBindingExpr] -> [Statement] -> [ScopeStatementContext]
scopeStatementContexts scopePath bindingBoundNames = go 0
  where
    go _ _ [] = []
    go statementIndex visibleBindings (statement : rest) =
      let statementPath = scopePath <> [statementIndex]
          nextVisibleBindings =
            case statement of
              SLet bindingName _ valueExpr ->
                ScopeBindingExpr
                  (ScopeBindingIdentity statementPath)
                  bindingName
                  valueExpr
                  visibleBindings
                  bindingBoundNames
                  statementPath
                  : visibleBindings
              _ -> visibleBindings
       in
        ScopeStatementContext statement visibleBindings statementPath
          : go (statementIndex + 1) nextVisibleBindings rest

lookupScopeBinding :: Name -> [ScopeBindingExpr] -> Maybe ScopeBindingExpr
lookupScopeBinding requestedName =
  go
  where
    go [] = Nothing
    go (binding@(ScopeBindingExpr _ bindingName _ _ _ _) : rest)
      | bindingName == requestedName = Just binding
      | otherwise = go rest

-- Keep callable-shape recognition beside canonical recursive ownership so
-- nested and top-level scopes agree on lambda self recursion.
exprContainsFunctionBranch :: Expr -> Bool
exprContainsFunctionBranch =
  go [] Set.empty [] Set.empty
  where
    go expressionPath boundNames scopeBindings visitedBindings expr =
      case expr of
        EVar bindingName
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
        ETypeApplication functionExpr _ _ ->
          go (expressionPath <> [0]) boundNames scopeBindings visitedBindings functionExpr
        EIf _ thenExpr elseExpr ->
          go (expressionPath <> [1]) boundNames scopeBindings visitedBindings thenExpr
            || go (expressionPath <> [2]) boundNames scopeBindings visitedBindings elseExpr
        EPatternCase _ caseArms ->
          any
            ( \(armIndex, CaseArm pattern _ bodyExpr) ->
                go
                  (expressionPath <> [1, armIndex])
                  (extendBoundWithPattern pattern boundNames)
                  scopeBindings
                  visitedBindings
                  bodyExpr
            )
            (zip [0 ..] caseArms)
        EBlock statements ->
          case reverse (scopeStatementContexts expressionPath boundNames scopeBindings statements) of
            ScopeStatementContext (SExpr _ terminalExpr) terminalBindings terminalPath : _ ->
              go terminalPath boundNames terminalBindings visitedBindings terminalExpr
            _ -> False
        _ -> False

selfReferenceOwnsRecursiveCell :: Name -> Expr -> Bool
selfReferenceOwnsRecursiveCell =
  selfReferenceOwnsRecursiveCellWith exprContainsFunctionBranch

selfReferenceOwnsRecursiveCellWith :: (Expr -> Bool) -> Name -> Expr -> Bool
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
        EVar name ->
          if Set.member name boundNames
              then noSummary
              else
                case lookupScopeBinding name scopeBindings of
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
        EOperatorValue operatorSymbol
          | not (isBuiltinOperatorSymbol operatorSymbol),
            operatorBindingName operatorSymbol == bindingName ->
              (True, False, False)
        EOperatorValue {} -> noSummary
        ETypeApplication functionExpr _ _ ->
          aliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings functionExpr
        EIf conditionExpr thenExpr elseExpr ->
          foldl'
            combineSummaries
            (nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings conditionExpr)
            [ aliasSummary (expressionPath <> [1]) boundNames scopeBindings visitedBindings thenExpr,
              aliasSummary (expressionPath <> [2]) boundNames scopeBindings visitedBindings elseExpr
            ]
        EPatternCase scrutineeExpr caseArms ->
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
              | (armIndex, CaseArm pattern guardExpr bodyExpr) <- zip [0 ..] caseArms,
                let armBoundNames = extendBoundWithPattern pattern boundNames
            ]
        EBlock blockStatements ->
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
           in
            combineSummaries terminalSummary eagerBindingSummary
        _ -> nonAliasSummary expressionPath boundNames scopeBindings visitedBindings expr

    nonAliasSummary expressionPath boundNames scopeBindings visitedBindings expr =
      case expr of
        ELit {} -> noSummary
        EVar name ->
          nonAliasReferenceSummary boundNames scopeBindings visitedBindings name
        ELambda {} -> noSummary
        EOperatorValue operatorSymbol ->
          nonAliasOperatorSummary boundNames scopeBindings visitedBindings operatorSymbol
        EList elements ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasSummary (expressionPath <> [elementIndex]) boundNames scopeBindings visitedBindings element
              | (elementIndex, element) <- zip [0 ..] elements
            ]
        ETuple elements ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasSummary (expressionPath <> [elementIndex]) boundNames scopeBindings visitedBindings element
              | (elementIndex, element) <- zip [0 ..] elements
            ]
        EApply functionExpr argumentExpr ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings functionExpr,
              nonAliasSummary (expressionPath <> [1]) boundNames scopeBindings visitedBindings argumentExpr
            ]
        ETypeApplication functionExpr _ _ ->
          nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings functionExpr
        EIf conditionExpr thenExpr elseExpr ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings conditionExpr,
              nonAliasSummary (expressionPath <> [1]) boundNames scopeBindings visitedBindings thenExpr,
              nonAliasSummary (expressionPath <> [2]) boundNames scopeBindings visitedBindings elseExpr
            ]
        EPatternCase scrutineeExpr caseArms ->
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
              | (armIndex, CaseArm pattern guardExpr bodyExpr) <- zip [0 ..] caseArms,
                let armBoundNames = extendBoundWithPattern pattern boundNames
            ]
        EBinary operatorSymbol leftExpr rightExpr ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasOperatorSummary boundNames scopeBindings visitedBindings operatorSymbol,
              nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings leftExpr,
              nonAliasSummary (expressionPath <> [1]) boundNames scopeBindings visitedBindings rightExpr
            ]
        ESectionLeft leftExpr operatorSymbol ->
          combineSummaries
            (nonAliasOperatorSummary boundNames scopeBindings visitedBindings operatorSymbol)
            (nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings leftExpr)
        ESectionRight operatorSymbol rightExpr ->
          combineSummaries
            (nonAliasOperatorSummary boundNames scopeBindings visitedBindings operatorSymbol)
            (nonAliasSummary (expressionPath <> [0]) boundNames scopeBindings visitedBindings rightExpr)
        EBlock blockStatements ->
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

    nonAliasOperatorSummary boundNames scopeBindings visitedBindings operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol = noSummary
      | otherwise =
          nonAliasReferenceSummary
            boundNames
            scopeBindings
            visitedBindings
            (operatorBindingName operatorSymbol)

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
