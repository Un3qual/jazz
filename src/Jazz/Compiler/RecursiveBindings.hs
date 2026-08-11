{-# LANGUAGE OverloadedStrings #-}

-- | Shared recursive-binding graph and free-variable helpers used by analyzer,
-- type inference, and runtime.
module Jazz.Compiler.RecursiveBindings
  ( LambdaCaptureHints,
    closureCaptureCandidatesWithBound,
    collectBindingNames,
    collectLambdaCaptureHints,
    freeVarsExprWithBound,
    freeVarsScopeWithBound,
    exprContainsFunctionBranch,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings,
    lookupLambdaCapturedNames
  ) where

import Data.Graph
  ( SCC (..),
    stronglyConnComp
  )
import Data.List
  ( unsnoc
  )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    ImplMethod (..),
    Statement (..)
  )
import Jazz.Compiler.Name
  ( Name,
    operatorBindingName
  )
import Jazz.Compiler.Pattern
  ( extendBoundWithPattern
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

-- | Free-variable facts arranged in the same nesting shape as the lambda AST.
-- Closures retain only their body's nested hints, avoiding both repeated AST
-- walks and retention of unrelated sibling expressions.
data LambdaCaptureHint = LambdaCaptureHint Name Expr (Set Name) LambdaCaptureHints

type LambdaCaptureHints = [LambdaCaptureHint]

collectLambdaCaptureHints :: Expr -> LambdaCaptureHints
collectLambdaCaptureHints expr =
  case expr of
    ELit _ -> []
    EVar _ -> []
    ELambda parameterName bodyExpr ->
      [ LambdaCaptureHint
          parameterName
          bodyExpr
          (closureCaptureCandidatesWithBound (Set.singleton parameterName) bodyExpr)
          (collectLambdaCaptureHints bodyExpr)
      ]
    EOperatorValue _ -> []
    EList elements -> concatMap collectLambdaCaptureHints elements
    ETuple elements -> concatMap collectLambdaCaptureHints elements
    EApply functionExpr argumentExpr ->
      collectLambdaCaptureHints functionExpr
        <> collectLambdaCaptureHints argumentExpr
    ETypeApplication functionExpr _ _ ->
      collectLambdaCaptureHints functionExpr
    EIf conditionExpr thenExpr elseExpr ->
      collectLambdaCaptureHints conditionExpr
        <> collectLambdaCaptureHints thenExpr
        <> collectLambdaCaptureHints elseExpr
    EPatternCase scrutineeExpr caseArms ->
      collectLambdaCaptureHints scrutineeExpr
        <> concatMap collectCaseArmLambdaCaptureHints caseArms
    EBinary _ leftExpr rightExpr ->
      collectLambdaCaptureHints leftExpr
        <> collectLambdaCaptureHints rightExpr
    ESectionLeft leftExpr _ ->
      collectLambdaCaptureHints leftExpr
    ESectionRight _ rightExpr ->
      collectLambdaCaptureHints rightExpr
    EBlock statements ->
      concatMap collectStatementLambdaCaptureHints statements

collectCaseArmLambdaCaptureHints :: CaseArm -> LambdaCaptureHints
collectCaseArmLambdaCaptureHints (CaseArm _ guardExpr bodyExpr) =
  maybe [] collectLambdaCaptureHints guardExpr
    <> collectLambdaCaptureHints bodyExpr

collectStatementLambdaCaptureHints :: Statement -> LambdaCaptureHints
collectStatementLambdaCaptureHints statement =
  case statement of
    SLet _ _ valueExpr -> collectLambdaCaptureHints valueExpr
    SImpl _ _ _ methods ->
      concatMap
        (\(ImplMethod _ _ methodExpr) -> collectLambdaCaptureHints methodExpr)
        methods
    SExpr _ valueExpr -> collectLambdaCaptureHints valueExpr
    SSignature {} -> []
    SData {} -> []
    SClass {} -> []
    SModule {} -> []
    SImport {} -> []

lookupLambdaCapturedNames :: Name -> Expr -> LambdaCaptureHints -> Maybe (Set Name, LambdaCaptureHints)
lookupLambdaCapturedNames parameterName bodyExpr =
  go
  where
    go [] = Nothing
    go (LambdaCaptureHint hintParameter hintBody capturedNames nestedHints : rest)
      | parameterName == hintParameter,
        bodyExpr == hintBody =
          Just (capturedNames, nestedHints)
      | otherwise = go rest

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
    recursiveGroupsByStatement =
      inferRecursiveGroupsOrdered
        (Set.union visibleBindingNames initialBound)
        indexedStatements
    bindingNamesByStatement = collectBindingNames indexedStatements

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
inferRecursiveGroupsOrdered outerBindingNames indexedStatements =
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
    declarationStatementsByName =
      foldl' collectDeclaration Map.empty declarationInfo
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

    collectDeclaration declarationsByName (statementIndex, bindingNameText, _) =
      Map.insertWith (\new old -> old ++ new) bindingNameText [statementIndex] declarationsByName

    addBindingDependencies dependencies (statementIndex, bindingNameText, valueExpr) =
      let localDependencyNames =
            Set.filter
              (`Map.member` declarationStatementsByName)
              ( freeVarsExprWithVisibleBindings
                  (visibleBindingNamesBefore statementIndex)
                  Set.empty
                  valueExpr
              )
          resolvedDependencies =
            Set.fromList
              [ dependencyStatementIndex
                | dependencyName <- Set.toList localDependencyNames,
                  Just dependencyStatementIndex <-
                    [resolveDependencyStatement statementIndex bindingNameText valueExpr dependencyName]
              ]
       in
        Map.insert statementIndex resolvedDependencies dependencies

    visibleBindingNamesBefore statementIndex =
      Set.union
        outerBindingNames
        ( Set.fromList
            [ bindingName
              | (candidateIndex, bindingName, _) <- declarationInfo,
                candidateIndex < statementIndex
            ]
        )

    resolveDependencyStatement statementIndex bindingNameText valueExpr dependencyName =
      case Map.lookup dependencyName declarationStatementsByName of
        Nothing -> Nothing
        Just declarationStatements ->
          -- Rebindings snapshot the nearest earlier declaration. If there is no
          -- prior local binding, fall back to an outer binding before creating
          -- a forward edge to the first later local declaration. Same-name
          -- references become self-edges for alias-shaped wrappers and
          -- callable-producing initializers, matching the cells owned during
          -- evaluation. Eager scalar self-use stays on the existing
          -- non-recursive path instead of forcing itself into an SCC.
          case closestPriorDeclaration declarationStatements of
            Just prior -> Just prior
            Nothing
              | Set.member dependencyName outerBindingNames -> Nothing
              | dependencyName == bindingNameText ->
                  if selfReferenceOwnsRecursiveCell bindingNameText valueExpr
                    then Just statementIndex
                    else Nothing
              | otherwise -> closestFutureDeclaration declarationStatements
      where
        closestPriorDeclaration declarations =
          case unsnoc (filter (< statementIndex) declarations) of
            Nothing -> Nothing
            Just (_, priorDeclaration) -> Just priorDeclaration

        closestFutureDeclaration declarations =
          case filter (> statementIndex) declarations of
            [] -> Nothing
            firstFuture : _ -> Just firstFuture

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

inferSelfRecursiveBindings :: (Expr -> Bool) -> [(Int, Statement)] -> Set Int
inferSelfRecursiveBindings predicate =
  foldl' step Set.empty
  where
    step recursiveStatements (statementIndex, statement) =
      case statement of
        SLet bindingName _ valueExpr
          | predicate valueExpr,
            selfReferenceOwnsRecursiveCellWith predicate bindingName valueExpr,
            Set.member
              bindingName
              (freeVarsExprWithBound Set.empty valueExpr) ->
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
    [Int]

data ScopeStatementContext =
  ScopeStatementContext Statement [ScopeBindingExpr] [Int]

scopeStatementContexts :: [Int] -> [ScopeBindingExpr] -> [Statement] -> [ScopeStatementContext]
scopeStatementContexts scopePath = go 0
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
    go (binding@(ScopeBindingExpr _ bindingName _ _ _) : rest)
      | bindingName == requestedName = Just binding
      | otherwise = go rest

-- Keep callable-shape recognition beside canonical recursive ownership so
-- nested and top-level scopes agree on lambda self recursion.
exprContainsFunctionBranch :: Expr -> Bool
exprContainsFunctionBranch =
  go [] [] Set.empty
  where
    go expressionPath scopeBindings visitedBindings expr =
      case expr of
        EVar bindingName ->
          case lookupScopeBinding bindingName scopeBindings of
            Just (ScopeBindingExpr identity _ bindingExpr priorBindings bindingPath)
              | Set.notMember identity visitedBindings ->
                  go
                    bindingPath
                    priorBindings
                    (Set.insert identity visitedBindings)
                    bindingExpr
            _ -> False
        ELambda {} -> True
        EIf _ thenExpr elseExpr ->
          go (expressionPath <> [1]) scopeBindings visitedBindings thenExpr
            || go (expressionPath <> [2]) scopeBindings visitedBindings elseExpr
        EPatternCase _ caseArms ->
          any
            ( \(armIndex, CaseArm _ _ bodyExpr) ->
                go
                  (expressionPath <> [1, armIndex])
                  scopeBindings
                  visitedBindings
                  bodyExpr
            )
            (zip [0 ..] caseArms)
        EBlock statements ->
          case reverse (scopeStatementContexts expressionPath scopeBindings statements) of
            ScopeStatementContext (SExpr _ terminalExpr) terminalBindings terminalPath : _ ->
              go terminalPath terminalBindings visitedBindings terminalExpr
            _ -> False
        _ -> False

selfReferenceOwnsRecursiveCell :: Name -> Expr -> Bool
selfReferenceOwnsRecursiveCell =
  selfReferenceOwnsRecursiveCellWith exprContainsFunctionBranch

selfReferenceOwnsRecursiveCellWith :: (Expr -> Bool) -> Name -> Expr -> Bool
selfReferenceOwnsRecursiveCellWith containsFunctionBranch bindingName candidateExpr =
  not hasEagerPath
    && (hasAliasPath || containsFunctionBranch candidateExpr)
  where
    (hasAliasPath, hasEagerPath) =
      aliasSummary [] Set.empty [] Set.empty candidateExpr

    noSummary = (False, False)

    combineSummaries (leftAliasPath, leftNonAliasPath) (rightAliasPath, rightNonAliasPath) =
      ( leftAliasPath || rightAliasPath,
        leftNonAliasPath || rightNonAliasPath
      )

    aliasSummary expressionPath boundNames scopeBindings visitedBindings expr =
      case expr of
        EVar name ->
          if Set.member name boundNames
              then noSummary
              else
                case lookupScopeBinding name scopeBindings of
                  Just (ScopeBindingExpr identity _ bindingExpr priorBindings bindingPath)
                    | Set.notMember identity visitedBindings ->
                        aliasSummary
                          bindingPath
                          boundNames
                          priorBindings
                          (Set.insert identity visitedBindings)
                          bindingExpr
                  Just _ -> noSummary
                  Nothing ->
                    if name == bindingName
                      then (True, False)
                      else noSummary
        EOperatorValue operatorSymbol
          | not (isBuiltinOperatorSymbol operatorSymbol),
            operatorBindingName operatorSymbol == bindingName ->
              (True, False)
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
                    (nonAliasSummary (expressionPath <> [1, armIndex, 0]) armBoundNames scopeBindings visitedBindings)
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
          let contexts = scopeStatementContexts expressionPath scopeBindings blockStatements
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
                  scopeStatementContexts expressionPath scopeBindings blockStatements,
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
            Just (ScopeBindingExpr identity _ bindingExpr priorBindings bindingPath)
              | Set.notMember identity visitedBindings ->
                  nonAliasSummary
                    bindingPath
                    boundNames
                    priorBindings
                    (Set.insert identity visitedBindings)
                    bindingExpr
            Just _ -> noSummary
            Nothing
              | name == bindingName -> (False, True)
              | otherwise -> noSummary
