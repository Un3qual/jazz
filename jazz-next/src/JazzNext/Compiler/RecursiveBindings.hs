{-# LANGUAGE OverloadedStrings #-}

-- | Shared recursive-binding graph and free-variable helpers used by analyzer,
-- type inference, and runtime.
module JazzNext.Compiler.RecursiveBindings
  ( collectBindingNames,
    freeVarsExprWithBound,
    freeVarsScopeWithBound,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings
  ) where

import Data.Graph
  ( SCC (..),
    stronglyConnComp
  )
import Data.List
  ( foldl'
  )
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    Pattern (..),
    Statement (..)
  )
import JazzNext.Compiler.Identifier
  ( identifierText
  )

collectBindingNames :: [(Int, Statement)] -> Map Int Text
collectBindingNames =
  foldl' step Map.empty
  where
    step bindingNames (statementIndex, statement) =
      case statement of
        SLet bindingName _ _ ->
          Map.insert statementIndex (identifierText bindingName) bindingNames
        _ -> bindingNames

freeVarsExprWithBound :: Set Text -> Expr -> Set Text
freeVarsExprWithBound bound expr =
  case expr of
    ELit _ -> Set.empty
    EVar name
      | Set.member nameText bound -> Set.empty
      | otherwise -> Set.singleton nameText
      where
        nameText = identifierText name
    ELambda parameterName bodyExpr ->
      freeVarsExprWithBound
        (Set.insert (identifierText parameterName) bound)
        bodyExpr
    EOperatorValue _ -> Set.empty
    EList elements ->
      Set.unions (map (freeVarsExprWithBound bound) elements)
    EApply functionExpr argumentExpr ->
      Set.union
        (freeVarsExprWithBound bound functionExpr)
        (freeVarsExprWithBound bound argumentExpr)
    EIf conditionExpr thenExpr elseExpr ->
      freeVarsExprWithBound bound (ECase conditionExpr thenExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr ->
      Set.unions
        [ freeVarsExprWithBound bound conditionExpr,
          freeVarsExprWithBound bound thenExpr,
          freeVarsExprWithBound bound elseExpr
        ]
    EPatternCase scrutineeExpr caseArms ->
      Set.unions
        ( freeVarsExprWithBound bound scrutineeExpr :
          [ freeVarsExprWithBound (extendBoundWithPattern pattern bound) bodyExpr
          | CaseArm pattern bodyExpr <- caseArms
          ]
        )
    EBinary _ leftExpr rightExpr ->
      Set.union
        (freeVarsExprWithBound bound leftExpr)
        (freeVarsExprWithBound bound rightExpr)
    ESectionLeft leftExpr _ ->
      freeVarsExprWithBound bound leftExpr
    ESectionRight _ rightExpr ->
      freeVarsExprWithBound bound rightExpr
    EBlock statements ->
      freeVarsScopeWithBound bound statements

freeVarsScopeWithBound :: Set Text -> [Statement] -> Set Text
freeVarsScopeWithBound initialBound statements =
  snd (foldl' step (initialBound, Set.empty) indexedStatements)
  where
    indexedStatements = zip [0 ..] statements
    recursiveGroupsByStatement =
      inferRecursiveGroupsOrdered initialBound indexedStatements
    bindingNamesByStatement = collectBindingNames indexedStatements

    recursivePeerNames statementIndex =
      Set.fromList
        [ peerName
          | peerIndex <- Map.findWithDefault [] statementIndex recursiveGroupsByStatement,
            peerIndex /= statementIndex,
            Just peerName <- [Map.lookup peerIndex bindingNamesByStatement]
        ]

    step (boundNames, freeNames) (statementIndex, statement) =
      case statement of
        SSignature {} -> (boundNames, freeNames)
        SModule {} -> (boundNames, freeNames)
        SImport {} -> (boundNames, freeNames)
        SData {} -> (boundNames, freeNames)
        SExpr _ expr ->
          ( boundNames,
            Set.union freeNames (freeVarsExprWithBound boundNames expr)
          )
        SLet bindingName _ valueExpr ->
          let boundWithSelf = Set.insert (identifierText bindingName) boundNames
              rhsBoundNames = Set.union boundWithSelf (recursivePeerNames statementIndex)
           in
            ( boundWithSelf,
              Set.union freeNames (freeVarsExprWithBound rhsBoundNames valueExpr)
            )

inferRecursiveGroupsOrdered :: Set Text -> [(Int, Statement)] -> Map Int [Int]
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
      [ (statementIndex, identifierText bindingName, valueExpr)
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
              (freeVarsExprWithBound Set.empty valueExpr)
          resolvedDependencies =
            Set.fromList
              [ dependencyStatementIndex
                | dependencyName <- Set.toList localDependencyNames,
                  Just dependencyStatementIndex <-
                    [resolveDependencyStatement statementIndex bindingNameText valueExpr dependencyName]
              ]
       in
        Map.insert statementIndex resolvedDependencies dependencies

    resolveDependencyStatement statementIndex bindingNameText valueExpr dependencyName =
      case Map.lookup dependencyName declarationStatementsByName of
        Nothing -> Nothing
        Just declarationStatements ->
          -- Rebindings snapshot the nearest earlier declaration. If there is no
          -- prior local binding, fall back to an outer binding before creating
          -- a forward edge to the first later local declaration. Same-name
          -- references only become self-edges for alias-shaped wrappers; eager
          -- self-use stays on the existing non-recursive path instead of
          -- forcing itself into an SCC.
          case closestPriorDeclaration declarationStatements of
            Just prior -> Just prior
            Nothing
              | Set.member dependencyName outerBindingNames -> Nothing
              | dependencyName == bindingNameText ->
                  if selfAliasLikeReference bindingNameText valueExpr
                    then Just statementIndex
                    else Nothing
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
            Set.member
              (identifierText bindingName)
              (freeVarsExprWithBound Set.empty valueExpr) ->
              Set.insert statementIndex recursiveStatements
        _ -> recursiveStatements

selfAliasLikeReference :: Text -> Expr -> Bool
selfAliasLikeReference bindingNameText =
  isAliasOnly . aliasSummary Set.empty Map.empty Set.empty
  where
    isAliasOnly (hasAliasPath, hasNonAliasPath) =
      hasAliasPath && not hasNonAliasPath

    noSummary = (False, False)

    combineSummaries (leftAliasPath, leftNonAliasPath) (rightAliasPath, rightNonAliasPath) =
      ( leftAliasPath || rightAliasPath,
        leftNonAliasPath || rightNonAliasPath
      )

    aliasSummary boundNames scopeBindings visitedBindings expr =
      case expr of
        EVar name ->
          let nameText = identifierText name
           in
            if Set.member nameText boundNames
              then noSummary
              else
                case Map.lookup nameText scopeBindings of
                  Just bindingExpr
                    | Set.notMember nameText visitedBindings ->
                        aliasSummary
                          boundNames
                          scopeBindings
                          (Set.insert nameText visitedBindings)
                          bindingExpr
                  _ ->
                    if nameText == bindingNameText
                      then (True, False)
                      else noSummary
        EIf conditionExpr thenExpr elseExpr ->
          foldl'
            combineSummaries
            (nonAliasSummary boundNames scopeBindings visitedBindings conditionExpr)
            [ aliasSummary boundNames scopeBindings visitedBindings thenExpr,
              aliasSummary boundNames scopeBindings visitedBindings elseExpr
            ]
        ECase conditionExpr thenExpr elseExpr ->
          foldl'
            combineSummaries
            (nonAliasSummary boundNames scopeBindings visitedBindings conditionExpr)
            [ aliasSummary boundNames scopeBindings visitedBindings thenExpr,
              aliasSummary boundNames scopeBindings visitedBindings elseExpr
            ]
        EPatternCase scrutineeExpr caseArms ->
          foldl'
            combineSummaries
            (nonAliasSummary boundNames scopeBindings visitedBindings scrutineeExpr)
            [ aliasSummary
                (extendBoundWithPattern pattern boundNames)
                scopeBindings
                visitedBindings
                bodyExpr
              | CaseArm pattern bodyExpr <- caseArms
            ]
        EBlock blockStatements ->
          let localScopeBindings = collectScopeBindingExprs blockStatements
              blockScopeBindings = localScopeBindings `Map.union` scopeBindings
              (eagerStatements, terminalSummary) =
                case reverse blockStatements of
                  SExpr _ terminalExpr : reversedLeadingStatements ->
                    ( reverse reversedLeadingStatements,
                      aliasSummary boundNames blockScopeBindings visitedBindings terminalExpr
                    )
                  _ ->
                    (blockStatements, noSummary)
              eagerBindingSummary =
                foldl'
                  combineSummaries
                  noSummary
                  [ summary
                    | statement <- eagerStatements,
                      summary <-
                        case statement of
                          SLet _ _ valueExpr ->
                            [nonAliasSummary boundNames blockScopeBindings Set.empty valueExpr]
                          SExpr _ statementExpr ->
                            [nonAliasSummary boundNames blockScopeBindings Set.empty statementExpr]
                          _ -> []
                  ]
           in
            combineSummaries terminalSummary eagerBindingSummary
        _ -> nonAliasSummary boundNames scopeBindings visitedBindings expr

    nonAliasSummary boundNames scopeBindings visitedBindings expr =
      case expr of
        ELit {} -> noSummary
        EVar name ->
          let nameText = identifierText name
           in
            if Set.member nameText boundNames
              then noSummary
              else
                case Map.lookup nameText scopeBindings of
                  Just bindingExpr
                    | Set.notMember nameText visitedBindings ->
                        nonAliasSummary
                          boundNames
                          scopeBindings
                          (Set.insert nameText visitedBindings)
                          bindingExpr
                  _ ->
                    if nameText == bindingNameText
                      then (False, True)
                      else noSummary
        ELambda {} -> noSummary
        EOperatorValue {} -> noSummary
        EList elements ->
          foldl'
            combineSummaries
            noSummary
            (map (nonAliasSummary boundNames scopeBindings visitedBindings) elements)
        EApply functionExpr argumentExpr ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasSummary boundNames scopeBindings visitedBindings functionExpr,
              nonAliasSummary boundNames scopeBindings visitedBindings argumentExpr
            ]
        EIf conditionExpr thenExpr elseExpr ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasSummary boundNames scopeBindings visitedBindings conditionExpr,
              nonAliasSummary boundNames scopeBindings visitedBindings thenExpr,
              nonAliasSummary boundNames scopeBindings visitedBindings elseExpr
            ]
        ECase conditionExpr thenExpr elseExpr ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasSummary boundNames scopeBindings visitedBindings conditionExpr,
              nonAliasSummary boundNames scopeBindings visitedBindings thenExpr,
              nonAliasSummary boundNames scopeBindings visitedBindings elseExpr
            ]
        EPatternCase scrutineeExpr caseArms ->
          foldl'
            combineSummaries
            (nonAliasSummary boundNames scopeBindings visitedBindings scrutineeExpr)
            [ nonAliasSummary
                (extendBoundWithPattern pattern boundNames)
                scopeBindings
                visitedBindings
                bodyExpr
              | CaseArm pattern bodyExpr <- caseArms
            ]
        EBinary _ leftExpr rightExpr ->
          foldl'
            combineSummaries
            noSummary
            [ nonAliasSummary boundNames scopeBindings visitedBindings leftExpr,
              nonAliasSummary boundNames scopeBindings visitedBindings rightExpr
            ]
        ESectionLeft leftExpr _ ->
          nonAliasSummary boundNames scopeBindings visitedBindings leftExpr
        ESectionRight _ rightExpr ->
          nonAliasSummary boundNames scopeBindings visitedBindings rightExpr
        EBlock blockStatements ->
          let localScopeBindings = collectScopeBindingExprs blockStatements
              blockScopeBindings = localScopeBindings `Map.union` scopeBindings
           in
            foldl'
              combineSummaries
              noSummary
              [ summary
                | statement <- blockStatements,
                  summary <-
                    case statement of
                      SLet _ _ valueExpr ->
                        [nonAliasSummary boundNames blockScopeBindings Set.empty valueExpr]
                      SExpr _ statementExpr ->
                        [nonAliasSummary boundNames blockScopeBindings Set.empty statementExpr]
                      _ -> []
              ]

    collectScopeBindingExprs =
      foldl' collect Map.empty
      where
        collect scopeBindings statement =
          case statement of
            SLet bindingName _ valueExpr ->
              Map.insert (identifierText bindingName) valueExpr scopeBindings
            _ -> scopeBindings

extendBoundWithPattern :: Pattern -> Set Text -> Set Text
extendBoundWithPattern pattern bound =
  case pattern of
    PVariable name -> Set.insert (identifierText name) bound
    PWildcard -> bound
    PLiteral {} -> bound
    PConstructor _ patterns ->
      foldl' (flip extendBoundWithPattern) bound patterns
    PList patterns ->
      foldl' (flip extendBoundWithPattern) bound patterns
