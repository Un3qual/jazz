{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Shared recursive-binding graph and free-variable helpers used by analyzer,
-- type inference, and runtime.
module Jazz.Compiler.RecursiveBindings
  ( PreparedRecursiveScope,
    RecursiveScopeFacts,
    buildRecursiveScopeFacts,
    collectBindingNames,
    freeVarsExprWithBound,
    freeVarsScopeWithBound,
    exprContainsFunctionBranch,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings,
    prepareResolvedScope,
    prepareAnalyzedScope,
    takePreparedScope,
    preparedRecursiveScopeFacts,
    preparedRecursiveScopeBindingNames,
    preparedRecursiveScopeGroups,
    preparedRecursiveScopeStatements,
    recursiveScopeBindingNames,
    recursiveScopeGroups,
    resolvedExpressionReferences,
    publishResolvedCaptures,
    resolveLexicalScopes,
  )
where

import Data.Graph
  ( SCC (..),
    stronglyConnComp,
  )
import Data.List (find, mapAccumL)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    CoreNameAt,
    CoreNode (..),
    CorePhase (Analyzed, Resolved),
    CoreSort (ExpressionSort),
    CoreUserNameAt,
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Pattern (..),
    Statement (..),
    expressionNode,
  )
import Jazz.Compiler.CoreIdentity (CoreBinderId, CoreNodeId, ResolvedNodeFacts (..), ResolvedReference (..), ResolvedScopeFacts (..))
import Jazz.Compiler.Name (Name (..), ResolvedName, ResolvedNameOrigin (..), ResolvedUserName (..), operatorBindingName)
import Jazz.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol,
  )
import Jazz.Compiler.Pattern
  ( extendBoundWithPattern,
  )
import Jazz.Compiler.SemanticFacts (ExpressionFacts (expressionResolution), SemanticFactInvariantFailure (..))
import Jazz.Compiler.StableSet
  ( stableSetDifference,
    stableSetMembershipSet,
    stableSetOrderedList,
    stableSetSingleton,
  )

-- | Declaration targets already selected by resolution. Nested declarations
-- need no name-based shadowing here: their IDs cannot select an outer binder.
resolvedExpressionReferences :: Expr 'Resolved -> Map CoreBinderId ResolvedName
resolvedExpressionReferences expression = case expression of
  EVar node name -> reference node name
  EOperatorValue node symbol -> reference node (operatorBindingName symbol)
  ELambda _ _ body -> recur body
  EList _ elements -> foldMap recur elements
  ETuple _ elements -> foldMap recur elements
  EApply _ function argument -> recur function <> recur argument
  ETypeApplication _ function _ _ -> recur function
  EIf _ condition yes no -> foldMap recur [condition, yes, no]
  EPatternCase _ scrutinee arms -> recur scrutinee <> foldMap armReferences arms
  EBinary node symbol left right -> reference node (operatorBindingName symbol) <> recur left <> recur right
  ESectionLeft node left symbol -> reference node (operatorBindingName symbol) <> recur left
  ESectionRight node symbol right -> reference node (operatorBindingName symbol) <> recur right
  EBlock _ statements -> foldMap statementReferences statements
  ELit {} -> Map.empty
  where
    recur = resolvedExpressionReferences
    reference node name = case resolvedNodeReference (coreNodeFacts node) of
      Just (LexicalReference binder) -> Map.singleton binder name
      _ -> Map.empty
    armReferences (CaseArm _ _ guard body) = foldMap recur guard <> recur body
    statementReferences statement = case statement of
      SLet _ _ value -> recur value
      SExpr _ value -> recur value
      SClass _ _ _ _ _ defaults -> foldMap (\(ImplMethod _ _ body) -> recur body) defaults
      SImpl _ _ _ methods _ -> foldMap (\(ImplMethod _ _ body) -> recur body) methods
      _ -> Map.empty

-- | Name resolution has chosen namespaces and nonlocal targets. This pass owns
-- ordered local visibility, recursive groups, and the references selecting each
-- declaration. Later phases consume the published product unchanged.
resolveLexicalScopes :: Map ResolvedName ResolvedReference -> Set ResolvedName -> Expr 'Resolved -> Expr 'Resolved
resolveLexicalScopes externalReferences externalNames = expression (Map.mapMaybe lexicalBinder externalReferences)
  where
    lexicalBinder (LexicalReference binder) = Just binder
    lexicalBinder _ = Nothing
    expression bound expr = case expr of
      ELit {} -> expr
      EVar node name -> EVar (reference bound name node) name
      EOperatorValue node symbol -> EOperatorValue (reference bound (operatorBindingName symbol) node) symbol
      ELambda node name body -> ELambda (shadowed bound name node) name (expression (insertBinder node name bound) body)
      EList node elements -> EList node (map (expression bound) elements)
      ETuple node elements -> ETuple node (map (expression bound) elements)
      EApply node function argument -> EApply node (expression bound function) (expression bound argument)
      ETypeApplication node function spanValue argument -> ETypeApplication node (expression bound function) spanValue argument
      EIf node condition yes no -> EIf node (expression bound condition) (expression bound yes) (expression bound no)
      EPatternCase node scrutinee arms -> EPatternCase node (expression bound scrutinee) (map (arm bound) arms)
      EBinary node symbol left right -> EBinary (reference bound (operatorBindingName symbol) node) symbol (expression bound left) (expression bound right)
      ESectionLeft node left symbol -> ESectionLeft (reference bound (operatorBindingName symbol) node) (expression bound left) symbol
      ESectionRight node symbol right -> ESectionRight (reference bound (operatorBindingName symbol) node) symbol (expression bound right)
      EBlock node statements -> block bound node statements
    reference :: Map ResolvedName CoreBinderId -> ResolvedName -> CoreNode 'Resolved sort -> CoreNode 'Resolved sort
    reference bound name node = node {coreNodeFacts = facts {resolvedNodeReference = Just target}}
      where
        facts = coreNodeFacts node
        target = case Map.lookup name bound of
          Just binder -> LexicalReference binder
          Nothing -> case resolvedNodeReference facts of
            Just existing@(LexicalReference _)
              | Set.member name externalNames -> existing
              | UserName (ResolvedUserName origin _ _) <- name, origin /= CurrentModule -> existing
              | otherwise -> UnresolvedReference name
            Just existing -> existing
            Nothing -> UnresolvedReference name
    shadowed :: Map ResolvedName CoreBinderId -> ResolvedName -> CoreNode 'Resolved sort -> CoreNode 'Resolved sort
    shadowed bound name node = node {coreNodeFacts = (coreNodeFacts node) {resolvedNodeShadowedReference = LexicalReference <$> Map.lookup name bound}}
    insertBinder node name bound = case resolvedNodeBinder (coreNodeFacts node) of
      Just binder -> Map.insert name binder bound
      Nothing -> bound
    arm bound (CaseArm node pattern guard body) =
      let resolvedPattern = resolvePattern bound Map.empty pattern
          visible = Map.union (patternBindings resolvedPattern) bound
       in CaseArm node resolvedPattern (fmap (expression visible) guard) (expression visible body)
    resolvePattern bound shared pattern = case pattern of
      PVariable node name -> PVariable (sharedBinder node name) name
      PAs node name nested -> PAs (sharedBinder node name) name (recur nested)
      PConstructor node name nested -> PConstructor (reference bound name node) name (map recur nested)
      PList node nested -> PList node (map recur nested)
      PTuple node nested -> PTuple node (map recur nested)
      PConsList node first rest -> PConsList node (recur first) (recur rest)
      POr node alternatives ->
        let common = Map.union shared (patternBindings pattern)
         in POr node (map (resolvePattern bound common) alternatives)
      _ -> pattern
      where
        recur = resolvePattern bound shared
        sharedBinder node name =
          let updated = shadowed bound name node
           in case Map.lookup name shared of
                Just binder -> updated {coreNodeFacts = (coreNodeFacts updated) {resolvedNodeBinder = Just binder}}
                Nothing -> updated
    patternBindings pattern = case pattern of
      PVariable node name -> insertBinder node name Map.empty
      PAs node name nested -> insertBinder node name (patternBindings nested)
      PConstructor _ _ nested -> Map.unions (map patternBindings nested)
      PList _ nested -> Map.unions (map patternBindings nested)
      PTuple _ nested -> Map.unions (map patternBindings nested)
      PConsList _ first rest -> Map.union (patternBindings first) (patternBindings rest)
      POr _ (first : rest) -> foldl' Map.intersection (patternBindings first) (map patternBindings rest)
      _ -> Map.empty
    block bound node statements = EBlock (node {coreNodeFacts = (coreNodeFacts node) {resolvedNodeScope = Just facts}}) resolvedStatements
      where
        indexed = zip [0 ..] statements
        outerNames = Set.union externalNames (Map.keysSet bound)
        recursion = buildRecursiveScopeFacts outerNames indexed
        groups = recursiveScopeGroups recursion
        definitions = Map.fromList [(index, (bindingNode, name)) | (index, SLet bindingNode name _) <- indexed]
        (_, resolvedStatements) = mapAccumL statement bound indexed
        binderPairs = [(index, binder) | (index, (bindingNode, _)) <- Map.toList definitions, Just binder <- [resolvedNodeBinder (coreNodeFacts bindingNode)]]
        binderIndices = Map.fromList [(binder, index) | (index, binder) <- binderPairs]
        facts =
          ResolvedScopeFacts
            { resolvedScopeBindingNames = recursiveScopeBindingNames recursion,
              resolvedScopeBinderIds = Map.fromList binderPairs,
              resolvedScopeBindingReplacements = Map.fromList [(previousIndex, index) | (index, SLet bindingNode _ _) <- zip [0 ..] resolvedStatements, Just (LexicalReference previous) <- [resolvedNodeShadowedReference (coreNodeFacts bindingNode)], Just previousIndex <- [Map.lookup previous binderIndices]],
              resolvedScopeRecursiveGroups = groups,
              resolvedScopeSelfRecursiveFunctions = inferSelfRecursiveBindings outerNames exprContainsFunctionBranch indexed,
              resolvedScopeSelfReferences = Set.fromList [index | (index, SLet bindingNode _ rhs) <- zip [0 ..] resolvedStatements, Just binder <- [resolvedNodeBinder (coreNodeFacts bindingNode)], Map.member binder (resolvedExpressionReferences rhs)]
            }
        statement visible (index, value) = case value of
          SLet bindingNode name rhs ->
            let selfVisible = if Map.member name visible then visible else insertBinder bindingNode name visible
                definitionVisible = foldl' insertPeer selfVisible (Map.findWithDefault [] index groups)
             in (insertBinder bindingNode name visible, SLet (shadowed visible name bindingNode) name (expression definitionVisible rhs))
          SExpr statementNode rhs -> (visible, SExpr statementNode (expression visible rhs))
          SSignature signatureNode name signature ->
            let target = case Map.lookup (index + 1) definitions of
                  Just (bindingNode, bindingName) | bindingName == name -> LexicalReference <$> resolvedNodeBinder (coreNodeFacts bindingNode)
                  _ -> Nothing
             in (visible, SSignature (signatureNode {coreNodeFacts = (coreNodeFacts signatureNode) {resolvedNodeReference = target}}) name signature)
          SData statementNode name parameters constructors ->
            let (nextVisible, resolvedConstructors) = mapAccumL (\acc (DataConstructor constructorNode constructor fields) -> (insertBinder constructorNode constructor acc, DataConstructor (shadowed acc constructor constructorNode) constructor fields)) visible constructors
             in (nextVisible, SData statementNode name parameters resolvedConstructors)
          SClass statementNode capability parameters signatures context defaults ->
            (visible, SClass statementNode capability parameters signatures context [ImplMethod methodNode name (expression visible body) | ImplMethod methodNode name body <- defaults])
          SImpl statementNode capability targets methods context ->
            let methodVisible = foldl' (\acc (ImplMethod methodNode name _) -> insertBinder methodNode name acc) visible methods
             in (visible, SImpl statementNode capability targets [ImplMethod (shadowed visible name methodNode) name (expression methodVisible body) | ImplMethod methodNode name body <- methods] context)
          _ -> (visible, value)
        insertPeer visible index = case Map.lookup index definitions of
          Just (bindingNode, name) | Map.notMember name visible -> insertBinder bindingNode name visible
          _ -> visible

-- | Compute capture candidates bottom-up once, while resolution owns lexical
-- identity. Removing declaration IDs also handles forward peers and rebinding
-- without a second name-based visibility walk.
publishResolvedCaptures :: Expr 'Resolved -> Expr 'Resolved
publishResolvedCaptures = snd . expression
  where
    expression expr = case expr of
      ELit {} -> (mempty, expr)
      EVar node name -> (reference node name, expr)
      EOperatorValue node symbol -> (reference node (operatorBindingName symbol), expr)
      ELambda node parameter body ->
        let (free, checkedBody) = expression body
            captures = without (binder node) free
            facts = (coreNodeFacts node) {resolvedNodeCaptures = stableSetOrderedList captures}
         in (captures, ELambda (node {coreNodeFacts = facts}) parameter checkedBody)
      EList node elements -> fmap (EList node) (children elements)
      ETuple node elements -> fmap (ETuple node) (children elements)
      EApply node function argument -> binary (EApply node) function argument
      ETypeApplication node function spanValue argument -> fmap (\checked -> ETypeApplication node checked spanValue argument) (expression function)
      EIf node condition yes no ->
        let (conditionFree, checkedCondition) = expression condition
            (yesFree, checkedYes) = expression yes
            (noFree, checkedNo) = expression no
         in (conditionFree <> yesFree <> noFree, EIf node checkedCondition checkedYes checkedNo)
      EPatternCase node scrutinee arms ->
        let (free, checkedScrutinee) = expression scrutinee
            (armFree, checkedArms) = unzip (map arm arms)
         in (free <> mconcat armFree, EPatternCase node checkedScrutinee checkedArms)
      EBinary node symbol left right ->
        let (free, checked) = binary (EBinary node symbol) left right
         in (reference node (operatorBindingName symbol) <> free, checked)
      ESectionLeft node left symbol ->
        let (free, checked) = expression left
         in (reference node (operatorBindingName symbol) <> free, ESectionLeft node checked symbol)
      ESectionRight node symbol right ->
        let (free, checked) = expression right
         in (reference node (operatorBindingName symbol) <> free, ESectionRight node symbol checked)
      EBlock node statements ->
        let (free, checked) = unzip (map statement statements)
         in (without (foldMap statementBinders statements) (mconcat free), EBlock node checked)
    reference node name = case resolvedNodeReference (coreNodeFacts node) of
      Just (BuiltinOperatorReference _) -> mempty
      Just target -> stableSetSingleton (target, name)
      Nothing -> mempty
    binder node = maybe Set.empty (Set.singleton . LexicalReference) (resolvedNodeBinder (coreNodeFacts node))
    without bound free = stableSetDifference free (Set.filter (\(target, _) -> Set.member target bound) (stableSetMembershipSet free))
    children values = let (free, checked) = unzip (map expression values) in (mconcat free, checked)
    binary construct left right =
      let (leftFree, checkedLeft) = expression left
          (rightFree, checkedRight) = expression right
       in (leftFree <> rightFree, construct checkedLeft checkedRight)
    arm (CaseArm node pattern guard body) =
      let (guardFree, checkedGuard) = maybe (mempty, Nothing) (fmap Just . expression) guard
          (bodyFree, checkedBody) = expression body
       in (without (patternBinders pattern) (guardFree <> bodyFree), CaseArm node pattern checkedGuard checkedBody)
    patternBinders pattern = case pattern of
      PVariable node _ -> binder node
      PAs node _ nested -> binder node <> patternBinders nested
      PConstructor _ _ nested -> foldMap patternBinders nested
      PList _ nested -> foldMap patternBinders nested
      PTuple _ nested -> foldMap patternBinders nested
      PConsList _ first rest -> patternBinders first <> patternBinders rest
      POr _ alternatives -> foldMap patternBinders alternatives
      _ -> Set.empty
    statement value = case value of
      SLet node name body -> fmap (SLet node name) (expression body)
      SExpr node body -> fmap (SExpr node) (expression body)
      SClass node capability parameters signatures context defaults ->
        let (free, checked) = unzip [fmap (ImplMethod methodNode name) (expression body) | ImplMethod methodNode name body <- defaults]
         in (mconcat free, SClass node capability parameters signatures context checked)
      SImpl node capability target methods context ->
        let (free, checked) = unzip [fmap (ImplMethod methodNode name) (expression body) | ImplMethod methodNode name body <- methods]
         in (without (foldMap (\(ImplMethod methodNode _ _) -> binder methodNode) methods) (mconcat free), SImpl node capability target checked context)
      _ -> (mempty, value)
    statementBinders value = case value of
      SLet node _ _ -> binder node
      SData _ _ _ constructors -> foldMap (\(DataConstructor node _ _) -> binder node) constructors
      SClass _ _ _ methods _ _ -> foldMap (\(ClassMethodSignature node _ _) -> binder node) methods
      _ -> Set.empty

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

buildRecursiveScopeFacts :: (Ord (CoreUserNameAt phase)) => Set (CoreNameAt phase) -> [(Int, Statement phase)] -> RecursiveScopeFacts phase
buildRecursiveScopeFacts outerBindingNames indexedStatements =
  RecursiveScopeFacts
    { recursiveScopeBindingNames = collectBindingNames indexedStatements,
      recursiveScopeGroups = inferRecursiveGroupsOrderedInternal outerBindingNames indexedStatements
    }

-- | A view of source-ordered statements and their published lexical facts.
-- Constructors stay private: consumers may select statements, never rediscover
-- visibility using a type or value environment.
data PreparedRecursiveScope phase = PreparedRecursiveScope ![Statement phase] !ResolvedScopeFacts

prepareResolvedScope :: CoreNode 'Resolved 'ExpressionSort -> [Statement 'Resolved] -> Either SemanticFactInvariantFailure (PreparedRecursiveScope 'Resolved)
prepareResolvedScope node = prepareScope (coreNodeId node) (coreNodeFacts node)

prepareAnalyzedScope :: Expr 'Analyzed -> Either SemanticFactInvariantFailure (PreparedRecursiveScope 'Analyzed)
prepareAnalyzedScope (EBlock node statements) = prepareScope (coreNodeId node) (expressionResolution (coreNodeFacts node)) statements
prepareAnalyzedScope expression = Left (AnalyzedModuleRootNotBlock (coreNodeId (expressionNode expression)))

prepareScope :: CoreNodeId -> ResolvedNodeFacts -> [Statement phase] -> Either SemanticFactInvariantFailure (PreparedRecursiveScope phase)
prepareScope nodeId facts statements = case resolvedNodeScope facts of
  Just scope -> Right (PreparedRecursiveScope statements scope)
  Nothing -> Left (MissingScopeFacts nodeId)

-- | Keep a statement prefix and its published facts. Prefixes preserve local
-- indices, declaration IDs, and the resolver's visibility decisions.
takePreparedScope :: Int -> PreparedRecursiveScope phase -> PreparedRecursiveScope phase
takePreparedScope count (PreparedRecursiveScope statements facts) =
  PreparedRecursiveScope (take count statements) selectedFacts
  where
    project :: Map Int a -> Map Int a
    project = fst . Map.split count
    projectSet = fst . Set.split count
    selectedFacts =
      facts
        { resolvedScopeBindingNames = project (resolvedScopeBindingNames facts),
          resolvedScopeBinderIds = project (resolvedScopeBinderIds facts),
          resolvedScopeBindingReplacements = Map.filter (< count) (project (resolvedScopeBindingReplacements facts)),
          resolvedScopeRecursiveGroups = Map.map (filter (< count)) (project (resolvedScopeRecursiveGroups facts)),
          resolvedScopeSelfRecursiveFunctions = projectSet (resolvedScopeSelfRecursiveFunctions facts),
          resolvedScopeSelfReferences = projectSet (resolvedScopeSelfReferences facts)
        }

preparedRecursiveScopeStatements :: PreparedRecursiveScope phase -> [Statement phase]
preparedRecursiveScopeStatements (PreparedRecursiveScope statements _) = statements

preparedRecursiveScopeFacts :: PreparedRecursiveScope phase -> ResolvedScopeFacts
preparedRecursiveScopeFacts (PreparedRecursiveScope _ facts) = facts

preparedRecursiveScopeBindingNames :: PreparedRecursiveScope phase -> Map Int ResolvedName
preparedRecursiveScopeBindingNames = resolvedScopeBindingNames . preparedRecursiveScopeFacts

preparedRecursiveScopeGroups :: PreparedRecursiveScope phase -> Map Int [Int]
preparedRecursiveScopeGroups = resolvedScopeRecursiveGroups . preparedRecursiveScopeFacts

freeVarsExprWithBound :: (Ord (CoreUserNameAt phase)) => Set (CoreNameAt phase) -> Expr phase -> Set (CoreNameAt phase)
freeVarsExprWithBound = freeVarsExprWithVisibleBindings Set.empty

freeVarsExprWithVisibleBindings :: (Ord (CoreUserNameAt phase)) => Set (CoreNameAt phase) -> Set (CoreNameAt phase) -> Expr phase -> Set (CoreNameAt phase)
freeVarsExprWithVisibleBindings visibleBindingNames =
  freeVarsExprUsing (freeVarsScopeWithVisibleBindings visibleBindingNames)

freeVarsExprUsing :: (Ord (CoreUserNameAt phase)) => (Set (CoreNameAt phase) -> [Statement phase] -> Set (CoreNameAt phase)) -> Set (CoreNameAt phase) -> Expr phase -> Set (CoreNameAt phase)
freeVarsExprUsing scopeFreeVars bound expr =
  case expr of
    ELit _ _ -> Set.empty
    EVar _ name
      | Set.member name bound -> Set.empty
      | otherwise -> Set.singleton name
    ELambda _ parameterName bodyExpr ->
      freeVarsExprUsing scopeFreeVars (Set.insert parameterName bound) bodyExpr
    EOperatorValue _ operatorSymbol -> operatorBindingFreeVar bound operatorSymbol
    EList _ elements -> Set.unions (map recur elements)
    ETuple _ elements -> Set.unions (map recur elements)
    EApply _ functionExpr argumentExpr -> recur functionExpr <> recur argumentExpr
    ETypeApplication _ functionExpr _ _ -> recur functionExpr
    EIf _ conditionExpr thenExpr elseExpr -> Set.unions (map recur [conditionExpr, thenExpr, elseExpr])
    EPatternCase _ scrutineeExpr caseArms -> Set.unions (recur scrutineeExpr : map armFreeVars caseArms)
    EBinary _ operatorSymbol leftExpr rightExpr ->
      Set.unions [operatorBindingFreeVar bound operatorSymbol, recur leftExpr, recur rightExpr]
    ESectionLeft _ leftExpr operatorSymbol -> operatorBindingFreeVar bound operatorSymbol <> recur leftExpr
    ESectionRight _ operatorSymbol rightExpr -> operatorBindingFreeVar bound operatorSymbol <> recur rightExpr
    EBlock _ statements -> scopeFreeVars bound statements
  where
    recur = freeVarsExprUsing scopeFreeVars bound
    armFreeVars (CaseArm _ pattern guardExpr bodyExpr) =
      let armRecur = freeVarsExprUsing scopeFreeVars (extendBoundWithPattern pattern bound)
       in foldMap armRecur guardExpr <> armRecur bodyExpr

operatorBindingFreeVar :: (Ord user) => Set (Name user) -> Text -> Set (Name user)
operatorBindingFreeVar bound operatorSymbol
  | isBuiltinOperatorSymbol operatorSymbol = Set.empty
  | Set.member bindingName bound = Set.empty
  | otherwise = Set.singleton bindingName
  where
    bindingName = operatorBindingName operatorSymbol

freeVarsScopeWithBound :: (Ord (CoreUserNameAt phase)) => Set (CoreNameAt phase) -> [Statement phase] -> Set (CoreNameAt phase)
freeVarsScopeWithBound = freeVarsScopeWithVisibleBindings Set.empty

freeVarsScopeWithVisibleBindings :: (Ord (CoreUserNameAt phase)) => Set (CoreNameAt phase) -> Set (CoreNameAt phase) -> [Statement phase] -> Set (CoreNameAt phase)
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

inferRecursiveGroupsOrdered :: (Ord (CoreUserNameAt phase)) => Set (CoreNameAt phase) -> [(Int, Statement phase)] -> Map Int [Int]
inferRecursiveGroupsOrdered outerBindingNames =
  recursiveScopeGroups . buildRecursiveScopeFacts outerBindingNames

inferRecursiveGroupsOrderedInternal :: (Ord (CoreUserNameAt phase)) => Set (CoreNameAt phase) -> [(Int, Statement phase)] -> Map Int [Int]
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

    componentStatementIndices (AcyclicSCC componentIndex) = [componentIndex]
    componentStatementIndices (CyclicSCC [componentIndex]) = [componentIndex]
    componentStatementIndices (CyclicSCC indices) =
      let memberIndices = Set.fromList indices
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

inferSelfRecursiveBindings :: (Ord (CoreUserNameAt phase)) => Set (CoreNameAt phase) -> (Expr phase -> Bool) -> [(Int, Statement phase)] -> Set Int
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
  (Ord (CoreUserNameAt phase)) =>
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

lookupScopeBinding :: (Ord (CoreUserNameAt phase)) => CoreNameAt phase -> [ScopeBindingExpr phase] -> Maybe (ScopeBindingExpr phase)
lookupScopeBinding requestedName =
  find (\(ScopeBindingExpr _ bindingName _ _ _ _) -> bindingName == requestedName)

-- Keep callable-shape recognition beside canonical recursive ownership so
-- nested and top-level scopes agree on lambda self recursion.
exprContainsFunctionBranch :: (Ord (CoreUserNameAt phase)) => Expr phase -> Bool
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

selfReferenceOwnsRecursiveCell :: (Ord (CoreUserNameAt phase)) => CoreNameAt phase -> Expr phase -> Bool
selfReferenceOwnsRecursiveCell =
  selfReferenceOwnsRecursiveCellWith exprContainsFunctionBranch

selfReferenceOwnsRecursiveCellWith :: (Ord (CoreUserNameAt phase)) => (Expr phase -> Bool) -> CoreNameAt phase -> Expr phase -> Bool
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
        EOperatorValue _ operatorSymbol
          | not (isBuiltinOperatorSymbol operatorSymbol),
            Set.member bindingName (operatorBindingFreeVar Set.empty operatorSymbol) ->
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
           in combineSummaries terminalSummary (eagerStatementsSummary boundNames eagerStatements)
        _ -> nonAliasSummary expressionPath boundNames scopeBindings visitedBindings expr

    eagerStatementsSummary boundNames contexts =
      foldl'
        combineSummaries
        noSummary
        [ nonAliasSummary statementPath boundNames statementBindings Set.empty valueExpr
        | ScopeStatementContext statement statementBindings statementPath <- contexts,
          valueExpr <- case statement of
            SLet _ _ value -> [value]
            SExpr _ value -> [value]
            _ -> []
        ]

    nonAliasSummary expressionPath boundNames scopeBindings visitedBindings expr =
      case expr of
        ELit {} -> noSummary
        EVar _ name ->
          nonAliasReferenceSummary boundNames scopeBindings visitedBindings name
        ELambda {} -> noSummary
        EOperatorValue _ operatorSymbol ->
          nonAliasOperatorSummary boundNames scopeBindings visitedBindings operatorSymbol
        EList _ elements -> children elements
        ETuple _ elements -> children elements
        EApply _ functionExpr argumentExpr -> children [functionExpr, argumentExpr]
        ETypeApplication _ functionExpr _ _ -> recur 0 functionExpr
        EIf _ conditionExpr thenExpr elseExpr -> children [conditionExpr, thenExpr, elseExpr]
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
        EBinary _ operatorSymbol leftExpr rightExpr ->
          combineSummaries (operator operatorSymbol) (children [leftExpr, rightExpr])
        ESectionLeft _ leftExpr operatorSymbol ->
          combineSummaries (operator operatorSymbol) (recur 0 leftExpr)
        ESectionRight _ operatorSymbol rightExpr ->
          combineSummaries (operator operatorSymbol) (recur 0 rightExpr)
        EBlock _ blockStatements ->
          eagerStatementsSummary boundNames (scopeStatementContexts expressionPath boundNames scopeBindings blockStatements)
      where
        recur childIndex = nonAliasSummary (expressionPath <> [childIndex]) boundNames scopeBindings visitedBindings
        children = foldl' combineSummaries noSummary . zipWith recur [0 ..]
        operator = nonAliasOperatorSummary boundNames scopeBindings visitedBindings

    nonAliasOperatorSummary boundNames scopeBindings visitedBindings operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol = noSummary
      | otherwise =
          case Set.toList (operatorBindingFreeVar Set.empty operatorSymbol) of
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
