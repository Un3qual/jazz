module Jazz.Compiler.TypeInference.Elaboration.Profiles
  ( FinalizationProfile (..),
    analyzeFinalizationProfile,
    provisionalFreeNames,
    shapeFor,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.Name
  ( GeneratedNameKind (OperatorBinding),
    Name (..),
    identifierText,
  )
import Jazz.Compiler.Pattern (patternBinderNames)
import Jazz.Compiler.TypeInference.Elaboration.Types
  ( FunctionProfile (..),
    ProvisionalCallableDeclaration (..),
    ProvisionalPatternCaseArm (..),
    ProvisionalTypedExpr (..),
    ProvisionalTypedStatement (..),
  )
import Jazz.Compiler.TypeInference.Types (ExpressionType (TFunctionType))
import Jazz.Compiler.TypedCore
  ( TypedBinderId (..),
    TypedCallableShape (..),
    TypedCoreName (..),
    TypedGeneratedNameKind (TypedOperatorBinding),
    TypedNameNamespace (TypedValueNamespace),
    TypedNameOrigin (TypedCurrentModule),
    TypedRecursiveGroup (..),
  )

-- | Pure, representation-level facts needed before final construction. The
-- construction-dependent recursive-support check intentionally remains in
-- @Finalize@: it validates candidate members by attempting Typed Core
-- construction, so moving it here would require a callback cycle or duplicate
-- construction logic.
data FinalizationProfile = FinalizationProfile
  { profileBaseFunctions :: Map.Map Name FunctionProfile,
    profileCallableShapes :: Map.Map Name TypedCallableShape,
    profileReboundFunctions :: Map.Map Int Name,
    profileTypedRecursiveGroups :: [TypedRecursiveGroup],
    profileRecursiveBinders :: Set.Set TypedBinderId
  }

analyzeFinalizationProfile :: [Text] -> [ProvisionalTypedStatement] -> FinalizationProfile
analyzeFinalizationProfile modulePath statements =
  FinalizationProfile
    { profileBaseFunctions = baseFunctions,
      profileCallableShapes = callableShapeTable baseFunctions statements,
      profileReboundFunctions = reboundFunctionStatements statements,
      profileTypedRecursiveGroups = orderedTypedRecursiveGroups modulePath declarations,
      profileRecursiveBinders = recursiveDeclarationBinders modulePath declarations
    }
  where
    baseFunctions = functionTable statements
    declarations = callableDeclarations statements

functionTable :: [ProvisionalTypedStatement] -> Map.Map Name FunctionProfile
functionTable statements =
  foldl'
    collect
    Map.empty
    statements
  where
    collect functions statement =
      case statement of
        ProvisionalFunctionBinding declaration expression
          | lambdaCount expression > 0 ->
              Map.insertWith
                (\_ firstFunction -> firstFunction)
                name
                (FunctionProfile statementIndex expressionType (lambdaCount expression))
                functions
          where
            statementIndex = provisionalCallableStatementIndex declaration
            name = provisionalCallableName declaration
            expressionType = provisionalCallableType declaration
        _ -> functions

callableDeclarations :: [ProvisionalTypedStatement] -> [ProvisionalCallableDeclaration]
callableDeclarations statements =
  [ declaration
  | statement <- statements,
    declaration <-
      case statement of
        ProvisionalFunctionBinding candidate _ -> [candidate]
        ProvisionalUnsupportedCallableBinding candidate _ _ _ -> [candidate]
        _ -> []
  ]

callableShapeTable :: Map.Map Name FunctionProfile -> [ProvisionalTypedStatement] -> Map.Map Name TypedCallableShape
callableShapeTable functions statements =
  foldl' promoteRecursiveGroup transitiveShapes orderedRecursiveGroupNames
  where
    (_, baseShapes, directCaptureFunctions) =
      foldl'
        collect
        (Set.empty, Map.map (const TypedDirectCallableShape) functions, Set.empty)
        statements
    closureDependencies =
      foldl'
        collectClosureDependencies
        Map.empty
        statements
    collectClosureDependencies dependencies statement =
      case statement of
        ProvisionalFunctionBinding declaration expression ->
          Map.insertWith
            Set.union
            (provisionalCallableName declaration)
            (Set.intersection (Map.keysSet functions) (provisionalFreeNames expression))
            dependencies
        _ -> dependencies
    transitiveCaptureFunctions = propagateCaptureDependencies directCaptureFunctions
    transitiveShapes = Map.mapWithKey promoteTransitiveCapture baseShapes
    orderedRecursiveGroupNames =
      snd (foldl' collectRecursiveGroup (Set.empty, []) (callableDeclarations statements))
    namesByStatement =
      Map.fromList
        [ (provisionalCallableStatementIndex declaration, provisionalCallableName declaration)
        | declaration <- callableDeclarations statements
        ]
    collectRecursiveGroup (seenGroups, groups) declaration =
      case provisionalCallableRecursiveGroupMembers declaration of
        Just memberStatements
          | Set.notMember memberStatements seenGroups ->
              case traverse (`Map.lookup` namesByStatement) memberStatements of
                Just memberNames ->
                  (Set.insert memberStatements seenGroups, groups <> [memberNames])
                Nothing -> (Set.insert memberStatements seenGroups, groups)
        _ -> (seenGroups, groups)
    promoteRecursiveGroup shapes memberNames
      | any ((== TypedClosureCallableShape) . shapeFor shapes) memberNames =
          foldl' (flip markClosure) shapes memberNames
      | otherwise = shapes
    propagateCaptureDependencies capturingFunctions =
      let nextCapturingFunctions =
            Map.foldlWithKey'
              promote
              capturingFunctions
              closureDependencies
       in if nextCapturingFunctions == capturingFunctions
            then capturingFunctions
            else propagateCaptureDependencies nextCapturingFunctions
      where
        promote accumulatedCaptures name dependencies
          | Set.null (Set.intersection accumulatedCaptures dependencies) = accumulatedCaptures
          | otherwise = Set.insert name accumulatedCaptures
    promoteTransitiveCapture name shape
      | Set.member name transitiveCaptureFunctions = TypedClosureCallableShape
      | otherwise = shape
    collect (visibleScalars, callableShapes, capturingFunctions) statement =
      let shapesAfterUses = collectStatementCallableUses functions Set.empty callableShapes statement
       in case statement of
            ProvisionalScalarBinding _ name _ _ _ ->
              (Set.insert name visibleScalars, shapesAfterUses, capturingFunctions)
            ProvisionalFunctionBinding declaration expression ->
              let name = provisionalCallableName declaration
                  capturesScalar = not (Set.disjoint visibleScalars (provisionalFreeNames expression))
                  shapesAfterCapture =
                    if capturesScalar
                      then markClosure name shapesAfterUses
                      else shapesAfterUses
                  nextCapturingFunctions =
                    if capturesScalar
                      then Set.insert name capturingFunctions
                      else capturingFunctions
               in (Set.delete name visibleScalars, shapesAfterCapture, nextCapturingFunctions)
            ProvisionalUnsupportedCallableBinding declaration _ _ _ ->
              (Set.delete (provisionalCallableName declaration) visibleScalars, shapesAfterUses, capturingFunctions)
            _ -> (visibleScalars, shapesAfterUses, capturingFunctions)

provisionalFreeNames :: ProvisionalTypedExpr -> Set.Set Name
provisionalFreeNames = freeNames Set.empty
  where
    freeNames boundNames expression =
      case expression of
        ProvisionalVariableExpression name _
          | Set.member name boundNames -> Set.empty
          | otherwise -> Set.singleton name
        ProvisionalLambdaExpression parameterName _ body ->
          freeNames (Set.insert parameterName boundNames) body
        ProvisionalApplyExpression _ function argument ->
          freeNames boundNames function <> freeNames boundNames argument
        ProvisionalBinaryExpression _ _ _ left right ->
          freeNames boundNames left <> freeNames boundNames right
        ProvisionalIfExpression _ condition thenExpression elseExpression ->
          freeNames boundNames condition
            <> freeNames boundNames thenExpression
            <> freeNames boundNames elseExpression
        ProvisionalPatternCaseExpression _ scrutinee arms ->
          freeNames boundNames scrutinee
            <> foldMap (armFreeNames boundNames) arms
        ProvisionalScopeStatements nestedStatements -> scopeFreeNames boundNames nestedStatements
        _ -> Set.empty

    armFreeNames boundNames (ProvisionalPatternCaseArm pattern maybeGuard body) =
      let armBoundNames = boundNames <> patternBinderNames pattern
       in maybe Set.empty (freeNames armBoundNames) maybeGuard
            <> freeNames armBoundNames body

    scopeFreeNames _ [] = Set.empty
    scopeFreeNames boundNames (statement : rest) =
      case statement of
        ProvisionalFunctionBinding declaration expression ->
          let name = provisionalCallableName declaration
              nextBoundNames = Set.insert name boundNames
           in freeNames nextBoundNames expression <> scopeFreeNames nextBoundNames rest
        ProvisionalScalarBinding _ name _ _ expression ->
          freeNames boundNames expression <> scopeFreeNames (Set.insert name boundNames) rest
        ProvisionalTerminalExpression _ _ expression ->
          freeNames boundNames expression <> scopeFreeNames boundNames rest
        _ -> scopeFreeNames boundNames rest

collectStatementCallableUses ::
  Map.Map Name FunctionProfile ->
  Set.Set Name ->
  Map.Map Name TypedCallableShape ->
  ProvisionalTypedStatement ->
  Map.Map Name TypedCallableShape
collectStatementCallableUses functions lexicalNames callableShapes statement =
  case statement of
    ProvisionalFunctionBinding _ expression ->
      collectExpressionCallableUses functions lexicalNames callableShapes expression
    ProvisionalScalarBinding _ _ _ _ expression ->
      collectExpressionCallableUses functions lexicalNames callableShapes expression
    ProvisionalTerminalExpression _ _ expression ->
      collectExpressionCallableUses functions lexicalNames callableShapes expression
    _ -> callableShapes

collectExpressionCallableUses ::
  Map.Map Name FunctionProfile ->
  Set.Set Name ->
  Map.Map Name TypedCallableShape ->
  ProvisionalTypedExpr ->
  Map.Map Name TypedCallableShape
collectExpressionCallableUses functions lexicalNames callableShapes expression =
  case expression of
    ProvisionalVariableExpression name _
      | Set.notMember name lexicalNames,
        Map.member name functions ->
          markClosure name callableShapes
      | otherwise -> callableShapes
    ProvisionalLambdaExpression parameterName _ body ->
      collectExpressionCallableUses functions (Set.insert parameterName lexicalNames) callableShapes body
    ProvisionalApplyExpression {} ->
      let (callee, arguments) = applicationSpine expression
          afterCallee =
            case callee of
              ProvisionalVariableExpression name _
                | Set.notMember name lexicalNames,
                  Just function <- Map.lookup name functions,
                  length arguments >= functionArity function ->
                    callableShapes
              _ -> collectExpressionCallableUses functions lexicalNames callableShapes callee
       in foldl'
            (\shapes argument -> collectExpressionCallableUses functions lexicalNames shapes argument)
            afterCallee
            arguments
    ProvisionalBinaryExpression _ _ _ left right ->
      collectExpressionCallableUses
        functions
        lexicalNames
        (collectExpressionCallableUses functions lexicalNames callableShapes left)
        right
    ProvisionalIfExpression _ condition thenExpression elseExpression ->
      foldl'
        (collectExpressionCallableUses functions lexicalNames)
        callableShapes
        [condition, thenExpression, elseExpression]
    ProvisionalPatternCaseExpression _ scrutinee arms ->
      foldl'
        collectArm
        (collectExpressionCallableUses functions lexicalNames callableShapes scrutinee)
        arms
    ProvisionalScopeStatements nestedStatements ->
      collectScopeCallableUses functions lexicalNames callableShapes nestedStatements
    _ -> callableShapes
  where
    collectArm shapes (ProvisionalPatternCaseArm pattern maybeGuard body) =
      let armLexicalNames = lexicalNames <> patternBinderNames pattern
          shapesAfterGuard =
            maybe
              shapes
              (collectExpressionCallableUses functions armLexicalNames shapes)
              maybeGuard
       in collectExpressionCallableUses functions armLexicalNames shapesAfterGuard body

markClosure :: Name -> Map.Map Name TypedCallableShape -> Map.Map Name TypedCallableShape
markClosure name = Map.insert name TypedClosureCallableShape

collectScopeCallableUses ::
  Map.Map Name FunctionProfile ->
  Set.Set Name ->
  Map.Map Name TypedCallableShape ->
  [ProvisionalTypedStatement] ->
  Map.Map Name TypedCallableShape
collectScopeCallableUses functions = go
  where
    go _ callableShapes [] = callableShapes
    go lexicalNames callableShapes statements@(statement : rest) =
      case statement of
        ProvisionalFunctionBinding declaration expression ->
          let expressionLexicalNames = Set.insert name (lexicalNames <> forwardFunctionNames statements)
              nextShapes = collectExpressionCallableUses functions expressionLexicalNames callableShapes expression
           in go (Set.insert name lexicalNames) nextShapes rest
          where
            name = provisionalCallableName declaration
        ProvisionalScalarBinding _ name _ _ expression ->
          let nextShapes = collectExpressionCallableUses functions lexicalNames callableShapes expression
           in go (Set.insert name lexicalNames) nextShapes rest
        ProvisionalTerminalExpression _ _ expression ->
          go lexicalNames (collectExpressionCallableUses functions lexicalNames callableShapes expression) rest
        _ -> go lexicalNames callableShapes rest
    forwardFunctionNames statements =
      Set.fromList
        [ name
        | ProvisionalSignature _ name _ (TFunctionType _ _) <- statements
        ]

shapeFor :: Map.Map Name TypedCallableShape -> Name -> TypedCallableShape
shapeFor callableShapes name =
  Map.findWithDefault TypedDirectCallableShape name callableShapes

reboundFunctionStatements :: [ProvisionalTypedStatement] -> Map.Map Int Name
reboundFunctionStatements statements =
  snd (foldl' collect (Set.empty, Map.empty) statements)
  where
    collect (seenNames, reboundStatements) statement =
      case statement of
        ProvisionalFunctionBinding declaration _ -> collectDeclaration seenNames reboundStatements declaration
        ProvisionalUnsupportedCallableBinding declaration _ _ _ -> collectDeclaration seenNames reboundStatements declaration
        _ -> (seenNames, reboundStatements)

    collectDeclaration seenNames reboundStatements declaration
      | Set.member name seenNames =
          (seenNames, Map.insert statementIndex name reboundStatements)
      | otherwise =
          (Set.insert name seenNames, reboundStatements)
      where
        statementIndex = provisionalCallableStatementIndex declaration
        name = provisionalCallableName declaration

recursiveDeclarationBinders :: [Text] -> [ProvisionalCallableDeclaration] -> Set.Set TypedBinderId
recursiveDeclarationBinders modulePath declarations =
  Set.fromList
    [ binderAt
        modulePath
        (provisionalCallableStatementIndex declaration)
        []
        (resolvedValueName (provisionalCallableName declaration))
    | declaration <- declarations,
      Just _ <- [provisionalCallableRecursiveGroupMembers declaration]
    ]

orderedTypedRecursiveGroups :: [Text] -> [ProvisionalCallableDeclaration] -> [TypedRecursiveGroup]
orderedTypedRecursiveGroups modulePath declarations =
  snd (foldl' collect (Set.empty, []) declarations)
  where
    declarationBindersByStatement =
      Map.fromList
        [ ( provisionalCallableStatementIndex declaration,
            binderAt
              modulePath
              (provisionalCallableStatementIndex declaration)
              []
              (resolvedValueName (provisionalCallableName declaration))
          )
        | declaration <- declarations
        ]
    collect (seenGroups, groups) declaration =
      case provisionalCallableRecursiveGroupMembers declaration of
        Just memberStatements
          | Set.notMember memberStatements seenGroups ->
              case traverse (`Map.lookup` declarationBindersByStatement) memberStatements of
                Just memberBinders ->
                  ( Set.insert memberStatements seenGroups,
                    groups <> [TypedRecursiveGroup memberBinders]
                  )
                Nothing -> (Set.insert memberStatements seenGroups, groups)
        _ -> (seenGroups, groups)

binderAt :: [Text] -> Int -> [Int] -> TypedCoreName -> TypedBinderId
binderAt modulePath statementIndex suffix name =
  TypedBinderId (modulePath, statementIndex : suffix, name)

resolvedValueName :: Name -> TypedCoreName
resolvedValueName name =
  case name of
    GeneratedName (OperatorBinding storageName) -> TypedGeneratedName (TypedOperatorBinding storageName)
    _ -> TypedResolvedName TypedCurrentModule TypedValueNamespace (identifierText name)

applicationSpine :: ProvisionalTypedExpr -> (ProvisionalTypedExpr, [ProvisionalTypedExpr])
applicationSpine = go []
  where
    go arguments expression =
      case expression of
        ProvisionalApplyExpression _ function argument ->
          go (argument : arguments) function
        _ -> (expression, arguments)

lambdaCount :: ProvisionalTypedExpr -> Int
lambdaCount expression =
  case expression of
    ProvisionalLambdaExpression _ _ body -> 1 + lambdaCount body
    _ -> 0
