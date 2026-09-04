{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Checked construction from an analyzed module body and its resolved public
-- interface. Inference decisions stay on the input nodes; construction owns the
-- concrete types and callable representations it selects.
module Jazz.Compiler.TypedCore.Build (buildTypedProgram) where

import Control.Applicative ((<|>))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST (CaseArm (..), CoreNode (..), CorePhase (Analyzed), CoreSort (StatementSort), Expr (..), ImplMethod (..), Statement (..))
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode (ResolveKernelOnly), builtinNamesInMode)
import Jazz.Compiler.Diagnostics (SourceSpan (..))
import Jazz.Compiler.ModuleExports (ModuleExport (..))
import Jazz.Compiler.ModuleGraph (ResolvedModuleFacts (..))
import Jazz.Compiler.Name (GeneratedNameKind (OperatorBinding), Name (..), NameNamespace (..), ResolvedName, identifierText, mkIdentifier, resolvedAmbientName)
import Jazz.Compiler.RecursiveBindings (freeVarsExprWithBound, inferRecursiveGroupsOrdered)
import Jazz.Compiler.SemanticFacts (AnalyzedScheme (..), AnalyzedType, ExpressionFacts (..), StatementFacts (..))
import Jazz.Compiler.TypeRepresentation (SemanticType (..))
import Jazz.Compiler.TypedCore
import Jazz.Compiler.TypedCore.Build.Exports (buildExports, sourceOrderedExports)
import Jazz.Compiler.TypedCore.Build.Expressions
import Jazz.Compiler.TypedCore.Build.Result
import Jazz.Compiler.TypedCore.Build.StructuredValues
import Jazz.Compiler.TypedCore.Query (typedExpressionChildren, typedExpressionReferencesAnyBinder)
import Jazz.Compiler.TypedCore.Validate (validateTypedProgramOnce)

buildTypedProgram :: TypedSourcePath -> [Text] -> ResolvedModuleFacts -> [Statement 'Analyzed] -> TypedCoreBuildResult
buildTypedProgram sourcePath modulePath publicFacts statements =
  case NonEmpty.nonEmpty (catalogFailures <> moduleFailures <> statementFailures) of
    Just failures -> TypedCoreProductionUnsupported failures
    Nothing -> case reverse typedStatements of
      TypedExpressionStatement _ result : _ ->
        let program = TypedProgram Nothing [TypedModule modulePath sourcePath [] typedExports interface recursiveGroups typedStatements (typedExpressionInfo result)] modulePath
         in case validateTypedProgramOnce program of
              Left failures -> TypedCoreProductionInvariantFailures failures
              Right validated -> TypedCoreProductionSucceeded validated
      _ -> TypedCoreProductionUnsupported (NonEmpty.singleton missingResult)
  where
    indexedStatements = zip [0 ..] statements
    (catalogFailures, catalog) = buildStructuredValueCatalog modulePath statements
    orderedExports = sourceOrderedExports (resolvedModuleExports publicFacts) (resolvedModuleExportSelectors publicFacts) statements
    (exportFailures, interface) = buildExports modulePath (resolvedModuleExports publicFacts) orderedExports statements catalog functionSchemes
    typedExports = [TypedModuleExport (namespace category) name | ModuleExport category name <- orderedExports]
    moduleFailures = [missingResult | not (hasResult statements)] <> exportFailures
    missingResult = TypedCoreProductionFailure (TypedCoreProductionModulePath modulePath) TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail
    hasResult body = case reverse body of SExpr {} : _ -> True; _ -> False

    declarations =
      [ (index, name, schemeValue, expression)
      | (index, SLet node name expression) <- indexedStatements,
        Just schemeValue <- [statementScheme node],
        SemanticFunction {} <- [analyzedSchemeType schemeValue]
      ]
    functions =
      Map.fromListWith
        (\_ first -> first)
        [ (name, (index, expressionTypeOf expression, lambdaCount expression))
        | (index, name, _, expression) <- declarations,
          lambdaCount expression > 0
        ]
    functionBindings =
      Map.mapWithKey
        (\name (index, expressionType, arity) -> ExpressionBinding (binderAt index name) expressionType (Just (shape name, arity)))
        functions
    functionSchemes =
      Map.mapWithKey
        (\name (index, expressionType, arity) -> scheme (binderAt index name) (shape name) <$> callableTypeInfo catalog (shape name) arity expressionType)
        functions
    reboundDeclarations = snd (foldl' collectRebinding (Set.empty, Set.empty) declarations)
    collectRebinding (names, indices) (index, name, _, _) =
      (Set.insert name names, if Set.member name names then Set.insert index indices else indices)

    -- Visibility is lexical, while capture dependencies between named functions
    -- may require more than one propagation step.
    directCaptures = snd (foldl' collectCaptures (Set.empty, Set.empty) indexedStatements)
    collectCaptures (visibleScalars, capturing) (_, statement) = case statement of
      SLet node name expression -> case statementScheme node of
        Just schemeValue
          | SemanticFunction {} <- analyzedSchemeType schemeValue ->
              (Set.delete name visibleScalars, if Set.disjoint visibleScalars (freeVarsExprWithBound Set.empty expression) then capturing else Set.insert name capturing)
        _ -> (Set.insert name visibleScalars, capturing)
      _ -> (visibleScalars, capturing)
    captureDependencies =
      Map.fromListWith
        Set.union
        [(name, Map.keysSet functions `Set.intersection` freeVarsExprWithBound Set.empty expression) | (_, name, _, expression) <- declarations]
    capturingFunctions = converge directCaptures
      where
        converge selected =
          let next = Map.foldlWithKey' (\acc name dependencies -> if Set.disjoint acc dependencies then acc else Set.insert name acc) selected captureDependencies
           in if next == selected then selected else converge next
    valueFunctions = foldMap statementValueUses statements
    shape name
      | Set.member name closureFunctions = TypedClosureCallableShape
      | otherwise = TypedDirectCallableShape
    closureFunctions = foldl' promoteGroup (capturingFunctions <> valueFunctions) (Map.elems groups)
    promoteGroup selected members =
      let names = Set.fromList [name | member <- members, Just name <- [Map.lookup member declarationNames]]
       in if Set.disjoint selected names then selected else selected <> names
    statementValueUses statement = case statement of
      SLet _ _ expression -> valueUses Set.empty expression
      SExpr _ expression -> valueUses Set.empty expression
      _ -> Set.empty
    valueUses bound expression = case expression of
      EVar _ name | Set.notMember name bound, Map.member name functions -> Set.singleton name
      ELambda _ name body -> valueUses (Set.insert name bound) body
      EApply {} ->
        let (callee, arguments) = applications expression
            calleeUses = case callee of
              EVar _ name | Set.notMember name bound, Just (_, _, arity) <- Map.lookup name functions, length arguments >= arity -> Set.empty
              _ -> valueUses bound callee
         in calleeUses <> foldMap (valueUses bound) arguments
      ETuple _ elements -> foldMap (valueUses bound) elements
      EBinary _ _ left right -> valueUses bound left <> valueUses bound right
      EIf _ condition thenExpression elseExpression -> foldMap (valueUses bound) [condition, thenExpression, elseExpression]
      EPatternCase _ scrutinee arms -> valueUses bound scrutinee <> foldMap (\(CaseArm _ _ guardExpression body) -> foldMap (valueUses bound) guardExpression <> valueUses bound body) arms
      _ -> Set.empty
    applications = go []
      where
        go arguments (EApply _ function argument) = go (argument : arguments) function
        go arguments callee = (callee, arguments)

    groups = inferRecursiveGroupsOrdered (Set.map (resolvedAmbientName ValueNamespace . mkIdentifier) (builtinNamesInMode ResolveKernelOnly)) indexedStatements
    declarationNames = Map.fromList [(index, name) | (index, name, _, _) <- declarations]
    recursiveGroups =
      [ TypedRecursiveGroup [binderAt member name | member <- members, Just name <- [Map.lookup member declarationNames]]
      | (index, members) <- Map.toAscList groups,
        case members of first : _ -> index == first; [] -> False
      ]

    scalarsBefore = snd (foldl' scalarScope (Map.empty, Map.empty) indexedStatements)
    scalarScope (visible, scopes) (index, statement) =
      let next = case statement of
            SLet _ name expression | not (Map.member name functions), not (isFunction (expressionTypeOf expression)) -> Map.insert name (ExpressionBinding (binderAt index name) (expressionTypeOf expression) Nothing) visible
            _ -> visible
       in (next, Map.insert index visible scopes)
    candidateBodies =
      Map.fromList
        [ (index, (name, arity, typedBody))
        | (index, name, declaredScheme, expression) <- declarations,
          let arity = lambdaCount expression,
          arity > 0,
          monomorphic declaredScheme,
          not (generatedOperator name),
          Set.notMember index reboundDeclarations,
          Right typedBody <- [buildExpression catalog (ExpressionContext modulePath index [0] (Just (expressionTypeOf expression)) (Map.union (Map.findWithDefault Map.empty index scalarsBefore) functionBindings) (FunctionDefinition (shape name) arity) True Map.empty) expression]
        ]
    rejectedRecursiveMembers =
      Set.fromList
        [ member
        | members <- Map.elems groups,
          member <- members,
          Map.member member declarationNames,
          not (supportedGroup members)
            || ( case Map.lookup member candidateBodies of
                   Just (name, _, _) | shape name == TypedClosureCallableShape -> any (>= minimum members) (Set.toList (captureStatements (Set.fromList members) member))
                   _ -> False
               )
        ]
    directScalarCaptures =
      Map.mapWithKey
        ( \index (_, _, body) ->
            let scalarTypes = Map.fromList [(bindingOwner binding, bindingSemanticType binding) | binding <- Map.elems (Map.findWithDefault Map.empty index scalarsBefore)]
             in [(owner, selectCaptureType original selected) | (owner, selected) <- typedReferences body, Just original <- [Map.lookup owner scalarTypes]]
        )
        candidateBodies
    captureStatements peers index = go Set.empty index
      where
        go seen current
          | Set.member current seen = Set.empty
          | otherwise =
              Set.fromList [statementIndex | (TypedBinderId (_, statementIndex : _, _), _) <- Map.findWithDefault [] current directScalarCaptures]
                <> foldMap
                  (go (Set.insert current seen))
                  [ dependencyIndex
                  | Just name <- [Map.lookup current declarationNames],
                    dependency <- Set.toList (Map.findWithDefault Set.empty name captureDependencies),
                    shape dependency == TypedClosureCallableShape,
                    Just (dependencyIndex, _, _) <- [Map.lookup dependency functions],
                    Set.notMember dependencyIndex peers
                  ]
    recursiveCaptureTypes =
      Map.fromListWith
        (\_ first -> first)
        [ capture
        | index <- Map.keys groups,
          Set.notMember index rejectedRecursiveMembers,
          capture <- Map.findWithDefault [] index directScalarCaptures
        ]
    eagerCaptureAvailability =
      Map.fromList
        [ (name, maximum (Set.toList captures))
        | (index, (name, _, _)) <- Map.toList candidateBodies,
          shape name == TypedClosureCallableShape,
          let captures = captureStatements Set.empty index,
          not (Set.null captures)
        ]
    unavailableCaptures =
      Set.fromList
        [ index
        | (index, (name, _, _)) <- Map.toList candidateBodies,
          Map.notMember index groups,
          Just availableAfter <- [Map.lookup name eagerCaptureAvailability],
          availableAfter >= index
        ]
    supportedGroup members = case traverse (`Map.lookup` candidateBodies) members of
      Just candidates@((firstName, _, _) : _) ->
        let selectedShape = shape firstName
            binders = Set.fromList [binderAt index name | index <- members, Just name <- [Map.lookup index declarationNames]]
         in all (\(name, _, _) -> shape name == selectedShape) candidates
              && (selectedShape == TypedClosureCallableShape || all (\(_, arity, body) -> not (nestedLambdaReferencesAnyBinder arity binders body)) candidates)
      _ -> False

    (failureChunks, reversedStatements, _) = foldl' buildStatement ([], [], Map.empty) indexedStatements
    statementFailures = concat (reverse failureChunks)
    typedStatements = reverse reversedStatements
    buildStatement (failures, built, scalars) (index, statement) =
      let bindings = Map.union scalars functionBindings
          context path expected purpose = ExpressionContext modulePath index path expected bindings purpose (case purpose of FunctionDefinition {} -> True; _ -> False) eagerCaptureAvailability
          append result nextBindings = case result of
            Left errors -> (map productionFailure errors : failures, built, scalars)
            Right typedStatement -> (failures, typedStatement : built, nextBindings)
          signatureInfo name expressionType = case Map.lookup name functions of
            Just (_, _, arity) -> callableTypeInfo catalog (shape name) arity expressionType
            Nothing -> structuredNodeInfo catalog expressionType
       in case statement of
            SSignature node name _ -> case statementScheme node >>= (signatureInfo name . analyzedSchemeType) of
              Just info -> append (Right (TypedSignatureStatement (binderAt index name) (valueName name) (typedSpan (coreNodeSpan node)) (scheme (binderAt index name) (shape name) info))) scalars
              Nothing -> append (Left [expressionFailure index [] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail]) scalars
            SLet node name expression ->
              let selectedScheme = statementScheme node
                  expressionType = if isFunction (expressionTypeOf expression) then expressionTypeOf expression else selectType (expressionFacts expression) (Map.lookup (binderAt index name) recursiveCaptureTypes <|> (analyzedSchemeType <$> selectedScheme)) (expressionSemanticType (expressionFacts expression))
                  callable = case expressionType of SemanticFunction {} -> True; _ -> False
                  arity = lambdaCount expression
                  selectedShape = shape name
                  recursiveFailures = [statementFailure index TypedCoreRecursiveFunctionUnsupported (TypedCoreNameDetail (identifierText name)) | Set.member index rejectedRecursiveMembers]
                  rebindingFailures = [statementFailure index TypedCoreFunctionRebindingUnsupported (TypedCoreNameDetail (identifierText name)) | Set.member index reboundDeclarations]
                  owningFailures
                    | callable && arity == 0 = recursiveFailures <> rebindingFailures <> [statementFailure index TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]
                    | otherwise =
                        [statementFailure index TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail | not callable && Map.member name functions]
                          <> [statementFailure index TypedCoreUserDefinedOperatorUnsupported TypedCoreUnsupportedRootDetail | generatedOperator name]
                          <> recursiveFailures
                          <> [statementFailure index TypedCoreCaptureUnsupported (TypedCoreNameDetail (identifierText name)) | Set.member index unavailableCaptures]
                          <> rebindingFailures
                          <> [statementFailure index TypedCoreNonMonomorphicFunctionUnsupported (TypedCoreNameDetail (identifierText name)) | callable && maybe True (not . monomorphic) selectedScheme]
                  purpose = if callable then FunctionDefinition selectedShape arity else ExpressionValue
                  result = buildExpression catalog (context [0] (Just expressionType) purpose) expression
                  owner = binderAt index name
                  typeSelected = selectType (expressionFacts expression) (Just expressionType) (expressionSemanticType (expressionFacts expression))
                  binding = ExpressionBinding owner typeSelected (if callable then Just (selectedShape, arity) else Nothing)
                  nextBindings = if callable then scalars else Map.insert name binding scalars
                  childrenFailures = either id (const []) result
                  retainedFailures = sourceFailures childrenFailures
                  infoFailures = [expressionFailure index [] TypedCoreUnresolvedExpressionType TypedCoreUnsupportedRootDetail | Nothing <- [signatureInfo name expressionType]]
                  selectedFailures
                    | callable && arity == 0 = owningFailures <> retainedFailures
                    | not callable && not (null retainedFailures) = statementFailure index TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail : retainedFailures
                    | otherwise = owningFailures <> infoFailures <> childrenFailures
               in case (selectedFailures, result) of
                    ([], Right body) -> append (Right (TypedLetStatement owner (valueName name) (typedSpan (coreNodeSpan node)) (scheme owner selectedShape (typedExpressionInfo body)) body)) nextBindings
                    _ -> append (Left selectedFailures) scalars
            SExpr node expression -> append (TypedExpressionStatement (typedSpan (coreNodeSpan node)) <$> buildExpression catalog (context [] Nothing ExpressionValue) expression) scalars
            SData {} -> case structuredDataStatement catalog index of
              Just typedStatement -> append (Right typedStatement) scalars
              Nothing -> (failures, built, scalars)
            SImpl _ _ _ methods ->
              let childFailures = concat [sourceFailures (either id (const []) (buildExpression catalog (context [methodIndex] Nothing ExpressionValue) body)) | (methodIndex, ImplMethod _ _ body) <- zip [0 ..] methods]
               in append (Left (statementFailure index TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail : childFailures)) scalars
            _ -> append (Left [statementFailure index TypedCoreUnsupportedRootExpression TypedCoreUnsupportedRootDetail]) scalars

    binderAt index name = TypedBinderId (modulePath, [index], valueName name)
    statementFailure index kind detail = ConstructionFailure (TypedCoreProductionFailure (TypedCoreProductionStatementPath modulePath index) kind detail)
    expressionFailure index path kind detail = ConstructionFailure (TypedCoreProductionFailure (TypedCoreProductionExpressionPath modulePath index path) kind detail)
    scheme owner selectedShape info = TypedScheme owner [] [] [] (typedNodeType info) (typedNodeRecipe info) (case typedNodeType info of SemanticFunction {} -> Just selectedShape; _ -> Nothing)

statementScheme :: CoreNode 'Analyzed 'StatementSort -> Maybe AnalyzedScheme
statementScheme node = case Map.elems (statementGeneralizedSchemes (coreNodeFacts node)) of
  [scheme] -> Just scheme
  _ -> Nothing

monomorphic :: AnalyzedScheme -> Bool
monomorphic scheme = null (analyzedSchemeVariables scheme) && null (analyzedSchemeConstraints scheme) && null (analyzedSchemePrimitiveConstraints scheme)

lambdaCount :: Expr 'Analyzed -> Int
lambdaCount (ELambda _ _ body) = 1 + lambdaCount body
lambdaCount _ = 0

generatedOperator :: ResolvedName -> Bool
generatedOperator (GeneratedName (OperatorBinding _)) = True
generatedOperator _ = False

valueName :: ResolvedName -> TypedCoreName
valueName (GeneratedName (OperatorBinding storageName)) = TypedGeneratedName (TypedOperatorBinding storageName)
valueName name = TypedResolvedName TypedCurrentModule TypedValueNamespace (identifierText name)

namespace :: NameNamespace -> TypedNameNamespace
namespace ValueNamespace = TypedValueNamespace
namespace ConstructorNamespace = TypedConstructorNamespace
namespace TypeNamespace = TypedTypeNamespace
namespace CapabilityNamespace = TypedCapabilityNamespace

typedSpan :: SourceSpan -> TypedSpan
typedSpan source = TypedSpan (spanLine source) (spanColumn source)

isFunction :: SemanticType name variable -> Bool
isFunction SemanticFunction {} = True
isFunction _ = False

expressionTypeOf :: Expr 'Analyzed -> AnalyzedType
expressionTypeOf expression = selectType (expressionFacts expression) Nothing (expressionSemanticType (expressionFacts expression))

nestedLambdaReferencesAnyBinder :: Int -> Set.Set TypedBinderId -> TypedExpr -> Bool
nestedLambdaReferencesAnyBinder leadingLambdaCount binders = skipLeading leadingLambdaCount
  where
    skipLeading remaining (TypedLambdaExpr _ _ _ body)
      | remaining > 0 = skipLeading (remaining - 1) body
    skipLeading _ expression = nestedReference expression

    nestedReference expression = case expression of
      TypedLambdaExpr _ _ _ body -> typedExpressionReferencesAnyBinder binders body
      _ -> any nestedReference (typedExpressionChildren expression)

typedReferences :: TypedExpr -> [(TypedBinderId, TypedType)]
typedReferences expression = case expression of
  TypedVariableExpr info _ (Just owner) -> [(owner, typedNodeType info)]
  _ -> concatMap typedReferences (typedExpressionChildren expression)

-- Keep analyzed data identities while adopting the concrete representation
-- proved by a constructed reference to the capture.
selectCaptureType :: AnalyzedType -> TypedType -> AnalyzedType
selectCaptureType original selected = case (original, selected) of
  (_, SemanticInt) -> SemanticInt
  (_, SemanticFloat) -> SemanticFloat
  (_, SemanticNumeric numeric) -> SemanticNumeric numeric
  (SemanticTuple elements, SemanticTuple selectedElements) -> SemanticTuple (zipWith selectCaptureType elements selectedElements)
  (SemanticData name arguments, SemanticData _ selectedArguments) -> SemanticData name (zipWith selectCaptureType arguments selectedArguments)
  _ -> original
