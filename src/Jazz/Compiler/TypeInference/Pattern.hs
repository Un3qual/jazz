{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.TypeInference.Pattern
  ( inferPatternCaseType,
    inferPatternType,
    instantiateConstructorBinding,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.AST
  ( CaseArm (..),
    CoreNode (coreNodeFacts, coreNodeId),
    CorePhase (..),
    Literal (..),
    Pattern (..),
    patternNode,
  )
import Jazz.Compiler.CoreIdentity (ResolvedNodeFacts)
import Jazz.Compiler.Name (ResolvedName, identifierText)
import Jazz.Compiler.Pattern
  ( commonPatternBinderNames,
    patternBinderNames,
  )
import Jazz.Compiler.SemanticFacts
  ( PatternConstructorFact (..),
    PatternFacts (..),
    PatternRefutability (..),
  )
import Jazz.Compiler.TypeInference.Capabilities (defaultLiteralTypes)
import Jazz.Compiler.TypeInference.Diagnostics
import Jazz.Compiler.TypeInference.Environment (insertResolvedTypeBinding)
import Jazz.Compiler.TypeInference.Solver
  ( freshIntegerLiteralType,
    freshTypeVar,
    freshTypeVars,
    resolveType,
    unifyTypes,
  )
import Jazz.Compiler.TypeInference.State
  ( InferState (..),
    InferenceOutput (..),
    inferErrorCount,
    inferErrorsRev,
    modifyInferenceOutput,
    recordExpressionFactType,
    recordPatternFactSeed,
  )
import Jazz.Compiler.TypeInference.Traversal (InferExprWithModeFn, InferenceMode)
import Jazz.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    ExpressionType,
    IntegerLiteralRange (..),
    SemanticBinding (..),
    SemanticType (..),
    TypeBinding,
    TypeEnv,
    instantiateDeclarationType,
    typeEnvReferenceKey,
  )

inferPatternCaseType ::
  InferExprWithModeFn ->
  InferenceMode ->
  TypeEnv ->
  ExpressionType ->
  InferState ->
  [CaseArm 'Resolved] ->
  (Maybe ExpressionType, InferState)
inferPatternCaseType inferExpression mode env scrutineeType initialState caseArms =
  foldl' step (Nothing, initialState) caseArms
  where
    step (maybeExpectedBodyType, stateAcc) (CaseArm armNode pattern guardExpr bodyExpr) =
      let (rawPatternTyping, stateAfterPatternCheck) =
            inferPatternType env scrutineeType pattern stateAcc
          (patternTyping, stateAfterPattern) =
            rejectDuplicatePatternBinders pattern rawPatternTyping stateAcc stateAfterPatternCheck
       in if patternSkipsBranchType patternTyping
            then
              (maybeExpectedBodyType, stateAfterPattern)
            else
              let armEnv =
                    extendTypeEnvWithPatternBindings
                      (patternBindings patternTyping)
                      env
                  stateAfterGuard =
                    inferCaseGuardType armEnv stateAfterPattern guardExpr
                  (bodyResult, stateAfterBody) =
                    inferExpression mode armEnv stateAfterGuard bodyExpr
                  maybeBodyType = bodyResult
                  stateAfterBodyFacts =
                    maybe
                      stateAfterBody
                      (\bodyType -> recordExpressionFactType (coreNodeId armNode) bodyType stateAfterBody)
                      maybeBodyType
               in case (maybeExpectedBodyType, maybeBodyType) of
                    (Nothing, _) ->
                      (fmap (resolveType stateAfterBodyFacts) maybeBodyType, stateAfterBodyFacts)
                    (expectedBodyType, Nothing) ->
                      (expectedBodyType, stateAfterBodyFacts)
                    (Just inferredExpectedBodyType, Just inferredBodyType) ->
                      case unifyTypes inferredExpectedBodyType inferredBodyType stateAfterBodyFacts of
                        Just unifiedState ->
                          (Just (resolveType unifiedState inferredExpectedBodyType), unifiedState)
                        Nothing ->
                          ( Just inferredExpectedBodyType,
                            addTypeError
                              stateAfterBodyFacts
                              ( mkPatternBranchTypeMismatchError
                                  (diagnosticType stateAfterBodyFacts inferredExpectedBodyType)
                                  (diagnosticType stateAfterBodyFacts inferredBodyType)
                              )
                          )

    inferCaseGuardType armEnv stateAcc guardExpr =
      case guardExpr of
        Nothing -> stateAcc
        Just conditionExpr ->
          let (guardResult, stateAfterGuard) =
                inferExpression mode armEnv stateAcc conditionExpr
              maybeGuardType = guardResult
              checkedState =
                case maybeGuardType of
                  Just inferredGuardType ->
                    case unifyTypes inferredGuardType SemanticBool stateAfterGuard of
                      Just unifiedState -> unifiedState
                      Nothing ->
                        addTypeError
                          stateAfterGuard
                          (mkCaseGuardTypeError (diagnosticType stateAfterGuard inferredGuardType))
                  Nothing ->
                    stateAfterGuard
           in checkedState

newtype PatternBindings = PatternBindings (Map ResolvedName (ResolvedNodeFacts, ExpressionType))
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

singletonPatternBinding :: ResolvedNodeFacts -> ResolvedName -> ExpressionType -> PatternBindings
singletonPatternBinding facts name expressionType =
  PatternBindings (Map.singleton name (facts, expressionType))

lookupPatternBinding :: ResolvedName -> PatternBindings -> Maybe ExpressionType
lookupPatternBinding name (PatternBindings bindings) = snd <$> Map.lookup name bindings

insertPatternBinding :: ResolvedNodeFacts -> ResolvedName -> ExpressionType -> PatternBindings -> PatternBindings
insertPatternBinding facts name expressionType (PatternBindings bindings) =
  PatternBindings (Map.insert name (facts, expressionType) bindings)

updatePatternBinding :: ResolvedName -> ExpressionType -> PatternBindings -> PatternBindings
updatePatternBinding name expressionType (PatternBindings bindings) =
  PatternBindings (Map.adjust (\(facts, _) -> (facts, expressionType)) name bindings)

patternBindingNames :: PatternBindings -> Set ResolvedName
patternBindingNames (PatternBindings bindings) = Map.keysSet bindings

extendTypeEnvWithPatternBindings :: PatternBindings -> TypeEnv -> TypeEnv
extendTypeEnvWithPatternBindings (PatternBindings bindings) env =
  Map.foldlWithKey'
    ( \extended name (facts, expressionType) ->
        insertResolvedTypeBinding facts name (PlainTypeBinding expressionType) extended
    )
    env
    bindings

data PatternTyping = PatternTyping
  { patternBindings :: PatternBindings,
    patternSkipsBranchType :: Bool
  }

instance Semigroup PatternTyping where
  left <> right =
    PatternTyping
      { patternBindings = patternBindings left <> patternBindings right,
        patternSkipsBranchType =
          patternSkipsBranchType left || patternSkipsBranchType right
      }

instance Monoid PatternTyping where
  mempty =
    PatternTyping
      { patternBindings = mempty,
        patternSkipsBranchType = False
      }

skipBranchPatternTyping :: PatternTyping
skipBranchPatternTyping =
  mempty {patternSkipsBranchType = True}

rejectDuplicatePatternBinders :: Pattern 'Resolved -> PatternTyping -> InferState -> InferState -> (PatternTyping, InferState)
rejectDuplicatePatternBinders pattern typing stableState checkedState =
  case patternDuplicateBinderNames pattern of
    [] -> (typing, checkedState)
    duplicateNames ->
      let stateWithDuplicateErrors =
            foldl' addDuplicateError checkedState duplicateNames
       in ( typing {patternSkipsBranchType = True},
            rollbackSkippedPatternState stableState stateWithDuplicateErrors
          )
  where
    addDuplicateError stateAcc duplicateName =
      addTypeError stateAcc (mkDuplicatePatternBinderError duplicateName)

patternDuplicateBinderNames :: Pattern 'Resolved -> [ResolvedName]
patternDuplicateBinderNames pattern =
  Set.toList duplicates
  where
    (_, duplicates) = collect pattern Set.empty Set.empty

    collect :: Pattern 'Resolved -> Set ResolvedName -> Set ResolvedName -> (Set ResolvedName, Set ResolvedName)
    collect candidate seen duplicatesAcc =
      case candidate of
        PVariable _ name ->
          if Set.member name seen
            then (seen, Set.insert name duplicatesAcc)
            else (Set.insert name seen, duplicatesAcc)
        PWildcard _ -> (seen, duplicatesAcc)
        PLiteral {} -> (seen, duplicatesAcc)
        PConstructor _ _ nestedPatterns ->
          collectNested seen duplicatesAcc nestedPatterns
        PList _ nestedPatterns ->
          collectNested seen duplicatesAcc nestedPatterns
        PConsList _ headPattern tailPattern ->
          collectNested seen duplicatesAcc [headPattern, tailPattern]
        PTuple _ nestedPatterns ->
          collectNested seen duplicatesAcc nestedPatterns
        PAs _ name nestedPattern ->
          let (seenAfterName, duplicatesAfterName) =
                if Set.member name seen
                  then (seen, Set.insert name duplicatesAcc)
                  else (Set.insert name seen, duplicatesAcc)
           in collect nestedPattern seenAfterName duplicatesAfterName
        POr _ alternatives ->
          let duplicatesAfterAlternatives =
                foldl'
                  ( \duplicatesAcc' alternative ->
                      Set.union duplicatesAcc' (Set.intersection seen (patternBinderNames alternative))
                  )
                  duplicatesAcc
                  alternatives
           in (Set.union seen (commonPatternBinderNames alternatives), duplicatesAfterAlternatives)

    collectNested seen duplicatesAcc =
      foldl'
        ( \(seenAcc, duplicatesAcc') nestedPattern ->
            collect nestedPattern seenAcc duplicatesAcc'
        )
        (seen, duplicatesAcc)

inferPatternType :: TypeEnv -> ExpressionType -> Pattern 'Resolved -> InferState -> (PatternTyping, InferState)
inferPatternType env scrutineeType pattern state =
  let (typing, inferredState) = inferPatternTypeRaw env scrutineeType pattern state
      facts =
        PatternFacts
          { patternResolution = coreNodeFacts (patternNode pattern),
            patternBindingTypes = resolvedPatternBindingMap inferredState (patternBindings typing),
            patternConstructorFact = patternConstructor pattern,
            patternRefutability = patternRefutabilityFact pattern
          }
   in (typing, recordPatternFactSeed (coreNodeId (patternNode pattern)) facts inferredState)

inferPatternTypeRaw :: TypeEnv -> ExpressionType -> Pattern 'Resolved -> InferState -> (PatternTyping, InferState)
inferPatternTypeRaw env scrutineeType pattern state =
  case pattern of
    PVariable node name ->
      ( mempty
          { patternBindings =
              singletonPatternBinding
                (coreNodeFacts node)
                name
                (resolveType state scrutineeType)
          },
        state
      )
    PWildcard _ -> (mempty, state)
    PLiteral _ literal ->
      let (literalType, stateAfterLiteral) = literalExpressionType literal state
       in case unifyTypes scrutineeType literalType stateAfterLiteral of
            Just unifiedState -> (mempty, unifiedState)
            Nothing ->
              ( skipBranchPatternTyping,
                addTypeError
                  stateAfterLiteral
                  ( mkPatternTypeMismatchError
                      (diagnosticType stateAfterLiteral scrutineeType)
                      (diagnosticType stateAfterLiteral literalType)
                  )
              )
    PConstructor node constructorName patterns ->
      inferConstructorPatternType env scrutineeType (coreNodeFacts node) constructorName patterns state
    PList _ patterns ->
      inferListPatternType env scrutineeType patterns state
    PConsList _ headPattern tailPattern ->
      inferConsListPatternType env scrutineeType headPattern tailPattern state
    PTuple _ patterns ->
      inferTuplePatternType env scrutineeType patterns state
    PAs node name nestedPattern ->
      let (typing, stateAfterPattern) =
            inferPatternType env scrutineeType nestedPattern state
       in if patternSkipsBranchType typing
            then (typing, stateAfterPattern)
            else
              ( typing
                  { patternBindings =
                      insertPatternBinding
                        (coreNodeFacts node)
                        name
                        (resolveType stateAfterPattern scrutineeType)
                        (patternBindings typing)
                  },
                stateAfterPattern
              )
    POr _ alternatives ->
      inferOrPatternType env scrutineeType alternatives state

resolvedPatternBindingMap :: InferState -> PatternBindings -> Map ResolvedName ExpressionType
resolvedPatternBindingMap state (PatternBindings bindings) = Map.map (resolveType state . snd) bindings

patternConstructor :: Pattern 'Resolved -> PatternConstructorFact
patternConstructor pattern =
  case pattern of
    PConstructor _ constructorName _ -> PatternConstructor constructorName
    _ -> PatternHasNoConstructor

patternRefutabilityFact :: Pattern phase -> PatternRefutability
patternRefutabilityFact pattern =
  case pattern of
    PWildcard {} -> IrrefutablePattern
    PVariable {} -> IrrefutablePattern
    PAs _ _ nestedPattern -> patternRefutabilityFact nestedPattern
    PTuple _ patterns
      | all ((== IrrefutablePattern) . patternRefutabilityFact) patterns -> IrrefutablePattern
    _ -> RefutablePattern

inferOrPatternType ::
  TypeEnv ->
  ExpressionType ->
  [Pattern 'Resolved] ->
  InferState ->
  (PatternTyping, InferState)
inferOrPatternType env scrutineeType alternatives initialState =
  case alternatives of
    [] ->
      ( skipBranchPatternTyping,
        addTypeError initialState mkEmptyOrPatternError
      )
    firstAlternative : rest ->
      let (firstTyping, stateAfterFirst) =
            inferOrPatternAlternative firstAlternative initialState
       in if patternSkipsBranchType firstTyping
            then (firstTyping, rollbackSkippedPatternState initialState stateAfterFirst)
            else
              let expectedBinderNames = patternBindingNames (patternBindings firstTyping)
               in inferRemainingAlternatives
                    expectedBinderNames
                    (patternBindings firstTyping)
                    stateAfterFirst
                    rest
  where
    inferOrPatternAlternative alternativePattern stateAcc =
      let (rawTyping, stateAfterPatternCheck) =
            inferPatternType env scrutineeType alternativePattern stateAcc
       in rejectDuplicatePatternBinders
            alternativePattern
            rawTyping
            stateAcc
            stateAfterPatternCheck

    inferRemainingAlternatives expectedBinderNames bindingsAcc stateAcc remainingAlternatives =
      case remainingAlternatives of
        [] ->
          ( mempty
              { patternBindings = resolvePatternBindings stateAcc bindingsAcc
              },
            stateAcc
          )
        alternativePattern : restAlternatives ->
          let (alternativeTyping, stateAfterAlternative) =
                inferOrPatternAlternative alternativePattern stateAcc
           in if patternSkipsBranchType alternativeTyping
                then (alternativeTyping, rollbackSkippedPatternState initialState stateAfterAlternative)
                else
                  let alternativeBindings = patternBindings alternativeTyping
                      alternativeBinderNames = patternBindingNames alternativeBindings
                   in if alternativeBinderNames /= expectedBinderNames
                        then
                          ( skipBranchPatternTyping,
                            rollbackSkippedPatternState
                              initialState
                              ( addTypeError
                                  stateAfterAlternative
                                  (mkOrPatternBinderSetMismatchError expectedBinderNames alternativeBinderNames)
                              )
                          )
                        else case unifyOrPatternBinders bindingsAcc alternativeBindings stateAfterAlternative of
                          Left failedState ->
                            (skipBranchPatternTyping, rollbackSkippedPatternState initialState failedState)
                          Right (mergedBindings, stateAfterBinders) ->
                            inferRemainingAlternatives
                              expectedBinderNames
                              mergedBindings
                              stateAfterBinders
                              restAlternatives

    unifyOrPatternBinders bindingsAcc alternativeBindings stateAcc =
      foldl'
        unifyBinder
        (Right (bindingsAcc, stateAcc))
        (Set.toList (patternBindingNames bindingsAcc))
      where
        unifyBinder maybeAcc binderName =
          case maybeAcc of
            Left failedState -> Left failedState
            Right (mergedBindings, stateForBinder) ->
              case (lookupPatternBinding binderName mergedBindings, lookupPatternBinding binderName alternativeBindings) of
                (Just leftType, Just rightType) ->
                  case unifyTypes leftType rightType stateForBinder of
                    Just unifiedState ->
                      Right
                        ( updatePatternBinding
                            binderName
                            (resolveType unifiedState leftType)
                            mergedBindings,
                          unifiedState
                        )
                    Nothing ->
                      Left
                        ( addTypeError
                            stateForBinder
                            ( mkOrPatternBinderTypeMismatchError
                                binderName
                                (diagnosticType stateForBinder leftType)
                                (diagnosticType stateForBinder rightType)
                            )
                        )
                _ ->
                  Left
                    ( addTypeError
                        stateForBinder
                        (mkOrPatternBinderSetMismatchError (patternBindingNames mergedBindings) (patternBindingNames alternativeBindings))
                    )

resolvePatternBindings :: InferState -> PatternBindings -> PatternBindings
resolvePatternBindings state (PatternBindings bindings) =
  PatternBindings (Map.map (fmap (resolveType state)) bindings)

inferConstructorPatternType ::
  TypeEnv ->
  ExpressionType ->
  ResolvedNodeFacts ->
  ResolvedName ->
  [Pattern 'Resolved] ->
  InferState ->
  (PatternTyping, InferState)
inferConstructorPatternType env scrutineeType facts constructorName patterns state =
  case Map.lookup (typeEnvReferenceKey facts constructorName) env of
    Just constructorBinding ->
      case instantiateConstructorBinding constructorBinding state of
        Just (argumentTypes, constructorResultType, stateAfterConstructor) ->
          let expectedArity = length argumentTypes
           in if expectedArity /= length patterns
                then
                  ( skipBranchPatternTyping,
                    addTypeError
                      stateAfterConstructor
                      (mkConstructorPatternArityError constructorNameText expectedArity (length patterns))
                  )
                else case unifyTypes scrutineeType constructorResultType stateAfterConstructor of
                  Just stateAfterResultCheck ->
                    inferConstructorArgumentPatterns
                      env
                      (map (resolveType stateAfterResultCheck) argumentTypes)
                      patterns
                      stateAfterResultCheck
                  Nothing ->
                    ( skipBranchPatternTyping,
                      addTypeError
                        stateAfterConstructor
                        ( mkPatternTypeMismatchError
                            (diagnosticType stateAfterConstructor scrutineeType)
                            (diagnosticType stateAfterConstructor constructorResultType)
                        )
                    )
        Nothing ->
          ( skipBranchPatternTyping,
            addTypeError
              state
              (mkUnknownConstructorPatternError constructorNameText)
          )
    _ ->
      ( skipBranchPatternTyping,
        addTypeError
          state
          (mkUnknownConstructorPatternError constructorNameText)
      )
  where
    constructorNameText = identifierText constructorName

inferConstructorArgumentPatterns ::
  TypeEnv ->
  [ExpressionType] ->
  [Pattern 'Resolved] ->
  InferState ->
  (PatternTyping, InferState)
inferConstructorArgumentPatterns env argumentTypes patterns initialState =
  go mempty initialState (zip argumentTypes patterns)
  where
    go typingAcc stateAcc remainingPatterns =
      case remainingPatterns of
        [] -> (typingAcc, stateAcc)
        (argumentType, pattern) : rest ->
          let (typing, stateAfterPattern) =
                inferPatternType env argumentType pattern stateAcc
              mergedTyping = typing <> typingAcc
           in if patternSkipsBranchType mergedTyping
                then (mergedTyping, rollbackSkippedPatternState initialState stateAfterPattern)
                else go mergedTyping stateAfterPattern rest

inferListPatternType ::
  TypeEnv ->
  ExpressionType ->
  [Pattern 'Resolved] ->
  InferState ->
  (PatternTyping, InferState)
inferListPatternType env scrutineeType patterns state =
  let (elementType, stateWithElementType) = freshTypeVar state
      listPatternType = SemanticList elementType
      stateAfterListCheck =
        case unifyTypes scrutineeType listPatternType stateWithElementType of
          Just unifiedState -> unifiedState
          Nothing ->
            addTypeError
              stateWithElementType
              ( mkListPatternTypeMismatchError
                  (diagnosticType stateWithElementType scrutineeType)
              )
   in if hasNewPatternError stateWithElementType stateAfterListCheck
        then (skipBranchPatternTyping, rollbackSkippedPatternState state stateAfterListCheck)
        else
          inferListElementPatterns
            env
            (resolveType stateAfterListCheck elementType)
            patterns
            stateAfterListCheck

inferListElementPatterns ::
  TypeEnv ->
  ExpressionType ->
  [Pattern 'Resolved] ->
  InferState ->
  (PatternTyping, InferState)
inferListElementPatterns env elementType patterns initialState =
  go mempty initialState patterns
  where
    go typingAcc stateAcc remainingPatterns =
      case remainingPatterns of
        [] -> (typingAcc, stateAcc)
        pattern : rest ->
          let (typing, stateAfterPattern) =
                inferPatternType env elementType pattern stateAcc
              mergedTyping = typing <> typingAcc
           in if patternSkipsBranchType mergedTyping
                then (mergedTyping, rollbackSkippedPatternState initialState stateAfterPattern)
                else go mergedTyping stateAfterPattern rest

inferConsListPatternType ::
  TypeEnv ->
  ExpressionType ->
  Pattern 'Resolved ->
  Pattern 'Resolved ->
  InferState ->
  (PatternTyping, InferState)
inferConsListPatternType env scrutineeType headPattern tailPattern state =
  let (elementType, stateWithElementType) = freshTypeVar state
      listPatternType = SemanticList elementType
      stateAfterListCheck =
        case unifyTypes scrutineeType listPatternType stateWithElementType of
          Just unifiedState -> unifiedState
          Nothing ->
            addTypeError
              stateWithElementType
              ( mkListPatternTypeMismatchError
                  (diagnosticType stateWithElementType scrutineeType)
              )
   in if hasNewPatternError stateWithElementType stateAfterListCheck
        then (skipBranchPatternTyping, rollbackSkippedPatternState state stateAfterListCheck)
        else
          inferConsListSubpatterns
            env
            (resolveType stateAfterListCheck elementType)
            headPattern
            tailPattern
            stateAfterListCheck

inferConsListSubpatterns ::
  TypeEnv ->
  ExpressionType ->
  Pattern 'Resolved ->
  Pattern 'Resolved ->
  InferState ->
  (PatternTyping, InferState)
inferConsListSubpatterns env elementType headPattern tailPattern initialState =
  let (headTyping, stateAfterHeadPattern) =
        inferPatternType env elementType headPattern initialState
   in if patternSkipsBranchType headTyping
        then (headTyping, rollbackSkippedPatternState initialState stateAfterHeadPattern)
        else
          let tailListType = SemanticList (resolveType stateAfterHeadPattern elementType)
              (tailTyping, stateAfterTailPattern) =
                inferPatternType env tailListType tailPattern stateAfterHeadPattern
              mergedTyping = tailTyping <> headTyping
           in if patternSkipsBranchType mergedTyping
                then (mergedTyping, rollbackSkippedPatternState initialState stateAfterTailPattern)
                else (mergedTyping, stateAfterTailPattern)

inferTuplePatternType ::
  TypeEnv ->
  ExpressionType ->
  [Pattern 'Resolved] ->
  InferState ->
  (PatternTyping, InferState)
inferTuplePatternType env scrutineeType patterns state =
  case resolveType state scrutineeType of
    SemanticTuple elementTypes
      | length elementTypes == length patterns ->
          inferConstructorArgumentPatterns env elementTypes patterns state
      | otherwise ->
          ( skipBranchPatternTyping,
            addTypeError
              state
              (mkTuplePatternArityMismatchError (length patterns) (length elementTypes))
          )
    resolvedScrutineeType ->
      let (elementTypes, stateWithElementTypes) =
            freshTypeVars (length patterns) state
          tuplePatternType = SemanticTuple elementTypes
          stateAfterTupleCheck =
            case unifyTypes scrutineeType tuplePatternType stateWithElementTypes of
              Just unifiedState -> unifiedState
              Nothing ->
                addTypeError
                  stateWithElementTypes
                  (mkTuplePatternTypeMismatchError (diagnosticType stateWithElementTypes resolvedScrutineeType))
       in if hasNewPatternError stateWithElementTypes stateAfterTupleCheck
            then (skipBranchPatternTyping, rollbackSkippedPatternState state stateAfterTupleCheck)
            else
              inferConstructorArgumentPatterns
                env
                (map (resolveType stateAfterTupleCheck) elementTypes)
                patterns
                stateAfterTupleCheck

rollbackSkippedPatternState :: InferState -> InferState -> InferState
rollbackSkippedPatternState stableState failedState =
  modifyInferenceOutput
    ( \output ->
        output
          { outputErrorsRev = inferErrorsRev failedState,
            outputErrorCount = inferErrorCount failedState
          }
    )
    stableState

hasNewPatternError :: InferState -> InferState -> Bool
hasNewPatternError previousState nextState =
  inferErrorCount nextState > inferErrorCount previousState

literalExpressionType :: Literal -> InferState -> (ExpressionType, InferState)
literalExpressionType literal state =
  case literal of
    LInt value -> freshIntegerLiteralType (IntegerLiteralRange value value) state
    LFloat _ _ maybeTargetType ->
      (maybe SemanticFloat SemanticNumeric maybeTargetType, state)
    LBool _ -> (SemanticBool, state)
    LChar _ -> (SemanticChar, state)
    LText _ -> (SemanticText, state)

diagnosticType :: InferState -> ExpressionType -> ExpressionType
diagnosticType state = defaultLiteralTypes state . resolveType state

instantiateConstructorBinding :: TypeBinding -> InferState -> Maybe ([ExpressionType], ExpressionType, InferState)
instantiateConstructorBinding binding state =
  case binding of
    ConstructorTypeBinding typeName typeParameters argumentTypes ->
      Just (instantiateConstructorType typeName typeParameters argumentTypes state)
    _ -> Nothing

instantiateConstructorType ::
  ResolvedName ->
  [ResolvedName] ->
  [ConstructorArgumentType] ->
  InferState ->
  ([ExpressionType], ExpressionType, InferState)
instantiateConstructorType typeName typeParameters argumentTypes state =
  let (typeParameterBindings, resultParameterTypes, stateAfterParameters) =
        instantiateConstructorTypeParameters typeParameters state
      (constructorArgumentTypesRev, stateAfterArguments) =
        instantiateConstructorArguments typeParameterBindings argumentTypes stateAfterParameters
   in ( reverse constructorArgumentTypesRev,
        SemanticData typeName (reverse resultParameterTypes),
        stateAfterArguments
      )

instantiateConstructorTypeParameters ::
  [ResolvedName] ->
  InferState ->
  (Map Text ExpressionType, [ExpressionType], InferState)
instantiateConstructorTypeParameters typeParameters initialState =
  foldl' step (Map.empty, [], initialState) typeParameters
  where
    step (bindings, parameterTypesRev, stateAcc) typeParameter =
      let (parameterType, nextState) = freshTypeVar stateAcc
       in ( Map.insert (identifierText typeParameter) parameterType bindings,
            parameterType : parameterTypesRev,
            nextState
          )

instantiateConstructorArguments ::
  Map Text ExpressionType ->
  [ConstructorArgumentType] ->
  InferState ->
  ([ExpressionType], InferState)
instantiateConstructorArguments typeParameterBindings argumentTypes initialState =
  foldl' step ([], initialState) argumentTypes
  where
    step (argumentTypesRev, stateAcc) argumentType =
      case argumentType of
        ConstructorArgumentType fieldType ->
          case instantiateDeclarationType typeParameterBindings fieldType of
            Just expressionType ->
              (resolveType stateAcc expressionType : argumentTypesRev, stateAcc)
            Nothing ->
              let (freshArgumentType, nextState) = freshTypeVar stateAcc
               in ( freshArgumentType : argumentTypesRev,
                    addTypeError
                      nextState
                      (mkInvalidConstructorPayloadTypeError "missing structured constructor type-parameter binding")
                  )
        ConstructorArgumentFresh ->
          let (freshArgumentType, nextState) = freshTypeVar stateAcc
           in (freshArgumentType : argumentTypesRev, nextState)
