{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.TypeInference.Pattern
  ( InferredPatternCaseArm (..),
    inferPatternCaseType,
    inferPatternCaseTypeWithResults,
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
    Expr,
    Literal (..),
    Pattern (..),
  )
import Jazz.Compiler.BuiltinCatalog (BuiltinResolutionMode)
import Jazz.Compiler.Name (Name, identifierText)
import Jazz.Compiler.Pattern
  ( commonPatternBinderNames,
    patternBinderNames,
  )
import Jazz.Compiler.TypeInference.Diagnostics
import Jazz.Compiler.TypeInference.Elaboration.Types (InferredExpr (..))
import Jazz.Compiler.TypeInference.Solver
  ( combineIntegerLiteralRanges,
    freshTypeVar,
    integerLiteralRangeFitsNumericType,
    resolveType,
    unifyTypes,
  )
import Jazz.Compiler.TypeInference.State
  ( InferState (..),
    InferenceOutput (..),
    inferErrorCount,
    inferErrorsRev,
    modifyInferenceOutput,
  )
import Jazz.Compiler.TypeInference.Traversal (InferExprFn)
import Jazz.Compiler.TypeInference.Types
  ( ConstructorArgumentType (..),
    ExpressionType (..),
    IntegerLiteralRange (..),
    TypeBinding (..),
    TypeEnv,
    instantiateConstructorFieldType,
  )

data InferredPatternCaseArm
  = InferredPatternCaseArm
      Pattern
      (Maybe InferredExpr)
      (Maybe InferredExpr)
  deriving (Eq, Show)

data PatternCaseArmResult result
  = PatternCaseArmResult
      Pattern
      (Maybe result)
      (Maybe result)

inferPatternCaseType ::
  InferExprFn ->
  BuiltinResolutionMode ->
  TypeEnv ->
  ExpressionType ->
  InferState ->
  [CaseArm] ->
  (Maybe ExpressionType, InferState)
inferPatternCaseType inferExpression builtinMode env scrutineeType initialState caseArms =
  let (expressionType, finalState, _) =
        inferPatternCaseTypeInternal
          ( \builtin childEnv childState childExpr ->
              let (childType, nextState) =
                    inferExpression builtin childEnv childState childExpr
               in (childType, nextState, Nothing)
          )
          builtinMode
          env
          scrutineeType
          initialState
          caseArms
   in (expressionType, finalState)

inferPatternCaseTypeWithResults ::
  (BuiltinResolutionMode -> TypeEnv -> InferState -> Expr -> (InferredExpr, InferState)) ->
  BuiltinResolutionMode ->
  TypeEnv ->
  ExpressionType ->
  InferState ->
  [CaseArm] ->
  (Maybe ExpressionType, InferState, [InferredPatternCaseArm])
inferPatternCaseTypeWithResults inferExpression builtinMode env scrutineeType initialState caseArms =
  let (expressionType, finalState, armResults) =
        inferPatternCaseTypeInternal
          ( \builtin childEnv childState childExpr ->
              let (childResult, nextState) =
                    inferExpression builtin childEnv childState childExpr
               in (inferredExpressionType childResult, nextState, Just childResult)
          )
          builtinMode
          env
          scrutineeType
          initialState
          caseArms
   in (expressionType, finalState, map retainArm armResults)
  where
    retainArm (PatternCaseArmResult pattern maybeGuardResult maybeBodyResult) =
      InferredPatternCaseArm pattern maybeGuardResult maybeBodyResult

inferPatternCaseTypeInternal ::
  (BuiltinResolutionMode -> TypeEnv -> InferState -> Expr -> (Maybe ExpressionType, InferState, Maybe result)) ->
  BuiltinResolutionMode ->
  TypeEnv ->
  ExpressionType ->
  InferState ->
  [CaseArm] ->
  (Maybe ExpressionType, InferState, [PatternCaseArmResult result])
inferPatternCaseTypeInternal inferExpression builtinMode env scrutineeType initialState caseArms =
  let (expressionType, finalState, reversedResults) =
        foldl' step (Nothing, initialState, []) caseArms
   in (expressionType, finalState, reverse reversedResults)
  where
    step (maybeExpectedBodyType, stateAcc, resultsAcc) (CaseArm pattern guardExpr bodyExpr) =
      let (rawPatternTyping, stateAfterPatternCheck) =
            inferPatternType env scrutineeType pattern stateAcc
          (patternTyping, stateAfterPattern) =
            rejectDuplicatePatternBinders pattern rawPatternTyping stateAcc stateAfterPatternCheck
       in if patternSkipsBranchType patternTyping
            then
              ( maybeExpectedBodyType,
                stateAfterPattern,
                PatternCaseArmResult pattern Nothing Nothing : resultsAcc
              )
            else
              let armEnv =
                    extendTypeEnvWithPatternBindings
                      (patternBindings patternTyping)
                      env
                  (stateAfterGuard, maybeGuardResult) =
                    inferCaseGuardType builtinMode armEnv stateAfterPattern guardExpr
                  (maybeBodyType, stateAfterBody, maybeBodyResult) =
                    inferExpression builtinMode armEnv stateAfterGuard bodyExpr
                  nextResults =
                    PatternCaseArmResult pattern maybeGuardResult maybeBodyResult : resultsAcc
               in case (maybeExpectedBodyType, maybeBodyType) of
                    (Nothing, _) ->
                      (fmap (resolveType stateAfterBody) maybeBodyType, stateAfterBody, nextResults)
                    (expectedBodyType, Nothing) ->
                      (expectedBodyType, stateAfterBody, nextResults)
                    (Just inferredExpectedBodyType, Just inferredBodyType) ->
                      case unifyTypes inferredExpectedBodyType inferredBodyType stateAfterBody of
                        Just unifiedState ->
                          (Just (mergedUnifiedType unifiedState inferredExpectedBodyType inferredBodyType), unifiedState, nextResults)
                        Nothing ->
                          ( Just inferredExpectedBodyType,
                            addTypeError
                              stateAfterBody
                              ( mkPatternBranchTypeMismatchError
                                  (resolveType stateAfterBody inferredExpectedBodyType)
                                  (resolveType stateAfterBody inferredBodyType)
                              ),
                            nextResults
                          )

    inferCaseGuardType builtinMode' armEnv stateAcc guardExpr =
      case guardExpr of
        Nothing -> (stateAcc, Nothing)
        Just conditionExpr ->
          let (maybeGuardType, stateAfterGuard, maybeGuardResult) =
                inferExpression builtinMode' armEnv stateAcc conditionExpr
              checkedState =
                case maybeGuardType of
                  Just inferredGuardType ->
                    case unifyTypes inferredGuardType TBoolType stateAfterGuard of
                      Just unifiedState -> unifiedState
                      Nothing ->
                        addTypeError
                          stateAfterGuard
                          (mkCaseGuardTypeError (resolveType stateAfterGuard inferredGuardType))
                  Nothing ->
                    stateAfterGuard
           in (checkedState, maybeGuardResult)

newtype PatternBindings = PatternBindings (Map Name ExpressionType)
  deriving (Eq, Show)

instance Semigroup PatternBindings where
  PatternBindings left <> PatternBindings right =
    PatternBindings (Map.union left right)

instance Monoid PatternBindings where
  mempty = PatternBindings Map.empty

singletonPatternBinding :: Name -> ExpressionType -> PatternBindings
singletonPatternBinding name expressionType =
  PatternBindings (Map.singleton name expressionType)

lookupPatternBinding :: Name -> PatternBindings -> Maybe ExpressionType
lookupPatternBinding name (PatternBindings bindings) = Map.lookup name bindings

insertPatternBinding :: Name -> ExpressionType -> PatternBindings -> PatternBindings
insertPatternBinding name expressionType (PatternBindings bindings) =
  PatternBindings (Map.insert name expressionType bindings)

patternBindingNames :: PatternBindings -> Set Name
patternBindingNames (PatternBindings bindings) = Map.keysSet bindings

extendTypeEnvWithPatternBindings :: PatternBindings -> TypeEnv -> TypeEnv
extendTypeEnvWithPatternBindings (PatternBindings bindings) env =
  Map.foldlWithKey'
    ( \extended name expressionType ->
        Map.insert name (PlainTypeBinding expressionType) extended
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

rejectDuplicatePatternBinders :: Pattern -> PatternTyping -> InferState -> InferState -> (PatternTyping, InferState)
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

patternDuplicateBinderNames :: Pattern -> [Name]
patternDuplicateBinderNames pattern =
  Set.toList duplicates
  where
    (_, duplicates) = collect pattern Set.empty Set.empty

    collect :: Pattern -> Set Name -> Set Name -> (Set Name, Set Name)
    collect candidate seen duplicatesAcc =
      case candidate of
        PVariable name ->
          if Set.member name seen
            then (seen, Set.insert name duplicatesAcc)
            else (Set.insert name seen, duplicatesAcc)
        PWildcard -> (seen, duplicatesAcc)
        PLiteral {} -> (seen, duplicatesAcc)
        PConstructor _ nestedPatterns ->
          collectNested seen duplicatesAcc nestedPatterns
        PList nestedPatterns ->
          collectNested seen duplicatesAcc nestedPatterns
        PConsList headPattern tailPattern ->
          collectNested seen duplicatesAcc [headPattern, tailPattern]
        PTuple nestedPatterns ->
          collectNested seen duplicatesAcc nestedPatterns
        PAs name nestedPattern ->
          let (seenAfterName, duplicatesAfterName) =
                if Set.member name seen
                  then (seen, Set.insert name duplicatesAcc)
                  else (Set.insert name seen, duplicatesAcc)
           in collect nestedPattern seenAfterName duplicatesAfterName
        POr alternatives ->
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

inferPatternType :: TypeEnv -> ExpressionType -> Pattern -> InferState -> (PatternTyping, InferState)
inferPatternType env scrutineeType pattern state =
  case pattern of
    PVariable name ->
      ( mempty
          { patternBindings =
              singletonPatternBinding
                name
                (resolveType state scrutineeType)
          },
        state
      )
    PWildcard -> (mempty, state)
    PLiteral literal ->
      let literalType = literalExpressionType literal
       in case unifyTypes scrutineeType literalType state of
            Just unifiedState -> (mempty, unifiedState)
            Nothing ->
              ( skipBranchPatternTyping,
                addTypeError
                  state
                  ( mkPatternTypeMismatchError
                      (resolveType state scrutineeType)
                      literalType
                  )
              )
    PConstructor constructorName patterns ->
      inferConstructorPatternType env scrutineeType constructorName patterns state
    PList patterns ->
      inferListPatternType env scrutineeType patterns state
    PConsList headPattern tailPattern ->
      inferConsListPatternType env scrutineeType headPattern tailPattern state
    PTuple patterns ->
      inferTuplePatternType env scrutineeType patterns state
    PAs name nestedPattern ->
      let (typing, stateAfterPattern) =
            inferPatternType env scrutineeType nestedPattern state
       in if patternSkipsBranchType typing
            then (typing, stateAfterPattern)
            else
              ( typing
                  { patternBindings =
                      insertPatternBinding
                        name
                        (resolveType stateAfterPattern scrutineeType)
                        (patternBindings typing)
                  },
                stateAfterPattern
              )
    POr alternatives ->
      inferOrPatternType env scrutineeType alternatives state

inferOrPatternType ::
  TypeEnv ->
  ExpressionType ->
  [Pattern] ->
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
                        ( insertPatternBinding
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
                                (resolveType stateForBinder leftType)
                                (resolveType stateForBinder rightType)
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
  PatternBindings (Map.map (resolveType state) bindings)

inferConstructorPatternType ::
  TypeEnv ->
  ExpressionType ->
  Name ->
  [Pattern] ->
  InferState ->
  (PatternTyping, InferState)
inferConstructorPatternType env scrutineeType constructorName patterns state =
  case Map.lookup constructorName env of
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
                            (resolveType stateAfterConstructor scrutineeType)
                            constructorResultType
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
  [Pattern] ->
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
  [Pattern] ->
  InferState ->
  (PatternTyping, InferState)
inferListPatternType env scrutineeType patterns state =
  let (elementType, stateWithElementType) = freshTypeVar state
      listPatternType = TListType elementType
      stateAfterListCheck =
        case unifyTypes scrutineeType listPatternType stateWithElementType of
          Just unifiedState -> unifiedState
          Nothing ->
            addTypeError
              stateWithElementType
              ( mkListPatternTypeMismatchError
                  (resolveType stateWithElementType scrutineeType)
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
  [Pattern] ->
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
  Pattern ->
  Pattern ->
  InferState ->
  (PatternTyping, InferState)
inferConsListPatternType env scrutineeType headPattern tailPattern state =
  let (elementType, stateWithElementType) = freshTypeVar state
      listPatternType = TListType elementType
      stateAfterListCheck =
        case unifyTypes scrutineeType listPatternType stateWithElementType of
          Just unifiedState -> unifiedState
          Nothing ->
            addTypeError
              stateWithElementType
              ( mkListPatternTypeMismatchError
                  (resolveType stateWithElementType scrutineeType)
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
  Pattern ->
  Pattern ->
  InferState ->
  (PatternTyping, InferState)
inferConsListSubpatterns env elementType headPattern tailPattern initialState =
  let (headTyping, stateAfterHeadPattern) =
        inferPatternType env elementType headPattern initialState
   in if patternSkipsBranchType headTyping
        then (headTyping, rollbackSkippedPatternState initialState stateAfterHeadPattern)
        else
          let tailListType = TListType (resolveType stateAfterHeadPattern elementType)
              (tailTyping, stateAfterTailPattern) =
                inferPatternType env tailListType tailPattern stateAfterHeadPattern
              mergedTyping = tailTyping <> headTyping
           in if patternSkipsBranchType mergedTyping
                then (mergedTyping, rollbackSkippedPatternState initialState stateAfterTailPattern)
                else (mergedTyping, stateAfterTailPattern)

inferTuplePatternType ::
  TypeEnv ->
  ExpressionType ->
  [Pattern] ->
  InferState ->
  (PatternTyping, InferState)
inferTuplePatternType env scrutineeType patterns state =
  case resolveType state scrutineeType of
    TTupleType elementTypes
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
          tuplePatternType = TTupleType elementTypes
          stateAfterTupleCheck =
            case unifyTypes scrutineeType tuplePatternType stateWithElementTypes of
              Just unifiedState -> unifiedState
              Nothing ->
                addTypeError
                  stateWithElementTypes
                  (mkTuplePatternTypeMismatchError resolvedScrutineeType)
       in if hasNewPatternError stateWithElementTypes stateAfterTupleCheck
            then (skipBranchPatternTyping, rollbackSkippedPatternState state stateAfterTupleCheck)
            else
              inferConstructorArgumentPatterns
                env
                (map (resolveType stateAfterTupleCheck) elementTypes)
                patterns
                stateAfterTupleCheck
  where
    freshTypeVars count initialState =
      go [] initialState count

    go reversedTypes stateAcc remainingCount
      | remainingCount <= 0 = (reverse reversedTypes, stateAcc)
      | otherwise =
          let (nextType, nextState) = freshTypeVar stateAcc
           in go (nextType : reversedTypes) nextState (remainingCount - 1)

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

literalExpressionType :: Literal -> ExpressionType
literalExpressionType literal =
  case literal of
    LInt value -> TIntegerLiteralType (IntegerLiteralRange value value)
    LFloat _ _ maybeTargetType ->
      maybe TFloatType TNumericType maybeTargetType
    LBool _ -> TBoolType
    LChar _ -> TCharType
    LText _ -> TTextType

instantiateConstructorBinding :: TypeBinding -> InferState -> Maybe ([ExpressionType], ExpressionType, InferState)
instantiateConstructorBinding binding state =
  case binding of
    ConstructorTypeBinding typeName typeParameters argumentTypes ->
      Just (instantiateConstructorType typeName typeParameters argumentTypes state)
    _ -> Nothing

instantiateConstructorType ::
  Name ->
  [Name] ->
  [ConstructorArgumentType] ->
  InferState ->
  ([ExpressionType], ExpressionType, InferState)
instantiateConstructorType typeName typeParameters argumentTypes state =
  let (typeParameterBindings, resultParameterTypes, stateAfterParameters) =
        instantiateConstructorTypeParameters typeParameters state
      (constructorArgumentTypesRev, stateAfterArguments) =
        instantiateConstructorArguments typeParameterBindings argumentTypes stateAfterParameters
   in ( reverse constructorArgumentTypesRev,
        TDataType typeName (reverse resultParameterTypes),
        stateAfterArguments
      )

instantiateConstructorTypeParameters ::
  [Name] ->
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
        ConstructorArgumentMonomorphic expressionType ->
          (resolveType stateAcc expressionType : argumentTypesRev, stateAcc)
        ConstructorArgumentParameter parameterName ->
          case Map.lookup parameterName typeParameterBindings of
            Just parameterType -> (parameterType : argumentTypesRev, stateAcc)
            Nothing ->
              let (freshArgumentType, nextState) = freshTypeVar stateAcc
               in ( freshArgumentType : argumentTypesRev,
                    addTypeError nextState (mkMissingConstructorTypeParameterBindingError parameterName)
                  )
        ConstructorArgumentStructured fieldType ->
          case instantiateConstructorFieldType typeParameterBindings fieldType of
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

mergedUnifiedType :: InferState -> ExpressionType -> ExpressionType -> ExpressionType
mergedUnifiedType state leftType rightType =
  mergeIntegerLiteralRanges (resolveType state leftType) (resolveType state rightType)

mergeIntegerLiteralRanges :: ExpressionType -> ExpressionType -> ExpressionType
mergeIntegerLiteralRanges leftType rightType =
  case (leftType, rightType) of
    (TIntegerLiteralType leftRange, TIntegerLiteralType rightRange) ->
      TIntegerLiteralType (combineIntegerLiteralRanges leftRange rightRange)
    (TIntegerLiteralType literalRange, numericType@(TNumericType concreteNumericType))
      | integerLiteralRangeFitsNumericType literalRange concreteNumericType -> numericType
    (numericType@(TNumericType concreteNumericType), TIntegerLiteralType literalRange)
      | integerLiteralRangeFitsNumericType literalRange concreteNumericType -> numericType
    (TIntegerLiteralType {}, TIntType) -> TIntType
    (TIntType, TIntegerLiteralType {}) -> TIntType
    (TListType leftElementType, TListType rightElementType) ->
      TListType (mergeIntegerLiteralRanges leftElementType rightElementType)
    (TTupleType leftElementTypes, TTupleType rightElementTypes)
      | length leftElementTypes == length rightElementTypes ->
          TTupleType (zipWith mergeIntegerLiteralRanges leftElementTypes rightElementTypes)
    (TDataType leftName leftArguments, TDataType rightName rightArguments)
      | leftName == rightName,
        length leftArguments == length rightArguments ->
          TDataType leftName (zipWith mergeIntegerLiteralRanges leftArguments rightArguments)
    (TFunctionType leftInputType leftOutputType, TFunctionType rightInputType rightOutputType) ->
      TFunctionType
        (mergeIntegerLiteralRanges leftInputType rightInputType)
        (mergeIntegerLiteralRanges leftOutputType rightOutputType)
    _ -> leftType
