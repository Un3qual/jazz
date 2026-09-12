{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Pure runtime value semantics. Evaluator control, callable execution, host
-- effects, continuations, and recursive scope forcing remain in the Runtime
-- façade.
module Jazz.Compiler.Runtime.Semantics
  ( renderRuntimeValue,
    renderRuntimeType,
    runtimeDiagnostic,
    runtimeDefinitionName,
    runtimeDefinitionNameIn,
    qualifyRuntimeType,
    literalRuntimeValue,
    runtimeValueMatchesLiteral,
    applyRuntimeTypeHint,
    applyRuntimeFunctionArgumentHint,
    applyRuntimeFunctionResultHint,
    applyExplicitTypeApplicationResultHint,
    explicitTypeApplicationRuntimeFunctionHint,
    explicitTypeApplicationRuntimeValueHint,
    matchCaseArm,
    isFunctionValue,
    runtimeValueExactlyMatchesConstraint,
    runtimeValueMatchesConstraint,
    runtimeTypesCompatible,
    substituteRuntimeVariable,
    runtimeFunctionArguments,
    runtimeIntMatchesTarget,
    integerValueMatchesTarget,
    runtimeQualifiedMethodIsFullyApplied,
    preferredRuntimeMethodCandidates,
    preferredRuntimeMethodCandidatesForTypeHint,
    applyConstructor,
    evalNumericConversion,
    numericConversionBuiltinForTarget,
    convertIntegerToNumericTarget,
    convertFloatToNumericTarget,
    convertIntegerToFloatTarget,
    integerValueWithinBounds,
    roundFloatTarget,
    exceedsFloatTarget,
    numericConversionFloatOverflowDiagnostic,
    attachDefaultBindingIntegerTarget,
    untypedIntMetadata,
    targetedIntMetadata,
    untypedFloatMetadata,
    targetedFloatMetadata,
  )
where

import Control.Monad (foldM, zipWithM)
import Data.Bifunctor (bimap)
import Data.Char
  ( isControl,
    ord,
    toUpper,
  )
import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CaseArm (..),
    CoreNode (coreNodeFacts),
    CorePhase (..),
    Expr,
    Literal (..),
    Pattern (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    builtinSymbolName,
    numericTypeFloatMax,
    numericTypeIntegerBounds,
    renderNumericTypeName,
  )
import Jazz.Compiler.CoreIdentity (resolvedBinderReference)
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    mkErrorDiagnostic,
  )
import Jazz.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
    fractionalLiteralIntegralValue,
  )
import Jazz.Compiler.Name
  ( Name (..),
    NameNamespace (..),
    ResolvedName,
    ResolvedNameOrigin (..),
    ResolvedUserName (..),
    identifierText,
  )
import Jazz.Compiler.Runtime.Types
  ( RuntimeAnnotation (..),
    RuntimeAppliedArguments,
    RuntimeClosure (..),
    RuntimeConstructorShape,
    RuntimeEnv,
    RuntimeFloatMetadata (..),
    RuntimeIntMetadata (..),
    RuntimeMethodCandidate (..),
    RuntimeMethodCandidates,
    RuntimeValue (..),
    appendRuntimeAppliedArgument,
    attachRuntimeExplicitResultHints,
    constructorApplicationIsSaturated,
    constructorIsSaturated,
    emptyRuntimeAppliedArguments,
    filterRuntimeMethodCandidates,
    foldrRuntimeMethodCandidates,
    prependRuntimeExplicitResultHint,
    runtimeAppliedArgumentCount,
    runtimeAppliedArgumentsInOrder,
    runtimeConstructorArity,
    runtimeConstructorName,
    runtimeConstructorTypeName,
    runtimeConstructorTypeParameters,
    runtimeMethodCandidatesInOrder,
    runtimeMethodIsSelected,
    pattern VQualifiedMethodApplication,
  )
import Jazz.Compiler.SemanticFacts (AnalyzedType, EvidenceReference (evidenceType), PatternFacts (patternResolution))
import Jazz.Compiler.SourceUnitOwnership (SourceUnitOwner (..), sourceUnitOwnerOrigin)
import Jazz.Compiler.TypeRepresentation
  ( InferenceVariable,
    NumericType (..),
    SemanticType (..),
    substituteSemanticVariables,
  )
import Numeric (showHex)

renderRuntimeValue :: RuntimeValue -> Text
renderRuntimeValue value =
  case value of
    VInt intValue _ -> Text.pack (show intValue)
    VFloat floatValue _ -> Text.pack (show floatValue)
    VBool boolValue ->
      if boolValue
        then "True"
        else "False"
    VChar charValue ->
      "'" <> renderQuotedScalar charValue <> "'"
    VText textValue ->
      "\"" <> Text.concatMap renderQuotedScalar textValue <> "\""
    VList elements _ ->
      "[" <> Text.intercalate ", " (map renderRuntimeValue elements) <> "]"
    VTuple elements ->
      "(" <> Text.intercalate ", " (map renderRuntimeValue elements) <> ")"
    VClosure {} -> "<function>"
    VBuiltin _ _ -> "<function>"
    VOperator {} -> "<function>"
    VSectionLeft {} -> "<function>"
    VSectionRight {} -> "<function>"
    VConstructorApplication shape capturedArgs
      | constructorApplicationIsSaturated shape capturedArgs ->
          renderConstructorValue
            (runtimeConstructorName shape)
            (runtimeAppliedArgumentsInOrder capturedArgs)
      | otherwise ->
          "<function>"
    VQualifiedMethodApplication {} -> "<function>"
    VAnnotated _ innerValue -> renderRuntimeValue innerValue
    VDeferredHostBinding {} -> "<deferred-host-binding>"

renderQuotedScalar :: Char -> Text
renderQuotedScalar value =
  case value of
    '\\' -> "\\\\"
    '\'' -> "\\'"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    '\0' -> "\\0"
    _
      | isControl value ->
          "\\u{" <> Text.pack (map toUpper (showHex (ord value) "")) <> "}"
    _ -> Text.singleton value

renderConstructorValue :: ResolvedName -> [RuntimeValue] -> Text
renderConstructorValue constructorName arguments =
  case arguments of
    [] -> renderConstructorName constructorName
    _ ->
      renderConstructorName constructorName
        <> "("
        <> Text.intercalate ", " (map renderRuntimeValue arguments)
        <> ")"

renderConstructorName :: ResolvedName -> Text
renderConstructorName constructorName =
  case constructorName of
    UserName (ResolvedUserName _ ConstructorNamespace identifier) -> identifierText identifier
    _ -> identifierText constructorName

runtimeDefinitionName :: Maybe SourceUnitOwner -> ResolvedName -> ResolvedName
runtimeDefinitionName maybeOwner name =
  case (maybeOwner, name) of
    (Just owner, UserName (ResolvedUserName CurrentModule namespace identifier)) ->
      UserName (ResolvedUserName (sourceUnitOwnerOrigin owner) namespace identifier)
    _ -> name

runtimeDefinitionNameIn :: NameNamespace -> Maybe SourceUnitOwner -> ResolvedName -> ResolvedName
runtimeDefinitionNameIn namespace maybeOwner name =
  case (maybeOwner, name) of
    (Just (PreludeSourceUnit _), UserName (ResolvedUserName CurrentModule _ identifier)) ->
      UserName (ResolvedUserName AmbientPrelude namespace identifier)
    _ -> runtimeDefinitionName maybeOwner name

qualifyRuntimeType :: Maybe SourceUnitOwner -> AnalyzedType -> AnalyzedType
qualifyRuntimeType modulePath = bimap (runtimeDefinitionNameIn TypeNamespace modulePath) id

literalRuntimeValue :: Literal -> RuntimeValue
literalRuntimeValue literal =
  case literal of
    LInt value -> VInt value untypedIntMetadata
    LFloat value literalSource maybeTargetType ->
      case maybeTargetType of
        Just targetType ->
          VFloat
            (roundFloatTarget targetType value)
            (targetedFloatMetadataWithSource targetType (Just literalSource))
        Nothing ->
          VFloat value (untypedFloatMetadata (Just literalSource))
    LBool value -> VBool value
    LChar value -> VChar value
    LText value -> VText value

runtimeValueMatchesLiteral :: RuntimeValue -> Literal -> Bool
runtimeValueMatchesLiteral runtimeValue literal =
  case runtimeValue of
    VAnnotated _ innerValue -> runtimeValueMatchesLiteral innerValue literal
    VInt actual _ -> case literal of LInt expected -> actual == expected; _ -> False
    VFloat actual _ ->
      case literal of
        LFloat expected _ (Just targetType) -> actual == roundFloatTarget targetType expected
        LFloat expected _ Nothing -> actual == expected
        _ -> False
    VBool actual -> case literal of LBool expected -> actual == expected; _ -> False
    VChar actual -> case literal of LChar expected -> actual == expected; _ -> False
    VText actual -> case literal of LText expected -> actual == expected; _ -> False
    _ -> False

applyRuntimeTypeHint :: AnalyzedType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeTypeHint typeHint runtimeValue =
  case runtimeValue of
    VDeferredHostBinding {} -> Right (VAnnotated (RuntimeTypeHint typeHint) runtimeValue)
    VAnnotated (RuntimeTypeHint existingTypeHint) _
      | runtimeTypeHintAtLeastAsSpecific existingTypeHint typeHint ->
          Right runtimeValue
    VAnnotated _ innerValue ->
      applyRuntimeTypeHint typeHint innerValue
    VQualifiedMethodApplication methodKey classParameter methodSignature candidates capturedArgs
      | Foldable.null typeHint ->
          Right
            ( VAnnotated
                (RuntimeTypeHint typeHint)
                ( VQualifiedMethodApplication
                    methodKey
                    classParameter
                    methodSignature
                    ( preferredRuntimeMethodCandidatesForTypeHint
                        typeHint
                        classParameter
                        methodSignature
                        capturedArgs
                        candidates
                    )
                    capturedArgs
                )
            )
    _ ->
      case (typeHint, runtimeValue) of
        (SemanticInt, _) -> do
          convertedValue <- evalNumericConversion (numericConversionBuiltinForTarget NumericInt64) NumericInt64 runtimeValue
          Right (VAnnotated (RuntimeTypeHint SemanticInt) convertedValue)
        (SemanticFloat, _) -> do
          convertedValue <- evalNumericConversion (numericConversionBuiltinForTarget NumericFloat64) NumericFloat64 runtimeValue
          Right (VAnnotated (RuntimeTypeHint SemanticFloat) convertedValue)
        (SemanticNumeric targetType, _) ->
          evalNumericConversion (numericConversionBuiltinForTarget targetType) targetType runtimeValue
        (SemanticBool, VBool {}) -> Right runtimeValue
        (SemanticChar, VChar {}) -> Right runtimeValue
        (SemanticText, VText {}) -> Right runtimeValue
        (SemanticData hintedTypeName [], VConstructor typeName typeParameters constructorName constructorArguments capturedArgs)
          | hintedTypeName == typeName,
            constructorIsSaturated constructorArguments capturedArgs -> do
              hintedCapturedArgs <-
                zipWithM
                  (applyConstructorArgumentRuntimeHint Map.empty)
                  constructorArguments
                  capturedArgs
              Right
                ( VAnnotated
                    (RuntimeTypeHint typeHint)
                    (VConstructor typeName typeParameters constructorName constructorArguments hintedCapturedArgs)
                )
        (SemanticList _, VList _ (Just existingTypeHint))
          | runtimeTypeHintAtLeastAsSpecific existingTypeHint typeHint ->
              Right runtimeValue
        (SemanticList elementType, VList elements _) -> do
          hintedElements <- mapM (applyRuntimeTypeHint elementType) elements
          Right (VList hintedElements (Just typeHint))
        (SemanticTuple elementTypes, VTuple elements)
          | length elementTypes == length elements ->
              VTuple <$> zipWithM applyRuntimeTypeHint elementTypes elements
        (SemanticFunction {}, VClosure closure) ->
          Right
            ( VClosure
                closure
                  { runtimeClosureTypeHint = Just typeHint
                  }
            )
        (SemanticFunction {}, _)
          | isFunctionValue runtimeValue ->
              Right (VAnnotated (RuntimeTypeHint typeHint) runtimeValue)
        (SemanticData hintedTypeName hintedArguments, VConstructor typeName typeParameters constructorName constructorArguments capturedArgs)
          | hintedTypeName == typeName,
            length hintedArguments == length typeParameters -> do
              let typeParameterHints =
                    Map.fromList (zip typeParameters hintedArguments)
              hintedCapturedArgs <-
                zipWithM
                  (applyConstructorArgumentRuntimeHint typeParameterHints)
                  constructorArguments
                  capturedArgs
              Right (VAnnotated (RuntimeTypeHint typeHint) (VConstructor typeName typeParameters constructorName constructorArguments hintedCapturedArgs))
        _ ->
          Right runtimeValue

-- Runtime hints form an information order rather than a replacement order.
-- A concrete value already carrying, for example, @[CanonicalToken]@ also
-- satisfies a later polymorphic @[a]@ result hint. Preserving the stronger
-- evidence avoids both losing concrete dispatch information and repeatedly
-- traversing persistent values at polymorphic function boundaries.
runtimeTypeHintAtLeastAsSpecific :: AnalyzedType -> AnalyzedType -> Bool
runtimeTypeHintAtLeastAsSpecific existingHint requestedHint
  | existingHint == requestedHint = True
runtimeTypeHintAtLeastAsSpecific _ (SemanticVariable _) = True
runtimeTypeHintAtLeastAsSpecific
  (SemanticData existingName existingArguments)
  (SemanticData requestedName requestedArguments) =
    existingName == requestedName
      && length existingArguments == length requestedArguments
      && and (zipWith runtimeTypeHintAtLeastAsSpecific existingArguments requestedArguments)
runtimeTypeHintAtLeastAsSpecific (SemanticList existingElement) (SemanticList requestedElement) =
  runtimeTypeHintAtLeastAsSpecific existingElement requestedElement
runtimeTypeHintAtLeastAsSpecific (SemanticTuple existingElements) (SemanticTuple requestedElements) =
  length existingElements == length requestedElements
    && and (zipWith runtimeTypeHintAtLeastAsSpecific existingElements requestedElements)
runtimeTypeHintAtLeastAsSpecific
  (SemanticFunction existingArgument existingResult)
  (SemanticFunction requestedArgument requestedResult) =
    runtimeTypeHintAtLeastAsSpecific existingArgument requestedArgument
      && runtimeTypeHintAtLeastAsSpecific existingResult requestedResult
runtimeTypeHintAtLeastAsSpecific _ _ = False

applyConstructorArgumentRuntimeHint ::
  Map InferenceVariable AnalyzedType ->
  AnalyzedType ->
  RuntimeValue ->
  Either Diagnostic RuntimeValue
applyConstructorArgumentRuntimeHint typeParameterHints fieldType runtimeValue =
  applyRuntimeTypeHint
    (substituteConstructorFieldType typeParameterHints fieldType)
    runtimeValue

substituteConstructorFieldType :: Map InferenceVariable AnalyzedType -> AnalyzedType -> AnalyzedType
substituteConstructorFieldType replacements =
  substituteSemanticVariables (\variable -> Map.findWithDefault (SemanticVariable variable) variable replacements)

untypedIntMetadata :: RuntimeIntMetadata
untypedIntMetadata =
  RuntimeIntMetadata {runtimeIntTargetType = Nothing}

targetedIntMetadata :: NumericType -> RuntimeIntMetadata
targetedIntMetadata targetType =
  RuntimeIntMetadata {runtimeIntTargetType = Just targetType}

untypedFloatMetadata :: Maybe FractionalLiteralSource -> RuntimeFloatMetadata
untypedFloatMetadata literalSource =
  RuntimeFloatMetadata
    { runtimeFloatLiteralSource = literalSource,
      runtimeFloatTargetType = Nothing
    }

targetedFloatMetadata :: NumericType -> RuntimeFloatMetadata
targetedFloatMetadata targetType =
  targetedFloatMetadataWithSource targetType Nothing

targetedFloatMetadataWithSource :: NumericType -> Maybe FractionalLiteralSource -> RuntimeFloatMetadata
targetedFloatMetadataWithSource targetType literalSource =
  RuntimeFloatMetadata
    { runtimeFloatLiteralSource =
        case targetType of
          NumericFloat64 -> literalSource
          _ -> Nothing,
      runtimeFloatTargetType = Just targetType
    }

-- | Pattern bindings are prepended to the arm environment so they shadow outer
-- runtime bindings only while evaluating the selected arm body.
matchCaseArm ::
  Maybe SourceUnitOwner ->
  RuntimeEnv ->
  RuntimeValue ->
  CaseArm 'Analyzed ->
  Maybe (RuntimeEnv, Maybe (Expr 'Analyzed), Expr 'Analyzed)
matchCaseArm currentModulePath env scrutineeValue (CaseArm _ casePattern guardExpr bodyExpr) =
  case matchPattern currentModulePath scrutineeValue casePattern of
    Just patternBindings ->
      Just (Map.union patternBindings env, guardExpr, bodyExpr)
    Nothing -> Nothing

matchPattern :: Maybe SourceUnitOwner -> RuntimeValue -> Pattern 'Analyzed -> Maybe RuntimeEnv
matchPattern currentModulePath scrutineeValue casePattern =
  case casePattern of
    PWildcard _ -> Just Map.empty
    PVariable node name ->
      Just
        (Map.singleton (resolvedBinderReference (patternResolution (coreNodeFacts node)) name) (Right scrutineeValue))
    PLiteral _ literal
      | runtimeValueMatchesLiteral scrutineeValue literal ->
          Just Map.empty
      | otherwise ->
          Nothing
    PConstructor _ constructorName patterns ->
      case constructorPatternScrutinee scrutineeValue of
        VConstructor _ _ valueConstructorName constructorArguments capturedArgs
          | valueConstructorName == runtimeDefinitionNameIn ConstructorNamespace currentModulePath constructorName,
            constructorIsSaturated constructorArguments capturedArgs ->
              matchPatternList currentModulePath capturedArgs patterns
        _ -> Nothing
    PList _ patterns ->
      case scrutineeValue of
        VList elements _ -> matchPatternList currentModulePath elements patterns
        _ -> Nothing
    PConsList _ headPattern tailPattern ->
      case scrutineeValue of
        VList (headValue : tailValues) maybeTypeHint -> do
          headBindings <- matchPattern currentModulePath headValue headPattern
          tailBindings <- matchPattern currentModulePath (VList tailValues maybeTypeHint) tailPattern
          Just (tailBindings `Map.union` headBindings)
        _ -> Nothing
    PTuple _ patterns ->
      case scrutineeValue of
        VTuple elements -> matchPatternList currentModulePath elements patterns
        _ -> Nothing
    PAs node name nestedPattern -> do
      patternBindings <- matchPattern currentModulePath scrutineeValue nestedPattern
      Just (Map.insert (resolvedBinderReference (patternResolution (coreNodeFacts node)) name) (Right scrutineeValue) patternBindings)
    POr _ alternatives ->
      matchFirstAlternative currentModulePath scrutineeValue alternatives

matchFirstAlternative :: Maybe SourceUnitOwner -> RuntimeValue -> [Pattern 'Analyzed] -> Maybe RuntimeEnv
matchFirstAlternative currentModulePath scrutineeValue alternatives =
  case alternatives of
    [] -> Nothing
    alternative : rest ->
      case matchPattern currentModulePath scrutineeValue alternative of
        Just patternBindings -> Just patternBindings
        Nothing -> matchFirstAlternative currentModulePath scrutineeValue rest

matchPatternList :: Maybe SourceUnitOwner -> [RuntimeValue] -> [Pattern 'Analyzed] -> Maybe RuntimeEnv
matchPatternList currentModulePath = go Map.empty
  where
    go bindings [] [] = Just bindings
    go bindings (value : values) (elementPattern : patterns) =
      case matchPattern currentModulePath value elementPattern of
        Just patternBindings -> go (patternBindings `Map.union` bindings) values patterns
        Nothing -> Nothing
    go _ _ _ = Nothing

constructorPatternScrutinee :: RuntimeValue -> RuntimeValue
constructorPatternScrutinee runtimeValue =
  case runtimeValue of
    VAnnotated _ innerValue -> constructorPatternScrutinee innerValue
    _ -> runtimeValue

applyRuntimeFunctionResultHint :: AnalyzedType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeFunctionResultHint typeHint runtimeValue =
  case typeHint of
    SemanticFunction _ resultType ->
      applyRuntimeTypeHint resultType runtimeValue
    _ ->
      Right runtimeValue

applyRuntimeFunctionArgumentHint :: AnalyzedType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeFunctionArgumentHint typeHint runtimeValue =
  case typeHint of
    SemanticFunction argumentType _ ->
      applyRuntimeTypeHint argumentType runtimeValue
    _ ->
      Right runtimeValue

applyExplicitTypeApplicationResultHint :: AnalyzedType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyExplicitTypeApplicationResultHint typeHint runtimeValue
  | isFunctionValue runtimeValue =
      Right (prependRuntimeExplicitResultHint typeHint runtimeValue)
  | runtimeValueCanAcceptTypeHint typeHint runtimeValue =
      applyRuntimeTypeHint typeHint runtimeValue
  | otherwise =
      Right runtimeValue

runtimeValueCanAcceptTypeHint :: AnalyzedType -> RuntimeValue -> Bool
runtimeValueCanAcceptTypeHint typeHint runtimeValue =
  case runtimeValue of
    VAnnotated _ innerValue ->
      runtimeValueCanAcceptTypeHint typeHint innerValue
    _ ->
      case (typeHint, runtimeValue) of
        (SemanticInt, VInt {}) -> True
        (SemanticFloat, VFloat {}) -> True
        (SemanticNumeric _, VInt {}) -> True
        (SemanticNumeric _, VFloat {}) -> True
        (SemanticBool, VBool {}) -> True
        (SemanticChar, VChar {}) -> True
        (SemanticText, VText {}) -> True
        (SemanticData typeName [], VConstructorApplication shape capturedArgs) ->
          identifierText typeName == identifierText (runtimeConstructorTypeName shape)
            && constructorApplicationIsSaturated shape capturedArgs
        (SemanticData typeName arguments, VConstructorApplication shape capturedArgs) ->
          identifierText typeName == identifierText (runtimeConstructorTypeName shape)
            && length arguments == length (runtimeConstructorTypeParameters shape)
            && constructorApplicationIsSaturated shape capturedArgs
        (SemanticList {}, VList {}) ->
          True
        (SemanticTuple elementTypes, VTuple elements) ->
          length elementTypes == length elements
        (SemanticFunction {}, _) ->
          isFunctionValue runtimeValue
        _ ->
          False

explicitTypeApplicationRuntimeFunctionHint :: AnalyzedType -> RuntimeValue -> Maybe AnalyzedType
explicitTypeApplicationRuntimeFunctionHint typeHint runtimeValue = do
  explicitTypeApplicationRuntimeTemplateHint typeHint runtimeValue

explicitTypeApplicationRuntimeValueHint :: AnalyzedType -> RuntimeValue -> Maybe AnalyzedType
explicitTypeApplicationRuntimeValueHint typeHint runtimeValue =
  case explicitTypeApplicationRuntimeTemplateHint typeHint runtimeValue of
    Just instantiatedTemplate -> Just instantiatedTemplate
    Nothing -> explicitTypeApplicationRuntimeShapeHint typeHint runtimeValue

explicitTypeApplicationRuntimeTemplateHint :: AnalyzedType -> RuntimeValue -> Maybe AnalyzedType
explicitTypeApplicationRuntimeTemplateHint typeHint runtimeValue = do
  templateHint <- runtimeValueTypeHint runtimeValue
  variableName <- listToMaybe (Foldable.toList templateHint)
  pure (substituteRuntimeVariable variableName typeHint templateHint)

explicitTypeApplicationRuntimeShapeHint :: AnalyzedType -> RuntimeValue -> Maybe AnalyzedType
explicitTypeApplicationRuntimeShapeHint typeHint runtimeValue =
  case runtimeValue of
    VAnnotated _ innerValue ->
      explicitTypeApplicationRuntimeShapeHint typeHint innerValue
    VList {} ->
      Just (SemanticList typeHint)
    VConstructorApplication shape capturedArgs
      | [_] <- runtimeConstructorTypeParameters shape,
        constructorApplicationIsSaturated shape capturedArgs ->
          Just (SemanticData (runtimeConstructorTypeName shape) [typeHint])
    _ -> Nothing

runtimeValueTypeHint :: RuntimeValue -> Maybe AnalyzedType
runtimeValueTypeHint runtimeValue =
  case runtimeValue of
    VAnnotated (RuntimeTypeHint typeHint) _ ->
      Just typeHint
    VAnnotated (RuntimeTypeApplication _) innerValue ->
      runtimeValueTypeHint innerValue
    VAnnotated (RuntimeResultHints _) innerValue ->
      runtimeValueTypeHint innerValue
    VClosure closure ->
      runtimeClosureTypeHint closure
    VList _ (Just typeHint) ->
      Just typeHint
    _ -> Nothing

substituteRuntimeVariable :: InferenceVariable -> AnalyzedType -> AnalyzedType -> AnalyzedType
substituteRuntimeVariable variable replacement =
  substituteSemanticVariables (\current -> if current == variable then replacement else SemanticVariable current)

runtimeFunctionArguments :: AnalyzedType -> ([AnalyzedType], AnalyzedType)
runtimeFunctionArguments (SemanticFunction argument result) =
  let (arguments, resultType) = runtimeFunctionArguments result
   in (argument : arguments, resultType)
runtimeFunctionArguments resultType = ([], resultType)

runtimeTypesCompatible :: AnalyzedType -> AnalyzedType -> Bool
runtimeTypesCompatible left right
  | left == right = True
runtimeTypesCompatible SemanticInt (SemanticNumeric NumericInt64) = True
runtimeTypesCompatible (SemanticNumeric NumericInt64) SemanticInt = True
runtimeTypesCompatible SemanticFloat (SemanticNumeric NumericFloat64) = True
runtimeTypesCompatible (SemanticNumeric NumericFloat64) SemanticFloat = True
runtimeTypesCompatible (SemanticList left) (SemanticList right) = runtimeTypesCompatible left right
runtimeTypesCompatible (SemanticTuple left) (SemanticTuple right) = compatibleElements left right
runtimeTypesCompatible (SemanticData leftName left) (SemanticData rightName right) = leftName == rightName && compatibleElements left right
runtimeTypesCompatible (SemanticFunction leftArgument leftResult) (SemanticFunction rightArgument rightResult) =
  runtimeTypesCompatible leftArgument rightArgument && runtimeTypesCompatible leftResult rightResult
runtimeTypesCompatible _ _ = False

compatibleElements :: [AnalyzedType] -> [AnalyzedType] -> Bool
compatibleElements left right = length left == length right && and (zipWith runtimeTypesCompatible left right)

runtimeQualifiedMethodIsFullyApplied ::
  InferenceVariable ->
  AnalyzedType ->
  RuntimeAppliedArguments ->
  RuntimeMethodCandidates ->
  Bool
runtimeQualifiedMethodIsFullyApplied classParameter methodSignature arguments candidates =
  foldrRuntimeMethodCandidates
    (\candidate fullyApplied -> candidateIsFullyApplied candidate || fullyApplied)
    False
    candidates
  where
    candidateIsFullyApplied (RuntimeMethodCandidate evidence _) =
      let substitutedSignature = substituteRuntimeVariable classParameter (evidenceType evidence) methodSignature
          (argumentTypes, _) = runtimeFunctionArguments substitutedSignature
       in runtimeAppliedArgumentCount arguments >= length argumentTypes

runtimeMethodCandidateExactlyMatches :: InferenceVariable -> AnalyzedType -> [RuntimeValue] -> RuntimeMethodCandidate -> Bool
runtimeMethodCandidateExactlyMatches classParameter methodSignature arguments (RuntimeMethodCandidate evidence _) =
  let substitutedSignature = substituteRuntimeVariable classParameter implTarget methodSignature
      (genericArgumentTypes, _) = runtimeFunctionArguments methodSignature
      (argumentTypes, _) = runtimeFunctionArguments substitutedSignature
      suppliedArgumentCount = length arguments
      suppliedGenericArgumentTypes = take suppliedArgumentCount genericArgumentTypes
      suppliedArgumentTypes = take suppliedArgumentCount argumentTypes
      targetArgumentPositions =
        map (Foldable.elem classParameter) suppliedGenericArgumentTypes
   in suppliedArgumentCount <= length genericArgumentTypes
        && suppliedArgumentCount <= length argumentTypes
        && or targetArgumentPositions
        && and
          ( zipWith3
              runtimeExactCandidateArgumentMatches
              targetArgumentPositions
              suppliedArgumentTypes
              arguments
          )
  where
    implTarget = evidenceType evidence

runtimeExactCandidateArgumentMatches :: Bool -> AnalyzedType -> RuntimeValue -> Bool
runtimeExactCandidateArgumentMatches targetArgumentPosition signatureType runtimeValue =
  not targetArgumentPosition || runtimeValueExactlyMatchesConstraint signatureType runtimeValue

runtimeValueExactlyMatchesConstraint :: AnalyzedType -> RuntimeValue -> Bool
runtimeValueExactlyMatchesConstraint signatureType runtimeValue =
  case runtimeValue of
    VAnnotated (RuntimeTypeApplication _) innerValue ->
      runtimeValueExactlyMatchesConstraint signatureType innerValue
    VAnnotated (RuntimeResultHints _) innerValue ->
      runtimeValueExactlyMatchesConstraint signatureType innerValue
    VAnnotated (RuntimeTypeHint typeHint) _ ->
      typeHint == signatureType
    VClosure closure ->
      runtimeClosureTypeHint closure == Just signatureType
    VInt _ metadata ->
      case signatureType of
        SemanticInt -> runtimeIntTargetType metadata == Nothing
        SemanticNumeric numericType -> runtimeIntTargetType metadata == Just numericType
        _ -> False
    VFloat _ metadata ->
      case signatureType of
        SemanticFloat -> runtimeFloatTargetType metadata == Nothing
        SemanticNumeric numericType -> runtimeFloatTargetType metadata == Just numericType
        _ -> False
    VChar {} ->
      case signatureType of
        SemanticChar -> True
        _ -> False
    VText {} ->
      case signatureType of
        SemanticText -> True
        _ -> False
    VBool {} ->
      case signatureType of
        SemanticBool -> True
        _ -> False
    VList _ (Just typeHint) ->
      typeHint == signatureType
    VList elements Nothing ->
      case signatureType of
        SemanticList elementType ->
          not (null elements)
            && all (runtimeValueExactlyMatchesConstraint elementType) elements
        _ -> False
    VTuple elements ->
      case signatureType of
        SemanticTuple elementTypes
          | length elementTypes == length elements ->
              and (zipWith runtimeValueExactlyMatchesConstraint elementTypes elements)
        _ -> False
    VConstructorApplication {} ->
      case signatureType of
        SemanticData typeName [] ->
          runtimeValueExactlyMatchesDataTypeName typeName runtimeValue
        SemanticData typeName typeArguments ->
          runtimeValueExactlyMatchesDataTypeApplication typeName typeArguments runtimeValue
        _ -> False
    _ -> False

runtimeMethodCandidateMatches :: InferenceVariable -> AnalyzedType -> [RuntimeValue] -> RuntimeMethodCandidate -> Bool
runtimeMethodCandidateMatches classParameter methodSignature arguments (RuntimeMethodCandidate evidence _) =
  let substitutedSignature = substituteRuntimeVariable classParameter implTarget methodSignature
      (argumentTypes, _) = runtimeFunctionArguments substitutedSignature
   in length arguments <= length argumentTypes
        && and (zipWith runtimeValueMatchesConstraint argumentTypes arguments)
  where
    implTarget = evidenceType evidence

runtimeValueMatchesConstraint :: AnalyzedType -> RuntimeValue -> Bool
runtimeValueMatchesConstraint signatureType runtimeValue =
  case runtimeValue of
    VAnnotated (RuntimeTypeApplication _) innerValue ->
      runtimeValueMatchesConstraint signatureType innerValue
    VAnnotated (RuntimeResultHints _) innerValue ->
      runtimeValueMatchesConstraint signatureType innerValue
    VAnnotated (RuntimeTypeHint typeHint) _ ->
      runtimeTypesCompatible typeHint signatureType
    _ ->
      case signatureType of
        SemanticInt -> runtimeIntMatchesIntAlias runtimeValue
        SemanticFloat -> runtimeFloatMatchesFloatAlias runtimeValue
        SemanticNumeric numericType
          | Just _ <- numericTypeIntegerBounds numericType -> runtimeIntMatchesTarget numericType runtimeValue
          | otherwise -> runtimeFloatHasTarget numericType runtimeValue
        SemanticBool -> isRuntimeBool runtimeValue
        SemanticChar -> isRuntimeChar runtimeValue
        SemanticText -> isRuntimeText runtimeValue
        SemanticVariable {} -> False
        SemanticData typeName [] ->
          runtimeValueExactlyMatchesDataTypeName typeName runtimeValue
        SemanticData typeName typeArguments ->
          runtimeValueMatchesDataTypeApplication typeName typeArguments runtimeValue
        SemanticList elementType ->
          case runtimeValue of
            VList elements maybeTypeHint ->
              case maybeTypeHint of
                Just typeHint -> runtimeTypesCompatible typeHint signatureType
                Nothing -> all (runtimeValueMatchesConstraint elementType) elements
            _ -> False
        SemanticTuple elementTypes ->
          case runtimeValue of
            VTuple elements
              | length elementTypes == length elements ->
                  and (zipWith runtimeValueMatchesConstraint elementTypes elements)
            _ -> False
        SemanticFunction {} ->
          case runtimeValue of
            VClosure closure ->
              case runtimeClosureTypeHint closure of
                Just typeHint -> runtimeTypesCompatible typeHint signatureType
                Nothing -> True
            _ -> isFunctionValue runtimeValue

runtimeValueMatchesDataTypeApplication :: ResolvedName -> [AnalyzedType] -> RuntimeValue -> Bool
runtimeValueMatchesDataTypeApplication typeName typeArguments runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName typeParameters _ constructorArguments capturedArgs
      | valueTypeName == typeName,
        length typeParameters == length typeArguments,
        constructorIsSaturated constructorArguments capturedArgs ->
          let typeParameterBindings = Map.fromList (zip typeParameters typeArguments)
           in and
                ( zipWith
                    (runtimeValueMatchesConstructorArgument typeParameterBindings)
                    constructorArguments
                    capturedArgs
                )
    _ -> False

runtimeValueExactlyMatchesDataTypeName :: ResolvedName -> RuntimeValue -> Bool
runtimeValueExactlyMatchesDataTypeName typeName runtimeValue =
  case runtimeValue of
    VConstructorApplication shape capturedArgs ->
      runtimeConstructorTypeName shape == typeName
        && constructorApplicationIsSaturated shape capturedArgs
    _ -> False

runtimeValueExactlyMatchesDataTypeApplication :: ResolvedName -> [AnalyzedType] -> RuntimeValue -> Bool
runtimeValueExactlyMatchesDataTypeApplication typeName typeArguments runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName typeParameters _ constructorArguments capturedArgs
      | valueTypeName == typeName,
        length typeParameters == length typeArguments,
        constructorIsSaturated constructorArguments capturedArgs ->
          let typeParameterBindings = Map.fromList (zip typeParameters typeArguments)
           in and
                ( zipWith
                    (runtimeValueExactlyMatchesConstructorArgument typeParameterBindings)
                    constructorArguments
                    capturedArgs
                )
    _ -> False

runtimeValueMatchesConstructorArgument :: Map InferenceVariable AnalyzedType -> AnalyzedType -> RuntimeValue -> Bool
runtimeValueMatchesConstructorArgument typeParameterBindings fieldType runtimeValue =
  runtimeValueMatchesConstraint
    (substituteConstructorFieldType typeParameterBindings fieldType)
    runtimeValue

runtimeValueExactlyMatchesConstructorArgument :: Map InferenceVariable AnalyzedType -> AnalyzedType -> RuntimeValue -> Bool
runtimeValueExactlyMatchesConstructorArgument typeParameterBindings fieldType runtimeValue =
  runtimeValueExactlyMatchesConstraint
    (substituteConstructorFieldType typeParameterBindings fieldType)
    runtimeValue

runtimeIntMatchesIntAlias :: RuntimeValue -> Bool
runtimeIntMatchesIntAlias runtimeValue =
  case runtimeValue of
    VInt _ metadata ->
      case runtimeIntTargetType metadata of
        Just NumericInt64 -> True
        Just _ -> False
        Nothing -> True
    _ -> False

runtimeIntMatchesTarget :: NumericType -> RuntimeValue -> Bool
runtimeIntMatchesTarget targetType runtimeValue =
  case runtimeValue of
    VInt integerValue metadata ->
      case runtimeIntTargetType metadata of
        Just runtimeTarget -> runtimeTarget == targetType
        Nothing -> integerValueMatchesTarget targetType integerValue
    _ -> False

integerValueMatchesTarget :: NumericType -> Integer -> Bool
integerValueMatchesTarget targetType integerValue =
  case numericTypeIntegerBounds targetType of
    Just bounds -> integerValueWithinBounds integerValue bounds
    Nothing -> False

runtimeFloatMatchesFloatAlias :: RuntimeValue -> Bool
runtimeFloatMatchesFloatAlias runtimeValue =
  case runtimeValue of
    VFloat _ metadata ->
      case runtimeFloatTargetType metadata of
        Just NumericFloat64 -> True
        Just _ -> False
        Nothing -> True
    _ -> False

runtimeFloatHasTarget :: NumericType -> RuntimeValue -> Bool
runtimeFloatHasTarget targetType runtimeValue =
  case runtimeValue of
    VFloat _ metadata ->
      case runtimeFloatTargetType metadata of
        Just runtimeTarget -> runtimeTarget == targetType
        Nothing -> targetType == NumericFloat64
    _ -> False

isRuntimeBool :: RuntimeValue -> Bool
isRuntimeBool runtimeValue =
  case runtimeValue of
    VBool {} -> True
    _ -> False

isRuntimeChar :: RuntimeValue -> Bool
isRuntimeChar runtimeValue =
  case runtimeValue of
    VChar {} -> True
    _ -> False

isRuntimeText :: RuntimeValue -> Bool
isRuntimeText runtimeValue =
  case runtimeValue of
    VText {} -> True
    _ -> False

-- | Constructor values are curried like builtins until their declared arity is
-- saturated; extra applications are runtime errors.
applyConstructor :: RuntimeConstructorShape -> RuntimeAppliedArguments -> Either Diagnostic RuntimeValue
applyConstructor shape arguments
  | receivedArity <= expectedArity =
      Right (VConstructorApplication shape arguments)
  | otherwise =
      Left
        ( runtimeDiagnostic
            E3023
            ( "runtime constructor '"
                <> identifierText (runtimeConstructorName shape)
                <> "' expected "
                <> renderArityCount expectedArity
                <> " but received "
                <> renderArityCount receivedArity
            )
        )
  where
    expectedArity = runtimeConstructorArity shape
    receivedArity = runtimeAppliedArgumentCount arguments

renderArityCount :: Int -> Text
renderArityCount count =
  Text.pack (show count) <> " " <> argumentWord
  where
    argumentWord =
      if count == 1
        then "argument"
        else "arguments"

evalNumericConversion :: BuiltinSymbol -> NumericType -> RuntimeValue -> Either Diagnostic RuntimeValue
evalNumericConversion builtinFunction targetType value =
  case value of
    VAnnotated _ innerValue ->
      evalNumericConversion builtinFunction targetType innerValue
    VInt integerValue _ ->
      convertIntegerToNumericTarget builtinFunction targetType integerValue
    VFloat floatValue floatMetadata ->
      convertFloatToNumericTarget builtinFunction targetType floatValue (runtimeFloatLiteralSource floatMetadata)
    other ->
      Left
        ( runtimeDiagnostic
            E3024
            ( "runtime numeric conversion '"
                <> builtinSymbolName builtinFunction
                <> "' expects a numeric value, found "
                <> renderRuntimeType other
            )
        )

numericConversionBuiltinForTarget :: NumericType -> BuiltinSymbol
numericConversionBuiltinForTarget targetType =
  case targetType of
    NumericInt8 -> BuiltinToInt8
    NumericInt16 -> BuiltinToInt16
    NumericInt32 -> BuiltinToInt32
    NumericInt64 -> BuiltinToInt64
    NumericUInt8 -> BuiltinToUInt8
    NumericUInt16 -> BuiltinToUInt16
    NumericUInt32 -> BuiltinToUInt32
    NumericUInt64 -> BuiltinToUInt64
    NumericFloat16 -> BuiltinToFloat16
    NumericFloat32 -> BuiltinToFloat32
    NumericFloat64 -> BuiltinToFloat64

convertIntegerToNumericTarget :: BuiltinSymbol -> NumericType -> Integer -> Either Diagnostic RuntimeValue
convertIntegerToNumericTarget builtinFunction targetType integerValue =
  case numericTypeIntegerBounds targetType of
    Just bounds ->
      if integerValueWithinBounds integerValue bounds
        then Right (VInt integerValue (targetedIntMetadata targetType))
        else Left (numericConversionRangeDiagnostic builtinFunction targetType integerValue bounds)
    Nothing ->
      convertIntegerToFloatTarget builtinFunction targetType integerValue

convertFloatToNumericTarget :: BuiltinSymbol -> NumericType -> Double -> Maybe FractionalLiteralSource -> Either Diagnostic RuntimeValue
convertFloatToNumericTarget builtinFunction targetType floatValue literalSource
  | isNaN floatValue || isInfinite floatValue =
      Left
        ( runtimeDiagnostic
            E3024
            ( "runtime numeric conversion '"
                <> builtinSymbolName builtinFunction
                <> "' cannot convert non-finite Float value"
            )
        )
  | otherwise =
      case numericTypeIntegerBounds targetType of
        Just bounds ->
          convertFloatToIntegerTarget builtinFunction targetType floatValue literalSource bounds
        Nothing ->
          convertFiniteFloatToFloatTarget builtinFunction targetType floatValue literalSource

convertFloatToIntegerTarget ::
  BuiltinSymbol ->
  NumericType ->
  Double ->
  Maybe FractionalLiteralSource ->
  (Integer, Integer) ->
  Either Diagnostic RuntimeValue
convertFloatToIntegerTarget builtinFunction targetType floatValue literalSource bounds =
  case literalSource of
    Just source ->
      case fractionalLiteralIntegralValue source of
        Just integralValue
          | integerValueWithinBounds integralValue bounds ->
              Right (VInt integralValue (targetedIntMetadata targetType))
        _ ->
          Left (numericConversionFloatToIntegralDiagnostic builtinFunction targetType floatValue bounds)
    Nothing ->
      -- `round` is half-to-even, but the equality check below rejects every
      -- non-integral value instead of observing a rounding mode.
      let roundedInteger = round floatValue :: Integer
       in if fromInteger roundedInteger == floatValue && integerValueWithinBounds roundedInteger bounds
            then Right (VInt roundedInteger (targetedIntMetadata targetType))
            else Left (numericConversionFloatToIntegralDiagnostic builtinFunction targetType floatValue bounds)

convertIntegerToFloatTarget :: BuiltinSymbol -> NumericType -> Integer -> Either Diagnostic RuntimeValue
convertIntegerToFloatTarget builtinFunction targetType integerValue =
  if integerExceedsFloatTarget targetType integerValue
    then Left (numericConversionFloatOverflowDiagnostic builtinFunction targetType)
    else
      let floatValue = fromInteger integerValue :: Double
       in if isInfinite floatValue || exceedsFloatTarget targetType floatValue
            then Left (numericConversionFloatOverflowDiagnostic builtinFunction targetType)
            else Right (VFloat (roundFloatTarget targetType floatValue) (targetedFloatMetadata targetType))

integerExceedsFloatTarget :: NumericType -> Integer -> Bool
integerExceedsFloatTarget targetType integerValue =
  case numericTypeFloatMax targetType of
    Just maxMagnitude ->
      abs integerValue > (floor maxMagnitude :: Integer)
    Nothing -> False

convertFiniteFloatToFloatTarget :: BuiltinSymbol -> NumericType -> Double -> Maybe FractionalLiteralSource -> Either Diagnostic RuntimeValue
convertFiniteFloatToFloatTarget builtinFunction targetType floatValue literalSource =
  if exceedsFloatTarget targetType floatValue || sourceExceedsFloatTarget targetType literalSource
    then Left (numericConversionFloatOverflowDiagnostic builtinFunction targetType)
    else Right (VFloat (roundFloatTarget targetType floatValue) (targetedFloatMetadataWithSource targetType literalSource))

roundFloatTarget :: NumericType -> Double -> Double
roundFloatTarget targetType value =
  case targetType of
    NumericFloat16 -> roundFloat16 value
    NumericFloat32 -> realToFrac (realToFrac value :: Float)
    _ -> value

roundFloat16 :: Double -> Double
roundFloat16 value
  | value == 0 = 0
  | magnitude < (halfMinSubnormal / 2.0) = 0
  | magnitude < halfMinNormal =
      withSign (fromInteger (round (magnitude / halfMinSubnormal) :: Integer) * halfMinSubnormal)
  | otherwise =
      let exponentValue = floor (logBase 2 magnitude) :: Int
          unit = 2.0 ** fromIntegral (exponentValue - 10)
          roundedMagnitude = fromInteger (round (magnitude / unit) :: Integer) * unit
       in withSign (min float16MaxFinite roundedMagnitude)
  where
    magnitude = abs value
    float16MaxFinite = 65504.0 :: Double
    halfMinNormal = 2.0 ** (-14.0 :: Double)
    halfMinSubnormal = 2.0 ** (-24.0 :: Double)
    withSign roundedMagnitude =
      if value < 0
        then negate roundedMagnitude
        else roundedMagnitude

exceedsFloatTarget :: NumericType -> Double -> Bool
exceedsFloatTarget targetType value =
  case numericTypeFloatMax targetType of
    Just maxMagnitude -> abs value > maxMagnitude
    Nothing -> False

sourceExceedsFloatTarget :: NumericType -> Maybe FractionalLiteralSource -> Bool
sourceExceedsFloatTarget targetType literalSource =
  case (numericTypeFloatMax targetType, literalSource) of
    (Just maxMagnitude, Just source) ->
      fractionalLiteralExceedsMagnitude source maxMagnitude
    _ -> False

integerValueWithinBounds :: Integer -> (Integer, Integer) -> Bool
integerValueWithinBounds value (lowerBound, upperBound) =
  value >= lowerBound && value <= upperBound

numericConversionRangeDiagnostic :: BuiltinSymbol -> NumericType -> Integer -> (Integer, Integer) -> Diagnostic
numericConversionRangeDiagnostic builtinFunction targetType value (lowerBound, upperBound) =
  runtimeDiagnostic
    E3024
    ( "runtime numeric conversion '"
        <> builtinSymbolName builtinFunction
        <> "' failed: integer value "
        <> Text.pack (show value)
        <> " outside "
        <> renderNumericTypeName targetType
        <> " range "
        <> Text.pack (show lowerBound)
        <> ".."
        <> Text.pack (show upperBound)
    )

numericConversionFloatToIntegralDiagnostic :: BuiltinSymbol -> NumericType -> Double -> (Integer, Integer) -> Diagnostic
numericConversionFloatToIntegralDiagnostic builtinFunction targetType value (lowerBound, upperBound) =
  runtimeDiagnostic
    E3024
    ( "runtime numeric conversion '"
        <> builtinSymbolName builtinFunction
        <> "' failed: Float value "
        <> Text.pack (show value)
        <> " must be integral and inside "
        <> renderNumericTypeName targetType
        <> " range "
        <> Text.pack (show lowerBound)
        <> ".."
        <> Text.pack (show upperBound)
    )

numericConversionFloatOverflowDiagnostic :: BuiltinSymbol -> NumericType -> Diagnostic
numericConversionFloatOverflowDiagnostic builtinFunction targetType =
  runtimeDiagnostic
    E3024
    ( "runtime numeric conversion '"
        <> builtinSymbolName builtinFunction
        <> "' failed: value cannot be represented as finite "
        <> renderNumericTypeName targetType
    )

attachDefaultBindingIntegerTarget :: RuntimeValue -> Either Diagnostic RuntimeValue
attachDefaultBindingIntegerTarget runtimeValue =
  case runtimeValue of
    VInt integerValue metadata
      | runtimeIntTargetType metadata == Nothing,
        integerValueMatchesTarget NumericInt64 integerValue ->
          Right (VInt integerValue (targetedIntMetadata NumericInt64))
    VList elements maybeTypeHint ->
      (`VList` maybeTypeHint) <$> traverse attachDefaultBindingIntegerTarget elements
    VTuple elements ->
      VTuple <$> traverse attachDefaultBindingIntegerTarget elements
    VBuiltin builtinSymbol capturedArgs ->
      VBuiltin builtinSymbol <$> traverse attachDefaultBindingIntegerTarget capturedArgs
    VOperator operatorSymbol capturedArgs ->
      VOperator operatorSymbol <$> traverse attachDefaultBindingIntegerTarget capturedArgs
    VSectionLeft operatorSymbol operand ->
      VSectionLeft operatorSymbol <$> attachDefaultBindingIntegerTarget operand
    VSectionRight operatorSymbol operand ->
      VSectionRight operatorSymbol <$> attachDefaultBindingIntegerTarget operand
    VConstructor typeName typeParameters constructorName constructorArguments capturedArgs ->
      VConstructor typeName typeParameters constructorName constructorArguments
        <$> traverse attachDefaultBindingIntegerTarget capturedArgs
    VQualifiedMethodApplication methodKey classParameter methodSignature candidates capturedArgs ->
      VQualifiedMethodApplication methodKey classParameter methodSignature candidates
        <$> foldM appendConvertedArgument emptyRuntimeAppliedArguments (runtimeAppliedArgumentsInOrder capturedArgs)
    VAnnotated (RuntimeTypeHint typeHint) innerValue
      | SemanticFunction {} <- typeHint ->
          Right (VAnnotated (RuntimeTypeHint typeHint) innerValue)
      | otherwise ->
          VAnnotated (RuntimeTypeHint typeHint) <$> attachDefaultBindingIntegerTarget innerValue
    VAnnotated (RuntimeTypeApplication typeHint) innerValue ->
      VAnnotated (RuntimeTypeApplication typeHint) <$> attachDefaultBindingIntegerTarget innerValue
    VAnnotated (RuntimeResultHints hints) innerValue ->
      attachRuntimeExplicitResultHints hints <$> attachDefaultBindingIntegerTarget innerValue
    _ ->
      Right runtimeValue
  where
    appendConvertedArgument arguments argumentValue =
      (`appendRuntimeAppliedArgument` arguments)
        <$> attachDefaultBindingIntegerTarget argumentValue

isFunctionValue :: RuntimeValue -> Bool
isFunctionValue value =
  case value of
    VAnnotated _ innerValue -> isFunctionValue innerValue
    VSectionLeft {} -> True
    VSectionRight {} -> True
    VClosure {} -> True
    VBuiltin {} -> True
    VOperator {} -> True
    VConstructorApplication shape capturedArgs ->
      not (constructorApplicationIsSaturated shape capturedArgs)
    VQualifiedMethodApplication {} -> True
    _ -> False

preferredRuntimeMethodCandidates ::
  InferenceVariable ->
  AnalyzedType ->
  RuntimeAppliedArguments ->
  RuntimeMethodCandidates ->
  RuntimeMethodCandidates
preferredRuntimeMethodCandidates _ _ _ candidates | runtimeMethodIsSelected candidates = candidates
preferredRuntimeMethodCandidates classParameter methodSignature arguments candidates =
  case runtimeMethodCandidatesInOrder exactMatchingCandidates of
    [] -> matchingCandidates
    _ -> exactMatchingCandidates
  where
    argumentsInOrder = runtimeAppliedArgumentsInOrder arguments
    exactMatchingCandidates =
      filterRuntimeMethodCandidates
        (runtimeMethodCandidateExactlyMatches classParameter methodSignature argumentsInOrder)
        matchingCandidates
    matchingCandidates =
      filterRuntimeMethodCandidates
        (runtimeMethodCandidateMatches classParameter methodSignature argumentsInOrder)
        candidates

preferredRuntimeMethodCandidatesForTypeHint ::
  AnalyzedType ->
  InferenceVariable ->
  AnalyzedType ->
  RuntimeAppliedArguments ->
  RuntimeMethodCandidates ->
  RuntimeMethodCandidates
preferredRuntimeMethodCandidatesForTypeHint _ _ _ _ candidates | runtimeMethodIsSelected candidates = candidates
preferredRuntimeMethodCandidatesForTypeHint typeHint classParameter methodSignature arguments candidates =
  case runtimeMethodCandidatesInOrder exactMatchingCandidates of
    [] -> compatibleCandidates
    _ -> exactMatchingCandidates
  where
    exactMatchingCandidates =
      filterRuntimeMethodCandidates
        ((== Just typeHint) . candidateRemainingType)
        compatibleCandidates

    compatibleCandidates =
      filterRuntimeMethodCandidates
        (maybe False (runtimeTypesCompatible typeHint) . candidateRemainingType)
        candidates

    candidateRemainingType (RuntimeMethodCandidate evidence _) =
      dropFunctionArguments
        (runtimeAppliedArgumentCount arguments)
        (substituteRuntimeVariable classParameter (evidenceType evidence) methodSignature)

    dropFunctionArguments remaining signatureType
      | remaining <= 0 = Just signatureType
      | otherwise =
          case signatureType of
            SemanticFunction _ resultType ->
              dropFunctionArguments (remaining - 1) resultType
            _ -> Nothing

-- | Runtime-specific wrapper for canonical error construction.
-- This alias exists solely to improve readability and make it clear that
-- diagnostics are being created in a runtime evaluation context rather than
-- during parsing or type checking.
runtimeDiagnostic :: ErrorCode -> Text -> Diagnostic
runtimeDiagnostic code = mkErrorDiagnostic code RuntimeOrigin

-- | Render coarse runtime type names for diagnostics.
renderRuntimeType :: RuntimeValue -> Text
renderRuntimeType value =
  case value of
    VInt _ metadata ->
      case runtimeIntTargetType metadata of
        Just targetType -> renderNumericTypeName targetType
        Nothing -> "Int"
    VFloat {} -> "Float"
    VBool {} -> "Bool"
    VChar {} -> "Char"
    VText {} -> "Text"
    VList {} -> "List"
    VTuple {} -> "Tuple"
    VSectionLeft {} -> "Function"
    VSectionRight {} -> "Function"
    VClosure {} -> "Function"
    VBuiltin {} -> "Function"
    VOperator {} -> "Function"
    VConstructorApplication shape capturedArgs
      | constructorApplicationIsSaturated shape capturedArgs -> "Data"
      | otherwise -> "Function"
    VQualifiedMethodApplication {} -> "Function"
    VAnnotated _ innerValue -> renderRuntimeType innerValue
    VDeferredHostBinding {} -> "Deferred"
