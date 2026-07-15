{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Pure runtime value semantics. Evaluator control, callable execution, host
-- effects, continuations, and recursive scope forcing remain in the Runtime
-- façade.
module JazzNext.Compiler.Runtime.Semantics
  ( renderRuntimeValue,
    renderRuntimeType,
    runtimeDiagnostic,
    runtimeDefinitionName,
    runtimeConstructorArgument,
    runtimeConstraintType,
    literalRuntimeValue,
    attachRuntimeTypeHint,
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
    runtimeIntMatchesTarget,
    integerValueMatchesTarget,
    runtimeQualifiedMethodIsFullyApplied,
    preferredRuntimeMethodCandidates,
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
    targetedFloatMetadata
  ) where

import Control.Monad (foldM, zipWithM)
import Data.Char
  ( isControl,
    ord,
    toUpper
  )
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( CaseArm (..),
    DataConstructorArgument (..),
    Expr,
    Literal (..),
    NumericType (..),
    Pattern (..),
    SignaturePayload (..),
    SignatureType (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinSymbol (..),
    builtinSymbolName,
    numericTypeFloatMax,
    numericTypeIntegerBounds,
    numericTypeFromName,
    renderNumericTypeName
  )
import JazzNext.Compiler.CapabilityFacts
  ( constraintFunctionArgumentTypes,
    constraintSignatureTypeContainsClassParameter,
    constraintSignatureTypeVariableNamesInOrder,
    constraintSignatureTypesCompatible,
    identifierLooksLikeTypeVariable,
    signaturePayloadConstraintType,
    substituteClassMethodSignature
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    mkErrorDiagnostic
  )
import JazzNext.Compiler.DiagnosticCatalog
  ( ErrorCode (..)
  )
import JazzNext.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
    fractionalLiteralIntegralValue
  )
import JazzNext.Compiler.Name
  ( Name (..),
    NameNamespace (ConstructorNamespace),
    ResolvedNameOrigin (..),
    identifierText
  )
import JazzNext.Compiler.Runtime.Types
  ( RuntimeEnv,
    RuntimeClosure (..),
    RuntimeFloatMetadata (..),
    RuntimeIntMetadata (..),
    RuntimeMethodCandidate (..),
    RuntimeValue (..),
    attachRuntimeExplicitResultHints,
    constructorIsSaturated,
    data VExplicitResultHints,
    prependRuntimeExplicitResultHint,
    runtimeEvidenceTarget
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
    VConstructor _ _ constructorName constructorArguments capturedArgs
      | constructorIsSaturated constructorArguments capturedArgs ->
          renderConstructorValue constructorName capturedArgs
      | otherwise ->
          "<function>"
    VQualifiedMethod {} -> "<function>"
    VTyped _ innerValue -> renderRuntimeValue innerValue
    VExplicitTypeApplication _ innerValue -> renderRuntimeValue innerValue
    VExplicitResultHints _ innerValue -> renderRuntimeValue innerValue
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

renderConstructorValue :: Name -> [RuntimeValue] -> Text
renderConstructorValue constructorName arguments =
  case arguments of
    [] -> renderConstructorName constructorName
    _ ->
      renderConstructorName constructorName
        <> "("
        <> Text.intercalate ", " (map renderRuntimeValue arguments)
        <> ")"

renderConstructorName :: Name -> Text
renderConstructorName constructorName =
  case constructorName of
    ResolvedName _ ConstructorNamespace identifier -> identifierText identifier
    _ -> identifierText constructorName

runtimeDefinitionName :: Maybe [Text] -> Name -> Name
runtimeDefinitionName maybeModulePath name =
  case (maybeModulePath, name) of
    (Just modulePath, ResolvedName CurrentModule namespace identifier) ->
      ResolvedName (ImportedModule modulePath) namespace identifier
    _ -> name

runtimeConstructorArgument :: Maybe [Text] -> DataConstructorArgument -> DataConstructorArgument
runtimeConstructorArgument maybeModulePath argument =
  case argument of
    DataConstructorArgumentName name ->
      DataConstructorArgumentName (runtimeTypeName maybeModulePath name)
    DataConstructorArgumentOpaque -> DataConstructorArgumentOpaque

runtimeConstraintType :: Maybe [Text] -> SignatureType -> SignatureType
runtimeConstraintType maybeModulePath signatureType =
  case signatureType of
    TypeVariable name -> TypeVariable (runtimeTypeName maybeModulePath name)
    TypeName name -> TypeName (runtimeTypeName maybeModulePath name)
    TypeApplication name arguments ->
      TypeApplication
        (runtimeTypeName maybeModulePath name)
        (map (runtimeConstraintType maybeModulePath) arguments)
    TypeList elementType -> TypeList (runtimeConstraintType maybeModulePath elementType)
    TypeTuple elementTypes -> TypeTuple (map (runtimeConstraintType maybeModulePath) elementTypes)
    TypeFunction argumentType resultType ->
      TypeFunction
        (runtimeConstraintType maybeModulePath argumentType)
        (runtimeConstraintType maybeModulePath resultType)
    _ -> signatureType

runtimeTypeName :: Maybe [Text] -> Name -> Name
runtimeTypeName maybeModulePath name
  | identifierText name `elem` ["Int", "Float", "Bool", "Char", "Text"] = name
  | Just _ <- numericTypeFromName (identifierText name) = name
  | identifierLooksLikeTypeVariable name = name
  | otherwise = runtimeDefinitionName maybeModulePath name

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

attachRuntimeTypeHint :: Maybe SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
attachRuntimeTypeHint maybeTypeHint runtimeValue =
  case maybeTypeHint of
    Just typeHint ->
      applyRuntimeTypeHint typeHint runtimeValue
    Nothing ->
      Right runtimeValue

applyRuntimeTypeHint :: SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeTypeHint typeHint runtimeValue =
  case runtimeValue of
    VTyped existingTypeHint _
      | runtimeTypeHintAtLeastAsSpecific existingTypeHint typeHint ->
          Right runtimeValue
    VTyped _ innerValue ->
      applyRuntimeTypeHint typeHint innerValue
    VExplicitTypeApplication _ innerValue ->
      applyRuntimeTypeHint typeHint innerValue
    VExplicitResultHints _ innerValue ->
      applyRuntimeTypeHint typeHint innerValue
    _ ->
      case (typeHint, runtimeValue) of
        (TypeInt, _) -> do
          convertedValue <- evalNumericConversion (numericConversionBuiltinForTarget NumericInt64) NumericInt64 runtimeValue
          Right (VTyped TypeInt convertedValue)
        (TypeFloat, _) -> do
          convertedValue <- evalNumericConversion (numericConversionBuiltinForTarget NumericFloat64) NumericFloat64 runtimeValue
          Right (VTyped TypeFloat convertedValue)
        (TypeNumeric targetType, _) ->
          evalNumericConversion (numericConversionBuiltinForTarget targetType) targetType runtimeValue
        (TypeBool, VBool {}) -> Right runtimeValue
        (TypeChar, VChar {}) -> Right runtimeValue
        (TypeText, VText {}) -> Right runtimeValue
        (TypeName typeName, _)
          | Just targetType <- constraintTypeNameNumericTarget typeName -> do
              convertedValue <- evalNumericConversion (numericConversionBuiltinForTarget targetType) targetType runtimeValue
              if identifierText typeName == "Int" || identifierText typeName == "Float"
                then Right (VTyped typeHint convertedValue)
                else Right convertedValue
        (TypeName typeName, VChar {})
          | identifierText typeName == "Char" ->
              Right runtimeValue
        (TypeName typeName, VText {})
          | identifierText typeName == "Text" ->
              Right runtimeValue
        (TypeName hintedTypeName, VConstructor typeName typeParameters constructorName constructorArguments capturedArgs)
          | identifierText hintedTypeName == identifierText typeName,
            constructorIsSaturated constructorArguments capturedArgs -> do
              hintedCapturedArgs <-
                zipWithM
                  (applyConstructorArgumentRuntimeHint Map.empty)
                  constructorArguments
                  capturedArgs
              Right
                ( VTyped
                    typeHint
                    (VConstructor typeName typeParameters constructorName constructorArguments hintedCapturedArgs)
                )
        (TypeList _, VList _ (Just existingTypeHint))
          | runtimeTypeHintAtLeastAsSpecific existingTypeHint typeHint ->
              Right runtimeValue
        (TypeList elementType, VList elements _) -> do
          hintedElements <- mapM (applyRuntimeTypeHint elementType) elements
          Right (VList hintedElements (Just typeHint))
        (TypeTuple elementTypes, VTuple elements)
          | length elementTypes == length elements ->
              VTuple <$> zipWithM applyRuntimeTypeHint elementTypes elements
        (TypeFunction {}, VClosure closure) ->
          Right
            ( VClosure
                closure
                  { runtimeClosureTypeHint = Just typeHint
                  }
            )
        (TypeFunction {}, _)
          | isFunctionValue runtimeValue ->
              Right (VTyped typeHint runtimeValue)
        (TypeApplication hintedTypeName hintedArguments, VConstructor typeName typeParameters constructorName constructorArguments capturedArgs)
          | identifierText hintedTypeName == identifierText typeName,
            length hintedArguments == length typeParameters -> do
              let typeParameterHints =
                    Map.fromList (zip (map identifierText typeParameters) hintedArguments)
              hintedCapturedArgs <-
                zipWithM
                  (applyConstructorArgumentRuntimeHint typeParameterHints)
                  constructorArguments
                  capturedArgs
              Right (VTyped typeHint (VConstructor typeName typeParameters constructorName constructorArguments hintedCapturedArgs))
        _ ->
          Right runtimeValue

-- Runtime hints form an information order rather than a replacement order.
-- A concrete value already carrying, for example, @[CanonicalToken]@ also
-- satisfies a later polymorphic @[a]@ result hint. Preserving the stronger
-- evidence avoids both losing concrete dispatch information and repeatedly
-- traversing persistent values at polymorphic function boundaries.
runtimeTypeHintAtLeastAsSpecific :: SignatureType -> SignatureType -> Bool
runtimeTypeHintAtLeastAsSpecific existingHint requestedHint
  | existingHint == requestedHint = True
runtimeTypeHintAtLeastAsSpecific _ (TypeVariable _) = True
runtimeTypeHintAtLeastAsSpecific _ (TypeName name)
  | identifierLooksLikeTypeVariable name = True
runtimeTypeHintAtLeastAsSpecific
  (TypeApplication existingName existingArguments)
  (TypeApplication requestedName requestedArguments) =
    existingName == requestedName
    && length existingArguments == length requestedArguments
    && and (zipWith runtimeTypeHintAtLeastAsSpecific existingArguments requestedArguments)
runtimeTypeHintAtLeastAsSpecific (TypeList existingElement) (TypeList requestedElement) =
  runtimeTypeHintAtLeastAsSpecific existingElement requestedElement
runtimeTypeHintAtLeastAsSpecific (TypeTuple existingElements) (TypeTuple requestedElements) =
  length existingElements == length requestedElements
    && and (zipWith runtimeTypeHintAtLeastAsSpecific existingElements requestedElements)
runtimeTypeHintAtLeastAsSpecific
  (TypeFunction existingArgument existingResult)
  (TypeFunction requestedArgument requestedResult) =
    runtimeTypeHintAtLeastAsSpecific existingArgument requestedArgument
      && runtimeTypeHintAtLeastAsSpecific existingResult requestedResult
runtimeTypeHintAtLeastAsSpecific _ _ = False

applyConstructorArgumentRuntimeHint ::
  Map Text SignatureType ->
  DataConstructorArgument ->
  RuntimeValue ->
  Either Diagnostic RuntimeValue
applyConstructorArgumentRuntimeHint typeParameterHints constructorArgument runtimeValue =
  case constructorArgument of
    DataConstructorArgumentName argumentName ->
      attachRuntimeTypeHint (constructorArgumentRuntimeHint typeParameterHints argumentName) runtimeValue
    DataConstructorArgumentOpaque ->
      Right runtimeValue

constructorArgumentRuntimeHint :: Map Text SignatureType -> Name -> Maybe SignatureType
constructorArgumentRuntimeHint typeParameterHints argumentName =
  case Map.lookup (identifierText argumentName) typeParameterHints of
    Just hintedType -> Just hintedType
    Nothing -> concreteConstructorPayloadRuntimeHint argumentName

concreteConstructorPayloadRuntimeHint :: Name -> Maybe SignatureType
concreteConstructorPayloadRuntimeHint argumentName
  | identifierText argumentName == "Int" = Just TypeInt
  | identifierText argumentName == "Float" = Just TypeFloat
  | Just numericType <- constraintTypeNameNumericTarget argumentName =
      Just (TypeNumeric numericType)
  | identifierText argumentName == "Bool" =
      Just TypeBool
  | identifierText argumentName == "Char" = Just TypeChar
  | identifierText argumentName == "Text" = Just TypeText
  | otherwise =
      Nothing

constraintTypeNameNumericTarget :: Name -> Maybe NumericType
constraintTypeNameNumericTarget typeName =
  case identifierText typeName of
    "Int" -> Just NumericInt64
    "Int8" -> Just NumericInt8
    "Int16" -> Just NumericInt16
    "Int32" -> Just NumericInt32
    "Int64" -> Just NumericInt64
    "UInt8" -> Just NumericUInt8
    "UInt16" -> Just NumericUInt16
    "UInt32" -> Just NumericUInt32
    "UInt64" -> Just NumericUInt64
    "Float" -> Just NumericFloat64
    "Float16" -> Just NumericFloat16
    "Float32" -> Just NumericFloat32
    "Float64" -> Just NumericFloat64
    _ -> Nothing

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
  Maybe [Text] ->
  RuntimeEnv ->
  RuntimeValue ->
  CaseArm ->
  Maybe (RuntimeEnv, Maybe Expr, Expr)
matchCaseArm currentModulePath env scrutineeValue (CaseArm casePattern guardExpr bodyExpr) =
  case matchPattern currentModulePath scrutineeValue casePattern of
    Just patternBindings ->
      Just (Map.union patternBindings env, guardExpr, bodyExpr)
    Nothing -> Nothing

matchPattern :: Maybe [Text] -> RuntimeValue -> Pattern -> Maybe RuntimeEnv
matchPattern currentModulePath scrutineeValue casePattern =
  case casePattern of
    PWildcard -> Just Map.empty
    PVariable name ->
      Just
        (Map.singleton name (Right scrutineeValue))
    PLiteral literal
      | scrutineeValue == literalRuntimeValue literal ->
          Just Map.empty
      | otherwise ->
          Nothing
    PConstructor constructorName patterns ->
      case constructorPatternScrutinee scrutineeValue of
        VConstructor _ _ valueConstructorName constructorArguments capturedArgs
          | valueConstructorName == runtimeDefinitionName currentModulePath constructorName,
            constructorIsSaturated constructorArguments capturedArgs,
            length capturedArgs == length patterns ->
              matchPatternList currentModulePath capturedArgs patterns
        _ -> Nothing
    PList patterns ->
      case scrutineeValue of
        VList elements _
          | length elements == length patterns ->
              matchPatternList currentModulePath elements patterns
        _ -> Nothing
    PConsList headPattern tailPattern ->
      case scrutineeValue of
        VList (headValue : tailValues) maybeTypeHint -> do
          headBindings <- matchPattern currentModulePath headValue headPattern
          tailBindings <- matchPattern currentModulePath (VList tailValues maybeTypeHint) tailPattern
          Just (tailBindings `Map.union` headBindings)
        _ -> Nothing
    PTuple patterns ->
      case scrutineeValue of
        VTuple elements
          | length elements == length patterns ->
              matchPatternList currentModulePath elements patterns
        _ -> Nothing
    PAs name nestedPattern -> do
      patternBindings <- matchPattern currentModulePath scrutineeValue nestedPattern
      Just (Map.insert name (Right scrutineeValue) patternBindings)
    POr alternatives ->
      matchFirstAlternative currentModulePath scrutineeValue alternatives

matchFirstAlternative :: Maybe [Text] -> RuntimeValue -> [Pattern] -> Maybe RuntimeEnv
matchFirstAlternative currentModulePath scrutineeValue alternatives =
  case alternatives of
    [] -> Nothing
    alternative : rest ->
      case matchPattern currentModulePath scrutineeValue alternative of
        Just patternBindings -> Just patternBindings
        Nothing -> matchFirstAlternative currentModulePath scrutineeValue rest

matchPatternList :: Maybe [Text] -> [RuntimeValue] -> [Pattern] -> Maybe RuntimeEnv
matchPatternList currentModulePath values patterns =
  foldM step Map.empty (zip values patterns)
  where
    step bindings (value, elementPattern) =
      case matchPattern currentModulePath value elementPattern of
        Just patternBindings -> Just (patternBindings `Map.union` bindings)
        Nothing -> Nothing

constructorPatternScrutinee :: RuntimeValue -> RuntimeValue
constructorPatternScrutinee runtimeValue =
  case runtimeValue of
    VTyped _ innerValue -> constructorPatternScrutinee innerValue
    VExplicitTypeApplication _ innerValue -> constructorPatternScrutinee innerValue
    VExplicitResultHints _ innerValue -> constructorPatternScrutinee innerValue
    _ -> runtimeValue

applyRuntimeFunctionResultHint :: SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeFunctionResultHint typeHint runtimeValue =
  case typeHint of
    TypeFunction _ resultType ->
      applyRuntimeTypeHint resultType runtimeValue
    _ ->
      Right runtimeValue

applyRuntimeFunctionArgumentHint :: SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeFunctionArgumentHint typeHint runtimeValue =
  case typeHint of
    TypeFunction argumentType _ ->
      applyRuntimeTypeHint argumentType runtimeValue
    _ ->
      Right runtimeValue

applyExplicitTypeApplicationResultHint :: SignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyExplicitTypeApplicationResultHint typeHint runtimeValue
  | isFunctionValue runtimeValue =
      Right (prependRuntimeExplicitResultHint typeHint runtimeValue)
  | runtimeValueCanAcceptTypeHint typeHint runtimeValue =
      applyRuntimeTypeHint typeHint runtimeValue
  | otherwise =
      Right runtimeValue

runtimeValueCanAcceptTypeHint :: SignatureType -> RuntimeValue -> Bool
runtimeValueCanAcceptTypeHint typeHint runtimeValue =
  case runtimeValue of
    VTyped _ innerValue ->
      runtimeValueCanAcceptTypeHint typeHint innerValue
    VExplicitTypeApplication _ innerValue ->
      runtimeValueCanAcceptTypeHint typeHint innerValue
    VExplicitResultHints _ innerValue ->
      runtimeValueCanAcceptTypeHint typeHint innerValue
    _ ->
      case (typeHint, runtimeValue) of
        (TypeInt, VInt {}) -> True
        (TypeFloat, VFloat {}) -> True
        (TypeNumeric _, VInt {}) -> True
        (TypeNumeric _, VFloat {}) -> True
        (TypeBool, VBool {}) -> True
        (TypeChar, VChar {}) -> True
        (TypeText, VText {}) -> True
        (TypeName typeName, VInt {}) ->
          identifierText typeName == "Int" || isJust (constraintTypeNameNumericTarget typeName)
        (TypeName typeName, VFloat {}) ->
          identifierText typeName == "Float" || isJust (constraintTypeNameNumericTarget typeName)
        (TypeName typeName, VBool {}) ->
          identifierText typeName == "Bool"
        (TypeName typeName, VChar {}) ->
          identifierText typeName == "Char"
        (TypeName typeName, VText {}) ->
          identifierText typeName == "Text"
        (TypeName typeName, VConstructor constructorTypeName _ _ constructorArguments capturedArgs) ->
          identifierText typeName == identifierText constructorTypeName
            && constructorIsSaturated constructorArguments capturedArgs
        (TypeApplication typeName arguments, VConstructor constructorTypeName typeParameters _ constructorArguments capturedArgs) ->
          identifierText typeName == identifierText constructorTypeName
            && length arguments == length typeParameters
            && constructorIsSaturated constructorArguments capturedArgs
        (TypeList {}, VList {}) ->
          True
        (TypeTuple elementTypes, VTuple elements) ->
          length elementTypes == length elements
        (TypeFunction {}, _) ->
          isFunctionValue runtimeValue
        _ ->
          False

explicitTypeApplicationRuntimeFunctionHint :: SignatureType -> RuntimeValue -> Maybe SignatureType
explicitTypeApplicationRuntimeFunctionHint typeHint runtimeValue = do
  explicitTypeApplicationRuntimeTemplateHint typeHint runtimeValue

explicitTypeApplicationRuntimeValueHint :: SignatureType -> RuntimeValue -> Maybe SignatureType
explicitTypeApplicationRuntimeValueHint typeHint runtimeValue =
  case explicitTypeApplicationRuntimeTemplateHint typeHint runtimeValue of
    Just instantiatedTemplate -> Just instantiatedTemplate
    Nothing -> explicitTypeApplicationRuntimeShapeHint typeHint runtimeValue

explicitTypeApplicationRuntimeTemplateHint :: SignatureType -> RuntimeValue -> Maybe SignatureType
explicitTypeApplicationRuntimeTemplateHint typeHint runtimeValue = do
  templateHint <- runtimeValueSignatureHint runtimeValue
  variableName <- listToMaybe (constraintSignatureTypeVariableNamesInOrder templateHint)
  pure (substituteSignatureTypeVariable variableName typeHint templateHint)

explicitTypeApplicationRuntimeShapeHint :: SignatureType -> RuntimeValue -> Maybe SignatureType
explicitTypeApplicationRuntimeShapeHint typeHint runtimeValue =
  case runtimeValue of
    VTyped _ innerValue ->
      explicitTypeApplicationRuntimeShapeHint typeHint innerValue
    VExplicitTypeApplication _ innerValue ->
      explicitTypeApplicationRuntimeShapeHint typeHint innerValue
    VExplicitResultHints _ innerValue ->
      explicitTypeApplicationRuntimeShapeHint typeHint innerValue
    VList {} ->
      Just (TypeList typeHint)
    VConstructor typeName typeParameters _ constructorArguments capturedArgs
      | length typeParameters == 1,
        constructorIsSaturated constructorArguments capturedArgs ->
          Just (TypeApplication typeName [typeHint])
    _ -> Nothing

runtimeValueSignatureHint :: RuntimeValue -> Maybe SignatureType
runtimeValueSignatureHint runtimeValue =
  case runtimeValue of
    VTyped typeHint _ ->
      Just typeHint
    VExplicitTypeApplication _ innerValue ->
      runtimeValueSignatureHint innerValue
    VExplicitResultHints _ innerValue ->
      runtimeValueSignatureHint innerValue
    VClosure closure ->
      runtimeClosureTypeHint closure
    VList _ (Just typeHint) ->
      Just typeHint
    _ -> Nothing

substituteSignatureTypeVariable :: Text -> SignatureType -> SignatureType -> SignatureType
substituteSignatureTypeVariable variableName replacementType signatureType =
  case signatureType of
    TypeVariable name
      | identifierText name == variableName -> replacementType
      | otherwise -> signatureType
    TypeName name
      | identifierLooksLikeTypeVariable name,
        identifierText name == variableName ->
          replacementType
      | otherwise ->
          signatureType
    TypeApplication typeName arguments ->
      TypeApplication typeName (map (substituteSignatureTypeVariable variableName replacementType) arguments)
    TypeList innerType ->
      TypeList (substituteSignatureTypeVariable variableName replacementType innerType)
    TypeTuple elementTypes ->
      TypeTuple (map (substituteSignatureTypeVariable variableName replacementType) elementTypes)
    TypeFunction argumentType resultType ->
      TypeFunction
        (substituteSignatureTypeVariable variableName replacementType argumentType)
        (substituteSignatureTypeVariable variableName replacementType resultType)
    _ -> signatureType

runtimeQualifiedMethodIsFullyApplied ::
  Text ->
  SignaturePayload ->
  [RuntimeValue] ->
  [RuntimeMethodCandidate] ->
  Bool
runtimeQualifiedMethodIsFullyApplied classParameter methodSignature arguments candidates =
  any candidateIsFullyApplied candidates
  where
    candidateIsFullyApplied (RuntimeMethodCandidate evidence _) =
      case substituteClassMethodSignature classParameter implTarget methodSignature of
        Just substitutedSignature ->
          let (argumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
           in length arguments >= length argumentTypes
        Nothing ->
          False
      where
        implTarget = runtimeEvidenceTarget evidence

runtimeMethodCandidateExactlyMatches :: Text -> SignaturePayload -> [RuntimeValue] -> RuntimeMethodCandidate -> Bool
runtimeMethodCandidateExactlyMatches classParameter methodSignature arguments (RuntimeMethodCandidate evidence _) =
  case (signaturePayloadConstraintType methodSignature, substituteClassMethodSignature classParameter implTarget methodSignature) of
    (Just genericSignature, Just substitutedSignature) ->
      let (genericArgumentTypes, _) = constraintFunctionArgumentTypes genericSignature
          (argumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
          suppliedArgumentCount = length arguments
          suppliedGenericArgumentTypes = take suppliedArgumentCount genericArgumentTypes
          suppliedArgumentTypes = take suppliedArgumentCount argumentTypes
          targetArgumentPositions =
            map (constraintSignatureTypeContainsClassParameter classParameter) suppliedGenericArgumentTypes
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
    _ ->
      False
  where
    implTarget = runtimeEvidenceTarget evidence

runtimeExactCandidateArgumentMatches :: Bool -> SignatureType -> RuntimeValue -> Bool
runtimeExactCandidateArgumentMatches targetArgumentPosition signatureType runtimeValue =
  not targetArgumentPosition || runtimeValueExactlyMatchesConstraint signatureType runtimeValue

runtimeValueExactlyMatchesConstraint :: SignatureType -> RuntimeValue -> Bool
runtimeValueExactlyMatchesConstraint signatureType runtimeValue =
  case runtimeValue of
    VExplicitTypeApplication _ innerValue ->
      runtimeValueExactlyMatchesConstraint signatureType innerValue
    VExplicitResultHints _ innerValue ->
      runtimeValueExactlyMatchesConstraint signatureType innerValue
    VTyped typeHint _ ->
      typeHint == signatureType
    VClosure closure ->
      runtimeClosureTypeHint closure == Just signatureType
    VInt _ metadata ->
      case signatureType of
        TypeInt -> runtimeIntTargetType metadata == Nothing
        TypeNumeric numericType -> runtimeIntTargetType metadata == Just numericType
        TypeName typeName ->
          runtimeIntExactlyMatchesTypeName (identifierText typeName) metadata
        _ -> False
    VFloat _ metadata ->
      case signatureType of
        TypeFloat -> runtimeFloatTargetType metadata == Nothing
        TypeNumeric numericType -> runtimeFloatTargetType metadata == Just numericType
        TypeName typeName ->
          runtimeFloatExactlyMatchesTypeName (identifierText typeName) metadata
        _ -> False
    VChar {} ->
      case signatureType of
        TypeChar -> True
        TypeName typeName -> identifierText typeName == "Char"
        _ -> False
    VText {} ->
      case signatureType of
        TypeText -> True
        TypeName typeName -> identifierText typeName == "Text"
        _ -> False
    VBool {} ->
      case signatureType of
        TypeBool -> True
        TypeName typeName -> identifierText typeName == "Bool"
        _ -> False
    VList _ (Just typeHint) ->
      typeHint == signatureType
    VList elements Nothing ->
      case signatureType of
        TypeList elementType ->
          not (null elements)
            && all (runtimeValueExactlyMatchesConstraint elementType) elements
        _ -> False
    VTuple elements ->
      case signatureType of
        TypeTuple elementTypes
          | length elementTypes == length elements ->
              and (zipWith runtimeValueExactlyMatchesConstraint elementTypes elements)
        _ -> False
    VConstructor {} ->
      case signatureType of
        TypeName typeName ->
          runtimeValueExactlyMatchesDataTypeName typeName runtimeValue
        TypeApplication typeName typeArguments ->
          runtimeValueExactlyMatchesDataTypeApplication typeName typeArguments runtimeValue
        _ -> False
    _ -> False

runtimeIntExactlyMatchesTypeName :: Text -> RuntimeIntMetadata -> Bool
runtimeIntExactlyMatchesTypeName typeName metadata =
  case (typeName, runtimeIntTargetType metadata) of
    ("Int", Nothing) -> True
    ("Int8", Just NumericInt8) -> True
    ("Int16", Just NumericInt16) -> True
    ("Int32", Just NumericInt32) -> True
    ("Int64", Just NumericInt64) -> True
    ("UInt8", Just NumericUInt8) -> True
    ("UInt16", Just NumericUInt16) -> True
    ("UInt32", Just NumericUInt32) -> True
    ("UInt64", Just NumericUInt64) -> True
    _ -> False

runtimeFloatExactlyMatchesTypeName :: Text -> RuntimeFloatMetadata -> Bool
runtimeFloatExactlyMatchesTypeName typeName metadata =
  case (typeName, runtimeFloatTargetType metadata) of
    ("Float", Nothing) -> True
    ("Float16", Just NumericFloat16) -> True
    ("Float32", Just NumericFloat32) -> True
    ("Float64", Just NumericFloat64) -> True
    _ -> False

runtimeMethodCandidateMatches :: Text -> SignaturePayload -> [RuntimeValue] -> RuntimeMethodCandidate -> Bool
runtimeMethodCandidateMatches classParameter methodSignature arguments (RuntimeMethodCandidate evidence _) =
  case substituteClassMethodSignature classParameter implTarget methodSignature of
    Just substitutedSignature ->
      let (argumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
       in length arguments <= length argumentTypes
            && and (zipWith runtimeValueMatchesConstraint argumentTypes arguments)
    Nothing ->
      False
  where
    implTarget = runtimeEvidenceTarget evidence

runtimeValueMatchesConstraint :: SignatureType -> RuntimeValue -> Bool
runtimeValueMatchesConstraint signatureType runtimeValue =
  case runtimeValue of
    VExplicitTypeApplication _ innerValue ->
      runtimeValueMatchesConstraint signatureType innerValue
    VExplicitResultHints _ innerValue ->
      runtimeValueMatchesConstraint signatureType innerValue
    VTyped typeHint _ ->
      constraintSignatureTypesCompatible typeHint signatureType
    _ ->
      case signatureType of
        TypeInt -> runtimeValueMatchesTypeName "Int" runtimeValue
        TypeFloat -> runtimeValueMatchesTypeName "Float" runtimeValue
        TypeNumeric numericType -> runtimeValueMatchesTypeName (renderNumericTypeName numericType) runtimeValue
        TypeBool -> runtimeValueMatchesTypeName "Bool" runtimeValue
        TypeChar -> runtimeValueMatchesTypeName "Char" runtimeValue
        TypeText -> runtimeValueMatchesTypeName "Text" runtimeValue
        TypeVariable {} -> False
        TypeName typeName ->
          runtimeValueMatchesTypeName (identifierText typeName) runtimeValue
        TypeApplication typeName typeArguments ->
          runtimeValueMatchesDataTypeApplication typeName typeArguments runtimeValue
        TypeList elementType ->
          case runtimeValue of
            VList elements maybeTypeHint ->
              case maybeTypeHint of
                Just typeHint -> constraintSignatureTypesCompatible typeHint signatureType
                Nothing -> all (runtimeValueMatchesConstraint elementType) elements
            _ -> False
        TypeTuple elementTypes ->
          case runtimeValue of
            VTuple elements
              | length elementTypes == length elements ->
                  and (zipWith runtimeValueMatchesConstraint elementTypes elements)
            _ -> False
        TypeFunction {} ->
          case runtimeValue of
            VClosure closure ->
              case runtimeClosureTypeHint closure of
                Just typeHint -> constraintSignatureTypesCompatible typeHint signatureType
                Nothing -> True
            _ -> isFunctionValue runtimeValue

runtimeValueMatchesTypeName :: Text -> RuntimeValue -> Bool
runtimeValueMatchesTypeName typeName runtimeValue =
  case typeName of
    "Int" -> runtimeIntMatchesIntAlias runtimeValue
    "Int8" -> runtimeIntMatchesTarget NumericInt8 runtimeValue
    "Int16" -> runtimeIntMatchesTarget NumericInt16 runtimeValue
    "Int32" -> runtimeIntMatchesTarget NumericInt32 runtimeValue
    "Int64" -> runtimeIntMatchesTarget NumericInt64 runtimeValue
    "UInt8" -> runtimeIntMatchesTarget NumericUInt8 runtimeValue
    "UInt16" -> runtimeIntMatchesTarget NumericUInt16 runtimeValue
    "UInt32" -> runtimeIntMatchesTarget NumericUInt32 runtimeValue
    "UInt64" -> runtimeIntMatchesTarget NumericUInt64 runtimeValue
    "Float" -> runtimeFloatMatchesFloatAlias runtimeValue
    "Float16" -> runtimeFloatHasTarget NumericFloat16 runtimeValue
    "Float32" -> runtimeFloatHasTarget NumericFloat32 runtimeValue
    "Float64" -> runtimeFloatHasTarget NumericFloat64 runtimeValue
    "Bool" -> isRuntimeBool runtimeValue
    "Char" -> isRuntimeChar runtimeValue
    "Text" -> isRuntimeText runtimeValue
    _ -> runtimeValueMatchesDataTypeName typeName runtimeValue

runtimeValueMatchesDataTypeName :: Text -> RuntimeValue -> Bool
runtimeValueMatchesDataTypeName typeName runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName _ _ constructorArguments capturedArgs ->
      identifierText valueTypeName == typeName
        && constructorIsSaturated constructorArguments capturedArgs
    _ -> False

runtimeValueMatchesDataTypeApplication :: Name -> [SignatureType] -> RuntimeValue -> Bool
runtimeValueMatchesDataTypeApplication typeName typeArguments runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName typeParameters _ constructorArguments capturedArgs
      | valueTypeName == typeName,
        length typeParameters == length typeArguments,
        constructorIsSaturated constructorArguments capturedArgs ->
          let typeParameterBindings = Map.fromList (zip (map identifierText typeParameters) typeArguments)
           in and
                ( zipWith
                    (runtimeValueMatchesConstructorArgument typeParameterBindings)
                    constructorArguments
                    capturedArgs
                )
    _ -> False

runtimeValueExactlyMatchesDataTypeName :: Name -> RuntimeValue -> Bool
runtimeValueExactlyMatchesDataTypeName typeName runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName _ _ constructorArguments capturedArgs ->
      valueTypeName == typeName
        && constructorIsSaturated constructorArguments capturedArgs
    _ -> False

runtimeValueExactlyMatchesDataTypeApplication :: Name -> [SignatureType] -> RuntimeValue -> Bool
runtimeValueExactlyMatchesDataTypeApplication typeName typeArguments runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName typeParameters _ constructorArguments capturedArgs
      | valueTypeName == typeName,
        length typeParameters == length typeArguments,
        constructorIsSaturated constructorArguments capturedArgs ->
          let typeParameterBindings = Map.fromList (zip (map identifierText typeParameters) typeArguments)
           in and
                ( zipWith
                    (runtimeValueExactlyMatchesConstructorArgument typeParameterBindings)
                    constructorArguments
                    capturedArgs
                )
    _ -> False

runtimeValueMatchesConstructorArgument :: Map Text SignatureType -> DataConstructorArgument -> RuntimeValue -> Bool
runtimeValueMatchesConstructorArgument typeParameterBindings constructorArgument runtimeValue =
  case constructorArgument of
    DataConstructorArgumentName argumentName ->
      case constructorArgumentRuntimeHint typeParameterBindings argumentName of
        Just concreteArgumentType ->
          runtimeValueMatchesConstraint concreteArgumentType runtimeValue
        Nothing ->
          True
    DataConstructorArgumentOpaque ->
      True

runtimeValueExactlyMatchesConstructorArgument :: Map Text SignatureType -> DataConstructorArgument -> RuntimeValue -> Bool
runtimeValueExactlyMatchesConstructorArgument typeParameterBindings constructorArgument runtimeValue =
  case constructorArgument of
    DataConstructorArgumentName argumentName ->
      case Map.lookup (identifierText argumentName) typeParameterBindings of
        Just concreteArgumentType ->
          runtimeValueExactlyMatchesConstraint concreteArgumentType runtimeValue
        Nothing ->
          True
    DataConstructorArgumentOpaque ->
      True

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
applyConstructor :: Name -> [Name] -> Name -> [DataConstructorArgument] -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyConstructor typeName typeParameters constructorName constructorArguments arguments
  | length arguments <= constructorArity =
      Right (VConstructor typeName typeParameters constructorName constructorArguments arguments)
  | otherwise =
      Left
        ( runtimeDiagnostic
            E3023
            ( "runtime constructor '"
                <> identifierText constructorName
                <> "' expected "
                <> renderArityCount constructorArity
                <> " but received "
                <> renderArityCount (length arguments)
            )
        )
  where
    constructorArity = length constructorArguments

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
    VExplicitTypeApplication _ innerValue ->
      evalNumericConversion builtinFunction targetType innerValue
    VExplicitResultHints _ innerValue ->
      evalNumericConversion builtinFunction targetType innerValue
    VTyped _ innerValue ->
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
       in
        if fromInteger roundedInteger == floatValue && integerValueWithinBounds roundedInteger bounds
          then Right (VInt roundedInteger (targetedIntMetadata targetType))
          else Left (numericConversionFloatToIntegralDiagnostic builtinFunction targetType floatValue bounds)

convertIntegerToFloatTarget :: BuiltinSymbol -> NumericType -> Integer -> Either Diagnostic RuntimeValue
convertIntegerToFloatTarget builtinFunction targetType integerValue =
  if integerExceedsFloatTarget targetType integerValue
    then Left (numericConversionFloatOverflowDiagnostic builtinFunction targetType)
    else
      let floatValue = fromInteger integerValue :: Double
       in
        if isInfinite floatValue || exceedsFloatTarget targetType floatValue
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
    VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs ->
      VQualifiedMethod methodKey classParameter methodSignature candidates
        <$> traverse attachDefaultBindingIntegerTarget capturedArgs
    VTyped typeHint innerValue
      | TypeFunction {} <- typeHint ->
          Right (VTyped typeHint innerValue)
      | otherwise ->
          VTyped typeHint <$> attachDefaultBindingIntegerTarget innerValue
    VExplicitTypeApplication typeHint innerValue ->
      VExplicitTypeApplication typeHint <$> attachDefaultBindingIntegerTarget innerValue
    VExplicitResultHints hints innerValue ->
      attachRuntimeExplicitResultHints hints <$> attachDefaultBindingIntegerTarget innerValue
    _ ->
      Right runtimeValue

isFunctionValue :: RuntimeValue -> Bool
isFunctionValue value =
  case value of
    VExplicitTypeApplication _ innerValue -> isFunctionValue innerValue
    VExplicitResultHints _ innerValue -> isFunctionValue innerValue
    VTyped _ innerValue -> isFunctionValue innerValue
    VSectionLeft {} -> True
    VSectionRight {} -> True
    VClosure {} -> True
    VBuiltin {} -> True
    VOperator {} -> True
    VConstructor _ _ _ constructorArguments capturedArgs ->
      not (constructorIsSaturated constructorArguments capturedArgs)
    VQualifiedMethod {} -> True
    _ -> False

preferredRuntimeMethodCandidates ::
  Text ->
  SignaturePayload ->
  [RuntimeValue] ->
  [RuntimeMethodCandidate] ->
  [RuntimeMethodCandidate]
preferredRuntimeMethodCandidates classParameter methodSignature arguments candidates =
  case exactMatchingCandidates of
    [] -> matchingCandidates
    exactMatches -> exactMatches
  where
    exactMatchingCandidates =
      filter
        (runtimeMethodCandidateExactlyMatches classParameter methodSignature arguments)
        matchingCandidates
    matchingCandidates =
      filter
        (runtimeMethodCandidateMatches classParameter methodSignature arguments)
        candidates

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
    VConstructor _ _ _ constructorArguments capturedArgs
      | constructorIsSaturated constructorArguments capturedArgs -> "Data"
      | otherwise -> "Function"
    VQualifiedMethod {} -> "Function"
    VTyped _ innerValue -> renderRuntimeType innerValue
    VExplicitTypeApplication _ innerValue -> renderRuntimeType innerValue
    VExplicitResultHints _ innerValue -> renderRuntimeType innerValue
    VDeferredHostBinding {} -> "Deferred"
