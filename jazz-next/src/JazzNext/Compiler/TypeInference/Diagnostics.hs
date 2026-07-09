{-# LANGUAGE OverloadedStrings #-}

-- | Type-inference diagnostics and error-state operations.
module JazzNext.Compiler.TypeInference.Diagnostics
  ( InferExprFn,
    addTypeError,
    annotateNewErrorsWithPrimarySpan,
    duplicateConstraintName,
    mkAmbiguousDeferredConstraintError,
    mkAmbiguousQualifiedMethodBodyError,
    mkAmbiguousQualifiedMethodBodyForArgumentsError,
    mkApplyTypeError,
    mkBinaryTypeError,
    mkBindingTypeMismatchError,
    mkCaseGuardTypeError,
    mkConstructorPatternArityError,
    mkDuplicateDataTypeDeclarationError,
    mkDuplicatePatternBinderError,
    mkEmptyOrPatternError,
    mkExplicitConstraintArityError,
    mkExplicitTypeApplicationTargetError,
    mkIfBranchTypeMismatchError,
    mkIfConditionTypeError,
    mkImplMethodMissingClassMethodError,
    mkImplMethodTypeMismatchError,
    mkInvalidSignatureTypeError,
    mkInvalidQualifiedMethodSignatureError,
    mkListElementTypeMismatchError,
    mkListPatternTypeMismatchError,
    mkMissingClassMethodError,
    mkMissingConstructorTypeParameterBindingError,
    mkMissingExplicitConstraintClassError,
    mkMissingExplicitConstraintImplFactError,
    mkMissingImplMethodBodyError,
    mkMissingOperatorBindingError,
    mkNoMatchingQualifiedMethodBodyError,
    mkNumericBinaryTypeError,
    mkNumericConversionFloatLiteralOverflowError,
    mkNumericConversionFractionalLiteralTypeError,
    mkNumericConversionLiteralTypeError,
    mkNumericSectionOperandTypeError,
    mkOrPatternBinderSetMismatchError,
    mkOrPatternBinderTypeMismatchError,
    mkPatternBranchTypeMismatchError,
    mkPatternTypeMismatchError,
    mkSignatureTypeMismatchError,
    mkStrictEqualityTypeError,
    mkStrictEqualityUnsupportedTypeError,
    mkTargetedFractionalLiteralOverflowError,
    mkTuplePatternArityMismatchError,
    mkTuplePatternTypeMismatchError,
    mkTypeSchemeNumericConstraintError,
    mkTypeSchemeStrictEqualityConstraintError,
    mkUnknownConstructorPatternError,
    mkUnknownConstructorPayloadTypeError,
    mkUnsupportedOperatorValueError,
    mkUnsupportedSectionOperatorError,
    renderSignaturePayload,
    renderType
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( ConstraintSignatureType (..),
    Expr,
    NumericType,
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode,
    renderNumericTypeName
  )
import JazzNext.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    constraintImplFactKey,
    identifierLooksLikeTypeVariable,
    renderConstraintSignatureType
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic (..),
    SourceSpan,
    mkDiagnostic,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject
  )
import JazzNext.Compiler.Identifier (identifierText)
import JazzNext.Compiler.Name (Name)
import JazzNext.Compiler.TypeInference.State
  ( InferState (..),
    InferenceOutput (..),
    inferErrorCount,
    inferErrorsRev,
    inferClassFacts,
    inferConcreteImplFacts
  )
import JazzNext.Compiler.TypeInference.Types
  ( ExpressionType (..),
    NumericConstraint,
    TypeEnv
  )

type InferExprFn =
  BuiltinResolutionMode ->
  TypeEnv ->
  InferState ->
  Expr ->
  (Maybe ExpressionType, InferState)

addTypeError :: InferState -> Diagnostic -> InferState
addTypeError state diagnostic =
  modifyInferenceOutput
    ( \output ->
        output
          { outputErrorsRev = diagnostic : inferErrorsRev state,
            outputErrorCount = inferErrorCount state + 1
          }
    )
    state

annotateNewErrorsWithPrimarySpan :: SourceSpan -> InferState -> InferState -> InferState
annotateNewErrorsWithPrimarySpan spanValue previousState nextState =
  modifyInferenceOutput
    (\output -> output {outputErrorsRev = updatedNewErrors ++ existingErrors})
    nextState
  where
    newErrorCount = inferErrorCount nextState - inferErrorCount previousState
    (newErrors, existingErrors) = splitAt newErrorCount (inferErrorsRev nextState)
    updatedNewErrors = map ensurePrimarySpan newErrors
    ensurePrimarySpan diagnostic =
      case diagnosticPrimary diagnostic of
        Just _ -> diagnostic
        Nothing -> setDiagnosticPrimarySpan spanValue diagnostic

    diagnosticPrimary = diagnosticPrimarySpan

modifyInferenceOutput :: (InferenceOutput -> InferenceOutput) -> InferState -> InferState
modifyInferenceOutput update state = state {inferOutput = update (inferOutput state)}

mkNumericBinaryTypeError :: Text -> ExpressionType -> ExpressionType -> Diagnostic
mkNumericBinaryTypeError = mkBinaryTypeError

mkBinaryTypeError :: Text -> ExpressionType -> ExpressionType -> Diagnostic
mkBinaryTypeError operatorSymbol leftType rightType =
  mkDiagnostic "E2003" $ "cannot apply operator '" <> operatorSymbol <> "' to operands of type " <> renderType leftType <> " and " <> renderType rightType

mkStrictEqualityTypeError :: Text -> ExpressionType -> ExpressionType -> Diagnostic
mkStrictEqualityTypeError operatorSymbol leftType rightType =
  mkDiagnostic "E2004" $ "strict equality operator '" <> operatorSymbol <> "' requires operands of the same type, found " <> renderType leftType <> " and " <> renderType rightType

mkStrictEqualityUnsupportedTypeError :: Text -> ExpressionType -> Diagnostic
mkStrictEqualityUnsupportedTypeError operatorSymbol foundType =
  mkDiagnostic "E2004" $ "strict equality operator '" <> operatorSymbol <> "' is only supported for Bool, integral numeric, Float/Float16/Float32/Float64, lists and tuples containing equality-supported elements, and ADTs containing equality-supported constructor payloads, found " <> renderType foundType <> callableNote
  where
    callableNote
      | typeContainsFunction foundType = "; callable values are not equality-supported"
      | otherwise = ""

typeContainsFunction :: ExpressionType -> Bool
typeContainsFunction expressionType =
  case expressionType of
    TFunctionType {} -> True
    TListType elementType -> typeContainsFunction elementType
    TTupleType elementTypes -> any typeContainsFunction elementTypes
    TDataType _ typeArguments -> any typeContainsFunction typeArguments
    _ -> False

mkDuplicateDataTypeDeclarationError :: Text -> SourceSpan -> Diagnostic
mkDuplicateDataTypeDeclarationError typeName spanValue =
  setDiagnosticSubject typeName $ setDiagnosticPrimarySpan spanValue $ mkDiagnostic "E2014" ("duplicate data type declaration '" <> typeName <> "'")

mkSignatureTypeMismatchError :: Text -> SourceSpan -> ExpressionType -> SourceSpan -> ExpressionType -> Diagnostic
mkSignatureTypeMismatchError bindingName signatureSpan declaredType bindingSpan inferredType =
  setDiagnosticSubject bindingName $ setDiagnosticRelatedSpan bindingSpan $ setDiagnosticPrimarySpan signatureSpan $
    mkDiagnostic "E2005" ("binding '" <> bindingName <> "' declared as " <> renderType declaredType <> " but inferred as " <> renderType inferredType)

mkApplyTypeError :: ExpressionType -> ExpressionType -> Diagnostic
mkApplyTypeError functionType argumentType =
  mkDiagnostic "E2006" $ "cannot apply function of type " <> renderType functionType <> " to argument of type " <> renderType argumentType

mkExplicitTypeApplicationTargetError :: Diagnostic
mkExplicitTypeApplicationTargetError = mkDiagnostic "E2017" "explicit type application target must be a generalized binding"

mkNumericConversionLiteralTypeError :: Text -> Integer -> NumericType -> (Integer, Integer) -> Diagnostic
mkNumericConversionLiteralTypeError conversionName literalValue targetType (lowerBound, upperBound) =
  mkDiagnostic "E2006" $ "numeric conversion '" <> conversionName <> "' cannot convert integer literal " <> tshow literalValue <> " outside " <> renderNumericTypeName targetType <> " range " <> tshow lowerBound <> ".." <> tshow upperBound

mkNumericConversionFractionalLiteralTypeError :: Text -> Double -> NumericType -> (Integer, Integer) -> Diagnostic
mkNumericConversionFractionalLiteralTypeError conversionName literalValue targetType (lowerBound, upperBound) =
  mkDiagnostic "E2006" $ "numeric conversion '" <> conversionName <> "' cannot convert fractional literal " <> tshow literalValue <> " to integral target " <> renderNumericTypeName targetType <> "; expected a finite integral value in range " <> tshow lowerBound <> ".." <> tshow upperBound

mkNumericConversionFloatLiteralOverflowError :: Text -> Double -> NumericType -> Double -> Diagnostic
mkNumericConversionFloatLiteralOverflowError conversionName literalValue targetType maxMagnitude =
  mkDiagnostic "E2006" $ "numeric conversion '" <> conversionName <> "' cannot convert fractional literal " <> tshow literalValue <> " outside finite " <> renderNumericTypeName targetType <> " magnitude " <> tshow maxMagnitude

mkTargetedFractionalLiteralOverflowError :: Double -> NumericType -> Double -> Diagnostic
mkTargetedFractionalLiteralOverflowError literalValue targetType maxMagnitude =
  mkDiagnostic "E2006" $ "fractional literal " <> tshow literalValue <> " cannot target finite " <> renderNumericTypeName targetType <> " magnitude " <> tshow maxMagnitude

mkBindingTypeMismatchError :: Text -> ExpressionType -> SourceSpan -> ExpressionType -> Diagnostic
mkBindingTypeMismatchError bindingName expectedType bindingSpan actualType =
  setDiagnosticPrimarySpan bindingSpan $ setDiagnosticSubject bindingName $
    mkDiagnostic "E2006" ("binding '" <> bindingName <> "' is used recursively as type " <> renderType expectedType <> " but its definition inferred " <> renderType actualType)

mkListElementTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkListElementTypeMismatchError expectedType foundType =
  mkDiagnostic "E2007" $ "list literal elements must have matching types, found " <> renderType expectedType <> " and " <> renderType foundType

mkUnsupportedSectionOperatorError :: Text -> Diagnostic
mkUnsupportedSectionOperatorError symbol = mkDiagnostic "E2008" ("unsupported operator section '" <> symbol <> "'")

mkUnsupportedOperatorValueError :: Text -> Diagnostic
mkUnsupportedOperatorValueError symbol = mkDiagnostic "E2003" ("builtin operator '" <> symbol <> "' has no value type rule")

mkNumericSectionOperandTypeError :: Text -> ExpressionType -> Diagnostic
mkNumericSectionOperandTypeError symbol operandType =
  mkDiagnostic "E2003" $ "operator section '" <> symbol <> "' requires a numeric operand, found " <> renderType operandType

mkTypeSchemeNumericConstraintError :: NumericConstraint -> ExpressionType -> Diagnostic
mkTypeSchemeNumericConstraintError _ foundType = mkDiagnostic "E2003" ("primitive numeric constraint cannot be satisfied by " <> renderType foundType)

mkTypeSchemeStrictEqualityConstraintError :: ExpressionType -> Diagnostic
mkTypeSchemeStrictEqualityConstraintError foundType = mkDiagnostic "E2004" ("primitive strict equality constraint cannot be satisfied by " <> renderType foundType)

mkMissingOperatorBindingError :: Text -> Diagnostic
mkMissingOperatorBindingError symbol = mkDiagnostic "E2010" ("operator '" <> symbol <> "' has no executable binding")

mkMissingClassMethodError, mkMissingImplMethodBodyError, mkAmbiguousQualifiedMethodBodyError :: Text -> Diagnostic
mkMissingClassMethodError key = withSubject key $ mkDiagnostic "E2015" ("missing class method '" <> key <> "'")
mkMissingImplMethodBodyError key = withSubject key $ mkDiagnostic "E2015" ("missing impl method body '" <> key <> "'")
mkAmbiguousQualifiedMethodBodyError key = withSubject key $ mkDiagnostic "E2015" ("ambiguous qualified method body '" <> key <> "'")

mkNoMatchingQualifiedMethodBodyError, mkAmbiguousQualifiedMethodBodyForArgumentsError :: Text -> [ExpressionType] -> Diagnostic
mkNoMatchingQualifiedMethodBodyError key types = withSubject key $ mkDiagnostic "E2015" ("no matching qualified method body '" <> key <> "' for argument types " <> renderTypes types)
mkAmbiguousQualifiedMethodBodyForArgumentsError key types = withSubject key $ mkDiagnostic "E2015" ("ambiguous qualified method body '" <> key <> "' for argument types " <> renderTypes types)

mkInvalidQualifiedMethodSignatureError :: Text -> SignaturePayload -> Diagnostic
mkInvalidQualifiedMethodSignatureError key payload =
  withSubject key $ mkDiagnostic "E2015" ("invalid or unsupported class method signature for '" <> key <> "': '" <> renderSignaturePayload payload <> "'")

mkImplMethodMissingClassMethodError :: Text -> SourceSpan -> Diagnostic
mkImplMethodMissingClassMethodError key spanValue = withSubject key $ setDiagnosticPrimarySpan spanValue $ mkDiagnostic "E2015" ("class method metadata for '" <> key <> "' must be declared before impl method body")

mkImplMethodTypeMismatchError :: Text -> SourceSpan -> ExpressionType -> ExpressionType -> Diagnostic
mkImplMethodTypeMismatchError key spanValue declaredType inferredType = withSubject key $ setDiagnosticPrimarySpan spanValue $ mkDiagnostic "E2016" ("impl method '" <> key <> "' declared as " <> renderType declaredType <> " but inferred as " <> renderType inferredType)

mkUnknownConstructorPayloadTypeError :: Name -> Diagnostic
mkUnknownConstructorPayloadTypeError name = mkDiagnostic "E2013" ("unknown constructor payload type '" <> identifierText name <> "' in generic data declaration")

mkMissingConstructorTypeParameterBindingError :: Text -> Diagnostic
mkMissingConstructorTypeParameterBindingError name = mkDiagnostic "E2013" ("internal constructor scheme error: missing binding for type parameter '" <> name <> "'")

mkMissingExplicitConstraintClassError :: Text -> Diagnostic
mkMissingExplicitConstraintClassError name = mkDiagnostic "E2009" ("missing class declaration '" <> name <> "'")

mkExplicitConstraintArityError :: Text -> Int -> Diagnostic
mkExplicitConstraintArityError name arity = mkDiagnostic "E2009" ("constraint '" <> name <> "' expects " <> tshow arity <> " argument(s), got 1")

mkMissingExplicitConstraintImplFactError :: Text -> Diagnostic
mkMissingExplicitConstraintImplFactError key = mkDiagnostic "E2009" ("missing impl fact '" <> key <> "'")

mkAmbiguousDeferredConstraintError :: Bool -> Text -> ExpressionType -> Diagnostic
mkAmbiguousDeferredConstraintError inferred name argumentType =
  if inferred
    then mkDiagnostic "E2009" $ "ambiguous/defaulting inferred constraint '" <> renderedConstraint <> "': inferred class constraints do not default unresolved type variables"
    else mkDiagnostic "E2009" $ "ambiguous/defaulting explicit constraint '" <> renderedConstraint <> "': explicit constrained signatures do not default unresolved type variables"
  where
    renderedConstraint = name <> "(" <> renderType argumentType <> ")"

mkPatternTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkPatternTypeMismatchError scrutineeType patternType = mkDiagnostic "E2011" ("case pattern of type " <> renderType patternType <> " does not match scrutinee type " <> renderType scrutineeType)

mkListPatternTypeMismatchError :: ExpressionType -> Diagnostic
mkListPatternTypeMismatchError scrutineeType = mkDiagnostic "E2011" ("case pattern of list type does not match scrutinee type " <> renderType scrutineeType)

mkTuplePatternTypeMismatchError :: ExpressionType -> Diagnostic
mkTuplePatternTypeMismatchError scrutineeType = mkDiagnostic "E2011" ("tuple case pattern does not match scrutinee type " <> renderType scrutineeType)

mkTuplePatternArityMismatchError :: Int -> Int -> Diagnostic
mkTuplePatternArityMismatchError patternArity scrutineeArity = mkDiagnostic "E2011" ("tuple case pattern expects " <> tshow patternArity <> " element(s), found " <> tshow scrutineeArity)

mkPatternBranchTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkPatternBranchTypeMismatchError leftType rightType = mkDiagnostic "E2012" ("case arms must have matching types, found " <> renderType leftType <> " and " <> renderType rightType)

mkIfConditionTypeError :: ExpressionType -> Diagnostic
mkIfConditionTypeError foundType = mkDiagnostic "E2001" ("if condition must have type Bool, found " <> renderType foundType)

mkCaseGuardTypeError :: ExpressionType -> Diagnostic
mkCaseGuardTypeError foundType = mkDiagnostic "E2001" ("case guard must have type Bool, found " <> renderType foundType)

mkIfBranchTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkIfBranchTypeMismatchError leftType rightType = mkDiagnostic "E2002" ("if branches must have matching types, found " <> renderType leftType <> " and " <> renderType rightType)

mkConstructorPatternArityError :: Text -> Int -> Int -> Diagnostic
mkConstructorPatternArityError name expected actual = mkDiagnostic "E2011" ("constructor case pattern '" <> name <> "' expects " <> tshow expected <> " argument(s), found " <> tshow actual)

mkUnknownConstructorPatternError :: Text -> Diagnostic
mkUnknownConstructorPatternError name = mkDiagnostic "E2011" ("unknown constructor case pattern '" <> name <> "'")

mkDuplicatePatternBinderError :: Name -> Diagnostic
mkDuplicatePatternBinderError name = mkDiagnostic "E2011" ("duplicate case pattern binder '" <> identifierText name <> "'")

mkEmptyOrPatternError :: Diagnostic
mkEmptyOrPatternError = mkDiagnostic "E2011" "or-pattern must contain at least one alternative"

mkOrPatternBinderSetMismatchError :: Set Name -> Set Name -> Diagnostic
mkOrPatternBinderSetMismatchError expected found = mkDiagnostic "E2011" ("or-pattern alternatives must bind the same names, expected " <> renderBinderSet expected <> " but found " <> renderBinderSet found)

mkOrPatternBinderTypeMismatchError :: Name -> ExpressionType -> ExpressionType -> Diagnostic
mkOrPatternBinderTypeMismatchError name leftType rightType = mkDiagnostic "E2011" ("or-pattern binder '" <> identifierText name <> "' has incompatible types " <> renderType leftType <> " and " <> renderType rightType)

renderType :: ExpressionType -> Text
renderType expressionType =
  case expressionType of
    TIntType -> "Int"
    TIntegerLiteralType {} -> "Int"
    TFloatType -> "Float"
    TNumericType numericType -> renderNumericTypeName numericType
    TBoolType -> "Bool"
    TListType elementType -> "[" <> renderType elementType <> "]"
    TTupleType elementTypes -> "(" <> renderTypes elementTypes <> ")"
    TDataType typeName [] -> identifierText typeName
    TDataType typeName typeArguments -> identifierText typeName <> "<" <> renderTypes typeArguments <> ">"
    TFunctionType inputType outputType -> renderTypeAtom inputType <> " -> " <> renderType outputType
    TVarType typeVar -> "t" <> tshow typeVar

renderTypeAtom :: ExpressionType -> Text
renderTypeAtom expressionType =
  case expressionType of
    TFunctionType {} -> "(" <> renderType expressionType <> ")"
    _ -> renderType expressionType

renderSignaturePayload :: SignaturePayload -> Text
renderSignaturePayload signaturePayload =
  case signaturePayload of
    SignatureType signatureType -> renderSignatureType signatureType
    ConstrainedSignature constraints signatureType ->
      "@{" <> Text.intercalate ", " (map renderSignatureConstraint constraints) <> "}: " <> renderConstraintSignatureType signatureType
    UnsupportedSignature tokens -> renderUnsupportedSignatureTokens tokens

renderSignatureConstraint :: SignatureConstraint -> Text
renderSignatureConstraint (SignatureConstraint name arguments) =
  identifierText name
    <> if null arguments
      then ""
      else "(" <> Text.intercalate ", " (map renderConstraintSignatureType arguments) <> ")"

renderSignatureType :: SignatureType -> Text
renderSignatureType signatureType =
  case signatureType of
    TypeInt -> "Int"
    TypeFloat -> "Float"
    TypeNumeric numericType -> renderNumericTypeName numericType
    TypeBool -> "Bool"
    TypeList innerType -> "[" <> renderSignatureTypeAtom innerType <> "]"
    TypeTuple elementTypes -> "(" <> Text.intercalate ", " (map renderSignatureType elementTypes) <> ")"
    TypeFunction argumentType resultType -> renderSignatureTypeAtom argumentType <> " -> " <> renderSignatureType resultType

renderSignatureTypeAtom :: SignatureType -> Text
renderSignatureTypeAtom signatureType =
  case signatureType of
    TypeFunction {} -> "(" <> renderSignatureType signatureType <> ")"
    _ -> renderSignatureType signatureType

renderUnsupportedSignatureTokens :: [SignatureToken] -> Text
renderUnsupportedSignatureTokens = Text.concat . go Nothing
  where
    go _ [] = []
    go previousToken (token : rest) =
      let prefix =
            case previousToken of
              Just previous
                | tokenNeedsLeadingSpace token && tokenNeedsTrailingSpace previous -> [" "]
              _ -> []
       in prefix <> [renderSignatureToken token] <> go (Just token) rest

tokenNeedsLeadingSpace :: SignatureToken -> Bool
tokenNeedsLeadingSpace token =
  case token of
    SignatureLParenToken -> False
    SignatureLBracketToken -> False
    SignatureLBraceToken -> False
    SignatureRParenToken -> False
    SignatureRBracketToken -> False
    SignatureRBraceToken -> False
    SignatureCommaToken -> False
    SignatureColonToken -> False
    SignatureArrowToken -> True
    _ -> True

tokenNeedsTrailingSpace :: SignatureToken -> Bool
tokenNeedsTrailingSpace token =
  case token of
    SignatureAtToken -> False
    SignatureLParenToken -> False
    SignatureLBracketToken -> False
    SignatureLBraceToken -> False
    _ -> True

renderSignatureToken :: SignatureToken -> Text
renderSignatureToken token =
  case token of
    SignatureNameToken name -> identifierText name
    SignatureIntToken value -> tshow value
    SignatureArrowToken -> "->"
    SignatureAtToken -> "@"
    SignatureColonToken -> ":"
    SignatureLParenToken -> "("
    SignatureRParenToken -> ")"
    SignatureLBraceToken -> "{"
    SignatureRBraceToken -> "}"
    SignatureLBracketToken -> "["
    SignatureRBracketToken -> "]"
    SignatureCommaToken -> ","
    SignatureOperatorToken symbol -> symbol
    SignatureOtherToken lexeme -> lexeme

renderTypes :: [ExpressionType] -> Text
renderTypes = Text.intercalate ", " . map renderType

renderBinderSet :: Set Name -> Text
renderBinderSet names = "{" <> Text.intercalate ", " (map identifierText (Set.toList names)) <> "}"

withSubject :: Text -> Diagnostic -> Diagnostic
withSubject = setDiagnosticSubject

tshow :: Show a => a -> Text
tshow = Text.pack . show
mkInvalidSignatureTypeError :: InferState -> Text -> SourceSpan -> SignaturePayload -> Diagnostic
mkInvalidSignatureTypeError state symbol signatureSpan signaturePayload =
  setDiagnosticSubject symbol $
    setDiagnosticPrimarySpan
      signatureSpan
      ( mkDiagnostic
          "E2009"
          (invalidSignatureSummary state symbol signaturePayload)
      )

invalidSignatureSummary :: InferState -> Text -> SignaturePayload -> Text
invalidSignatureSummary state symbol signaturePayload =
  case signaturePayload of
    ConstrainedSignature constraints _
      | Just duplicateName <- duplicateConstraintName constraints ->
          "invalid or unsupported signature for '"
            <> symbol
            <> "': duplicate constraint '"
            <> duplicateName
            <> "' in '"
            <> renderSignaturePayload signaturePayload
            <> "'"
    ConstrainedSignature constraints signatureType
      | constrainedSignatureHasTypeVariable constraints signatureType ->
          "invalid or unsupported signature for '"
            <> symbol
            <> "': type-variable constrained signatures require every constrained variable to appear in the signature body before inference can accept '"
            <> renderSignaturePayload signaturePayload
            <> "'"
    ConstrainedSignature constraints _
      | Just reason <- concreteConstraintFailureSummary state constraints ->
          "invalid or unsupported signature for '"
            <> symbol
            <> "': "
            <> reason
            <> " in '"
            <> renderSignaturePayload signaturePayload
            <> "'"
    _ ->
      "invalid or unsupported signature for '"
        <> symbol
        <> "': '"
        <> renderSignaturePayload signaturePayload
        <> "'"

concreteConstraintFailureSummary :: InferState -> [SignatureConstraint] -> Maybe Text
concreteConstraintFailureSummary state constraints
  | null constraints = Nothing
  | otherwise = firstJust (map constraintFailureSummary constraints)
  where
    constraintFailureSummary (SignatureConstraint constraintName arguments)
      | Nothing <- maybeClassArity =
          Just ("missing class declaration '" <> constraintNameText <> "'")
      | Just expectedArity <- maybeClassArity,
        expectedArity /= length arguments =
          Just
            ( "constraint '"
                <> constraintNameText
                <> "' expects "
                <> Text.pack (show expectedArity)
                <> " argument(s), got "
                <> Text.pack (show (length arguments))
            )
      | [argument] <- arguments,
        concreteConstraintArgument argument,
        let implFactKey = constraintImplFactKey constraintName argument,
        Set.notMember implFactKey (inferConcreteImplFacts state) =
          Just ("missing impl fact '" <> implFactKey <> "'")
      | otherwise =
          Nothing
      where
        constraintNameText = identifierText constraintName
        maybeClassArity = Map.lookup constraintNameText (inferClassFacts state)

    firstJust results =
      case results of
        [] -> Nothing
        Just result : _ -> Just result
        Nothing : rest -> firstJust rest

constrainedSignatureHasTypeVariable :: [SignatureConstraint] -> ConstraintSignatureType -> Bool
constrainedSignatureHasTypeVariable constraints signatureType =
  any constraintHasTypeVariable constraints
    || constraintTypeHasTypeVariable signatureType

constraintHasTypeVariable :: SignatureConstraint -> Bool
constraintHasTypeVariable (SignatureConstraint _ arguments) =
  any constraintTypeHasTypeVariable arguments

constraintTypeHasTypeVariable :: ConstraintSignatureType -> Bool
constraintTypeHasTypeVariable signatureType =
  case signatureType of
    ConstraintTypeName name ->
      identifierLooksLikeTypeVariable name
    ConstraintTypeApplication name arguments ->
      identifierLooksLikeTypeVariable name || any constraintTypeHasTypeVariable arguments
    ConstraintTypeList innerType ->
      constraintTypeHasTypeVariable innerType
    ConstraintTypeTuple elementTypes ->
      any constraintTypeHasTypeVariable elementTypes
    ConstraintTypeFunction argumentType resultType ->
      constraintTypeHasTypeVariable argumentType || constraintTypeHasTypeVariable resultType

duplicateConstraintName :: [SignatureConstraint] -> Maybe Text
duplicateConstraintName constraints =
  go Set.empty constraints
  where
    go seen remainingConstraints =
      case remainingConstraints of
        [] -> Nothing
        SignatureConstraint constraintName _ : rest ->
          let constraintNameText = identifierText constraintName
           in if Set.member constraintNameText seen
                then Just constraintNameText
                else go (Set.insert constraintNameText seen) rest
