{-# LANGUAGE OverloadedStrings #-}

-- | Type-inference diagnostics and error-state operations.
module Jazz.Compiler.TypeInference.Diagnostics
  ( addTypeError,
    annotateNewErrorsWithPrimarySpan,
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
    mkInvalidExplicitTypeApplicationArgumentError,
    mkExplicitTypeApplicationTargetError,
    mkIfBranchTypeMismatchError,
    mkIfConditionTypeError,
    mkImplMethodMissingClassMethodError,
    mkImplMethodTypeMismatchError,
    mkInvalidImplTargetError,
    mkInvalidSignatureTypeError,
    mkInvalidQualifiedMethodSignatureError,
    mkMethodLocalTypeVariableError,
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
    mkNonExhaustivePatternMatchError,
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
    mkUndeclaredSignatureConstraintError,
    mkTypeSchemeNumericConstraintError,
    mkTypeSchemeStrictEqualityConstraintError,
    mkUnknownConstructorPatternError,
    mkUnknownConstructorPayloadTypeError,
    mkUnreachablePatternArmError,
    mkInvalidConstructorPayloadTypeError,
    mkUnsupportedOperatorValueError,
    mkUnsupportedSectionOperatorError,
    renderSignaturePayload,
    renderType,
  )
where

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( NumericType,
    Pattern,
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..),
  )
import Jazz.Compiler.BuiltinCatalog
  ( renderNumericTypeName,
  )
import Jazz.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    constraintImplFactKey,
    identifierLooksLikeTypeVariable,
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticOrigin (..),
    SourceSpan,
    diagnosticPrimarySpan,
    mkErrorDiagnostic,
    setDiagnosticHelp,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject,
  )
import Jazz.Compiler.Name (Name, identifierText)
import Jazz.Compiler.PatternCoverage (renderCoveragePattern)
import Jazz.Compiler.SignatureRendering
  ( renderSignatureType,
  )
import qualified Jazz.Compiler.TypeInference.Signature as Signature
import Jazz.Compiler.TypeInference.State
  ( InferState (..),
    InferenceOutput (..),
    inferClassFacts,
    inferConcreteImplFacts,
    inferErrorCount,
    inferErrorsRev,
    modifyInferenceOutput,
  )
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType (..),
    NumericConstraint,
  )

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

mkNumericBinaryTypeError :: Text -> ExpressionType -> ExpressionType -> Diagnostic
mkNumericBinaryTypeError = mkBinaryTypeError

mkBinaryTypeError :: Text -> ExpressionType -> ExpressionType -> Diagnostic
mkBinaryTypeError operatorSymbol leftType rightType =
  mkErrorDiagnostic E2003 CompilationOrigin $ "cannot apply operator '" <> operatorSymbol <> "' to operands of type " <> renderType leftType <> " and " <> renderType rightType

mkStrictEqualityTypeError :: Text -> ExpressionType -> ExpressionType -> Diagnostic
mkStrictEqualityTypeError operatorSymbol leftType rightType =
  mkErrorDiagnostic E2004 CompilationOrigin $ "strict equality operator '" <> operatorSymbol <> "' requires operands of the same type, found " <> renderType leftType <> " and " <> renderType rightType

mkStrictEqualityUnsupportedTypeError :: Text -> ExpressionType -> Diagnostic
mkStrictEqualityUnsupportedTypeError operatorSymbol foundType =
  mkErrorDiagnostic E2004 CompilationOrigin $ "strict equality operator '" <> operatorSymbol <> "' is only supported for Bool, Char, Text, integral numeric, Float/Float16/Float32/Float64, lists and tuples containing equality-supported elements, and ADTs containing equality-supported constructor payloads, found " <> renderType foundType <> callableNote
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
  setDiagnosticSubject typeName $ setDiagnosticPrimarySpan spanValue $ mkErrorDiagnostic E2014 CompilationOrigin ("duplicate data type declaration '" <> typeName <> "'")

mkSignatureTypeMismatchError :: Text -> SourceSpan -> ExpressionType -> SourceSpan -> ExpressionType -> Diagnostic
mkSignatureTypeMismatchError bindingName signatureSpan declaredType bindingSpan inferredType =
  setDiagnosticSubject bindingName $
    setDiagnosticRelatedSpan bindingSpan $
      setDiagnosticPrimarySpan signatureSpan $
        mkErrorDiagnostic E2005 CompilationOrigin ("binding '" <> bindingName <> "' declared as " <> renderType declaredType <> " but inferred as " <> renderType inferredType)

mkApplyTypeError :: ExpressionType -> ExpressionType -> Diagnostic
mkApplyTypeError functionType argumentType =
  mkErrorDiagnostic E2006 CompilationOrigin $ "cannot apply function of type " <> renderType functionType <> " to argument of type " <> renderType argumentType

mkExplicitTypeApplicationTargetError :: Diagnostic
mkExplicitTypeApplicationTargetError = mkErrorDiagnostic E2017 CompilationOrigin "explicit type application target must be a generalized binding"

mkNumericConversionLiteralTypeError :: Text -> Integer -> NumericType -> (Integer, Integer) -> Diagnostic
mkNumericConversionLiteralTypeError conversionName literalValue targetType (lowerBound, upperBound) =
  mkErrorDiagnostic E2006 CompilationOrigin $ "numeric conversion '" <> conversionName <> "' cannot convert integer literal " <> tshow literalValue <> " outside " <> renderNumericTypeName targetType <> " range " <> tshow lowerBound <> ".." <> tshow upperBound

mkNumericConversionFractionalLiteralTypeError :: Text -> Double -> NumericType -> (Integer, Integer) -> Diagnostic
mkNumericConversionFractionalLiteralTypeError conversionName literalValue targetType (lowerBound, upperBound) =
  mkErrorDiagnostic E2006 CompilationOrigin $ "numeric conversion '" <> conversionName <> "' cannot convert fractional literal " <> tshow literalValue <> " to integral target " <> renderNumericTypeName targetType <> "; expected a finite integral value in range " <> tshow lowerBound <> ".." <> tshow upperBound

mkNumericConversionFloatLiteralOverflowError :: Text -> Double -> NumericType -> Double -> Diagnostic
mkNumericConversionFloatLiteralOverflowError conversionName literalValue targetType maxMagnitude =
  mkErrorDiagnostic E2006 CompilationOrigin $ "numeric conversion '" <> conversionName <> "' cannot convert fractional literal " <> tshow literalValue <> " outside finite " <> renderNumericTypeName targetType <> " magnitude " <> tshow maxMagnitude

mkTargetedFractionalLiteralOverflowError :: Double -> NumericType -> Double -> Diagnostic
mkTargetedFractionalLiteralOverflowError literalValue targetType maxMagnitude =
  mkErrorDiagnostic E2006 CompilationOrigin $ "fractional literal " <> tshow literalValue <> " cannot target finite " <> renderNumericTypeName targetType <> " magnitude " <> tshow maxMagnitude

mkBindingTypeMismatchError :: Text -> ExpressionType -> SourceSpan -> ExpressionType -> Diagnostic
mkBindingTypeMismatchError bindingName expectedType bindingSpan actualType =
  setDiagnosticPrimarySpan bindingSpan $
    setDiagnosticSubject bindingName $
      mkErrorDiagnostic E2006 CompilationOrigin ("binding '" <> bindingName <> "' is used recursively as type " <> renderType expectedType <> " but its definition inferred " <> renderType actualType)

mkListElementTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkListElementTypeMismatchError expectedType foundType =
  mkErrorDiagnostic E2007 CompilationOrigin $ "list literal elements must have matching types, found " <> renderType expectedType <> " and " <> renderType foundType

mkUnsupportedSectionOperatorError :: Text -> Diagnostic
mkUnsupportedSectionOperatorError symbol = mkErrorDiagnostic E2008 CompilationOrigin ("unsupported operator section '" <> symbol <> "'")

mkUnsupportedOperatorValueError :: Text -> Diagnostic
mkUnsupportedOperatorValueError symbol = mkErrorDiagnostic E2003 CompilationOrigin ("builtin operator '" <> symbol <> "' has no value type rule")

mkNumericSectionOperandTypeError :: Text -> ExpressionType -> Diagnostic
mkNumericSectionOperandTypeError symbol operandType =
  mkErrorDiagnostic E2003 CompilationOrigin $ "operator section '" <> symbol <> "' requires a numeric operand, found " <> renderType operandType

mkTypeSchemeNumericConstraintError :: NumericConstraint -> ExpressionType -> Diagnostic
mkTypeSchemeNumericConstraintError _ foundType = mkErrorDiagnostic E2003 CompilationOrigin ("primitive numeric constraint cannot be satisfied by " <> renderType foundType)

mkTypeSchemeStrictEqualityConstraintError :: ExpressionType -> Diagnostic
mkTypeSchemeStrictEqualityConstraintError foundType = mkErrorDiagnostic E2004 CompilationOrigin ("primitive strict equality constraint cannot be satisfied by " <> renderType foundType)

mkMissingOperatorBindingError :: Text -> Diagnostic
mkMissingOperatorBindingError symbol = mkErrorDiagnostic E2010 CompilationOrigin ("operator '" <> symbol <> "' has no executable binding")

mkMissingClassMethodError, mkMissingImplMethodBodyError, mkAmbiguousQualifiedMethodBodyError :: Text -> Diagnostic
mkMissingClassMethodError key = withSubject key $ mkErrorDiagnostic E2015 CompilationOrigin ("missing class method '" <> key <> "'")
mkMissingImplMethodBodyError key = withSubject key $ mkErrorDiagnostic E2015 CompilationOrigin ("missing impl method body '" <> key <> "'")
mkAmbiguousQualifiedMethodBodyError key = withSubject key $ mkErrorDiagnostic E2015 CompilationOrigin ("ambiguous qualified method body '" <> key <> "'")

mkNoMatchingQualifiedMethodBodyError, mkAmbiguousQualifiedMethodBodyForArgumentsError :: Text -> [ExpressionType] -> Diagnostic
mkNoMatchingQualifiedMethodBodyError key types = withSubject key $ mkErrorDiagnostic E2015 CompilationOrigin ("no matching qualified method body '" <> key <> "' for argument types " <> renderTypes types)
mkAmbiguousQualifiedMethodBodyForArgumentsError key types = withSubject key $ mkErrorDiagnostic E2015 CompilationOrigin ("ambiguous qualified method body '" <> key <> "' for argument types " <> renderTypes types)

mkInvalidQualifiedMethodSignatureError :: Text -> SignaturePayload -> Diagnostic
mkInvalidQualifiedMethodSignatureError key payload =
  withSubject key $ mkErrorDiagnostic E2015 CompilationOrigin ("invalid or unsupported class method signature for '" <> key <> "': '" <> renderSignaturePayload payload <> "'")

mkMethodLocalTypeVariableError :: Text -> Text -> SourceSpan -> Diagnostic
mkMethodLocalTypeVariableError methodKey variableName methodSpan =
  withSubject methodKey $
    setDiagnosticPrimarySpan methodSpan $
      mkErrorDiagnostic
        E2009
        CompilationOrigin
        ( "class method '"
            <> methodKey
            <> "' uses unsupported method-local type variable '"
            <> variableName
            <> "'; only declared class parameters may appear"
        )

mkUndeclaredSignatureConstraintError :: Text -> Bool -> Text -> ExpressionType -> SourceSpan -> Diagnostic
mkUndeclaredSignatureConstraintError bindingName primitive constraintName argumentType signatureSpan =
  withSubject bindingName $
    setDiagnosticPrimarySpan signatureSpan $
      mkErrorDiagnostic
        E2009
        CompilationOrigin
        ( "signature for '"
            <> bindingName
            <> "' does not declare required "
            <> (if primitive then "primitive " else "")
            <> "constraint '"
            <> constraintName
            <> "("
            <> renderType argumentType
            <> ")'"
        )

mkImplMethodMissingClassMethodError :: Text -> SourceSpan -> Diagnostic
mkImplMethodMissingClassMethodError key spanValue = withSubject key $ setDiagnosticPrimarySpan spanValue $ mkErrorDiagnostic E2015 CompilationOrigin ("class method metadata for '" <> key <> "' must be declared before impl method body")

mkImplMethodTypeMismatchError :: Text -> SourceSpan -> ExpressionType -> ExpressionType -> Diagnostic
mkImplMethodTypeMismatchError key spanValue declaredType inferredType = withSubject key $ setDiagnosticPrimarySpan spanValue $ mkErrorDiagnostic E2016 CompilationOrigin ("impl method '" <> key <> "' declared as " <> renderType declaredType <> " but inferred as " <> renderType inferredType)

mkUnknownConstructorPayloadTypeError :: Name -> Diagnostic
mkUnknownConstructorPayloadTypeError name = mkErrorDiagnostic E2013 CompilationOrigin ("unknown constructor payload type '" <> identifierText name <> "' in generic data declaration")

mkInvalidConstructorPayloadTypeError :: Text -> Diagnostic
mkInvalidConstructorPayloadTypeError detail =
  mkErrorDiagnostic E2013 CompilationOrigin ("invalid constructor payload type: " <> detail)

mkMissingConstructorTypeParameterBindingError :: Text -> Diagnostic
mkMissingConstructorTypeParameterBindingError name = mkErrorDiagnostic E2013 CompilationOrigin ("internal constructor scheme error: missing binding for type parameter '" <> name <> "'")

mkMissingExplicitConstraintClassError :: Text -> Diagnostic
mkMissingExplicitConstraintClassError name = mkErrorDiagnostic E2009 CompilationOrigin ("missing class declaration '" <> name <> "'")

mkExplicitConstraintArityError :: Text -> Int -> Diagnostic
mkExplicitConstraintArityError name arity = mkErrorDiagnostic E2009 CompilationOrigin ("constraint '" <> name <> "' expects " <> tshow arity <> " argument(s), got 1")

mkMissingExplicitConstraintImplFactError :: Text -> Diagnostic
mkMissingExplicitConstraintImplFactError key = mkErrorDiagnostic E2009 CompilationOrigin ("missing impl fact '" <> key <> "'")

mkAmbiguousDeferredConstraintError :: Bool -> Text -> ExpressionType -> Diagnostic
mkAmbiguousDeferredConstraintError inferred name argumentType =
  if inferred
    then mkErrorDiagnostic E2009 CompilationOrigin $ "ambiguous/defaulting inferred constraint '" <> renderedConstraint <> "': inferred class constraints do not default unresolved type variables"
    else mkErrorDiagnostic E2009 CompilationOrigin $ "ambiguous/defaulting explicit constraint '" <> renderedConstraint <> "': explicit constrained signatures do not default unresolved type variables"
  where
    renderedConstraint = name <> "(" <> renderType argumentType <> ")"

mkPatternTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkPatternTypeMismatchError scrutineeType patternType = mkErrorDiagnostic E2011 CompilationOrigin ("case pattern of type " <> renderType patternType <> " does not match scrutinee type " <> renderType scrutineeType)

mkListPatternTypeMismatchError :: ExpressionType -> Diagnostic
mkListPatternTypeMismatchError scrutineeType = mkErrorDiagnostic E2011 CompilationOrigin ("case pattern of list type does not match scrutinee type " <> renderType scrutineeType)

mkTuplePatternTypeMismatchError :: ExpressionType -> Diagnostic
mkTuplePatternTypeMismatchError scrutineeType = mkErrorDiagnostic E2011 CompilationOrigin ("tuple case pattern does not match scrutinee type " <> renderType scrutineeType)

mkTuplePatternArityMismatchError :: Int -> Int -> Diagnostic
mkTuplePatternArityMismatchError patternArity scrutineeArity = mkErrorDiagnostic E2011 CompilationOrigin ("tuple case pattern expects " <> tshow patternArity <> " element(s), found " <> tshow scrutineeArity)

mkPatternBranchTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkPatternBranchTypeMismatchError leftType rightType = mkErrorDiagnostic E2012 CompilationOrigin ("case arms must have matching types, found " <> renderType leftType <> " and " <> renderType rightType)

mkIfConditionTypeError :: ExpressionType -> Diagnostic
mkIfConditionTypeError foundType = mkErrorDiagnostic E2001 CompilationOrigin ("if condition must have type Bool, found " <> renderType foundType)

mkCaseGuardTypeError :: ExpressionType -> Diagnostic
mkCaseGuardTypeError foundType = mkErrorDiagnostic E2001 CompilationOrigin ("case guard must have type Bool, found " <> renderType foundType)

mkIfBranchTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkIfBranchTypeMismatchError leftType rightType = mkErrorDiagnostic E2002 CompilationOrigin ("if branches must have matching types, found " <> renderType leftType <> " and " <> renderType rightType)

mkConstructorPatternArityError :: Text -> Int -> Int -> Diagnostic
mkConstructorPatternArityError name expected actual = mkErrorDiagnostic E2011 CompilationOrigin ("constructor case pattern '" <> name <> "' expects " <> tshow expected <> " argument(s), found " <> tshow actual)

mkUnknownConstructorPatternError :: Text -> Diagnostic
mkUnknownConstructorPatternError name = mkErrorDiagnostic E2011 CompilationOrigin ("unknown constructor case pattern '" <> name <> "'")

mkDuplicatePatternBinderError :: Name -> Diagnostic
mkDuplicatePatternBinderError name = mkErrorDiagnostic E2011 CompilationOrigin ("duplicate case pattern binder '" <> identifierText name <> "'")

mkEmptyOrPatternError :: Diagnostic
mkEmptyOrPatternError = mkErrorDiagnostic E2011 CompilationOrigin "or-pattern must contain at least one alternative"

mkOrPatternBinderSetMismatchError :: Set Name -> Set Name -> Diagnostic
mkOrPatternBinderSetMismatchError expected found = mkErrorDiagnostic E2011 CompilationOrigin ("or-pattern alternatives must bind the same names, expected " <> renderBinderSet expected <> " but found " <> renderBinderSet found)

mkOrPatternBinderTypeMismatchError :: Name -> ExpressionType -> ExpressionType -> Diagnostic
mkOrPatternBinderTypeMismatchError name leftType rightType = mkErrorDiagnostic E2011 CompilationOrigin ("or-pattern binder '" <> identifierText name <> "' has incompatible types " <> renderType leftType <> " and " <> renderType rightType)

mkNonExhaustivePatternMatchError :: Pattern -> Diagnostic
mkNonExhaustivePatternMatchError missingPattern =
  setDiagnosticHelp
    "add an unguarded arm that covers the missing pattern"
    ( mkErrorDiagnostic
        E2018
        CompilationOrigin
        ("non-exhaustive pattern match; missing pattern: " <> renderCoveragePattern missingPattern)
    )

mkUnreachablePatternArmError :: Int -> Diagnostic
mkUnreachablePatternArmError armIndex =
  mkErrorDiagnostic
    E2019
    CompilationOrigin
    ("pattern arm " <> tshow armIndex <> " is unreachable because earlier unguarded arms cover it")

renderType :: ExpressionType -> Text
renderType expressionType =
  case expressionType of
    TIntType -> "Int"
    TIntegerLiteralType {} -> "Int"
    TFloatType -> "Float"
    TNumericType numericType -> renderNumericTypeName numericType
    TBoolType -> "Bool"
    TCharType -> "Char"
    TTextType -> "Text"
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
      "@{" <> Text.intercalate ", " (map renderSignatureConstraint constraints) <> "}: " <> renderSignatureType signatureType
    UnsupportedSignature tokens -> renderUnsupportedSignatureTokens tokens

renderSignatureConstraint :: SignatureConstraint -> Text
renderSignatureConstraint (SignatureConstraint name arguments) =
  identifierText name
    <> if null arguments
      then ""
      else "(" <> Text.intercalate ", " (map renderSignatureType arguments) <> ")"

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

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

mkInvalidSignatureTypeError :: InferState -> Text -> SourceSpan -> SignaturePayload -> Diagnostic
mkInvalidSignatureTypeError state symbol signatureSpan signaturePayload =
  setDiagnosticSubject symbol $
    setDiagnosticPrimarySpan
      signatureSpan
      ( mkErrorDiagnostic
          E2009
          CompilationOrigin
          (invalidSignatureSummary state symbol signaturePayload)
      )

invalidSignatureSummary :: InferState -> Text -> SignaturePayload -> Text
invalidSignatureSummary state symbol signaturePayload =
  case signaturePayloadNamedTypeFailure state signaturePayload of
    Just reason ->
      "invalid or unsupported signature for '" <> symbol <> "': " <> reason
    Nothing ->
      case signaturePayload of
        ConstrainedSignature constraints _
          | Just duplicateName <- Signature.duplicateConstraintName constraints ->
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

mkInvalidExplicitTypeApplicationArgumentError :: InferState -> SourceSpan -> SignatureType -> Diagnostic
mkInvalidExplicitTypeApplicationArgumentError state spanValue signatureType =
  setDiagnosticPrimarySpan spanValue $
    mkErrorDiagnostic
      E2009
      CompilationOrigin
      ( case signatureTypeFailureSummary state signatureType of
          Just reason -> reason
          Nothing -> "invalid or unsupported explicit type application argument '" <> renderSignatureType signatureType <> "'"
      )

mkInvalidImplTargetError :: InferState -> SourceSpan -> SignatureType -> Maybe Diagnostic
mkInvalidImplTargetError state implSpan signatureType =
  case signatureTypeFailureSummary state signatureType of
    Just failureSummary ->
      Just
        ( setDiagnosticPrimarySpan
            implSpan
            (mkErrorDiagnostic E2009 CompilationOrigin ("invalid impl target: " <> failureSummary))
        )
    Nothing -> Nothing

signaturePayloadNamedTypeFailure :: InferState -> SignaturePayload -> Maybe Text
signaturePayloadNamedTypeFailure state payload =
  firstJust (map (declarationSignatureTypeFailureSummary state) payloadTypes)
  where
    payloadTypes =
      case payload of
        SignatureType signatureType -> [signatureType]
        ConstrainedSignature constraints signatureType ->
          signatureType : [argument | SignatureConstraint _ arguments <- constraints, argument <- arguments]
        UnsupportedSignature {} -> []

signatureTypeFailureSummary :: InferState -> SignatureType -> Maybe Text
signatureTypeFailureSummary state signatureType =
  case Signature.signatureTypeToExpressionType state Map.empty signatureType of
    Left failure -> Just (Signature.renderSignatureTypeFailure failure)
    Right _ -> Nothing

declarationSignatureTypeFailureSummary :: InferState -> SignatureType -> Maybe Text
declarationSignatureTypeFailureSummary state signatureType =
  case Signature.validateSignatureType state signatureType of
    Left failure -> Just (Signature.renderSignatureTypeFailure failure)
    Right () -> Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust results =
  case results of
    [] -> Nothing
    Just result : _ -> Just result
    Nothing : rest -> firstJust rest

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

constrainedSignatureHasTypeVariable :: [SignatureConstraint] -> SignatureType -> Bool
constrainedSignatureHasTypeVariable constraints signatureType =
  any constraintHasTypeVariable constraints
    || constraintTypeHasTypeVariable signatureType

constraintHasTypeVariable :: SignatureConstraint -> Bool
constraintHasTypeVariable (SignatureConstraint _ arguments) =
  any constraintTypeHasTypeVariable arguments

constraintTypeHasTypeVariable :: SignatureType -> Bool
constraintTypeHasTypeVariable signatureType =
  case signatureType of
    TypeVariable {} -> True
    TypeName name ->
      identifierLooksLikeTypeVariable name
    TypeApplication name arguments ->
      identifierLooksLikeTypeVariable name || any constraintTypeHasTypeVariable arguments
    TypeList innerType ->
      constraintTypeHasTypeVariable innerType
    TypeTuple elementTypes ->
      any constraintTypeHasTypeVariable elementTypes
    TypeFunction argumentType resultType ->
      constraintTypeHasTypeVariable argumentType || constraintTypeHasTypeVariable resultType
    _ -> False
