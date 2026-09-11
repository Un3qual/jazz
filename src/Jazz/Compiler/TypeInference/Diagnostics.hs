{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Type-inference diagnostics and error-state operations.
module Jazz.Compiler.TypeInference.Diagnostics
  ( addTypeError,
    annotateNewErrorsWithPrimarySpan,
    annotateNewErrorsWithContext,
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
    targetedFloatLiteralDiagnostic,
    renderSignaturePayload,
    renderType,
  )
where

import Data.Bifunctor (first)
import Data.Foldable (asum)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CorePhase (..),
    NumericType,
    SignatureConstraint,
    SignaturePayload,
    SignatureToken,
    SignatureType,
  )
import Jazz.Compiler.BuiltinCatalog
  ( numericTypeFloatMax,
    renderNumericTypeName,
  )
import Jazz.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    concreteImplFact,
    identifierLooksLikeTypeVariable,
    renderConcreteImplFact,
  )
import Jazz.Compiler.DiagnosticCatalog
  ( ErrorCode (..),
  )
import Jazz.Compiler.Diagnostics
  ( Diagnostic,
    DiagnosticContext,
    DiagnosticOrigin (..),
    SourceSpan,
    appendDiagnosticContext,
    diagnosticPrimarySpan,
    mkErrorDiagnostic,
    mkTypeErrorDiagnostic,
    setDiagnosticHelp,
    setDiagnosticPrimarySpan,
    setDiagnosticRelatedSpan,
    setDiagnosticSubject,
  )
import Jazz.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
  )
import Jazz.Compiler.Name (ResolvedName, identifierText)
import Jazz.Compiler.PatternCoverage (CoveragePattern, renderCoveragePattern)
import Jazz.Compiler.SignatureRendering
  ( renderSignatureType,
  )
import Jazz.Compiler.TypeInference.DiagnosticCause (TypeErrorCause (..), renderDiagnosticType)
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
  ( ExpressionType,
    NumericConstraint,
  )
import Jazz.Compiler.TypeRepresentation
  ( pattern ConstrainedSignature,
    pattern SignatureArrowToken,
    pattern SignatureAtToken,
    pattern SignatureColonToken,
    pattern SignatureCommaToken,
    pattern SignatureConstraint,
    pattern SignatureIntToken,
    pattern SignatureLBraceToken,
    pattern SignatureLBracketToken,
    pattern SignatureLParenToken,
    pattern SignatureNameToken,
    pattern SignatureOperatorToken,
    pattern SignatureOtherToken,
    pattern SignatureRBraceToken,
    pattern SignatureRBracketToken,
    pattern SignatureRParenToken,
    pattern SignatureType,
    pattern TypeApplication,
    pattern TypeFunction,
    pattern TypeList,
    pattern TypeName,
    pattern TypeTuple,
    pattern TypeVariable,
    pattern UnsupportedSignature,
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

-- | Add an enclosing operation only to errors emitted by this computation.
annotateNewErrorsWithContext :: DiagnosticContext -> SourceSpan -> InferState -> InferState -> InferState
annotateNewErrorsWithContext context spanValue previousState nextState =
  modifyInferenceOutput
    (\output -> output {outputErrorsRev = map (appendDiagnosticContext context) newErrors <> existingErrors})
    locatedState
  where
    locatedState = annotateNewErrorsWithPrimarySpan spanValue previousState nextState
    (newErrors, existingErrors) = splitAt (inferErrorCount nextState - inferErrorCount previousState) (inferErrorsRev locatedState)

mkInferenceTypeError :: ErrorCode -> TypeErrorCause ExpressionType -> Diagnostic
mkInferenceTypeError code = mkTypeErrorDiagnostic code . fmap (first identifierText)

mkNumericBinaryTypeError :: Text -> ExpressionType -> ExpressionType -> Diagnostic
mkNumericBinaryTypeError = mkBinaryTypeError

mkBinaryTypeError :: Text -> ExpressionType -> ExpressionType -> Diagnostic
mkBinaryTypeError operatorSymbol leftType rightType =
  mkInferenceTypeError E2003 (BinaryOperandTypeMismatch operatorSymbol leftType rightType)

mkStrictEqualityTypeError :: Text -> ExpressionType -> ExpressionType -> Diagnostic
mkStrictEqualityTypeError operatorSymbol leftType rightType =
  mkInferenceTypeError E2004 (StrictEqualityTypeMismatch operatorSymbol leftType rightType)

mkStrictEqualityUnsupportedTypeError :: Text -> ExpressionType -> Diagnostic
mkStrictEqualityUnsupportedTypeError operatorSymbol foundType =
  mkInferenceTypeError E2004 (UnsupportedStrictEqualityType operatorSymbol foundType)

mkDuplicateDataTypeDeclarationError :: Text -> SourceSpan -> Diagnostic
mkDuplicateDataTypeDeclarationError typeName spanValue =
  setDiagnosticSubject typeName $ setDiagnosticPrimarySpan spanValue $ mkErrorDiagnostic E2014 CompilationOrigin ("duplicate data type declaration '" <> typeName <> "'")

mkSignatureTypeMismatchError :: Text -> SourceSpan -> ExpressionType -> SourceSpan -> ExpressionType -> Diagnostic
mkSignatureTypeMismatchError bindingName signatureSpan declaredType bindingSpan inferredType =
  setDiagnosticSubject bindingName $
    setDiagnosticRelatedSpan bindingSpan $
      setDiagnosticPrimarySpan signatureSpan $
        mkInferenceTypeError E2005 (SignatureTypeMismatch bindingName declaredType inferredType)

mkApplyTypeError :: ExpressionType -> ExpressionType -> Diagnostic
mkApplyTypeError functionType argumentType =
  mkInferenceTypeError E2006 (ApplicationTypeMismatch functionType argumentType)

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

targetedFloatLiteralDiagnostic :: NumericType -> Double -> FractionalLiteralSource -> Maybe Diagnostic
targetedFloatLiteralDiagnostic targetType literalValue literalSource =
  case numericTypeFloatMax targetType of
    Just maxMagnitude
      | not (finiteFloat literalValue)
          || abs literalValue > maxMagnitude
          || fractionalLiteralExceedsMagnitude literalSource maxMagnitude ->
          Just (mkTargetedFractionalLiteralOverflowError literalValue targetType maxMagnitude)
    _ -> Nothing
  where
    finiteFloat value = not (isNaN value) && not (isInfinite value)

mkBindingTypeMismatchError :: Text -> ExpressionType -> SourceSpan -> ExpressionType -> Diagnostic
mkBindingTypeMismatchError bindingName expectedType bindingSpan actualType =
  setDiagnosticPrimarySpan bindingSpan $
    setDiagnosticSubject bindingName $
      mkInferenceTypeError E2006 (RecursiveBindingTypeMismatch bindingName expectedType actualType)

mkListElementTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkListElementTypeMismatchError expectedType foundType =
  mkInferenceTypeError E2007 (ListElementTypeMismatch expectedType foundType)

mkUnsupportedSectionOperatorError :: Text -> Diagnostic
mkUnsupportedSectionOperatorError symbol = mkErrorDiagnostic E2008 CompilationOrigin ("unsupported operator section '" <> symbol <> "'")

mkUnsupportedOperatorValueError :: Text -> Diagnostic
mkUnsupportedOperatorValueError symbol = mkErrorDiagnostic E2003 CompilationOrigin ("builtin operator '" <> symbol <> "' has no value type rule")

mkNumericSectionOperandTypeError :: Text -> ExpressionType -> Diagnostic
mkNumericSectionOperandTypeError symbol operandType =
  mkInferenceTypeError E2003 (NumericSectionOperandType symbol operandType)

mkTypeSchemeNumericConstraintError :: NumericConstraint -> ExpressionType -> Diagnostic
mkTypeSchemeNumericConstraintError _ foundType = mkInferenceTypeError E2003 (UnsatisfiedNumericConstraint foundType)

mkTypeSchemeStrictEqualityConstraintError :: ExpressionType -> Diagnostic
mkTypeSchemeStrictEqualityConstraintError foundType = mkInferenceTypeError E2004 (UnsatisfiedStrictEqualityConstraint foundType)

mkMissingOperatorBindingError :: Text -> Diagnostic
mkMissingOperatorBindingError symbol = mkErrorDiagnostic E2010 CompilationOrigin ("operator '" <> symbol <> "' has no executable binding")

mkMissingClassMethodError, mkMissingImplMethodBodyError, mkAmbiguousQualifiedMethodBodyError :: Text -> Diagnostic
mkMissingClassMethodError key = withSubject key $ mkErrorDiagnostic E2015 CompilationOrigin ("missing class method '" <> key <> "'")
mkMissingImplMethodBodyError key = withSubject key $ mkErrorDiagnostic E2015 CompilationOrigin ("missing impl method body '" <> key <> "'")
mkAmbiguousQualifiedMethodBodyError key = withSubject key $ mkErrorDiagnostic E2015 CompilationOrigin ("ambiguous qualified method body '" <> key <> "'")

mkNoMatchingQualifiedMethodBodyError, mkAmbiguousQualifiedMethodBodyForArgumentsError :: Text -> [ExpressionType] -> Diagnostic
mkNoMatchingQualifiedMethodBodyError key types = withSubject key $ mkInferenceTypeError E2015 (NoMatchingMethodArguments key types)
mkAmbiguousQualifiedMethodBodyForArgumentsError key types = withSubject key $ mkInferenceTypeError E2015 (AmbiguousMethodArguments key types)

mkInvalidQualifiedMethodSignatureError :: Text -> SignaturePayload 'Resolved -> Diagnostic
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
      mkInferenceTypeError E2009 (UndeclaredSignatureConstraint bindingName primitive constraintName argumentType)

mkImplMethodMissingClassMethodError :: Text -> SourceSpan -> Diagnostic
mkImplMethodMissingClassMethodError key spanValue = withSubject key $ setDiagnosticPrimarySpan spanValue $ mkErrorDiagnostic E2015 CompilationOrigin ("class method metadata for '" <> key <> "' must be declared before impl method body")

mkImplMethodTypeMismatchError :: Text -> SourceSpan -> ExpressionType -> ExpressionType -> Diagnostic
mkImplMethodTypeMismatchError key spanValue declaredType inferredType = withSubject key $ setDiagnosticPrimarySpan spanValue $ mkInferenceTypeError E2016 (ImplMethodTypeMismatch key declaredType inferredType)

mkUnknownConstructorPayloadTypeError :: ResolvedName -> Diagnostic
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
  mkInferenceTypeError E2009 (AmbiguousDeferredConstraint inferred name argumentType)

mkPatternTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkPatternTypeMismatchError scrutineeType patternType = mkInferenceTypeError E2011 (PatternTypeMismatch patternType scrutineeType)

mkListPatternTypeMismatchError :: ExpressionType -> Diagnostic
mkListPatternTypeMismatchError scrutineeType = mkInferenceTypeError E2011 (ListPatternTypeMismatch scrutineeType)

mkTuplePatternTypeMismatchError :: ExpressionType -> Diagnostic
mkTuplePatternTypeMismatchError scrutineeType = mkInferenceTypeError E2011 (TuplePatternTypeMismatch scrutineeType)

mkTuplePatternArityMismatchError :: Int -> Int -> Diagnostic
mkTuplePatternArityMismatchError patternArity scrutineeArity = mkErrorDiagnostic E2011 CompilationOrigin ("tuple case pattern expects " <> tshow patternArity <> " element(s), found " <> tshow scrutineeArity)

mkPatternBranchTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkPatternBranchTypeMismatchError leftType rightType = mkInferenceTypeError E2012 (PatternBranchTypeMismatch leftType rightType)

mkIfConditionTypeError :: ExpressionType -> Diagnostic
mkIfConditionTypeError foundType = mkInferenceTypeError E2001 (IfConditionTypeMismatch foundType)

mkCaseGuardTypeError :: ExpressionType -> Diagnostic
mkCaseGuardTypeError foundType = mkInferenceTypeError E2001 (CaseGuardTypeMismatch foundType)

mkIfBranchTypeMismatchError :: ExpressionType -> ExpressionType -> Diagnostic
mkIfBranchTypeMismatchError leftType rightType = mkInferenceTypeError E2002 (IfBranchTypeMismatch leftType rightType)

mkConstructorPatternArityError :: Text -> Int -> Int -> Diagnostic
mkConstructorPatternArityError name expected actual = mkErrorDiagnostic E2011 CompilationOrigin ("constructor case pattern '" <> name <> "' expects " <> tshow expected <> " argument(s), found " <> tshow actual)

mkUnknownConstructorPatternError :: Text -> Diagnostic
mkUnknownConstructorPatternError name = mkErrorDiagnostic E2011 CompilationOrigin ("unknown constructor case pattern '" <> name <> "'")

mkDuplicatePatternBinderError :: ResolvedName -> Diagnostic
mkDuplicatePatternBinderError name = mkErrorDiagnostic E2011 CompilationOrigin ("duplicate case pattern binder '" <> identifierText name <> "'")

mkEmptyOrPatternError :: Diagnostic
mkEmptyOrPatternError = mkErrorDiagnostic E2011 CompilationOrigin "or-pattern must contain at least one alternative"

mkOrPatternBinderSetMismatchError :: Set ResolvedName -> Set ResolvedName -> Diagnostic
mkOrPatternBinderSetMismatchError expected found = mkErrorDiagnostic E2011 CompilationOrigin ("or-pattern alternatives must bind the same names, expected " <> renderBinderSet expected <> " but found " <> renderBinderSet found)

mkOrPatternBinderTypeMismatchError :: ResolvedName -> ExpressionType -> ExpressionType -> Diagnostic
mkOrPatternBinderTypeMismatchError name leftType rightType = mkInferenceTypeError E2011 (OrPatternBinderTypeMismatch (identifierText name) leftType rightType)

mkNonExhaustivePatternMatchError :: CoveragePattern -> Diagnostic
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
renderType = renderDiagnosticType . first identifierText

renderSignaturePayload :: SignaturePayload 'Resolved -> Text
renderSignaturePayload signaturePayload =
  case signaturePayload of
    SignatureType signatureType -> renderSignatureType signatureType
    ConstrainedSignature constraints signatureType ->
      "@{" <> Text.intercalate ", " (map renderSignatureConstraint constraints) <> "}: " <> renderSignatureType signatureType
    UnsupportedSignature tokens -> renderUnsupportedSignatureTokens tokens

renderSignatureConstraint :: SignatureConstraint 'Resolved -> Text
renderSignatureConstraint (SignatureConstraint name arguments) =
  identifierText name
    <> if null arguments
      then ""
      else "(" <> Text.intercalate ", " (map renderSignatureType arguments) <> ")"

renderUnsupportedSignatureTokens :: [SignatureToken 'Resolved] -> Text
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

tokenNeedsLeadingSpace :: SignatureToken 'Resolved -> Bool
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

tokenNeedsTrailingSpace :: SignatureToken 'Resolved -> Bool
tokenNeedsTrailingSpace token =
  case token of
    SignatureAtToken -> False
    SignatureLParenToken -> False
    SignatureLBracketToken -> False
    SignatureLBraceToken -> False
    _ -> True

renderSignatureToken :: SignatureToken 'Resolved -> Text
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

renderBinderSet :: Set ResolvedName -> Text
renderBinderSet names = "{" <> Text.intercalate ", " (map identifierText (Set.toList names)) <> "}"

withSubject :: Text -> Diagnostic -> Diagnostic
withSubject = setDiagnosticSubject

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

mkInvalidSignatureTypeError :: InferState -> Text -> SourceSpan -> SignaturePayload 'Resolved -> Diagnostic
mkInvalidSignatureTypeError state symbol signatureSpan signaturePayload =
  setDiagnosticSubject symbol $
    setDiagnosticPrimarySpan
      signatureSpan
      ( mkErrorDiagnostic
          E2009
          CompilationOrigin
          (invalidSignatureSummary state symbol signaturePayload)
      )

invalidSignatureSummary :: InferState -> Text -> SignaturePayload 'Resolved -> Text
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

mkInvalidExplicitTypeApplicationArgumentError :: InferState -> SourceSpan -> SignatureType 'Resolved -> Diagnostic
mkInvalidExplicitTypeApplicationArgumentError state spanValue signatureType =
  setDiagnosticPrimarySpan spanValue $
    mkErrorDiagnostic
      E2009
      CompilationOrigin
      ( case signatureTypeFailureSummary state signatureType of
          Just reason -> reason
          Nothing -> "invalid or unsupported explicit type application argument '" <> renderSignatureType signatureType <> "'"
      )

mkInvalidImplTargetError :: SourceSpan -> Signature.SignatureTypeFailure -> Diagnostic
mkInvalidImplTargetError implSpan failure =
  setDiagnosticPrimarySpan implSpan (mkErrorDiagnostic E2009 CompilationOrigin ("invalid impl target: " <> Signature.renderSignatureTypeFailure failure))

signaturePayloadNamedTypeFailure :: InferState -> SignaturePayload 'Resolved -> Maybe Text
signaturePayloadNamedTypeFailure state payload =
  asum (map (declarationSignatureTypeFailureSummary state) payloadTypes)
  where
    payloadTypes =
      case payload of
        SignatureType signatureType -> [signatureType]
        ConstrainedSignature constraints signatureType ->
          signatureType : [argument | SignatureConstraint _ arguments <- constraints, argument <- arguments]
        UnsupportedSignature {} -> []

signatureTypeFailureSummary :: InferState -> SignatureType 'Resolved -> Maybe Text
signatureTypeFailureSummary state signatureType =
  case Signature.signatureTypeToExpressionType state Map.empty signatureType of
    Left failure -> Just (Signature.renderSignatureTypeFailure failure)
    Right _ -> Nothing

declarationSignatureTypeFailureSummary :: InferState -> SignatureType 'Resolved -> Maybe Text
declarationSignatureTypeFailureSummary state signatureType =
  case Signature.validateSignatureType state signatureType of
    Left failure -> Just (Signature.renderSignatureTypeFailure failure)
    Right () -> Nothing

concreteConstraintFailureSummary :: InferState -> [SignatureConstraint 'Resolved] -> Maybe Text
concreteConstraintFailureSummary state constraints
  | null constraints = Nothing
  | otherwise = asum (map constraintFailureSummary constraints)
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
        Just implFact <- concreteImplFact constraintName [argument],
        Set.notMember implFact (inferConcreteImplFacts state) =
          Just ("missing impl fact '" <> renderConcreteImplFact implFact <> "'")
      | otherwise =
          Nothing
      where
        constraintNameText = identifierText constraintName
        maybeClassArity = Map.lookup constraintNameText (inferClassFacts state)

constrainedSignatureHasTypeVariable :: [SignatureConstraint 'Resolved] -> SignatureType 'Resolved -> Bool
constrainedSignatureHasTypeVariable constraints signatureType =
  any constraintHasTypeVariable constraints
    || constraintTypeHasTypeVariable signatureType

constraintHasTypeVariable :: SignatureConstraint 'Resolved -> Bool
constraintHasTypeVariable (SignatureConstraint _ arguments) =
  any constraintTypeHasTypeVariable arguments

constraintTypeHasTypeVariable :: SignatureType 'Resolved -> Bool
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
