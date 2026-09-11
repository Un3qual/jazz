{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Canonical conversion between surface signature types and inference types.
module Jazz.Compiler.TypeInference.Signature
  ( SignaturePayloadType (..),
    SignatureTypeFailure (..),
    constraintSignatureTypeToExpressionType,
    constraintSignatureTypeToExpressionTypeWithState,
    duplicateConstraintName,
    expressionTypeToConcreteSignature,
    renderSignatureTypeFailure,
    signaturePayloadToSignatureType,
    signatureTypeToExpressionType,
    validateSignatureType,
  )
where

import Control.Monad (guard)
import Data.Functor (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( CorePhase (..),
    SignatureConstraint,
    SignaturePayload,
    SignatureType,
  )
import Jazz.Compiler.CapabilityFacts
  ( constraintSignatureTypeVariableNamesInOrder,
  )
import Jazz.Compiler.CoreIdentity (CapabilityId (..))
import Jazz.Compiler.Name (identifierText)
import Jazz.Compiler.SemanticDeclarations (ConcreteImplFact (..), SignatureTypeFailure (..), concreteImplementationType, normalizeSignatureType)
import Jazz.Compiler.TypeInference.Solver
  ( freshTypeVar,
  )
import Jazz.Compiler.TypeInference.State
  ( InferState,
    inferClassFacts,
    inferConcreteImplFacts,
    inferDataTypes,
    initialInferState,
  )
import Jazz.Compiler.TypeInference.Types
  ( ExpressionType,
    SchemeConstraint (..),
    SemanticType (..),
    TypeSchemeConstraint,
  )
import Jazz.Compiler.TypeRepresentation
  ( InferenceVariable (..),
    pattern ConstrainedSignature,
    pattern SignatureConstraint,
    pattern SignatureType,
    pattern TypeApplication,
    pattern TypeBool,
    pattern TypeChar,
    pattern TypeFloat,
    pattern TypeFunction,
    pattern TypeInt,
    pattern TypeList,
    pattern TypeName,
    pattern TypeNumeric,
    pattern TypeText,
    pattern TypeTuple,
    pattern TypeVariable,
    pattern UnsupportedSignature,
  )

signatureTypeToExpressionType ::
  InferState ->
  Map Text ExpressionType ->
  SignatureType 'Resolved ->
  Either SignatureTypeFailure ExpressionType
signatureTypeToExpressionType state =
  normalizeSignatureType (inferDataTypes state)

-- | Validate a declaration signature while treating its free variables as
-- universally quantified placeholders. Callers that require a concrete type
-- use 'signatureTypeToExpressionType' with an empty variable environment.
validateSignatureType :: InferState -> SignatureType 'Resolved -> Either SignatureTypeFailure ()
validateSignatureType state signatureType =
  void (signatureTypeToExpressionType state variables signatureType)
  where
    variables =
      Map.fromList
        [ (variableName, SemanticVariable (InferenceVariable (negate position - 1)))
        | (position, variableName) <-
            zip [0 :: Int ..] (constraintSignatureTypeVariableNamesInOrder signatureType)
        ]

renderSignatureTypeFailure :: SignatureTypeFailure -> Text
renderSignatureTypeFailure failure =
  case failure of
    UnknownNamedType name ->
      "unknown named type '" <> identifierText name <> "'"
    NamedTypeArityMismatch name expected received ->
      "type '"
        <> identifierText name
        <> "' expects "
        <> tshow expected
        <> " argument(s), found "
        <> tshow received
    TypeVariableApplicationHead name ->
      "type variable '" <> identifierText name <> "' cannot be used as an application head"
    UnboundSignatureTypeVariable name ->
      "unbound type variable '" <> identifierText name <> "'"

constraintSignatureTypeToExpressionTypeWithState ::
  InferState ->
  Map Text ExpressionType ->
  SignatureType 'Resolved ->
  Maybe ExpressionType
constraintSignatureTypeToExpressionTypeWithState state signatureVariables signatureType =
  either
    (const Nothing)
    Just
    (signatureTypeToExpressionType state signatureVariables signatureType)

data SignaturePayloadType = SignaturePayloadType
  { signaturePayloadDeclaredType :: ExpressionType,
    signaturePayloadExplicitConstraints :: [TypeSchemeConstraint],
    signaturePayloadVariableOrder :: [InferenceVariable]
  }

-- | Normalize the currently accepted signature subset. Unsupported surfaces
-- return `Nothing` so callers can emit the stable signature diagnostic.
signaturePayloadToSignatureType :: SignaturePayload 'Resolved -> InferState -> (Maybe SignaturePayloadType, InferState)
signaturePayloadToSignatureType signaturePayload state =
  case signaturePayload of
    SignatureType signatureType -> normalize [] signatureType
    ConstrainedSignature constraints signatureType -> normalize constraints signatureType
    UnsupportedSignature {} -> (Nothing, state)
  where
    normalize constraints signatureType =
      let variableNames = constraintSignatureTypeVariableNamesInOrder signatureType
          (variables, nextState) = allocateSignatureTypeVariables variableNames state
          variableOrder = [variable | name <- variableNames, Just (SemanticVariable variable) <- [Map.lookup name variables]]
          checked = do
            guard (isNothing (duplicateConstraintName constraints))
            expressionType <- constraintSignatureTypeToExpressionTypeWithState nextState variables signatureType
            explicitConstraints <- checkConstraints variables constraints
            pure (SignaturePayloadType expressionType explicitConstraints variableOrder)
       in case checked of
            Just result -> (Just result, nextState)
            Nothing -> (Nothing, state)

    checkConstraints variables constraints
      | all variableConstraint constraints = traverse (checkVariableConstraint variables) constraints
      | otherwise = [] <$ traverse checkConcreteConstraint constraints

    variableConstraint (SignatureConstraint _ [TypeVariable _]) = True
    variableConstraint _ = False

    checkVariableConstraint variables (SignatureConstraint capability [TypeVariable name]) = do
      guard (unaryCapability capability)
      TypeSchemeConstraint (CapabilityId capability) <$> Map.lookup (identifierText name) variables
    checkVariableConstraint _ _ = Nothing

    checkConcreteConstraint (SignatureConstraint capability [argument]) = do
      guard (unaryCapability capability)
      target <- either (const Nothing) Just (normalizeSignatureType (inferDataTypes state) Map.empty argument)
      guard (concreteImplementationType target)
      guard (Set.member (ConcreteImplFact (CapabilityId capability) target) (inferConcreteImplFacts state))
    checkConcreteConstraint _ = Nothing

    unaryCapability capability = Map.lookup (CapabilityId capability) (inferClassFacts state) == Just 1

constraintSignatureTypeToExpressionType :: SignatureType 'Resolved -> Maybe ExpressionType
constraintSignatureTypeToExpressionType signatureType =
  either
    (const Nothing)
    Just
    (signatureTypeToExpressionType initialInferState Map.empty signatureType)

allocateSignatureTypeVariables :: [Text] -> InferState -> (Map Text ExpressionType, InferState)
allocateSignatureTypeVariables variableNames state =
  foldl' allocate (Map.empty, state) variableNames
  where
    allocate (signatureVariables, stateAcc) variableName =
      let (variableType, nextState) = freshTypeVar stateAcc
       in (Map.insert variableName variableType signatureVariables, nextState)

duplicateConstraintName :: [SignatureConstraint 'Resolved] -> Maybe Text
duplicateConstraintName constraints =
  go Set.empty constraints
  where
    go seen remainingConstraints =
      case remainingConstraints of
        [] -> Nothing
        SignatureConstraint constraintName _ : rest ->
          let constraintNameText = identifierText constraintName
           in if Set.member (CapabilityId constraintName) seen
                then Just constraintNameText
                else go (Set.insert (CapabilityId constraintName) seen) rest

-- | Project a concrete inferred type for the signature-based capability rules.
-- Quantified variables have no concrete signature and propagate failure.
expressionTypeToConcreteSignature :: ExpressionType -> Maybe (SignatureType 'Resolved)
expressionTypeToConcreteSignature expressionType =
  case expressionType of
    SemanticInt -> Just TypeInt
    SemanticFloat -> Just TypeFloat
    SemanticNumeric numericType -> Just (TypeNumeric numericType)
    SemanticBool -> Just TypeBool
    SemanticChar -> Just TypeChar
    SemanticText -> Just TypeText
    SemanticList elementType ->
      TypeList <$> convert elementType
    SemanticTuple elementTypes ->
      TypeTuple <$> traverse convert elementTypes
    SemanticData typeName typeArguments ->
      case traverse convert typeArguments of
        Just [] -> Just (TypeName typeName)
        Just argumentTemplates -> Just (TypeApplication typeName argumentTemplates)
        Nothing -> Nothing
    SemanticFunction inputType outputType ->
      TypeFunction
        <$> convert inputType
        <*> convert outputType
    SemanticVariable _ -> Nothing
  where
    convert = expressionTypeToConcreteSignature

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
