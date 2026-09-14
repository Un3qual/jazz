{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Canonical conversion between surface signature types and inference types.
module Jazz.Compiler.TypeInference.Signature
  ( SignaturePayloadType (..),
    SignatureTypeFailure (..),
    constraintSignatureTypeToExpressionTypeWithState,
    duplicateConstraintName,
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
  ( freshTypeVars,
  )
import Jazz.Compiler.TypeInference.State
  ( InferState,
    inferClassFacts,
    inferConcreteImplFacts,
    inferDataTypes,
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
    SignatureKindMismatch message -> "kind mismatch: " <> message
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

    unaryCapability capability = Map.member (CapabilityId capability) (inferClassFacts state)

allocateSignatureTypeVariables :: [Text] -> InferState -> (Map Text ExpressionType, InferState)
allocateSignatureTypeVariables variableNames state =
  let (variableTypes, nextState) = freshTypeVars (length variableNames) state
   in (Map.fromList (zip variableNames variableTypes), nextState)

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

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
