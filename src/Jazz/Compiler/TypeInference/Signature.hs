{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Canonical conversion between surface signature types and inference types.
module Jazz.Compiler.TypeInference.Signature
  ( SignaturePayloadType (..),
    SignatureTypeFailure (..),
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
import Data.Void (absurd)
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
import Jazz.Compiler.SemanticDeclarations (ClassDefinition (..), SignatureTypeFailure (..), normalizeSignatureStructure, normalizeSignatureType, signatureVariableKindsAt)
import Jazz.Compiler.TypeInference.Solver
  ( freshTypeVars,
  )
import Jazz.Compiler.TypeInference.State
  ( InferState,
    inferClassFacts,
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
    Kind (..),
    pattern ConstrainedSignature,
    pattern SignatureConstraint,
    pattern SignatureType,
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
      let variableNames = dedupe (constraintSignatureTypeVariableNamesInOrder signatureType <> concat [concatMap constraintSignatureTypeVariableNamesInOrder arguments | SignatureConstraint _ arguments <- constraints])
          (variables, nextState) = allocateSignatureTypeVariables variableNames state
          variableOrder = [variable | name <- variableNames, Just (SemanticVariable variable) <- [Map.lookup name variables]]
          checked = do
            guard (isNothing (duplicateConstraintName constraints))
            expressionType <- either (const Nothing) Just (normalizeSignatureStructure (inferDataTypes state) variables signatureType)
            explicitConstraints <- traverse (checkConstraint variables) constraints
            _ <- either (const Nothing) Just (signatureVariableKindsAt (inferDataTypes state) Map.empty ((expressionType, TypeKind) : [(target, fmap absurd (classParameterKind definition)) | TypeSchemeConstraint capability target <- explicitConstraints, Just definition <- [Map.lookup capability (inferClassFacts state)]]))
            pure (SignaturePayloadType expressionType explicitConstraints variableOrder)
       in case checked of
            Just result -> (Just result, nextState)
            Nothing -> (Nothing, state)

    dedupe = foldr (\name rest -> name : filter (/= name) rest) []

    checkConstraint variables (SignatureConstraint capability [argument]) = do
      _ <- Map.lookup (CapabilityId capability) (inferClassFacts state)
      TypeSchemeConstraint (CapabilityId capability) <$> either (const Nothing) Just (normalizeSignatureStructure (inferDataTypes state) variables argument)
    checkConstraint _ _ = Nothing

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
        SignatureConstraint constraintName arguments : rest ->
          let constraintNameText = identifierText constraintName
           in if Set.member (CapabilityId constraintName, arguments) seen
                then Just constraintNameText
                else go (Set.insert (CapabilityId constraintName, arguments) seen) rest

tshow :: (Show a) => a -> Text
tshow = Text.pack . show
