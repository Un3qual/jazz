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
  ( concreteConstraintArgument,
    concreteImplFact,
    constraintSignatureTypeVariableNamesInOrder,
    identifierLooksLikeTypeVariable,
  )
import Jazz.Compiler.Name (identifierText)
import Jazz.Compiler.SemanticDeclarations (SignatureTypeFailure (..), normalizeSignatureType)
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
    SignatureType signatureType ->
      signaturePayloadFromType [] signatureType state
    ConstrainedSignature [] signatureType ->
      signaturePayloadFromType [] signatureType state
    ConstrainedSignature constraints signatureType
      | supportedVariableConstraints state constraints signatureType ->
          variableConstraintSignaturePayloadToExpressionType constraints signatureType state
      | supportedConcreteConstraints state constraints ->
          signaturePayloadFromType [] signatureType state
      | otherwise ->
          (Nothing, state)
    UnsupportedSignature {} ->
      (Nothing, state)

signaturePayloadFromType ::
  [TypeSchemeConstraint] ->
  SignatureType 'Resolved ->
  InferState ->
  (Maybe SignaturePayloadType, InferState)
signaturePayloadFromType explicitConstraints signatureType state =
  let variableNames = constraintSignatureTypeVariableNamesInOrder signatureType
      (signatureVariables, nextState) = allocateSignatureTypeVariables variableNames state
      variableOrder =
        [ typeVar
        | variableName <- variableNames,
          Just (SemanticVariable typeVar) <- [Map.lookup variableName signatureVariables]
        ]
   in case constraintSignatureTypeToExpressionTypeWithState nextState signatureVariables signatureType of
        Just expressionType ->
          (Just (SignaturePayloadType expressionType explicitConstraints variableOrder), nextState)
        Nothing -> (Nothing, state)

constraintSignatureTypeToExpressionType :: SignatureType 'Resolved -> Maybe ExpressionType
constraintSignatureTypeToExpressionType signatureType =
  either
    (const Nothing)
    Just
    (signatureTypeToExpressionType initialInferState Map.empty signatureType)

variableConstraintSignaturePayloadToExpressionType ::
  [SignatureConstraint 'Resolved] ->
  SignatureType 'Resolved ->
  InferState ->
  (Maybe SignaturePayloadType, InferState)
variableConstraintSignaturePayloadToExpressionType constraints signatureType state =
  let variableNames = constraintSignatureTypeVariableNamesInOrder signatureType
      (signatureVariables, nextState) = allocateSignatureTypeVariables variableNames state
      convertedType =
        constraintSignatureTypeToExpressionTypeWithState nextState signatureVariables signatureType
      convertedConstraints =
        traverse (variableConstraintToTypeSchemeConstraint signatureVariables) constraints
      variableOrder =
        [ typeVar
        | variableName <- variableNames,
          Just (SemanticVariable typeVar) <- [Map.lookup variableName signatureVariables]
        ]
   in case (convertedType, convertedConstraints) of
        (Just expressionType, Just explicitConstraints) ->
          (Just (SignaturePayloadType expressionType explicitConstraints variableOrder), nextState)
        _ -> (Nothing, state)

variableConstraintToTypeSchemeConstraint ::
  Map Text ExpressionType ->
  SignatureConstraint 'Resolved ->
  Maybe TypeSchemeConstraint
variableConstraintToTypeSchemeConstraint signatureVariables (SignatureConstraint constraintName arguments) =
  case arguments of
    [TypeVariable argumentName] ->
      TypeSchemeConstraint (identifierText constraintName)
        <$> Map.lookup (identifierText argumentName) signatureVariables
    _ -> Nothing

allocateSignatureTypeVariables :: [Text] -> InferState -> (Map Text ExpressionType, InferState)
allocateSignatureTypeVariables variableNames state =
  foldl' allocate (Map.empty, state) variableNames
  where
    allocate (signatureVariables, stateAcc) variableName =
      let (variableType, nextState) = freshTypeVar stateAcc
       in (Map.insert variableName variableType signatureVariables, nextState)

supportedConcreteConstraints :: InferState -> [SignatureConstraint 'Resolved] -> Bool
supportedConcreteConstraints state constraints =
  not (null constraints)
    && isNothing (duplicateConstraintName constraints)
    && all (supportedConcreteConstraint state) constraints

-- | Variable constrained signatures are accepted when every constrained
-- variable appears in the body; extra body variables remain unconstrained.
supportedVariableConstraints :: InferState -> [SignatureConstraint 'Resolved] -> SignatureType 'Resolved -> Bool
supportedVariableConstraints state constraints signatureType =
  not (null constraints)
    && isNothing (duplicateConstraintName constraints)
    && all (supportedVariableConstraint state) constraints
    && constraintSignatureTypeSupportsVariableBody signatureType
    && not (Set.null signatureVariableNames)
    && constraintVariableNames `Set.isSubsetOf` signatureVariableNames
  where
    signatureVariableNames =
      constraintSignatureTypeVariableNames signatureType
    constraintVariableNames =
      Set.unions (map constraintVariableNamesInSupportedConstraint constraints)

supportedConcreteConstraint :: InferState -> SignatureConstraint 'Resolved -> Bool
supportedConcreteConstraint state (SignatureConstraint constraintName arguments) =
  case (Map.lookup (identifierText constraintName) (inferClassFacts state), arguments) of
    (Just 1, [argument]) ->
      concreteConstraintArgument argument
        && maybe False (`Set.member` inferConcreteImplFacts state) (concreteImplFact constraintName [argument])
    _ -> False

supportedVariableConstraint :: InferState -> SignatureConstraint 'Resolved -> Bool
supportedVariableConstraint state (SignatureConstraint constraintName arguments) =
  case (Map.lookup (identifierText constraintName) (inferClassFacts state), arguments) of
    (Just 1, [TypeVariable {}]) -> True
    _ -> False

constraintVariableNamesInSupportedConstraint :: SignatureConstraint 'Resolved -> Set.Set Text
constraintVariableNamesInSupportedConstraint constraint =
  case constraint of
    SignatureConstraint _ [TypeVariable argumentName] ->
      Set.singleton (identifierText argumentName)
    _ -> Set.empty

constraintSignatureTypeVariableNames :: SignatureType 'Resolved -> Set.Set Text
constraintSignatureTypeVariableNames signatureType =
  case signatureType of
    TypeVariable name -> Set.singleton (identifierText name)
    TypeName name
      | identifierLooksLikeTypeVariable name ->
          Set.singleton (identifierText name)
      | otherwise ->
          Set.empty
    TypeApplication _ arguments ->
      Set.unions (map constraintSignatureTypeVariableNames arguments)
    TypeList innerType ->
      constraintSignatureTypeVariableNames innerType
    TypeTuple elementTypes ->
      Set.unions (map constraintSignatureTypeVariableNames elementTypes)
    TypeFunction argumentType resultType ->
      Set.union
        (constraintSignatureTypeVariableNames argumentType)
        (constraintSignatureTypeVariableNames resultType)
    _ -> Set.empty

constraintSignatureTypeSupportsVariableBody :: SignatureType 'Resolved -> Bool
constraintSignatureTypeSupportsVariableBody signatureType =
  case signatureType of
    TypeVariable {} -> True
    TypeName {} -> True
    TypeApplication _ arguments -> all constraintSignatureTypeSupportsVariableBody arguments
    TypeList innerType ->
      constraintSignatureTypeSupportsVariableBody innerType
    TypeTuple elementTypes ->
      all constraintSignatureTypeSupportsVariableBody elementTypes
    TypeFunction argumentType resultType ->
      constraintSignatureTypeSupportsVariableBody argumentType
        && constraintSignatureTypeSupportsVariableBody resultType
    _ -> True

duplicateConstraintName :: [SignatureConstraint 'Resolved] -> Maybe Text
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
