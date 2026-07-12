{-# LANGUAGE OverloadedStrings #-}

-- | Canonical conversion between surface signature types and inference types.
module JazzNext.Compiler.TypeInference.Signature
  ( SignaturePayloadType (..),
    SignatureTypeFailure (..),
    constraintSignatureTypeToExpressionType,
    constraintSignatureTypeToExpressionTypeWithState,
    duplicateConstraintName,
    expressionTypeToRuntimeHint,
    expressionTypeToRuntimeTemplate,
    renderSignatureTypeFailure,
    signaturePayloadToSignatureType,
    signatureTypeToExpressionType,
    validateSignatureType
  ) where

import Control.Applicative ((<|>))
import Data.Functor (void)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( NumericType (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureType (..)
  )
import JazzNext.Compiler.BuiltinCatalog (numericTypeFromName)
import JazzNext.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    constraintImplFactKey,
    constraintSignatureTypeVariableNamesInOrder,
    identifierLooksLikeTypeVariable
  )
import JazzNext.Compiler.Name (Name, identifierText)
import JazzNext.Compiler.TypeInference.Solver
  ( freshTypeVar,
    integerLiteralRangeFitsNumericType
  )
import JazzNext.Compiler.TypeInference.State
  ( InferState,
    inferClassFacts,
    inferConcreteImplFacts,
    inferDataTypes,
    initialInferState
  )
import JazzNext.Compiler.TypeInference.Types
  ( DataTypeBinding (..),
    ExpressionType (..),
    TypeSchemeConstraint (..)
  )

data SignatureTypeFailure
  = UnknownNamedType Name
  | NamedTypeArityMismatch Name Int Int
  | TypeVariableApplicationHead Name
  | UnboundSignatureTypeVariable Name
  deriving (Eq, Show)

signatureTypeToExpressionType ::
  InferState ->
  Map Text ExpressionType ->
  SignatureType ->
  Either SignatureTypeFailure ExpressionType
signatureTypeToExpressionType state =
  convertSignatureType (inferDataTypes state)

-- | Validate a declaration signature while treating its free variables as
-- universally quantified placeholders. Callers that require a concrete type
-- use 'signatureTypeToExpressionType' with an empty variable environment.
validateSignatureType :: InferState -> SignatureType -> Either SignatureTypeFailure ()
validateSignatureType state signatureType =
  void (signatureTypeToExpressionType state variables signatureType)
  where
    variables =
      Map.fromList
        [ (variableName, TVarType (negate position - 1))
          | (position, variableName) <-
              zip [0 :: Int ..] (constraintSignatureTypeVariableNamesInOrder signatureType)
        ]

convertSignatureType ::
  Map Text DataTypeBinding ->
  Map Text ExpressionType ->
  SignatureType ->
  Either SignatureTypeFailure ExpressionType
convertSignatureType dataTypes variables signatureType =
  case signatureType of
    TypeInt -> Right TIntType
    TypeFloat -> Right TFloatType
    TypeNumeric numericType -> Right (TNumericType numericType)
    TypeBool -> Right TBoolType
    TypeChar -> Right TCharType
    TypeText -> Right TTextType
    TypeVariable name ->
      maybe (Left (UnboundSignatureTypeVariable name)) Right
        (Map.lookup (identifierText name) variables)
    TypeName name ->
      case builtinOrVariableType name of
        Just expressionType -> Right expressionType
        Nothing -> namedType name []
    TypeApplication name arguments
      | identifierLooksLikeTypeVariable name ->
          Left (TypeVariableApplicationHead name)
      | otherwise -> namedType name arguments
    TypeList innerType ->
      TListType <$> convert innerType
    TypeTuple elementTypes ->
      TTupleType <$> traverse convert elementTypes
    TypeFunction argumentType resultType ->
      TFunctionType <$> convert argumentType <*> convert resultType
  where
    convert = convertSignatureType dataTypes variables

    builtinOrVariableType name =
      case identifierText name of
        "Int" -> Just TIntType
        "Float" -> Just TFloatType
        "Bool" -> Just TBoolType
        "Char" -> Just TCharType
        "Text" -> Just TTextType
        typeName ->
          (TNumericType <$> numericTypeFromName typeName)
            <|> Map.lookup typeName variables

    namedType name arguments =
      case Map.lookup (identifierText name) dataTypes of
        Nothing -> Left (UnknownNamedType name)
        Just (DataTypeBinding parameters _)
          | length parameters /= length arguments ->
              Left (NamedTypeArityMismatch name (length parameters) (length arguments))
          | otherwise ->
              TDataType name <$> traverse convert arguments

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
  SignatureType ->
  Maybe ExpressionType
constraintSignatureTypeToExpressionTypeWithState state signatureVariables signatureType =
  either (const Nothing) Just
    (signatureTypeToExpressionType state signatureVariables signatureType)

data SignaturePayloadType = SignaturePayloadType
  { signaturePayloadDeclaredType :: ExpressionType,
    signaturePayloadExplicitConstraints :: [TypeSchemeConstraint],
    signaturePayloadVariableOrder :: [Int]
  }

-- | Normalize the currently accepted signature subset. Unsupported surfaces
-- return `Nothing` so callers can emit the stable signature diagnostic.
signaturePayloadToSignatureType :: SignaturePayload -> InferState -> (Maybe SignaturePayloadType, InferState)
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
  SignatureType ->
  InferState ->
  (Maybe SignaturePayloadType, InferState)
signaturePayloadFromType explicitConstraints signatureType state =
  let variableNames = constraintSignatureTypeVariableNamesInOrder signatureType
      (signatureVariables, nextState) = allocateSignatureTypeVariables variableNames state
      variableOrder =
        [ typeVar
          | variableName <- variableNames,
            Just (TVarType typeVar) <- [Map.lookup variableName signatureVariables]
        ]
   in case constraintSignatureTypeToExpressionTypeWithState nextState signatureVariables signatureType of
        Just expressionType ->
          (Just (SignaturePayloadType expressionType explicitConstraints variableOrder), nextState)
        Nothing -> (Nothing, state)

constraintSignatureTypeToExpressionType :: SignatureType -> Maybe ExpressionType
constraintSignatureTypeToExpressionType signatureType =
  either (const Nothing) Just
    (signatureTypeToExpressionType initialInferState Map.empty signatureType)

variableConstraintSignaturePayloadToExpressionType ::
  [SignatureConstraint] ->
  SignatureType ->
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
            Just (TVarType typeVar) <- [Map.lookup variableName signatureVariables]
        ]
   in case (convertedType, convertedConstraints) of
        (Just expressionType, Just explicitConstraints) ->
          (Just (SignaturePayloadType expressionType explicitConstraints variableOrder), nextState)
        _ -> (Nothing, state)

variableConstraintToTypeSchemeConstraint ::
  Map Text ExpressionType ->
  SignatureConstraint ->
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

supportedConcreteConstraints :: InferState -> [SignatureConstraint] -> Bool
supportedConcreteConstraints state constraints =
  not (null constraints)
    && isNothing (duplicateConstraintName constraints)
    && all (supportedConcreteConstraint state) constraints

-- | Variable constrained signatures are accepted when every constrained
-- variable appears in the body; extra body variables remain unconstrained.
supportedVariableConstraints :: InferState -> [SignatureConstraint] -> SignatureType -> Bool
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

supportedConcreteConstraint :: InferState -> SignatureConstraint -> Bool
supportedConcreteConstraint state (SignatureConstraint constraintName arguments) =
  case (Map.lookup (identifierText constraintName) (inferClassFacts state), arguments) of
    (Just 1, [argument]) ->
      concreteConstraintArgument argument
        && Set.member
          (constraintImplFactKey constraintName argument)
          (inferConcreteImplFacts state)
    _ -> False

supportedVariableConstraint :: InferState -> SignatureConstraint -> Bool
supportedVariableConstraint state (SignatureConstraint constraintName arguments) =
  case (Map.lookup (identifierText constraintName) (inferClassFacts state), arguments) of
    (Just 1, [TypeVariable {}]) -> True
    _ -> False

constraintVariableNamesInSupportedConstraint :: SignatureConstraint -> Set.Set Text
constraintVariableNamesInSupportedConstraint constraint =
  case constraint of
    SignatureConstraint _ [TypeVariable argumentName] ->
      Set.singleton (identifierText argumentName)
    _ -> Set.empty

constraintSignatureTypeVariableNames :: SignatureType -> Set.Set Text
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

constraintSignatureTypeSupportsVariableBody :: SignatureType -> Bool
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

-- | Convert a resolved expression type into a concrete runtime dispatch hint.
-- Uncommitted integer ranges are valid only when the complete range fits Int64.
expressionTypeToRuntimeHint :: ExpressionType -> Maybe SignatureType
expressionTypeToRuntimeHint =
  expressionTypeToRuntimeSignature RuntimeHintPolicy

-- | Preserve quantified variables as actual signature variables when building
-- runtime templates. This avoids disguising them as zero-arity data types.
expressionTypeToRuntimeTemplate :: Map Int Name -> ExpressionType -> Maybe SignatureType
expressionTypeToRuntimeTemplate variableNames =
  expressionTypeToRuntimeSignature (RuntimeTemplatePolicy variableNames)

data RuntimeSignaturePolicy
  = RuntimeHintPolicy
  | RuntimeTemplatePolicy (Map Int Name)

expressionTypeToRuntimeSignature :: RuntimeSignaturePolicy -> ExpressionType -> Maybe SignatureType
expressionTypeToRuntimeSignature policy expressionType =
  case expressionType of
    TIntType -> Just TypeInt
    TIntegerLiteralType literalRange ->
      case policy of
        RuntimeHintPolicy
          | integerLiteralRangeFitsNumericType literalRange NumericInt64 -> Just TypeInt
        _ -> Nothing
    TFloatType -> Just TypeFloat
    TNumericType numericType -> Just (TypeNumeric numericType)
    TBoolType -> Just TypeBool
    TCharType -> Just TypeChar
    TTextType -> Just TypeText
    TListType elementType ->
      TypeList <$> convert elementType
    TTupleType elementTypes ->
      TypeTuple <$> traverse convert elementTypes
    TDataType typeName typeArguments ->
      case traverse convert typeArguments of
        Just [] -> Just (TypeName typeName)
        Just argumentTemplates -> Just (TypeApplication typeName argumentTemplates)
        Nothing -> Nothing
    TFunctionType inputType outputType ->
      TypeFunction
        <$> convert inputType
        <*> convert outputType
    TVarType typeVar ->
      case policy of
        RuntimeHintPolicy -> Nothing
        RuntimeTemplatePolicy variableNames ->
          TypeVariable <$> Map.lookup typeVar variableNames
  where
    convert = expressionTypeToRuntimeSignature policy

tshow :: Show a => a -> Text
tshow = Text.pack . show
