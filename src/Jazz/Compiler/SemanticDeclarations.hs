{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Validated declaration templates, independent of a checker's solver state.
module Jazz.Compiler.SemanticDeclarations
  ( ClassMethodType (..),
    ConstructorArgumentType (..),
    ConcreteImplFact (..),
    concreteSignatureType,
    DataTypeBinding (..),
    ImplMethodType (..),
    SignatureTypeFailure (..),
    DeclarationVariable (..),
    IntegerLiteralRange (..),
    NumericConstraint (..),
    QuantifiedVariables,
    ScopeCapabilityFacts (..),
    SemanticBinding (..),
    SemanticScheme (..),
    SchemeConstraint (..),
    SchemePrimitiveConstraint (..),
    emptyScopeCapabilityFacts,
    quantifiedVariablesFromPreferred,
    quantifiedVariablesMembershipSet,
    quantifiedVariablesOrderedList,
    bindingQuantifiedVariables,
    bindingVariableOrder,
    mapBindingTypes,
    instantiateDeclarationType,
    concreteImplementationType,
    implementationTargetSignature,
    normalizeSignatureType,
    semanticFunctionArguments,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Jazz.Compiler.BuiltinCatalog (BuiltinSymbol, numericTypeFromName)
import Jazz.Compiler.CoreIdentity (CapabilityId, CapabilityMethodKey, CoreBinderId, MethodId)
import Jazz.Compiler.Name (ResolvedName, identifierLooksLikeTypeVariable, identifierText)
import Jazz.Compiler.StableSet (StableSet, stableSetFromPreferred, stableSetMembershipSet, stableSetOrderedList)
import Jazz.Compiler.TypeRepresentation (SemanticType (..), SignatureType (..), semanticTypeToSignature, substituteSemanticVariables)

-- | A checked method type with its class parameter explicitly bound.
data ClassMethodType = ClassMethodType Text (SemanticType ResolvedName Text)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | A checked implementation target with nominal capability/type identity.
data ConcreteImplFact = ConcreteImplFact CapabilityId (SemanticType ResolvedName Void)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | The declaration selected by method checking also owns its evidence identity.
data ImplMethodType = ImplMethodType
  { implMethodTarget :: SemanticType ResolvedName Void,
    implMethodCapability :: CapabilityId,
    implMethodIdentity :: MethodId
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Parameters are bound by the enclosing data declaration. Invalid fields
-- remain only during diagnostic recovery and cannot reach successful analysis.
data ConstructorArgumentType
  = ConstructorArgumentType (SemanticType ResolvedName Text)
  | ConstructorArgumentFresh
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data DataTypeBinding = DataTypeBinding [ResolvedName] [[ConstructorArgumentType]]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instantiateDeclarationType :: Map Text (SemanticType ResolvedName variable) -> SemanticType ResolvedName Text -> Maybe (SemanticType ResolvedName variable)
instantiateDeclarationType parameters field =
  substituteSemanticVariables id <$> traverse (`Map.lookup` parameters) field

data SignatureTypeFailure
  = UnknownNamedType ResolvedName
  | NamedTypeArityMismatch ResolvedName Int Int
  | TypeVariableApplicationHead ResolvedName
  | UnboundSignatureTypeVariable ResolvedName
  deriving (Eq, Show)

normalizeSignatureType ::
  Map ResolvedName DataTypeBinding ->
  Map Text (SemanticType ResolvedName variable) ->
  SignatureType ResolvedName ResolvedName ->
  Either SignatureTypeFailure (SemanticType ResolvedName variable)
normalizeSignatureType dataTypes = normalizeSignatureTypeWith checkNamed
  where
    checkNamed name argumentCount = case Map.lookup name dataTypes of
      Nothing -> Left (UnknownNamedType name)
      Just (DataTypeBinding parameters _)
        | length parameters /= argumentCount -> Left (NamedTypeArityMismatch name (length parameters) argumentCount)
        | otherwise -> Right ()

-- Declaration diagnostics compare concrete targets before data-type arity
-- checking. The checker itself supplies that validation to the same converter.
concreteSignatureType :: SignatureType ResolvedName ResolvedName -> Maybe (SemanticType ResolvedName Void)
concreteSignatureType signature = do
  target <- either (const Nothing) Just (normalizeSignatureTypeWith concreteName Map.empty signature)
  if concreteImplementationType target then Just target else Nothing
  where
    concreteName name _
      | identifierLooksLikeTypeVariable name = Left (UnboundSignatureTypeVariable name)
      | otherwise = Right ()

normalizeSignatureTypeWith :: (ResolvedName -> Int -> Either SignatureTypeFailure ()) -> Map Text (SemanticType ResolvedName variable) -> SignatureType ResolvedName ResolvedName -> Either SignatureTypeFailure (SemanticType ResolvedName variable)
normalizeSignatureTypeWith checkNamed variables signatureType =
  case signatureType of
    TypeInt -> Right SemanticInt
    TypeFloat -> Right SemanticFloat
    TypeNumeric numericType -> Right (SemanticNumeric numericType)
    TypeBool -> Right SemanticBool
    TypeChar -> Right SemanticChar
    TypeText -> Right SemanticText
    TypeVariable name ->
      maybe
        (Left (UnboundSignatureTypeVariable name))
        Right
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
      SemanticList <$> convert innerType
    TypeTuple elementTypes ->
      SemanticTuple <$> traverse convert elementTypes
    TypeFunction argumentType resultType ->
      SemanticFunction <$> convert argumentType <*> convert resultType
  where
    convert = normalizeSignatureTypeWith checkNamed variables

    builtinOrVariableType name =
      case identifierText name of
        "Int" -> Just SemanticInt
        "Float" -> Just SemanticFloat
        "Bool" -> Just SemanticBool
        "Char" -> Just SemanticChar
        "Text" -> Just SemanticText
        typeName ->
          (SemanticNumeric <$> numericTypeFromName typeName)
            <|> Map.lookup typeName variables

    namedType name arguments = do
      checkNamed name (length arguments)
      SemanticData name <$> traverse convert arguments

semanticFunctionArguments :: SemanticType name variable -> ([SemanticType name variable], SemanticType name variable)
semanticFunctionArguments (SemanticFunction argument result) =
  let (arguments, finalResult) = semanticFunctionArguments result
   in (argument : arguments, finalResult)
semanticFunctionArguments result = ([], result)

concreteImplementationType :: SemanticType name variable -> Bool
concreteImplementationType target = case target of
  SemanticVariable {} -> False
  SemanticFunction {} -> False
  SemanticList element -> concreteImplementationType element
  SemanticTuple elements -> all concreteImplementationType elements
  SemanticData _ arguments -> all concreteImplementationType arguments
  _ -> True

implementationTargetSignature :: SemanticType ResolvedName Void -> SignatureType ResolvedName ResolvedName
implementationTargetSignature = semanticTypeToSignature . fmap absurd

-- | Quantifiers are local to a scheme. A monomorphic parameter instead belongs
-- to a declaration, so aliases preserve sharing without exposing solver IDs.
data DeclarationVariable
  = SchemeParameter Int
  | DeclarationParameter CoreBinderId Int
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data IntegerLiteralRange = IntegerLiteralRange Integer Integer
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

data NumericConstraint
  = AnyNumericConstraint
  | RuntimeArithmeticNumericConstraint
  | RuntimeComparisonNumericConstraint
  | IntegralNumericConstraint
  | IntegralLiteralNumericConstraint IntegerLiteralRange
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data SemanticBinding variable
  = PlainTypeBinding (SemanticType ResolvedName variable)
  | SchemeTypeBinding (SemanticScheme variable)
  | BuiltinAliasTypeBinding BuiltinSymbol
  | BuiltinOperatorAliasTypeBinding Text
  | OperatorAliasSchemeTypeBinding Text (SemanticScheme variable)
  | ConstructorTypeBinding ResolvedName [ResolvedName] [ConstructorArgumentType]
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

newtype QuantifiedVariables variable = QuantifiedVariables (StableSet variable)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

quantifiedVariablesFromPreferred :: (Ord variable) => [variable] -> Set variable -> QuantifiedVariables variable
quantifiedVariablesFromPreferred preferred variables =
  QuantifiedVariables (stableSetFromPreferred preferred variables)

quantifiedVariablesMembershipSet :: QuantifiedVariables variable -> Set variable
quantifiedVariablesMembershipSet (QuantifiedVariables variables) =
  stableSetMembershipSet variables

quantifiedVariablesOrderedList :: QuantifiedVariables variable -> [variable]
quantifiedVariablesOrderedList (QuantifiedVariables variables) =
  stableSetOrderedList variables

data SemanticScheme variable = SemanticScheme
  { schemeQuantifiedVariables :: QuantifiedVariables variable,
    schemeClassConstraints :: [SchemeConstraint (SemanticType ResolvedName variable)],
    schemePrimitiveConstraints :: [SchemePrimitiveConstraint (SemanticType ResolvedName variable)],
    schemeDefiningCapabilities :: ScopeCapabilityFacts,
    schemeResultType :: SemanticType ResolvedName variable
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data SchemePrimitiveConstraint typeValue
  = TypeSchemeNumericConstraint NumericConstraint typeValue
  | TypeSchemeStrictEqualityConstraint typeValue
  deriving stock (Eq, Foldable, Functor, Generic, Show, Traversable)
  deriving anyclass (NFData)

data SchemeConstraint typeValue
  = TypeSchemeConstraint CapabilityId typeValue
  | TypeSchemeInferredConstraint CapabilityId typeValue
  | TypeSchemeMethodConstraint CapabilityId CapabilityMethodKey typeValue
  deriving stock (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)
  deriving anyclass (NFData)

data ScopeCapabilityFacts = ScopeCapabilityFacts
  { scopeClassFacts :: Map CapabilityId Int,
    scopeGeneratedEqualityClassFacts :: Set CapabilityId,
    scopeConcreteImplFacts :: Set ConcreteImplFact,
    scopeClassMethodSignatures :: Map CapabilityMethodKey ClassMethodType,
    scopeConcreteImplMethods :: Map CapabilityMethodKey [ImplMethodType]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Semigroup ScopeCapabilityFacts where
  leftFacts <> rightFacts =
    ScopeCapabilityFacts
      { scopeClassFacts = Map.union (scopeClassFacts leftFacts) (scopeClassFacts rightFacts),
        scopeGeneratedEqualityClassFacts =
          Set.union
            (scopeGeneratedEqualityClassFacts leftFacts)
            (scopeGeneratedEqualityClassFacts rightFacts),
        scopeConcreteImplFacts =
          Set.union
            (scopeConcreteImplFacts leftFacts)
            (scopeConcreteImplFacts rightFacts),
        scopeClassMethodSignatures =
          Map.union
            (scopeClassMethodSignatures leftFacts)
            (scopeClassMethodSignatures rightFacts),
        scopeConcreteImplMethods =
          Map.unionWith
            (<>)
            (scopeConcreteImplMethods leftFacts)
            (scopeConcreteImplMethods rightFacts)
      }

instance Monoid ScopeCapabilityFacts where
  mempty =
    ScopeCapabilityFacts
      { scopeClassFacts = Map.empty,
        scopeGeneratedEqualityClassFacts = Set.empty,
        scopeConcreteImplFacts = Set.empty,
        scopeClassMethodSignatures = Map.empty,
        scopeConcreteImplMethods = Map.empty
      }

emptyScopeCapabilityFacts :: ScopeCapabilityFacts
emptyScopeCapabilityFacts = mempty

-- | Transform solved types and their binders together, preserving binder order.
mapBindingTypes :: (Ord target) => (variable -> target) -> (SemanticType ResolvedName variable -> SemanticType ResolvedName target) -> SemanticBinding variable -> SemanticBinding target
mapBindingTypes variable expression binding = case binding of
  PlainTypeBinding value -> PlainTypeBinding (expression value)
  SchemeTypeBinding scheme -> SchemeTypeBinding (mapScheme scheme)
  OperatorAliasSchemeTypeBinding symbol scheme -> OperatorAliasSchemeTypeBinding symbol (mapScheme scheme)
  BuiltinAliasTypeBinding symbol -> BuiltinAliasTypeBinding symbol
  BuiltinOperatorAliasTypeBinding symbol -> BuiltinOperatorAliasTypeBinding symbol
  ConstructorTypeBinding name parameters fields -> ConstructorTypeBinding name parameters fields
  where
    mapScheme scheme =
      SemanticScheme
        { schemeQuantifiedVariables =
            let ordered = map variable (quantifiedVariablesOrderedList (schemeQuantifiedVariables scheme))
             in quantifiedVariablesFromPreferred ordered (Set.fromList ordered),
          schemeClassConstraints = map (fmap expression) (schemeClassConstraints scheme),
          schemePrimitiveConstraints = map (fmap expression) (schemePrimitiveConstraints scheme),
          schemeDefiningCapabilities = schemeDefiningCapabilities scheme,
          schemeResultType = expression (schemeResultType scheme)
        }

bindingQuantifiedVariables :: SemanticBinding variable -> [variable]
bindingQuantifiedVariables binding = case binding of
  SchemeTypeBinding scheme -> quantifiedVariablesOrderedList (schemeQuantifiedVariables scheme)
  OperatorAliasSchemeTypeBinding _ scheme -> quantifiedVariablesOrderedList (schemeQuantifiedVariables scheme)
  _ -> []

bindingVariableOrder :: SemanticBinding variable -> [variable]
bindingVariableOrder binding = case binding of
  PlainTypeBinding value -> toList value
  SchemeTypeBinding scheme -> schemeVariables scheme
  OperatorAliasSchemeTypeBinding _ scheme -> schemeVariables scheme
  _ -> []
  where
    schemeVariables scheme =
      toList (schemeResultType scheme)
        <> concatMap (foldMap toList) (schemeClassConstraints scheme)
        <> concatMap (foldMap toList) (schemePrimitiveConstraints scheme)
