{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

-- | Validated declaration templates, independent of a checker's solver state.
module Jazz.Compiler.SemanticDeclarations
  ( ClassMethodType (.., ClassMethodType),
    ClassDefinition (..),
    ConstructorArgumentType (..),
    ConcreteImplFact (..),
    concreteSignatureType,
    DataTypeBinding (.., DataTypeBinding),
    prepareDataTypeKinds,
    signatureVariableKinds,
    signatureVariableKindsAt,
    normalizeSignatureTypeAt,
    normalizeSignatureStructure,
    ImplementationTemplate (..),
    implementationTarget,
    scopeConcreteImplFacts,
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
    filterScopeCapabilities,
    quantifiedVariablesFromPreferred,
    quantifiedVariablesMembershipSet,
    quantifiedVariablesOrderedList,
    bindingQuantifiedVariables,
    bindingVariableOrder,
    mapBindingTypes,
    traverseBindingTypes,
    instantiateDeclarationType,
    concreteImplementationType,
    normalizeSignatureType,
    semanticFunctionArguments,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Jazz.Compiler.BuiltinCatalog (BuiltinSymbol, numericTypeFromName)
import Jazz.Compiler.CoreIdentity (CapabilityId, CapabilityMethodKey, ImplId, MethodId, ResolvedReference)
import Jazz.Compiler.KindInference (inferDataKinds, inferSignatureKinds, inferSignatureKindsAt)
import Jazz.Compiler.Name (Identifier, ResolvedName, identifierLooksLikeTypeVariable, identifierText)
import Jazz.Compiler.StableSet (StableSet, stableSetFromPreferred, stableSetMembershipSet, stableSetOrderedList)
import Jazz.Compiler.TypeRepresentation (Kind (..), SemanticType (..), SignatureType (..), substituteSemanticVariables)

-- | A checked method type with its class parameter explicitly bound.
data ClassMethodType = ClassMethodScheme
  { classMethodParameter :: Text,
    classMethodScheme :: SemanticScheme Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

pattern ClassMethodType :: Text -> SemanticType ResolvedName Text -> ClassMethodType
pattern ClassMethodType parameter result <- ClassMethodScheme parameter (SemanticScheme {schemeResultType = result})
  where
    ClassMethodType parameter result =
      ClassMethodScheme
        parameter
        (SemanticScheme (quantifiedVariablesFromPreferred (parameter : toList result) (Set.insert parameter (Set.fromList (toList result)))) [] [] emptyScopeCapabilityFacts result)

{-# COMPLETE ClassMethodType #-}

data ClassDefinition = ClassDefinition
  { classParameterKind :: Kind Void,
    classSuperclasses :: [CapabilityId],
    classDefaultMethods :: Set Identifier
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | A checked implementation target with nominal capability/type identity.
data ConcreteImplFact = ConcreteImplFact CapabilityId (SemanticType ResolvedName Void)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

-- | One checked instance declaration, shared by all of its methods.
data ImplementationTemplate = ImplementationTemplate
  { implementationIdentity :: ImplId,
    implementationCapability :: CapabilityId,
    implementationScheme :: SemanticScheme Text,
    implementationParameterKinds :: Map Text (Kind Void),
    implementationMethods :: Map Identifier MethodId
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

implementationTarget :: ImplementationTemplate -> SemanticType ResolvedName Text
implementationTarget = schemeResultType . implementationScheme

-- | Parameters are bound by the enclosing data declaration. Invalid fields
-- remain only during diagnostic recovery and cannot reach successful analysis.
data ConstructorArgumentType
  = ConstructorArgumentType (SemanticType ResolvedName Text)
  | ConstructorArgumentFresh
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

data DataTypeBinding = KindedDataTypeBinding
  { dataTypeParameters :: [ResolvedName],
    dataTypeConstructors :: [[ConstructorArgumentType]],
    dataTypeParameterKinds :: [Kind Void]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- Existing first-order declaration producers default every parameter to Type.
pattern DataTypeBinding :: [ResolvedName] -> [[ConstructorArgumentType]] -> DataTypeBinding
pattern DataTypeBinding parameters constructors <- KindedDataTypeBinding parameters constructors _
  where
    DataTypeBinding parameters constructors = KindedDataTypeBinding parameters constructors (map (const TypeKind) parameters)

{-# COMPLETE DataTypeBinding #-}

prepareDataTypeKinds :: Map ResolvedName DataTypeBinding -> [(ResolvedName, [ResolvedName], [SignatureType ResolvedName ResolvedName])] -> Either (ResolvedName, SignatureTypeFailure) (Map ResolvedName DataTypeBinding)
prepareDataTypeKinds existing declarations = do
  normalized <- traverse normalize declarations
  kinds <- first (fmap SignatureKindMismatch) (inferDataKinds (dataConstructorKinds existing) normalized)
  pure $
    Map.fromList
      [ (name, KindedDataTypeBinding parameters [] (Map.findWithDefault [] name kinds))
      | (name, parameters, _) <- declarations
      ]
  where
    normalize (name, parameters, fields) = do
      let variables = Map.fromList [(identifierText parameter, SemanticVariable (identifierText parameter)) | parameter <- parameters]
      normalized <- first (name,) (traverse (normalizeSignatureTypeWith checkName variables) fields)
      pure (name, map identifierText parameters, normalized)
    checkName name _
      | Map.member name existing || any (\(candidate, _, _) -> candidate == name) declarations = Right ()
      | otherwise = Left (UnknownNamedType name)

dataConstructorKinds :: Map ResolvedName DataTypeBinding -> Map ResolvedName (Kind Void)
dataConstructorKinds = Map.map (foldr FunctionKind TypeKind . dataTypeParameterKinds)

signatureVariableKinds :: (Ord variable) => Map ResolvedName DataTypeBinding -> Map variable (Kind Void) -> [SemanticType ResolvedName variable] -> Either SignatureTypeFailure (Map variable (Kind Void))
signatureVariableKinds dataTypes known =
  either (Left . SignatureKindMismatch) Right . inferSignatureKinds (dataConstructorKinds dataTypes) known

signatureVariableKindsAt :: (Ord variable) => Map ResolvedName DataTypeBinding -> Map variable (Kind Void) -> [(SemanticType ResolvedName variable, Kind Void)] -> Either SignatureTypeFailure (Map variable (Kind Void))
signatureVariableKindsAt dataTypes known =
  either (Left . SignatureKindMismatch) Right . inferSignatureKindsAt (dataConstructorKinds dataTypes) known

instantiateDeclarationType :: Map Text (SemanticType ResolvedName variable) -> SemanticType ResolvedName Text -> Maybe (SemanticType ResolvedName variable)
instantiateDeclarationType parameters field =
  substituteSemanticVariables id <$> traverse (`Map.lookup` parameters) field

data SignatureTypeFailure
  = UnknownNamedType ResolvedName
  | NamedTypeArityMismatch ResolvedName Int Int
  | SignatureKindMismatch Text
  | UnboundSignatureTypeVariable ResolvedName
  deriving (Eq, Show)

normalizeSignatureType ::
  (Ord variable) =>
  Map ResolvedName DataTypeBinding ->
  Map Text (SemanticType ResolvedName variable) ->
  SignatureType ResolvedName ResolvedName ->
  Either SignatureTypeFailure (SemanticType ResolvedName variable)
normalizeSignatureType dataTypes variables = normalizeSignatureTypeAt dataTypes variables TypeKind

normalizeSignatureTypeAt :: (Ord variable) => Map ResolvedName DataTypeBinding -> Map Text (SemanticType ResolvedName variable) -> Kind Void -> SignatureType ResolvedName ResolvedName -> Either SignatureTypeFailure (SemanticType ResolvedName variable)
normalizeSignatureTypeAt dataTypes variables expected signature = do
  normalized <- normalizeSignatureStructure dataTypes variables signature
  _ <- signatureVariableKindsAt dataTypes Map.empty [(normalized, expected)]
  pure normalized

normalizeSignatureStructure :: Map ResolvedName DataTypeBinding -> Map Text (SemanticType ResolvedName variable) -> SignatureType ResolvedName ResolvedName -> Either SignatureTypeFailure (SemanticType ResolvedName variable)
normalizeSignatureStructure dataTypes = normalizeSignatureTypeWith checkNamed
  where
    checkNamed name argumentCount = case Map.lookup name dataTypes of
      Nothing -> Left (UnknownNamedType name)
      Just (DataTypeBinding parameters _)
        | length parameters < argumentCount -> Left (NamedTypeArityMismatch name (length parameters) argumentCount)
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
    TypeApplication name arguments ->
      case builtinOrVariableType name of
        Just headType -> foldl SemanticApplication headType <$> traverse convert arguments
        Nothing -> namedType name arguments
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
        "List" -> Just SemanticListConstructor
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
  SemanticApplication constructor argument -> concreteImplementationType constructor && concreteImplementationType argument
  _ -> True

-- | Quantifiers are local to a scheme. A monomorphic parameter instead belongs
-- to a declaration, so aliases preserve sharing without exposing solver IDs.
data DeclarationVariable
  = SchemeParameter Int
  | DeclarationParameter ResolvedReference Int
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
  { scopeClassFacts :: Map CapabilityId ClassDefinition,
    scopeGeneratedEqualityClassFacts :: Set CapabilityId,
    scopeClassMethodSignatures :: Map CapabilityMethodKey ClassMethodType,
    scopeImplementations :: Map ImplId ImplementationTemplate
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Semigroup ScopeCapabilityFacts where
  leftFacts <> rightFacts =
    ScopeCapabilityFacts
      { scopeClassFacts = Map.union (scopeClassFacts leftFacts) (scopeClassFacts rightFacts),
        scopeGeneratedEqualityClassFacts = Set.union (scopeGeneratedEqualityClassFacts leftFacts) (scopeGeneratedEqualityClassFacts rightFacts),
        scopeClassMethodSignatures = Map.union (scopeClassMethodSignatures leftFacts) (scopeClassMethodSignatures rightFacts),
        scopeImplementations = Map.union (scopeImplementations leftFacts) (scopeImplementations rightFacts)
      }

instance Monoid ScopeCapabilityFacts where
  mempty = ScopeCapabilityFacts Map.empty Set.empty Map.empty Map.empty

-- Instance transport is independent of source name selection.
filterScopeCapabilities :: (CapabilityId -> Bool) -> ScopeCapabilityFacts -> ScopeCapabilityFacts
filterScopeCapabilities selected facts =
  facts
    { scopeClassFacts = Map.filterWithKey (\capability _ -> selected capability) (scopeClassFacts facts),
      scopeGeneratedEqualityClassFacts = Set.filter selected (scopeGeneratedEqualityClassFacts facts),
      scopeClassMethodSignatures = Map.filterWithKey (\(capability, _) _ -> selected capability) (scopeClassMethodSignatures facts)
    }

-- Concrete views serve the existing structural-equality diagnostics during
-- inference. Instance declarations themselves have only one stored catalog.
scopeConcreteImplFacts :: ScopeCapabilityFacts -> Set ConcreteImplFact
scopeConcreteImplFacts facts =
  Set.fromList
    [ ConcreteImplFact (implementationCapability template) target
    | template <- Map.elems (scopeImplementations facts),
      Just target <- [traverse (const Nothing) (implementationTarget template)]
    ]

emptyScopeCapabilityFacts :: ScopeCapabilityFacts
emptyScopeCapabilityFacts = mempty

-- | Transform solved types and their binders together, preserving binder order.
mapBindingTypes :: (Ord target) => (variable -> target) -> (SemanticType ResolvedName variable -> SemanticType ResolvedName target) -> SemanticBinding variable -> SemanticBinding target
mapBindingTypes variable expression = runIdentity . traverseBindingTypes (Identity . variable) (Identity . expression)

-- | Allocate each parameter at its occurrence, including quantifiers and
-- constraints, without assuming a separately collected variable inventory.
traverseBindingTypes :: (Applicative f, Ord target) => (variable -> f target) -> (SemanticType ResolvedName variable -> f (SemanticType ResolvedName target)) -> SemanticBinding variable -> f (SemanticBinding target)
traverseBindingTypes variable expression binding = case binding of
  PlainTypeBinding value -> PlainTypeBinding <$> expression value
  SchemeTypeBinding scheme -> SchemeTypeBinding <$> traverseScheme scheme
  OperatorAliasSchemeTypeBinding symbol scheme -> OperatorAliasSchemeTypeBinding symbol <$> traverseScheme scheme
  BuiltinAliasTypeBinding symbol -> pure (BuiltinAliasTypeBinding symbol)
  BuiltinOperatorAliasTypeBinding symbol -> pure (BuiltinOperatorAliasTypeBinding symbol)
  ConstructorTypeBinding name parameters fields -> pure (ConstructorTypeBinding name parameters fields)
  where
    traverseScheme scheme =
      SemanticScheme . quantified
        <$> traverse variable (quantifiedVariablesOrderedList (schemeQuantifiedVariables scheme))
        <*> traverse (traverse expression) (schemeClassConstraints scheme)
        <*> traverse (traverse expression) (schemePrimitiveConstraints scheme)
        <*> pure (schemeDefiningCapabilities scheme)
        <*> expression (schemeResultType scheme)
    quantified ordered = quantifiedVariablesFromPreferred ordered (Set.fromList ordered)

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
