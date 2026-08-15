{-# LANGUAGE OverloadedStrings #-}

-- | Shared contracts for the Typed Core validator implementation.
--
-- This module is private to the compiler library.  The public validator
-- facade deliberately exports 'ValidatedTypedProgram' without its
-- constructor, so construction of the proof remains tied to a successful
-- validation pass.
module Jazz.Compiler.TypedCore.Validate.Internal
  ( ModuleContext (..),
    ForwardSignedFunctionContext (..),
    ResolvedNameKey (..),
    BinderOccurrence (..),
    BinderContract (..),
    PatternBinderContract (..),
    ValueContract (..),
    ConstructorContract (..),
    DataContract (..),
    CapabilityContract (..),
    InstantiationContract (..),
    ValidatedTypedProgram (..),
    validatedTypedProgram,
    definitionNameKey,
    resolvedNameKey,
    resolvedNameFromKey,
    withVisibleNames,
    withLexicalContracts,
    validateSpan,
    qualifyExternalType,
    qualifyExternalRecipe,
    qualifyExternalImplId,
    qualifyExternalName,
    coreNameIdentifier,
    typedModulePath,
    implModulePath,
    implTargetTypes,
    binderModulePath,
    binderDefinitionKey,
    renderModulePath,
    failure,
    maybeToList,
    firstJust,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.TypedCore

data ModuleContext = ModuleContext
  { moduleContextPath :: [Text],
    moduleContextVisibleModules :: Set [Text],
    moduleContextSchemes :: Map TypedBinderId TypedScheme,
    moduleContextActiveSchemes :: Map ResolvedNameKey TypedScheme,
    moduleContextVisibleNames :: Set ResolvedNameKey,
    moduleContextSourceVisibleCapabilities :: Set ResolvedNameKey,
    moduleContextVisibleImpls :: Set TypedImplId,
    moduleContextImplMethods :: Map TypedImplId (Set Text),
    moduleContextDataArities :: Map ResolvedNameKey Int,
    moduleContextDataContracts :: Map ResolvedNameKey DataContract,
    moduleContextConstructorContracts :: Map ResolvedNameKey ConstructorContract,
    moduleContextCapabilityContracts :: Map ResolvedNameKey CapabilityContract,
    moduleContextEvidenceCapabilities :: Map TypedEvidenceParameterRef ResolvedNameKey,
    moduleContextLexicalContracts :: Map ResolvedNameKey BinderContract,
    moduleContextTypeScope :: Set TypedTypeParameterId,
    moduleContextPrimitiveConstraints :: [TypedPrimitiveConstraint]
  }

data ForwardSignedFunctionContext = ForwardSignedFunctionContext
  { forwardFunctionSchemes :: Map TypedBinderId TypedScheme,
    forwardFunctionActiveSchemes :: Map ResolvedNameKey TypedScheme,
    forwardFunctionVisibleNames :: Set ResolvedNameKey
  }

data ResolvedNameKey
  = ResolvedNameKey [Text] TypedNameNamespace Text
  | GeneratedNameKey TypedGeneratedNameKind
  deriving (Eq, Ord, Show)

data BinderOccurrence = BinderOccurrence TypedCoreValidationPath TypedBinderId

data BinderContract = BinderContract TypedBinderId TypedCoreName TypedType TypedRepresentationRecipe

data PatternBinderContract = PatternBinderContract TypedBinderId TypedCoreName TypedType TypedRepresentationRecipe

data ValueContract = ValueContract TypedType TypedRepresentationRecipe

data ConstructorContract = ConstructorContract TypedBinderId ResolvedNameKey [TypedTypeParameterId] [TypedType]

data DataContract = DataContract [TypedTypeParameterId] [[TypedType]]

data CapabilityContract = CapabilityContract [TypedTypeParameterId] (Map Text TypedScheme)
  deriving (Eq)

data InstantiationContract
  = InstantiationContract
      TypedBinderId
      [TypedTypeParameterId]
      [TypedEvidenceParameter]
      [TypedPrimitiveConstraint]

newtype ValidatedTypedProgram = ValidatedTypedProgram TypedProgram
  deriving (Eq, Show)

validatedTypedProgram :: ValidatedTypedProgram -> TypedProgram
validatedTypedProgram (ValidatedTypedProgram typedProgram) = typedProgram

definitionNameKey :: [Text] -> TypedCoreName -> Maybe ResolvedNameKey
definitionNameKey modulePath name =
  case name of
    TypedResolvedName _ namespace identifier -> Just (ResolvedNameKey modulePath namespace identifier)
    TypedGeneratedName kind -> Just (GeneratedNameKey kind)
    _ -> Nothing

resolvedNameKey :: [Text] -> TypedCoreName -> Maybe ResolvedNameKey
resolvedNameKey currentModulePath name =
  case name of
    TypedResolvedName origin namespace identifier ->
      Just (ResolvedNameKey (originModulePath origin) namespace identifier)
    TypedGeneratedName kind -> Just (GeneratedNameKey kind)
    _ -> Nothing
  where
    originModulePath origin =
      case origin of
        TypedCurrentModule -> currentModulePath
        TypedImportedModule modulePath -> modulePath
        TypedAmbientPrelude -> ["Prelude"]

withVisibleNames :: [TypedCoreName] -> ModuleContext -> ModuleContext
withVisibleNames names context =
  context
    { moduleContextVisibleNames =
        Set.union
          (moduleContextVisibleNames context)
          (Set.fromList [key | name <- names, key <- maybeToList (resolvedNameKey (moduleContextPath context) name)])
    }

withLexicalContracts :: [BinderContract] -> ModuleContext -> ModuleContext
withLexicalContracts contracts context =
  context
    { moduleContextVisibleNames = Set.union localNames (moduleContextVisibleNames context),
      moduleContextLexicalContracts = Map.union localContracts (moduleContextLexicalContracts context)
    }
  where
    entries =
      [ (key, contract)
      | contract@(BinderContract _ name _ _) <- contracts,
        key <- maybeToList (resolvedNameKey (moduleContextPath context) name)
      ]
    localNames = Set.fromList (map fst entries)
    localContracts = Map.fromList entries

coreNameIdentifier :: TypedCoreName -> Maybe Text
coreNameIdentifier name =
  case name of
    TypedResolvedName _ _ identifier -> Just identifier
    TypedBuiltinName identifier -> Just identifier
    _ -> Nothing

typedModulePath :: TypedModule -> [Text]
typedModulePath (TypedModule modulePath _ _ _ _ _ _ _) = modulePath

implModulePath :: TypedImplId -> [Text]
implModulePath (TypedImplId modulePath _ _) = modulePath

implTargetTypes :: TypedImplId -> [TypedType]
implTargetTypes (TypedImplId _ _ targetTypes) = targetTypes

renderModulePath :: [Text] -> Text
renderModulePath = Text.intercalate "::"

failure :: TypedCoreValidationPath -> TypedCoreValidationKind -> TypedCoreValidationDetail -> TypedCoreValidationFailure
failure = TypedCoreValidationFailure

maybeToList :: Maybe value -> [value]
maybeToList maybeValue =
  case maybeValue of
    Nothing -> []
    Just value -> [value]

firstJust :: [Maybe value] -> Maybe value
firstJust values =
  case values of
    [] -> Nothing
    Nothing : rest -> firstJust rest
    Just value : _ -> Just value

validateSpan :: TypedCoreValidationPath -> TypedSpan -> [TypedCoreValidationFailure]
validateSpan path (TypedSpan line column)
  | line > 0 && column > 0 = []
  | otherwise = [failure path TypedInvalidSpan TypedNoValidationDetail]

qualifyExternalType :: [Text] -> TypedType -> TypedType
qualifyExternalType modulePath typeValue =
  case typeValue of
    TypedListType elementType -> TypedListType (qualifyExternalType modulePath elementType)
    TypedTupleType elementTypes -> TypedTupleType (map (qualifyExternalType modulePath) elementTypes)
    TypedDataType name arguments -> TypedDataType (qualifyExternalName modulePath name) (map (qualifyExternalType modulePath) arguments)
    TypedFunctionType argument result -> TypedFunctionType (qualifyExternalType modulePath argument) (qualifyExternalType modulePath result)
    _ -> typeValue

qualifyExternalRecipe :: [Text] -> TypedRepresentationRecipe -> TypedRepresentationRecipe
qualifyExternalRecipe modulePath recipe =
  case recipe of
    TypedManagedListRecipe elementRecipe -> TypedManagedListRecipe (qualifyExternalRecipe modulePath elementRecipe)
    TypedManagedProductRecipe elementRecipes -> TypedManagedProductRecipe (map (qualifyExternalRecipe modulePath) elementRecipes)
    TypedManagedVariantRecipe name arguments ->
      TypedManagedVariantRecipe
        (qualifyExternalName modulePath name)
        (map (qualifyExternalType modulePath) arguments)
    TypedClosureRecipe parameters result ->
      TypedClosureRecipe
        (map (qualifyExternalRecipe modulePath) parameters)
        (qualifyExternalRecipe modulePath result)
    _ -> recipe

qualifyExternalImplId :: [Text] -> TypedImplId -> TypedImplId
qualifyExternalImplId modulePath (TypedImplId implPath capability targetTypes) =
  TypedImplId
    implPath
    (qualifyExternalName modulePath capability)
    (map (qualifyExternalType modulePath) targetTypes)

qualifyExternalName :: [Text] -> TypedCoreName -> TypedCoreName
qualifyExternalName modulePath name =
  case name of
    TypedResolvedName TypedCurrentModule namespace identifier ->
      TypedResolvedName
        (if modulePath == ["Prelude"] then TypedAmbientPrelude else TypedImportedModule modulePath)
        namespace
        identifier
    _ -> name

binderModulePath :: TypedBinderId -> [Text]
binderModulePath (TypedBinderId (modulePath, _, _)) = modulePath

resolvedNameFromKey :: ModuleContext -> ResolvedNameKey -> TypedCoreName
resolvedNameFromKey _ (GeneratedNameKey kind) = TypedGeneratedName kind
resolvedNameFromKey context (ResolvedNameKey modulePath namespace identifier) =
  TypedResolvedName origin namespace identifier
  where
    origin
      | modulePath == moduleContextPath context = TypedCurrentModule
      | modulePath == ["Prelude"] = TypedAmbientPrelude
      | otherwise = TypedImportedModule modulePath

binderDefinitionKey :: TypedBinderId -> Maybe ResolvedNameKey
binderDefinitionKey (TypedBinderId (modulePath, _, name)) = definitionNameKey modulePath name
