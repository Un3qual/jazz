{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Nominal declaration keys and source signature variable discovery.
module Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact (..),
    concreteImplFact,
    constraintSignatureTypeVariableNamesInOrder,
    identifierLooksLikeTypeVariable,
    qualifiedMethodKey,
    renderConcreteImplFact,
    splitQualifiedMethodKey,
  )
where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (absurd)
import qualified Jazz.Compiler.AST as AST
import Jazz.Compiler.CoreIdentity (CapabilityId (..), CapabilityMethodKey, renderCapabilityId)
import Jazz.Compiler.Name
  ( ResolvedName,
    identifierLooksLikeTypeVariable,
    mkIdentifier,
    renderName,
  )
import Jazz.Compiler.SemanticDeclarations (ConcreteImplFact (..), concreteSignatureType)
import Jazz.Compiler.SignatureRendering
  ( renderSemanticType,
  )
import Jazz.Compiler.TypeRepresentation
  ( pattern TypeApplication,
    pattern TypeFunction,
    pattern TypeList,
    pattern TypeName,
    pattern TypeTuple,
    pattern TypeVariable,
  )

type SignatureType = AST.SignatureType 'AST.Resolved

concreteImplFact :: ResolvedName -> [SignatureType] -> Maybe ConcreteImplFact
concreteImplFact capabilityName arguments =
  case arguments of
    [argument] -> ConcreteImplFact (CapabilityId capabilityName) <$> concreteSignatureType argument
    _ -> Nothing

renderConcreteImplFact :: ConcreteImplFact -> Text
renderConcreteImplFact (ConcreteImplFact capabilityName argument) =
  renderCapabilityId capabilityName <> "(" <> renderSemanticType (fmap absurd argument) <> ")"

qualifiedMethodKey :: ResolvedName -> ResolvedName -> CapabilityMethodKey
qualifiedMethodKey capabilityName methodName =
  (CapabilityId capabilityName, mkIdentifier (renderName methodName))

splitQualifiedMethodKey :: Text -> Maybe (Text, Text)
splitQualifiedMethodKey nameText =
  case Text.breakOnEnd "::" nameText of
    (capabilityNameWithSeparator, methodName)
      | not (Text.null capabilityName),
        not (Text.null methodName) ->
          Just (capabilityName, methodName)
      where
        capabilityName = Text.dropEnd 2 capabilityNameWithSeparator
    _ -> Nothing

constraintSignatureTypeVariableNamesInOrder :: SignatureType -> [Text]
constraintSignatureTypeVariableNamesInOrder =
  stableUnique . go
  where
    go signatureType =
      case signatureType of
        TypeVariable name -> [renderName name]
        TypeName name
          | identifierLooksLikeTypeVariable name ->
              [renderName name]
          | otherwise ->
              []
        TypeApplication name arguments ->
          [renderName name | identifierLooksLikeTypeVariable name] <> concatMap go arguments
        TypeList innerType ->
          go innerType
        TypeTuple elementTypes ->
          concatMap go elementTypes
        TypeFunction argumentType resultType ->
          go argumentType ++ go resultType
        _ -> []

    stableUnique = reverse . snd . foldl' keep (Set.empty, [])
      where
        keep (seen, reversedNames) name
          | Set.member name seen = (seen, reversedNames)
          | otherwise = (Set.insert name seen, name : reversedNames)
