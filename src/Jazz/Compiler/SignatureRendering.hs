{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Canonical rendering for source-level signature types. This stays distinct
-- from inferred-type rendering because the two representations have different
-- syntax and responsibilities.
module Jazz.Compiler.SignatureRendering
  ( renderSignatureType,
    renderSemanticType,
    renderSemanticTypeWith,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.BuiltinCatalog
  ( renderNumericTypeName,
  )
import Jazz.Compiler.Name
  ( Name,
    UserNameLike,
    renderName,
  )
import Jazz.Compiler.TypeRepresentation
  ( SemanticType (..),
    SignatureType,
    semanticApplicationSpine,
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
  )

renderSignatureType :: (UserNameLike user) => SignatureType (Name user) (Name user) -> Text
renderSignatureType signatureType =
  case signatureType of
    TypeInt -> "Int"
    TypeFloat -> "Float"
    TypeNumeric numericType -> renderNumericTypeName numericType
    TypeBool -> "Bool"
    TypeChar -> "Char"
    TypeText -> "Text"
    TypeVariable name -> renderName name
    TypeName name -> renderName name
    TypeApplication name arguments ->
      renderName name <> "(" <> Text.intercalate ", " (map renderSignatureType arguments) <> ")"
    TypeList innerType -> "[" <> renderSignatureTypeAtom innerType <> "]"
    TypeTuple elementTypes -> "(" <> Text.intercalate ", " (map renderSignatureType elementTypes) <> ")"
    TypeFunction argumentType resultType -> renderSignatureTypeAtom argumentType <> " -> " <> renderSignatureType resultType

renderSignatureTypeAtom :: (UserNameLike user) => SignatureType (Name user) (Name user) -> Text
renderSignatureTypeAtom signatureType =
  case signatureType of
    TypeFunction {} -> "(" <> renderSignatureType signatureType <> ")"
    _ -> renderSignatureType signatureType

-- | Render canonical semantic applications using the same source spellings.
renderSemanticType :: (UserNameLike user) => SemanticType (Name user) (Name user) -> Text
renderSemanticType = renderSemanticTypeWith renderName

renderSemanticTypeWith :: (UserNameLike user) => (variable -> Text) -> SemanticType (Name user) variable -> Text
renderSemanticTypeWith renderVariable = render
  where
    render semanticType = case semanticType of
      SemanticInt -> "Int"
      SemanticFloat -> "Float"
      SemanticNumeric numeric -> renderNumericTypeName numeric
      SemanticBool -> "Bool"
      SemanticChar -> "Char"
      SemanticText -> "Text"
      SemanticVariable variable -> renderVariable variable
      SemanticListConstructor -> "List"
      SemanticNamedConstructor name -> renderName name
      SemanticList element -> "[" <> atom element <> "]"
      SemanticTuple elements -> "(" <> renderMany elements <> ")"
      SemanticFunction argument result -> atom argument <> " -> " <> render result
      application@SemanticApplication {} ->
        let (constructor, arguments) = semanticApplicationSpine application
         in atom constructor <> "(" <> renderMany arguments <> ")"
    renderMany = Text.intercalate ", " . map render
    atom semanticType = case semanticType of
      SemanticFunction {} -> "(" <> render semanticType <> ")"
      _ -> render semanticType
