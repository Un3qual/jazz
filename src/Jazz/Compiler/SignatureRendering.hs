{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Canonical rendering for source-level signature types. This stays distinct
-- from inferred-type rendering because the two representations have different
-- syntax and responsibilities.
module Jazz.Compiler.SignatureRendering
  ( renderSignatureType,
  )
where

import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( SignatureType,
  )
import Jazz.Compiler.BuiltinCatalog
  ( renderNumericTypeName,
  )
import Jazz.Compiler.Name
  ( renderName,
  )
import Jazz.Compiler.TypeRepresentation
  ( pattern TypeApplication,
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

renderSignatureType :: SignatureType -> Text
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

renderSignatureTypeAtom :: SignatureType -> Text
renderSignatureTypeAtom signatureType =
  case signatureType of
    TypeFunction {} -> "(" <> renderSignatureType signatureType <> ")"
    _ -> renderSignatureType signatureType
