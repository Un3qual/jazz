{-# LANGUAGE OverloadedStrings #-}

-- | Canonical rendering for source-level signature types. This stays distinct
-- from inferred-type rendering because the two representations have different
-- syntax and responsibilities.
module JazzNext.Compiler.SignatureRendering
  ( renderSignatureType
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( SignatureType (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( renderNumericTypeName
  )
import JazzNext.Compiler.Name
  ( renderName
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
