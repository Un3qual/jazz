{-# LANGUAGE OverloadedStrings #-}

-- | Canonical core AST shared by lowering, analysis, type inference, and the
-- small interpreter/runtime slice in `jazz-next`.
module JazzNext.Compiler.AST
  ( CaseArm (..),
    Expr (..),
    Literal (..),
    Pattern (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..),
    Statement (..)
  ) where

import Data.Char
  ( isSpace
  )
import Data.String
  ( IsString (..)
  )
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.Diagnostics
  ( SourceSpan
  )
import JazzNext.Compiler.Identifier
  ( Identifier
  )

-- | Literals currently supported by the lowered core language.
data Literal
  = LInt Int
  | LBool Bool
  deriving (Eq, Show)

-- | Core patterns for the first active-path case-expression slice.
data Pattern
  = PWildcard
  | PVariable Identifier
  | PLiteral Literal
  | PConstructor Identifier [Pattern]
  | PList [Pattern]
  deriving (Eq, Show)

-- | One lowered pattern-match arm.
data CaseArm = CaseArm Pattern Expr
  deriving (Eq, Show)

-- | Core expressions after surface syntax has been lowered into the stable
-- analyzer/runtime representation.
data Expr
  = ELit Literal
  | EVar Identifier
  | ELambda Identifier Expr
  | EOperatorValue Text
  | EList [Expr]
  | EApply Expr Expr
  | EIf Expr Expr Expr
  -- Internal canonical branch form used after control-flow desugaring.
  | ECase Expr Expr Expr
  | EPatternCase Expr [CaseArm]
  | EBinary Text Expr Expr
  | ESectionLeft Expr Text
  | ESectionRight Text Expr
  | EBlock [Statement]
  deriving (Eq, Show)

-- | Lowered signature payload used by analyzer/type inference.
data SignaturePayload
  = SignatureType SignatureType
  | SignatureFunction SignatureType SignatureType
  | UnsupportedSignature [SignatureToken]
  deriving (Eq, Show)

-- | Supported monomorphic signature types.
data SignatureType
  = TypeInt
  | TypeBool
  | TypeList SignatureType
  | TypeFunction SignatureType SignatureType
  deriving (Eq, Show)

-- | Tokenized fallback for unsupported signature surfaces.
data SignatureToken
  = SignatureNameToken Text
  | SignatureIntToken Int
  | SignatureArrowToken
  | SignatureLParenToken
  | SignatureRParenToken
  | SignatureLBracketToken
  | SignatureRBracketToken
  | SignatureOperatorToken Text
  | SignatureOtherToken Text
  deriving (Eq, Show)

-- | Dot-terminated statements that can appear either at the top level or
-- inside block expressions.
data Statement
  = SLet Identifier SourceSpan Expr
  | SSignature Identifier SourceSpan SignaturePayload
  | SModule SourceSpan [Text]
  | SImport SourceSpan [Text] (Maybe Text) (Maybe [Text])
  | SExpr SourceSpan Expr
  deriving (Eq, Show)

-- Convenience for manually constructed tests that still use overloaded string
-- literals for simple supported signatures.
instance IsString SignaturePayload where
  fromString rawText =
    case parseSupportedSignaturePayload (Text.pack rawText) of
      Just signaturePayload -> signaturePayload
      Nothing ->
        error ("unsupported signature literal in SignaturePayload IsString instance: " <> rawText)

parseSupportedSignaturePayload :: Text -> Maybe SignaturePayload
parseSupportedSignaturePayload signatureText =
  signaturePayloadFromType <$> parseSupportedSignatureType rawSignature
  where
    rawSignature = Text.filter (not . isSpace) signatureText

parseSupportedSignatureType :: Text -> Maybe SignatureType
parseSupportedSignatureType rawSignature =
  case splitFirstTopLevelArrow rawSignature of
    Left () -> Nothing
    Right (Just (argumentText, resultText)) ->
      TypeFunction
        <$> parseFunctionOperandType argumentText
        <*> parseSupportedSignatureType resultText
    Right Nothing ->
      parseFunctionOperandType rawSignature

parseFunctionOperandType :: Text -> Maybe SignatureType
parseFunctionOperandType rawSignature
  | Text.null rawSignature = Nothing
  | rawSignature == "Int" = Just TypeInt
  | rawSignature == "Bool" = Just TypeBool
  | Just innerType <- stripWrappedType '[' ']' rawSignature =
      TypeList <$> parseNonFunctionSignatureType innerType
  | Just innerType <- stripWrappedType '(' ')' rawSignature =
      parseSupportedSignatureType innerType
  | otherwise = Nothing

parseNonFunctionSignatureType :: Text -> Maybe SignatureType
parseNonFunctionSignatureType rawSignature
  | Text.null rawSignature = Nothing
  | rawSignature == "Int" = Just TypeInt
  | rawSignature == "Bool" = Just TypeBool
  | Just innerType <- stripWrappedType '[' ']' rawSignature =
      TypeList <$> parseNonFunctionSignatureType innerType
  | Just innerType <- stripWrappedType '(' ')' rawSignature =
      parseNonFunctionSignatureType innerType
  | otherwise = Nothing

signaturePayloadFromType :: SignatureType -> SignaturePayload
signaturePayloadFromType signatureType =
  case signatureType of
    TypeFunction argumentType resultType ->
      SignatureFunction argumentType resultType
    _ ->
      SignatureType signatureType

stripWrappedType :: Char -> Char -> Text -> Maybe Text
stripWrappedType openChar closeChar rawSignature = do
  withoutOpen <- Text.stripPrefix (Text.singleton openChar) rawSignature
  innerType <- Text.stripSuffix (Text.singleton closeChar) withoutOpen
  if Text.null innerType
    then Nothing
    else Just innerType

splitFirstTopLevelArrow :: Text -> Either () (Maybe (Text, Text))
splitFirstTopLevelArrow rawSignature = go 0 0 0 (Text.unpack rawSignature)
  where
    go _ 0 0 [] =
      Right Nothing
    go _ _ _ [] =
      Left ()
    go index parenDepth bracketDepth ('-':'>':rest)
      | parenDepth == 0 && bracketDepth == 0 =
          Right (Just (Text.take index rawSignature, Text.drop (index + 2) rawSignature))
      | otherwise =
          go (index + 2) parenDepth bracketDepth rest
    go index parenDepth bracketDepth (nextChar : rest) =
      case nextChar of
        '(' ->
          go (index + 1) (parenDepth + 1) bracketDepth rest
        ')' ->
          if parenDepth > 0
            then go (index + 1) (parenDepth - 1) bracketDepth rest
            else Left ()
        '[' ->
          go (index + 1) parenDepth (bracketDepth + 1) rest
        ']' ->
          if bracketDepth > 0
            then go (index + 1) parenDepth (bracketDepth - 1) rest
            else Left ()
        _ ->
          go (index + 1) parenDepth bracketDepth rest
