{-# LANGUAGE OverloadedStrings #-}

module Jazz.Compiler.Bootstrap.CanonicalValue
  ( CanonicalSourcePath (..),
    CanonicalSpan (..),
    canonicalConstructor,
    canonicalList,
    canonicalMaybe,
    canonicalNullaryConstructor,
    canonicalSourcePathRuntimeValue,
    canonicalSpanRuntimeValue,
    canonicalizeSpan,
    decodeInt,
    decodeInteger,
    decodeList,
    decodeText,
    expectArity,
    expectConstructor,
    expectNamedConstructor,
    expectNullary,
    normalizeCanonicalSourcePath,
    runtimeIntValue,
    runtimeValueCategory,
  )
where

import Data.Char (isAlpha)
import Data.Text (Text)
import qualified Data.Text as Text
import Jazz.Compiler.AST
  ( SignatureType (TypeName),
  )
import Jazz.Compiler.Diagnostics
  ( SourceSpan (..),
  )
import Jazz.Compiler.Name
  ( identifierText,
    mkIdentifier,
    sourceName,
  )
import Jazz.Compiler.Runtime
  ( RuntimeValue (..),
    untypedIntMetadata,
  )

newtype CanonicalSourcePath = CanonicalSourcePath Text
  deriving (Eq, Show)

data CanonicalSpan = CanonicalSpan Int Int
  deriving (Eq, Show)

normalizeCanonicalSourcePath :: FilePath -> Either Text CanonicalSourcePath
normalizeCanonicalSourcePath sourcePath
  | null sourcePath = Left "canonical source path must not be empty"
  | '\\' `elem` sourcePath = Left "canonical source path must use '/' separators"
  | isLogicalAbsolute sourcePath = Left "canonical source path must be relative"
  | any (== "..") segments = Left "canonical source path must not contain '..'"
  | null normalizedSegments = Left "canonical source path must not be empty"
  | otherwise = Right (CanonicalSourcePath (Text.intercalate "/" normalizedSegments))
  where
    segments = Text.splitOn "/" (Text.pack sourcePath)
    normalizedSegments = filter (\segment -> not (Text.null segment) && segment /= ".") segments

isLogicalAbsolute :: FilePath -> Bool
isLogicalAbsolute sourcePath =
  case sourcePath of
    '/' : _ -> True
    drive : ':' : _ -> isAlpha drive
    _ -> False

canonicalizeSpan :: SourceSpan -> CanonicalSpan
canonicalizeSpan spanValue = CanonicalSpan (spanLine spanValue) (spanColumn spanValue)

canonicalSourcePathRuntimeValue :: CanonicalSourcePath -> RuntimeValue
canonicalSourcePathRuntimeValue (CanonicalSourcePath sourcePath) =
  canonicalConstructor "CanonicalSourcePath" [VText sourcePath]

canonicalSpanRuntimeValue :: CanonicalSpan -> RuntimeValue
canonicalSpanRuntimeValue (CanonicalSpan line column) =
  canonicalConstructor "CanonicalSpan" [runtimeIntValue line, runtimeIntValue column]

canonicalNullaryConstructor :: Text -> RuntimeValue
canonicalNullaryConstructor name = canonicalConstructor name []

canonicalConstructor :: Text -> [RuntimeValue] -> RuntimeValue
canonicalConstructor name arguments =
  VConstructor
    (sourceName (mkIdentifier name))
    []
    (sourceName (mkIdentifier name))
    (replicate (length arguments) canonicalFieldType)
    arguments
  where
    canonicalFieldType = TypeName (sourceName (mkIdentifier "$canonical-field"))

runtimeIntValue :: Int -> RuntimeValue
runtimeIntValue value = VInt (fromIntegral value) untypedIntMetadata

canonicalList :: (value -> RuntimeValue) -> [value] -> RuntimeValue
canonicalList render values = VList (map render values) Nothing

canonicalMaybe :: (value -> RuntimeValue) -> Maybe value -> RuntimeValue
canonicalMaybe render maybeInput =
  case maybeInput of
    Nothing -> canonicalNullaryConstructor "Nothing"
    Just value -> canonicalConstructor "Just" [render value]

decodeList :: Text -> (RuntimeValue -> Either Text value) -> RuntimeValue -> Either Text [value]
decodeList label decodeElement value =
  case value of
    VList elements _ -> traverse decodeElement elements
    _ -> Left (label <> " expected a List, got " <> runtimeValueCategory value)

decodeText :: Text -> RuntimeValue -> Either Text Text
decodeText label value =
  case value of
    VText textValue -> Right textValue
    _ -> Left (label <> " expected Text, got " <> runtimeValueCategory value)

decodeInteger :: Text -> RuntimeValue -> Either Text Integer
decodeInteger label value =
  case value of
    VInt integer _ -> Right integer
    _ -> Left (label <> " expected Int, got " <> runtimeValueCategory value)

decodeInt :: Text -> RuntimeValue -> Either Text Int
decodeInt label value = do
  integer <- decodeInteger label value
  if integer < toInteger (minBound :: Int) || integer > toInteger (maxBound :: Int)
    then Left (label <> " is outside the host Int range: " <> Text.pack (show integer))
    else Right (fromInteger integer)

expectConstructor :: Text -> RuntimeValue -> Either Text (Text, [RuntimeValue])
expectConstructor label value =
  case value of
    VConstructor _ _ constructorName _ arguments -> Right (identifierText constructorName, arguments)
    _ -> Left (label <> " expected a constructor, got " <> runtimeValueCategory value)

expectNamedConstructor :: Text -> Text -> Int -> RuntimeValue -> Either Text [RuntimeValue]
expectNamedConstructor label expectedName expectedArity value = do
  (actualName, arguments) <- expectConstructor label value
  if actualName /= expectedName
    then Left (label <> " expected constructor '" <> expectedName <> "', got '" <> actualName <> "'")
    else expectArity expectedName expectedArity arguments

expectArity :: Text -> Int -> [RuntimeValue] -> Either Text [RuntimeValue]
expectArity name expected arguments
  | length arguments == expected = Right arguments
  | otherwise = Left (name <> " expected " <> Text.pack (show expected) <> " field(s), got " <> Text.pack (show (length arguments)))

expectNullary :: Text -> [RuntimeValue] -> value -> Either Text value
expectNullary name arguments value = do
  _ <- expectArity name 0 arguments
  Right value

runtimeValueCategory :: RuntimeValue -> Text
runtimeValueCategory value =
  case value of
    VInt {} -> "Int"
    VFloat {} -> "Float"
    VBool {} -> "Bool"
    VChar {} -> "Char"
    VText {} -> "Text"
    VList {} -> "List"
    VTuple {} -> "Tuple"
    VConstructor {} -> "constructor"
    VClosure {} -> "closure"
    VBuiltin {} -> "builtin"
    VOperator {} -> "operator"
    VSectionLeft {} -> "left section"
    VSectionRight {} -> "right section"
    VQualifiedMethod {} -> "qualified method"
    VTyped {} -> "typed value"
    VExplicitTypeApplication {} -> "explicit type application"
    _ -> "runtime value"
