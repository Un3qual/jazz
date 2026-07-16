-- | Source-exact metadata for decimal fractional literals.
module JazzNext.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
    fractionalLiteralHasNonZeroFractionalDigits,
    fractionalLiteralIntegralValue,
    fractionalLiteralSourceParts,
    mkFractionalLiteralSource,
  )
where

import Data.Ratio
  ( (%),
  )

-- | Preserve enough decimal source structure to validate literal conversions
-- before binary floating-point rounding can hide boundary cases.
data FractionalLiteralSource = FractionalLiteralSource Integer Integer Integer
  deriving (Eq, Show)

mkFractionalLiteralSource :: Integer -> Integer -> Int -> FractionalLiteralSource
mkFractionalLiteralSource wholePart fractionalPart fractionalDigitCount =
  FractionalLiteralSource wholePart signedFractionalPart (10 ^ fractionalDigitCount)
  where
    signedFractionalPart
      | wholePart < 0 = negate (abs fractionalPart)
      | otherwise = abs fractionalPart

fractionalLiteralHasNonZeroFractionalDigits :: FractionalLiteralSource -> Bool
fractionalLiteralHasNonZeroFractionalDigits (FractionalLiteralSource _ fractionalPart _) =
  fractionalPart /= 0

fractionalLiteralIntegralValue :: FractionalLiteralSource -> Maybe Integer
fractionalLiteralIntegralValue source@(FractionalLiteralSource wholePart _ _)
  | fractionalLiteralHasNonZeroFractionalDigits source = Nothing
  | otherwise = Just wholePart

-- | Recover the source-normalized whole part, fractional digits as an integer,
-- and decimal scale. This keeps exact decimal serialization independent of the
-- rounded binary value carried beside it in the surface AST.
fractionalLiteralSourceParts :: FractionalLiteralSource -> (Integer, Integer, Integer)
fractionalLiteralSourceParts (FractionalLiteralSource wholePart fractionalPart fractionalScale) =
  (wholePart, fractionalPart, fractionalScale)

fractionalLiteralExceedsMagnitude :: FractionalLiteralSource -> Double -> Bool
fractionalLiteralExceedsMagnitude source maxMagnitude =
  abs (fractionalLiteralToRational source) > toRational maxMagnitude

fractionalLiteralToRational :: FractionalLiteralSource -> Rational
fractionalLiteralToRational (FractionalLiteralSource wholePart fractionalPart fractionalScale) =
  ((wholePart * fractionalScale) + fractionalPart) % fractionalScale
