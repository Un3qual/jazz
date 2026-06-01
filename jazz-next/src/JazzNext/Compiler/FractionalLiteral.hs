-- | Source-exact metadata for decimal fractional literals.
module JazzNext.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
    fractionalLiteralHasNonZeroFractionalDigits,
    fractionalLiteralIntegralValue,
    mkFractionalLiteralSource
  ) where

import Data.Ratio
  ( (%) )

-- | Preserve enough decimal source structure to validate literal conversions
-- before binary floating-point rounding can hide boundary cases.
data FractionalLiteralSource = FractionalLiteralSource Integer Integer Integer
  deriving (Eq, Show)

mkFractionalLiteralSource :: Integer -> Integer -> Int -> FractionalLiteralSource
mkFractionalLiteralSource wholePart fractionalPart fractionalDigitCount =
  FractionalLiteralSource wholePart fractionalPart (10 ^ fractionalDigitCount)

fractionalLiteralHasNonZeroFractionalDigits :: FractionalLiteralSource -> Bool
fractionalLiteralHasNonZeroFractionalDigits (FractionalLiteralSource _ fractionalPart _) =
  fractionalPart /= 0

fractionalLiteralIntegralValue :: FractionalLiteralSource -> Maybe Integer
fractionalLiteralIntegralValue source@(FractionalLiteralSource wholePart _ _)
  | fractionalLiteralHasNonZeroFractionalDigits source = Nothing
  | otherwise = Just wholePart

fractionalLiteralExceedsMagnitude :: FractionalLiteralSource -> Double -> Bool
fractionalLiteralExceedsMagnitude source maxMagnitude =
  abs (fractionalLiteralToRational source) > toRational maxMagnitude

fractionalLiteralToRational :: FractionalLiteralSource -> Rational
fractionalLiteralToRational (FractionalLiteralSource wholePart fractionalPart fractionalScale) =
  ((wholePart * fractionalScale) + fractionalPart) % fractionalScale
