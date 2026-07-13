{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser.Foundation.Shared
  ( float64MaxFiniteInteger,
    float64MaxFinite
  ) where


float64MaxFiniteInteger :: Integer
float64MaxFiniteInteger =
  ceiling (float64MaxFinite :: Double)

float64MaxFinite :: Double
float64MaxFinite =
  encodeFloat
    (floatRadix sample ^ floatDigits sample - 1)
    (snd (floatRange sample) - floatDigits sample)
  where
    sample = 0 :: Double
