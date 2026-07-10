{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.Parser.Foundation.Shared
  ( float64MaxFiniteInteger,
    float64MaxFinite
  ) where


import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( ClassMethodSignature (..),
    ConstraintSignatureType (..),
    Expr (..),
    Literal (..),
    NumericType (..),
    SignatureConstraint (..),
    SignaturePayload (..),
    SignatureToken (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( SourceSpan (..)
  )
import JazzNext.Compiler.Parser
  ( parseSurfaceProgram
  )
import JazzNext.Compiler.Parser.AST
  ( SurfaceClassMethodSignature (..),
    SurfaceConstrainedSignatureType (..),
    SurfaceExpr (..),
    SurfaceLiteral (..),
    SurfaceNumericType (..),
    SurfaceSignatureConstraint (..),
    SurfaceSignaturePayload (..),
    SurfaceSignatureToken (..),
    SurfaceSignatureType (..),
    SurfaceStatement (..)
  )
import JazzNext.Compiler.Parser.Lower
  ( lowerSurfaceExpr
  )
import JazzNext.Compiler.Name (qualifiedName)
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    assertLeftDiagnosticContains,
    assertRight,
    runTestSuite
  )

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
