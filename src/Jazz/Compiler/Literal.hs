{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Literal values and source metadata shared unchanged across compiler phases.
module Jazz.Compiler.Literal (Literal (..)) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)
import Jazz.Compiler.FractionalLiteral (FractionalLiteralSource)
import Jazz.Compiler.TypeRepresentation (NumericType)

data Literal
  = LInt Integer
  | LFloat Double FractionalLiteralSource (Maybe NumericType)
  | LBool Bool
  | LChar Char
  | LText Text
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
