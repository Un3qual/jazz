-- | Hosted compiler contract outcomes. These raw values belong to the portable
-- schema; compiler production uses its proof-carrying concrete build result.
module Jazz.Compiler.TypedCore.Portable
  ( PortableTypedCoreOutcome (..),
  )
where

import Jazz.Compiler.TypedCore (TypedCoreValidationFailure, TypedProgram)

data PortableTypedCoreOutcome
  = TypedCoreBlockedByDiagnostics
  | TypedCoreInvariantFailures [TypedCoreValidationFailure]
  | TypedCoreSucceeded TypedProgram
  deriving (Eq, Ord, Show)
