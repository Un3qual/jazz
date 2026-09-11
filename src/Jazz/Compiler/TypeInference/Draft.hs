{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Private checked-tree builders. Checking chooses the tree structure and
-- captures each node's facts; finalization supplies only the solved state.
module Jazz.Compiler.TypeInference.Draft
  ( Attachment (..),
    attachmentResult,
    Draft (..),
    finalizeDraft,
    CheckedExpr (..),
  )
where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Jazz.Compiler.AST (CorePhase (Analyzed), Expr)
import Jazz.Compiler.SemanticFacts (SemanticFactInvariantFailure)
import Jazz.Compiler.TypeInference.State (InferState)
import Jazz.Compiler.TypeInference.Types (ExpressionType)

data Attachment value
  = Attached value
  | AttachmentFailed SemanticFactInvariantFailure !(Seq SemanticFactInvariantFailure)
  deriving (Functor)

instance Applicative Attachment where
  pure = Attached
  Attached project <*> Attached value = Attached (project value)
  AttachmentFailed failure failures <*> AttachmentFailed next rest =
    AttachmentFailed failure (failures Seq.>< (next Seq.<| rest))
  AttachmentFailed failure failures <*> Attached _ = AttachmentFailed failure failures
  Attached _ <*> AttachmentFailed failure failures = AttachmentFailed failure failures

attachmentResult :: Attachment value -> Either (NonEmpty SemanticFactInvariantFailure) value
attachmentResult (Attached value) = Right value
attachmentResult (AttachmentFailed failure failures) = Left (failure :| toList failures)

newtype Draft value = Draft {runDraft :: InferState -> Attachment value}
  deriving (Functor)

instance Applicative Draft where
  pure value = Draft (const (pure value))
  Draft function <*> Draft argument = Draft (\solved -> function solved <*> argument solved)

finalizeDraft :: InferState -> Draft value -> Either (NonEmpty SemanticFactInvariantFailure) value
finalizeDraft solved (Draft draft) = attachmentResult (draft solved)

data CheckedExpr = CheckedExpr
  { checkedExprType :: Maybe ExpressionType,
    checkedExprTree :: Draft (Expr 'Analyzed)
  }
