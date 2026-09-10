{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | One-based source positions. Range endpoints are exclusive. Point-only
-- constructors remain available for synthetic nodes and legacy interchange.
module Jazz.Compiler.SourceSpan
  ( SourceSpan (..),
    sourceSpanEnd,
    sourceSpanStart,
    spanThrough,
    unqualifySourceSpan,
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data SourceSpan
  = SourceSpan {spanLine :: Int, spanColumn :: Int}
  | SourceSpanIn {spanSourcePath :: FilePath, spanLine :: Int, spanColumn :: Int}
  | SourceRange {spanLine :: Int, spanColumn :: Int, spanEndLine :: Int, spanEndColumn :: Int}
  | SourceRangeIn {spanSourcePath :: FilePath, spanLine :: Int, spanColumn :: Int, spanEndLine :: Int, spanEndColumn :: Int}
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

sourceSpanEnd :: SourceSpan -> Maybe (Int, Int)
sourceSpanEnd SourceRange {spanEndLine = line, spanEndColumn = column} = Just (line, column)
sourceSpanEnd SourceRangeIn {spanEndLine = line, spanEndColumn = column} = Just (line, column)
sourceSpanEnd _ = Nothing

-- | Explicit projection for consumers whose schema stores only start positions.
sourceSpanStart :: SourceSpan -> SourceSpan
sourceSpanStart spanValue = case spanValue of
  SourceSpanIn path line column -> SourceSpanIn path line column
  SourceRangeIn path line column _ _ -> SourceSpanIn path line column
  _ -> SourceSpan (spanLine spanValue) (spanColumn spanValue)

-- | Extend the first location through the second range, retaining qualification.
-- A point endpoint has no known extent, so it cannot extend a range.
spanThrough :: SourceSpan -> SourceSpan -> SourceSpan
spanThrough start end = case sourceSpanEnd end of
  Nothing -> start
  Just (line, column) -> case start of
    SourceSpanIn path startLine startColumn -> SourceRangeIn path startLine startColumn line column
    SourceRangeIn path startLine startColumn _ _ -> SourceRangeIn path startLine startColumn line column
    _ -> SourceRange (spanLine start) (spanColumn start) line column

-- | Remove a synthetic source path without discarding the known extent.
unqualifySourceSpan :: SourceSpan -> SourceSpan
unqualifySourceSpan spanValue = case sourceSpanEnd spanValue of
  Nothing -> SourceSpan (spanLine spanValue) (spanColumn spanValue)
  Just (line, column) -> SourceRange (spanLine spanValue) (spanColumn spanValue) line column
