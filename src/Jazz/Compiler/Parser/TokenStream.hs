{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Indexed parser input. Slices share one immutable token array, so parser
-- cursors and Megaparsec position state cannot retain separate list prefixes.
module Jazz.Compiler.Parser.TokenStream
  ( TokenStream,
    data EmptyTokens,
    data (:<),
    tokenStreamBreak,
    tokenStreamDrop,
    tokenStreamFromList,
    tokenStreamLength,
    tokenStreamNull,
    tokenStreamSpan,
    tokenStreamSplitAt,
    tokenStreamTake,
    tokenStreamToList,
    tokenStreamUncons,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Jazz.Compiler.Diagnostics (spanColumn, spanLine)
import qualified Jazz.Compiler.Parser.Lexer as Lexer
import Text.Megaparsec
  ( PosState (..),
    Stream (..),
    TraversableStream (..),
    VisualStream (..),
  )
import Text.Megaparsec.Pos (SourcePos (..), mkPos)

newtype TokenStream = TokenStream (Vector.Vector Lexer.Token)
  deriving (Eq, Ord, Show)

tokenStreamFromList :: [Lexer.Token] -> TokenStream
tokenStreamFromList = TokenStream . Vector.fromList

tokenStreamToList :: TokenStream -> [Lexer.Token]
tokenStreamToList (TokenStream tokens) = Vector.toList tokens

tokenStreamNull :: TokenStream -> Bool
tokenStreamNull (TokenStream tokens) = Vector.null tokens

tokenStreamLength :: TokenStream -> Int
tokenStreamLength (TokenStream tokens) = Vector.length tokens

tokenStreamTake :: Int -> TokenStream -> TokenStream
tokenStreamTake count (TokenStream tokens) =
  TokenStream (Vector.take count tokens)

tokenStreamDrop :: Int -> TokenStream -> TokenStream
tokenStreamDrop count (TokenStream tokens) =
  TokenStream (Vector.drop count tokens)

tokenStreamSplitAt :: Int -> TokenStream -> (TokenStream, TokenStream)
tokenStreamSplitAt count (TokenStream tokens) =
  let (prefix, suffix) = Vector.splitAt count tokens
   in (TokenStream prefix, TokenStream suffix)

tokenStreamSpan :: (Lexer.Token -> Bool) -> TokenStream -> (TokenStream, TokenStream)
tokenStreamSpan predicate (TokenStream tokens) =
  let (prefix, suffix) = Vector.span predicate tokens
   in (TokenStream prefix, TokenStream suffix)

tokenStreamBreak :: (Lexer.Token -> Bool) -> TokenStream -> (TokenStream, TokenStream)
tokenStreamBreak predicate = tokenStreamSpan (not . predicate)

tokenStreamUncons :: TokenStream -> Maybe (Lexer.Token, TokenStream)
tokenStreamUncons (TokenStream tokens) =
  case Vector.uncons tokens of
    Nothing -> Nothing
    Just (token, remaining) -> Just (token, TokenStream remaining)

pattern EmptyTokens :: TokenStream
pattern EmptyTokens <- (tokenStreamUncons -> Nothing)

pattern (:<) :: Lexer.Token -> TokenStream -> TokenStream
pattern token :< remaining <- (tokenStreamUncons -> Just (token, remaining))

infixr 5 :<

{-# COMPLETE EmptyTokens, (:<) #-}

instance Stream TokenStream where
  type Token TokenStream = Lexer.Token
  type Tokens TokenStream = TokenStream

  tokenToChunk _ token = tokenStreamFromList [token]
  tokensToChunk _ = tokenStreamFromList
  chunkToTokens _ = tokenStreamToList
  chunkLength _ = tokenStreamLength
  chunkEmpty _ = tokenStreamNull
  take1_ = tokenStreamUncons
  takeN_ requested stream
    | requested <= 0 = Just (tokenStreamTake 0 stream, stream)
    | tokenStreamNull stream = Nothing
    | otherwise = Just (tokenStreamSplitAt requested stream)
  takeWhile_ = tokenStreamSpan

instance VisualStream TokenStream where
  showTokens _ =
    Text.unpack
      . Text.intercalate " "
      . map Lexer.tokenLexeme
      . NonEmpty.toList

instance TraversableStream TokenStream where
  reachOffsetNoLine requested positionState =
    let consumedCount = max 0 (requested - pstateOffset positionState)
        remaining = tokenStreamDrop consumedCount (pstateInput positionState)
        nextSourcePosition =
          case remaining of
            token :< _ -> sourcePositionForToken (sourceName (pstateSourcePos positionState)) token
            EmptyTokens -> pstateSourcePos positionState
     in positionState
          { pstateInput = remaining,
            pstateOffset = requested,
            pstateSourcePos = nextSourcePosition,
            pstateLinePrefix = ""
          }

sourcePositionForToken :: FilePath -> Lexer.Token -> SourcePos
sourcePositionForToken label token =
  let spanValue = Lexer.tokenSpan token
   in SourcePos label (mkPos (spanLine spanValue)) (mkPos (spanColumn spanValue))
