-- | Concrete context and callback types shared by the recursive surface
-- expression and statement grammars.
module JazzNext.Compiler.Parser.Context
  ( ExpressionParser,
    ParserContext (..),
    StatementBlockParser,
    StatementContext (..),
    initialParserContext
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.Parser.AST
  ( SurfaceExpr,
    SurfaceStatement
  )
import JazzNext.Compiler.Parser.Operator (OperatorInfo)
import JazzNext.Compiler.Parser.TokenParser (Parser)

data StatementContext
  = TopLevelContext
  | ModuleBodyContext
  | NestedBlockContext
  deriving (Eq, Show)

data ParserContext = ParserContext
  { parserKnownAliases :: Set Text,
    parserDeclaredOperators :: [OperatorInfo],
    parserStatementContext :: StatementContext
  }
  deriving (Eq, Show)

type ExpressionParser = ParserContext -> Parser SurfaceExpr

type StatementBlockParser = ParserContext -> Parser [SurfaceStatement]

initialParserContext :: ParserContext
initialParserContext =
  ParserContext
    { parserKnownAliases = Set.empty,
      parserDeclaredOperators = [],
      parserStatementContext = TopLevelContext
    }
