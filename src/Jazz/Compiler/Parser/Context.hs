-- | Concrete context and callback types shared by the recursive surface
-- expression and statement grammars.
module Jazz.Compiler.Parser.Context
  ( ExpressionParser,
    ParserContext (..),
    StatementBlockParser,
    StatementContext (..),
    initialParserContext
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Jazz.Compiler.Parser.AST
  ( SurfaceExpr,
    SurfaceStatement
  )
import Jazz.Compiler.Parser.Operator
  ( OperatorTable,
    emptyOperatorTable
  )
import Jazz.Compiler.Parser.TokenParser (Parser)

data StatementContext
  = TopLevelContext
  | ModuleBodyContext
  | NestedBlockContext
  deriving (Eq, Show)

data ParserContext = ParserContext
  { parserKnownAliases :: Set Text,
    parserDeclaredOperators :: OperatorTable,
    parserStatementContext :: StatementContext
  }
  deriving (Eq, Show)

type ExpressionParser = ParserContext -> Parser SurfaceExpr

type StatementBlockParser = ParserContext -> Parser [SurfaceStatement]

initialParserContext :: ParserContext
initialParserContext =
  ParserContext
    { parserKnownAliases = Set.empty,
      parserDeclaredOperators = emptyOperatorTable,
      parserStatementContext = TopLevelContext
    }
