module AST where

import Data.Text
import qualified Data.Text as T

type Program = [Expr]

data Literal
  = LInt Integer
  | LFloat Double
  | LBool Bool
  | LString T.Text
  | LList [Expr]
  | LTuple [Expr]
  deriving (Eq, Show)

-- Allowed characters for infix operators: !#$%&*+./<=>?@\^|-~:
-- data InfixExpr = InfixExpr
--   { ieSymbol :: T.Text
--   , ieLhs :: Expr
--   , ieRhs :: Expr
--   } deriving (Eq, Show)

data Type
  = TVar T.Text
  | TCon T.Text
  | TString
  | TInt
  | TFloat
  | TBool
  | TList Type
  | TTuple [Type]
  | TConstructor T.Text [Type]
  | TPoly T.Text Type -- here for "forall stuff"
  | TLambda Type Type
  | TVoid
  | TUnit
  deriving (Show, Eq)

data Variable = Variable {
  varName :: T.Text
, varType :: Maybe Type
} deriving (Show, Eq)

data Expr
  = ELiteral Literal
  | EVar Variable -- for usage of a variable
  | ETuple [Expr]
  | ELambda (Maybe FunParam) Expr (Maybe Type) -- Lambda function with a list of arguments for supporting currying
  | EApply Expr Expr -- Function application
  | ELet Variable Expr --Expr -- Let bindings for immutable variables
  | EData T.Text [Type] [Constructor] -- Algebraic data type definitions with type parameters and constructors
  | ETypeSignature Variable [Variable] Type -- Variable name, class constraints, type
  -- | ETypeAnnotation Expr Type -- Type annotations for expressions
  -- | EInfixExpr InfixExpr
  deriving (Show, Eq)

data Statement 
  = SLet Variable Expr
  | SExpr Expr
  deriving (Show, Eq)

data Constructor = Constructor T.Text [Type]
  deriving (Show, Eq)

data FunParam 
  = FPSimple Variable
  | FPPattern Pattern
  deriving (Show, Eq)


-- Functions that are impure must end with an exclamation mark (e.g add is ok, but println, must be println!)
data FunId = FunId {
  funName :: T.Text,
  funReturnType :: Type,
  funParams :: [FunParam],
  funIsInfix :: Bool,
  funIsPure :: Bool
} deriving (Show, Eq)

data Pattern
  = PatternLiteral Literal
  | PatternVar Variable
  | PatternTuple [FunParam]
  | PatternList [FunParam]
  | PatternConstructor T.Text [Pattern]
  deriving (Show, Eq)

-- data Expr
--   = EValDelc ValDelc
--   | EFuncCall FuncCall
--   deriving (Show, Eq)

-- data PrimType
--   = JInt
--   | JBool
--   | JFloat
--   | JString
--   | JList

-- data Val = PrimType a

-- data FuncDecl
--   = FD T.Text PrimType [(T.Text, PrimType)] Expr -- name, type, args, body
--   deriving (Show, Eq)

-- data ValDecl
--   = VDFuncDecl FuncDecl
--   | VDValDecl T.Text PrimType Expr
--   deriving (Show Eq)

