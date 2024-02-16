module AST where

import Data.Text
import qualified Data.Text as T

type Program = [Expr]

data Literal
  = LInt Integer
  | LFloat Double
  | LBool Bool
  | LString String
  deriving (Eq, Show)

data Type
  = TVar T.Text
  | TCon T.Text
  | TString
  | TInt
  | TFloat
  | TBool
  | TList Type
  | TTuple [Type]
  | TConstructor String [Type]
  | TypePoly String Type
  deriving (Show, Eq)

data Variable = Variable {
  varName :: T.Text,
  varType :: T.Text
} deriving (Show, Eq)

data Expr
  = ELiteral Literal
  | EVar Variable
  | ETuple [Expr]
  | ASTLambda [FunParam] Expr -- Lambda function with a list of arguments for supporting currying
  | ASTApply Expr Expr -- Function application
  | ASTLet Variable Expr Expr -- Let bindings for immutable variables
  | ASTData String [Type] [Constructor] -- Algebraic data type definitions with type parameters and constructors
  | ASTTypeAnnotation Expr Type -- Type annotations for expressions
  deriving (Show, Eq)

data Constructor = Constructor String [Type]
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
  funIsPure :: Bool
} deriving (Show, Eq)

data Pattern
  = PatternLiteral Literal
  | PatternVar Variable
  | PatternTuple [Pattern]
  | PatternList [Pattern]
  | PatternConstructor String [Pattern]
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

