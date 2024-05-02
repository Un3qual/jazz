{-# LANGUAGE NamedFieldPuns
           , LambdaCase
           , DeriveFunctor
           , TypeSynonymInstances
           , FlexibleInstances #-}

module AST where

import qualified Data.Text as T
import           Data.Text.Lens
import           Data.Data (DataType)
import           Data.String
import           Data.List
import           Text.Megaparsec
import           Text.Megaparsec.Pos (Pos)
import Test.Hspec (Expectation, shouldBe)
import Control.Applicative (Const)


data Ann a b =
    Ann a b
    deriving (Show, Eq, Functor)

annotation :: Ann a b -> a
annotation (Ann a _) = a

type SpannedProgram = [SpannedExpr]
type TypedProgram = [TypedExpr]
type SchemedProgram = [SchemedExpr]

data Span = Span SourcePos SourcePos deriving (Eq, Ord)

nullSpan :: Span
nullSpan = Span (initialPos "") (initialPos "")
ns = nullSpan

formatSourcePos :: SourcePos -> String
formatSourcePos SourcePos{sourceName, sourceLine = sLine, sourceColumn = sCol} = sourceName <> ":" <> (show . unPos) sLine <> ":" <> (show . unPos) sCol
instance Show Span where
  show sp@(Span start end)
    | sp == ns = "ns"
    | otherwise  = (show . formatSourcePos) start <> "-" <> (show . formatSourcePos) end

type Spanned a = Ann Span a
type SpannedParam = Spanned (FunParam Span)
type SpannedExpr = Spanned (Expr Span)
type SpannedLiteral = Spanned (Literal Span)
type SpannedPattern = Spanned (Pattern Span)

type Typed a = Ann (Span, Type) a
type TypedParam = Typed (FunParam (Span, Type))
type TypedExpr = Typed (Expr (Span, Type))
type TypedLiteral = Typed (Literal (Span, Type))
type TypedPattern = Typed (Pattern (Span, Type))

type Schemed a = Ann (Span, Scheme) a
type SchemedParam = Schemed (FunParam (Span, Scheme))
type SchemedExpr = Schemed (Expr (Span, Scheme))
type SchemedLiteral = Schemed (Literal (Span, Scheme))
type SchemedPattern = Schemed (Pattern (Span, Scheme))

data Scheme = Scheme [(TVar, [Trait])] Type deriving (Show, Eq)
newtype Trait = Trait Name deriving (Show, Eq, Ord)
newtype TCon = TC T.Text deriving (Show, Eq, Ord)
-- instance T.IsText Trait where
--     fromString = Trait

data Literal a
  = LInt Integer
  | LFloat Double
  | LBool Bool
  | LString T.Text
  | LList [Ann a (Expr a)]
  | LTuple [Ann a (Expr a)]
  deriving (Eq, Show)

type Name = T.Text
newtype TVar = TV Name deriving (Eq, Ord, Show)

data Type
  = TVar TVar
  | TCon TCon [Type]
  | TString
  | TInt
  | TFloat
  | TBool
  | TList Type
  | TTuple [Type]
  -- | TConstructor T.Text [Type]
  | TPoly T.Text Type -- here for "forall stuff"
  | TLambda Type Type
  | TVoid
  | TUnit
  deriving (Eq, Ord)

instance Show Type where
  show (TVar (TV n)) = "_" <> T.unpack n
  show (TCon (TC n) ts) = "{" <> T.unpack n <> "}" <> "(" <> intercalate ", " (map show ts) <> ")"
  show TString = "String"
  show TInt = "Int"
  show TFloat = "Float"
  show TBool = "Bool"
  show (TList t) = "[" <> show t <> "]"
  show (TTuple ts) = "(" <> intercalate ", " (map show ts) <> ")"
  show (TPoly n t) = "forall " <> T.unpack n <> ". " <> show t
  show (TLambda t1 t2) = "(" <> show t1 <> " -> " <> show t2 <> ")"
  show TVoid = "Void"
  show TUnit = "Unit"



data Variable = Variable T.Text
  deriving (Show, Eq)

data Expr a
  = ELiteral (Ann a (Literal a))
  | EVar Variable -- for usage of a variable
  -- | ETuple [Ann a (Expr a)]
  | ELambda (Maybe (Ann a (FunParam a))) (Ann a (Expr a))-- Lambda function with a list of arguments for supporting currying
  | EApply (Ann a (Expr a)) (Ann a (Expr a)) -- Function application
  | ELet Variable (Ann a (Expr a)) --Expr -- Let bindings for immutable variables
  | EData T.Text [Type] [Constructor] -- Algebraic data type definitions with type parameters and constructors
  | ETypeSignature Variable [Type] Type -- Variable name, class constraints, type
  | EBlock [Ann a (Expr a)]
  | EModule ModuleName (Ann a (Expr a))
  | EImport ImportStatement
  | EClass [Type] Type (Ann a (Expr a))
  | EClassImpl [Type] Type (Ann a (Expr a))
  | ECase (Ann a (Expr a)) [(Ann a (Pattern a), Ann a (Expr a))]
  | EIf (Ann a (Expr a)) (Ann a (Expr a)) (Ann a (Expr a))
  deriving (Show, Eq)

mkSpannedLIntExpr :: Integer -> SpannedExpr
mkSpannedLIntExpr i = Ann ns (ELiteral (Ann ns (LInt i)))
mkTypedLIntExpr :: Integer -> TypedExpr
mkTypedLIntExpr i = Ann (ns, TCon (TC "Integer") []) (ELiteral (Ann (ns, TCon (TC "Integer") []) (LInt i)))

mkTCon :: T.Text -> Type
mkTCon n = TCon (TC n) []

data ModuleName
  = ModuleNameGlobal [T.Text]
  | ModuleNameChild [T.Text]
  deriving (Show, Eq)

data ImportQualifier
  = ModuleQualified T.Text
  | FunctionQualified [T.Text]
  deriving (Show, Eq)

data ImportStatement = ImportStatement {
    iModuleName :: ModuleName
  , iQualifier :: Maybe ImportQualifier
} deriving (Show, Eq)

data Constructor = Constructor T.Text [Type]
  deriving (Show, Eq)

data FunParam a
  = FPSimple Variable
  | FPPattern (Ann a (Pattern a))
  deriving (Show, Eq)

data Pattern a
  = PatternLiteral (Ann a (Literal a))
  | PatternVar Variable
  | PatternTuple [Ann a (FunParam a)]
  | PatternList [Ann a (FunParam a)]
  | PatternConstructor T.Text [Ann a (FunParam a)]
  | PatternWildcard
  deriving (Show, Eq)


-- Unannotated AST
data TLiteral
  = TLInt Integer
  | TLFloat Double
  | TLBool Bool
  | TLString T.Text
  | TLList [TExpr]
  | TLTuple [TExpr]
  deriving (Eq, Show)

data TExpr
  = TELiteral TLiteral
  | TEVar Variable
  | TELambda (Maybe TFunParam) TExpr
  | TEApply TExpr TExpr
  | TELet Variable TExpr
  | TEData T.Text [Type] [Constructor]
  | TETypeSignature Variable [Type] Type
  | TEBlock [TExpr]
  | TEModule ModuleName TExpr
  | TEImport ImportStatement
  | TEClass [Type] Type TExpr
  | TEClassImpl [Type] Type TExpr
  | TECase TExpr [(TPattern, TExpr)]
  | TEIf TExpr TExpr TExpr
  deriving (Show, Eq)

data TFunParam
  = TFPSimple Variable
  | TFPPattern TPattern
  deriving (Show, Eq)

data TPattern
  = TPatternLiteral TLiteral
  | TPatternVar Variable
  | TPatternTuple [TFunParam]
  | TPatternList [TFunParam]
  | TPatternConstructor T.Text [TFunParam]
  | TPatternWildcard
  deriving (Show, Eq)

unSpan :: Ann a (Expr a) -> TExpr
unSpan (Ann _ expr) = case expr of
  ELiteral lit -> TELiteral (unSpanLiteral lit)
  EVar v -> TEVar v
  ELambda p e -> TELambda (fmap unSpanParam p) (unSpan e)
  EApply f a -> TEApply (unSpan f) (unSpan a)
  ELet v e -> TELet v (unSpan e)
  EData n t cs -> TEData n t cs
  ETypeSignature v cs t -> TETypeSignature v cs t
  EBlock es -> TEBlock (map unSpan es)
  EModule m e -> TEModule m (unSpan e)
  EImport i -> TEImport i
  EClass cs t e -> TEClass cs t (unSpan e)
  EClassImpl cs t e -> TEClassImpl cs t (unSpan e)
  ECase e ps -> TECase (unSpan e) (map (\(p, e) -> (unSpanPattern p, unSpan e)) ps)
  EIf c t f -> TEIf (unSpan c) (unSpan t) (unSpan f)

unSpanLiteral :: Ann a (Literal a) -> TLiteral
unSpanLiteral (Ann _ lit) = case lit of
  LInt i -> TLInt i
  LFloat f -> TLFloat f
  LBool b -> TLBool b
  LString s -> TLString s
  LList es -> TLList (map unSpan es)
  LTuple es -> TLTuple (map unSpan es)

unSpanParam :: Ann a (FunParam a) -> TFunParam
unSpanParam (Ann _ param) = case param of
  FPSimple v -> TFPSimple v
  FPPattern p -> TFPPattern (unSpanPattern p)

unSpanPattern :: Ann a (Pattern a) -> TPattern
unSpanPattern (Ann _ pat) = case pat of
  PatternLiteral lit -> TPatternLiteral (unSpanLiteral lit)
  PatternVar v -> TPatternVar v
  PatternTuple ps -> TPatternTuple (map unSpanParam ps)
  PatternList ps -> TPatternList (map unSpanParam ps)
  PatternConstructor n ps -> TPatternConstructor n (map unSpanParam ps)
  PatternWildcard -> TPatternWildcard


-- -- Null spanned Expr generators
-- tELiteral :: SpannedLiteral -> SpannedExpr
-- tELiteral lit = Ann undefined (ELiteral lit)
-- tEVar :: Variable -> SpannedExpr
-- tEVar var = Ann undefined (EVar var)
-- tETuple :: [SpannedExpr] -> SpannedExpr
-- tETuple exprs = Ann undefined (ETuple exprs)
-- tELambda :: Maybe SpannedParam -> SpannedExpr -> SpannedExpr
-- tELambda param body = Ann undefined (ELambda param body)
-- tEApply :: SpannedExpr -> SpannedExpr -> SpannedExpr
-- tEApply f a = Ann undefined (EApply f a)
-- tELet :: Variable -> SpannedExpr -> SpannedExpr
-- tELet var expr = Ann undefined (ELet var expr)
-- tEData :: T.Text -> [Type] -> [Constructor] -> SpannedExpr
-- tEData n t cs = Ann undefined (EData n t cs)
-- tETypeSignature :: Variable -> [Type] -> Type -> SpannedExpr
-- tETypeSignature v cs t = Ann undefined (ETypeSignature v cs t)
-- tEBlock :: [SpannedExpr] -> SpannedExpr
-- tEBlock exprs = Ann undefined (EBlock exprs)
-- tEModule :: ModuleName -> SpannedExpr -> SpannedExpr
-- tEModule m e = Ann undefined (EModule m e)
-- tEImport :: ImportStatement -> SpannedExpr
-- tEImport i = Ann undefined (EImport i)
-- tEClass :: [Type] -> Type -> SpannedExpr -> SpannedExpr
-- tEClass cs t e = Ann undefined (EClass cs t e)
-- tEClassImpl :: [Type] -> Type -> SpannedExpr -> SpannedExpr
-- tEClassImpl cs t e = Ann undefined (EClassImpl cs t e)
-- tECase :: SpannedExpr -> [(SpannedPattern, SpannedExpr)] -> SpannedExpr
-- tECase e ps = Ann undefined (ECase e ps)
-- tEIf :: SpannedExpr -> SpannedExpr -> SpannedExpr -> SpannedExpr
-- tEIf c t f = Ann undefined (EIf c t f)

-- -- Null spanned Literal generators
-- tLInt :: Integer -> SpannedLiteral
-- tLInt i = Ann undefined (LInt i)
-- tLFloat :: Double -> SpannedLiteral
-- tLFloat f = Ann undefined (LFloat f)
-- tLBool :: Bool -> SpannedLiteral
-- tLBool b = Ann undefined (LBool b)
-- tLString :: T.Text -> SpannedLiteral
-- tLString s = Ann undefined (LString s)
-- tLList :: [SpannedExpr] -> SpannedLiteral
-- tLList exprs = Ann undefined (LList exprs)
-- tLTuple :: [SpannedExpr] -> SpannedLiteral
-- tLTuple exprs = Ann undefined (LTuple exprs)

-- -- Null spanned Param generators
-- tFPSimple :: Variable -> SpannedParam
-- tFPSimple v = Ann undefined (FPSimple v)
-- tFPPattern :: SpannedPattern -> SpannedParam
-- tFPPattern p = Ann undefined (FPPattern p)

-- -- Null spanned Pattern generators
-- tPatternLiteral :: SpannedLiteral -> SpannedPattern
-- tPatternLiteral lit = Ann undefined (PatternLiteral lit)
-- tPatternVar :: Variable -> SpannedPattern
-- tPatternVar var = Ann undefined (PatternVar var)
-- tPatternTuple :: [SpannedParam] -> SpannedPattern
-- tPatternTuple ps = Ann undefined (PatternTuple ps)
-- tPatternList :: [SpannedParam] -> SpannedPattern
-- tPatternList ps = Ann undefined (PatternList ps)
-- tPatternConstructor :: T.Text -> [SpannedParam] -> SpannedPattern
-- tPatternConstructor n ps = Ann undefined (PatternConstructor n ps)
-- tPatternWildcard :: SpannedPattern
-- tPatternWildcard = Ann undefined PatternWildcard



-- class EqNoAnn a where
--   eqNoAnn :: a -> a -> Bool

-- shouldBeNoAnn :: (EqNoAnn a) => a -> a -> Expectation
-- shouldBeNoAnn actual expected = actual `eqNoAnn` expected `shouldBe` True

-- instance EqNoAnn SpannedProgram where
--   eqNoAnn [] [] = True
--   eqNoAnn (e1:es1) (e2:es2) = eqNoAnn e1 e2 && eqNoAnn es1 es2
--   eqNoAnn _ _ = False

-- instance EqNoAnn SpannedExpr where
--   eqNoAnn (Ann _ e1) (Ann _ e2) = eqNoAnn e1 e2

-- instance EqNoAnn (Expr Span) where
--   eqNoAnn (ELiteral l1) (ELiteral l2) = eqNoAnn l1 l2
--   eqNoAnn (EVar v1) (EVar v2) = v1 == v2
--   eqNoAnn (ETuple es1) (ETuple es2) = all (uncurry eqNoAnn) (zip es1 es2)
--   eqNoAnn (ELambda p1 e1) (ELambda p2 e2) = eqNoAnn p1 p2 && eqNoAnn e1 e2
--   eqNoAnn (EApply f1 a1) (EApply f2 a2) = eqNoAnn f1 f2 && eqNoAnn a1 a2
--   eqNoAnn (ELet v1 e1) (ELet v2 e2) = v1 == v2 && eqNoAnn e1 e2
--   eqNoAnn (EData n1 t1 cs1) (EData n2 t2 cs2) = n1 == n2 && t1 == t2 && all (uncurry eqNoAnn) (zip cs1 cs2)
--   eqNoAnn (ETypeSignature v1 cs1 t1) (ETypeSignature v2 cs2 t2) = v1 == v2 && cs1 == cs2 && t1 == t2
--   eqNoAnn (EBlock es1) (EBlock es2) = all (uncurry eqNoAnn) (zip es1 es2)
--   eqNoAnn (EModule m1 e1) (EModule m2 e2) = m1 == m2 && eqNoAnn e1 e2
--   eqNoAnn (EImport i1) (EImport i2) = i1 == i2
--   eqNoAnn (EClass cs1 t1 e1) (EClass cs2 t2 e2) = cs1 == cs2 && t1 == t2 && eqNoAnn e1 e2
--   eqNoAnn (EClassImpl cs1 t1 e1) (EClassImpl cs2 t2 e2) = cs1 == cs2 && t1 == t2 && eqNoAnn e1 e2
--   -- eqNoAnn (ECase e1 ps1) (ECase e2 ps2) = eqNoAnn e1 e2 && all (uncurry eqNoAnn) (zip ps1 ps2)
--   eqNoAnn (EIf c1 t1 f1) (EIf c2 t2 f2) = eqNoAnn c1 c2 && eqNoAnn t1 t2 && eqNoAnn f1 f2
--   eqNoAnn _ _ = False

-- instance EqNoAnn SpannedLiteral where
--   eqNoAnn (Ann _ l1) (Ann _ l2) = eqNoAnn l1 l2

-- instance EqNoAnn (Literal Span) where
--   eqNoAnn (LInt i1) (LInt i2) = i1 == i2
--   eqNoAnn (LFloat f1) (LFloat f2) = f1 == f2
--   eqNoAnn (LBool b1) (LBool b2) = b1 == b2
--   eqNoAnn (LString s1) (LString s2) = s1 == s2
--   eqNoAnn (LList ls1) (LList ls2) = all (uncurry eqNoAnn) (zip ls1 ls2)
--   eqNoAnn (LTuple ts1) (LTuple ts2) = all (uncurry eqNoAnn) (zip ts1 ts2)
--   eqNoAnn _ _ = False

-- instance EqNoAnn (FunParam Span) where
--     eqNoAnn (FPSimple v1) (FPSimple v2) = v1 == v2
--     eqNoAnn (FPPattern p1) (FPPattern p2) = eqNoAnn p1 p2
--     eqNoAnn _ _ = False

-- instance EqNoAnn SpannedParam where
--   eqNoAnn (Ann _ p1) (Ann _ p2) = eqNoAnn p1 p2

-- instance EqNoAnn (Maybe SpannedParam) where
--   eqNoAnn Nothing Nothing = True
--   eqNoAnn (Just e1) (Just e2) = eqNoAnn e1 e2
--   eqNoAnn _ _ = False

-- instance EqNoAnn SpannedPattern where
--   eqNoAnn (Ann _ p1) (Ann _ p2) = eqNoAnn p1 p2

-- instance EqNoAnn (Pattern Span) where
--   eqNoAnn (PatternLiteral l1) (PatternLiteral l2) = eqNoAnn l1 l2
--   eqNoAnn (PatternVar v1) (PatternVar v2) = v1 == v2
--   eqNoAnn (PatternTuple ts1) (PatternTuple ts2) = all (uncurry eqNoAnn) (zip ts1 ts2)
--   eqNoAnn (PatternList ls1) (PatternList ls2) = all (uncurry eqNoAnn) (zip ls1 ls2)
--   eqNoAnn (PatternConstructor n1 ps1) (PatternConstructor n2 ps2) = n1 == n2 && all (uncurry eqNoAnn) (zip ps1 ps2)
--   eqNoAnn PatternWildcard PatternWildcard = True
--   eqNoAnn _ _ = False

-- instance EqNoAnn Constructor where
--   eqNoAnn (Constructor n1 ts1) (Constructor n2 ts2) = n1 == n2 && ts1 == ts2

-- instance (EqNoAnn a, EqNoAnn b) => EqNoAnn (Either a b) where
--   eqNoAnn (Left _) (Right _) = False
--   eqNoAnn (Right _) (Left _) = False
--   eqNoAnn (Left a) (Left b) = eqNoAnn a b
--   eqNoAnn (Right a) (Right b) = eqNoAnn a b