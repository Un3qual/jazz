{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Types where

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text, pack, unpack)
import           Data.Semigroup
import           Data.Monoid
import           Data.Bifunctor as BF
import           Control.Monad

import AST
import Data.Foldable

-- Subst stuff
newtype Subst = Subst (Map.Map Type Type) deriving (Show, Eq)

instance Substitutable Subst where
  applySubst s (Subst target) = Subst (fmap (applySubst s) target)
  freeTypeVars (Subst tvars) = Set.fromList (map (\(TVar tv) -> tv) (filter isTVar (Map.keys tvars)))

instance Semigroup Subst where
  subst1 <> subst2 = Subst (s1 <> s2)
    where
      Subst s1 = subst1
      Subst s2 = applySubst subst1 subst2

instance Monoid Subst where
  mappend = (<>)
  mempty = Subst Map.empty

getSubst :: Type -> Subst -> Maybe Type
getSubst tv (Subst m) = Map.lookup tv m

-- Scheme stuff
instance Substitutable Scheme where
  applySubst (Subst subst) (Scheme cs ty) =
    let cs' = Map.fromList $ map (BF.first TVar) cs
        subst' = Subst (Map.difference subst cs')
    in Scheme cs (applySubst subst' ty)
  freeTypeVars (Scheme cs ty) = Set.difference (freeTypeVars ty) (Set.fromList $ map fst cs)

-- Constraint stuff
data Constraint
  = Matches Type Type
  | Implements Type [Trait]
  deriving (Show, Eq)

instance Substitutable Constraint where
  applySubst s = \case
    Matches t1 t2 -> Matches (applySubst s t1) (applySubst s t2)
    Implements t1 traits -> Implements (applySubst s t1) traits
  freeTypeVars = \case
    Matches t1 t2 -> freeTypeVars t1 <> freeTypeVars t2
    Implements t1 _ -> freeTypeVars t1

isImplConstraint :: Constraint -> Bool
isImplConstraint (Implements _ _) = True
isImplConstraint _ = False

class Substitutable a where
  applySubst :: Subst -> a -> a
  freeTypeVars :: a -> Set.Set TVar

instance Substitutable Type where
  applySubst s@(Subst s') = \case
    TVar a           -> Map.findWithDefault (TVar a) (TVar a) s'
    TCon name types  -> TCon name types
    TList t          -> TList (applySubst s t)
    TTuple ts        -> TTuple (map (applySubst s) ts)
    TLambda arg body -> TLambda (applySubst s arg) (applySubst s body)
    t -> t
  freeTypeVars = \case
    TVar a            -> Set.singleton a
    TCon _ _          -> Set.empty
    TList t           -> freeTypeVars t
    TTuple ts         -> foldl' (\acc t -> Set.union acc (freeTypeVars t)) Set.empty ts
    TLambda bind body -> freeTypeVars bind <> freeTypeVars body
    _ -> Set.empty

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
  applySubst s (x, y) = (applySubst s x, applySubst s y)
  freeTypeVars (x, y) = freeTypeVars x <> freeTypeVars y

instance Substitutable a => Substitutable [a] where
  applySubst s = map (applySubst s)
  freeTypeVars = Set.unions . map freeTypeVars

isTVar :: Type -> Bool
isTVar (TVar _) = True
isTVar _ = False

getFuncReturnType :: Type -> Type
getFuncReturnType (TLambda _ ty) = ty
getFuncReturnType _ = error "Asked for the return type of a non-callable."


-- In the future built in types will be self-hosted constructors that desugar to data constructors that contain hard-coded unboxed types
-- with the unboxed type syntax (e.g data Integer {Integer(#Int)}). The `#` symbol means unboxed type that maps to GNU C/system types, e.g #Int = int64_t
-- because there will be a C++ (maybe rust if i'm feeling frisky) runtime that is linked with the llvm code. This allows for the compiler
-- to intelligently pick data structures based on usage. For example, when it makes sense, a String type will map to char*, but if it makes
-- more sense to use a more complex type (jz::string (similar to std::string)), it will map to that.
-- That's why I am using TCon types for now, (e.g `TCon (TC "Integer") []` instead of my existing TInt type)
traitsTable :: Map.Map Trait ([TCon], TCon)
traitsTable = Map.fromList [ (Trait "Num",        ([TC "Integer", TC "Float"], TC "Integer")),
                             (Trait "Integral",   ([TC "Integer"], TC "Integer")),
                             (Trait "Fractional", ([TC "Float"], TC "Float")),
                             (Trait "Eq",         ([TC "Integer", TC "Float", TC "Bool", TC "String"], TC "Integer")),
                             (Trait "Ord",        ([TC "Integer", TC "Float", TC "String"], TC "Integer")),
                             (Trait "Showable",   ([TC "String", TC "Integer", TC "Float", TC "Bool"], TC "String")),
                             (Trait "Default",    ([TC "Integer", TC "Float", TC "Bool"], TC "Integer"))]

builtinFuncs :: [(Name, Scheme)]
builtinFuncs = [
  ("+", Scheme [(TV "a", [Trait "Num"])] (TLambda (TVar . TV $ "a") (TLambda (TVar . TV $ "a") (TVar . TV $ "a")))),
  ("-", Scheme [(TV "a", [Trait "Num"])] (TLambda (TVar . TV $ "a") (TLambda (TVar . TV $ "a") (TVar . TV $ "a")))),
  ("*", Scheme [(TV "a", [Trait "Num"])] (TLambda (TVar . TV $ "a") (TLambda (TVar . TV $ "a") (TVar . TV $ "a")))),
  ("/", Scheme [(TV "a", [Trait "Num"])] (TLambda (TVar . TV $ "a") (TLambda (TVar . TV $ "a") (TVar . TV $ "a")))),
  ("==", Scheme [(TV "a", [Trait "Eq"])] (TLambda (TVar . TV $ "a") (TLambda (TVar . TV $ "a") (TCon (TC "Bool") [])))),
  ("print!", Scheme [(TV "a", [])] (TLambda (TVar . TV $ "a") (TCon (TC "String") []))),
  ("map", Scheme [(TV "a", []), (TV "b", [])]
    (TLambda
      (TLambda (TVar . TV $ "a") (TVar . TV $ "b"))
      (TLambda
        (TList . TVar . TV $ "a")
        (TList . TVar . TV $ "b")))),
  ("hd", Scheme [(TV "a", [])]
    (TLambda
      (TList . TVar . TV $ "a")
      (TVar . TV $ "a"))),
  ("tl", Scheme [(TV "a", [])]
    (TLambda
      (TList . TVar . TV $ "a")
      (TList . TVar . TV $ "a")))
  ]