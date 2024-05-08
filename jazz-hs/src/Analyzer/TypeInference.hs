{-# LANGUAGE DuplicateRecordFields
           , FlexibleContexts
           , GeneralizedNewtypeDeriving
           , LambdaCase
           , NamedFieldPuns
           , OverloadedStrings
           , RecordWildCards
           , TypeOperators
           , PatternSynonyms
            #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Analyzer.TypeInference where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List (partition)
import qualified Data.Map as Map
import qualified Data.Map.Strict as SMap
import qualified Data.Set as Set
import           Data.Text (Text, pack, unpack)
import Debug.Trace

import AST
import Errors
import Types

type Unifier = (Subst, [Constraint])
class Unify a where
  unify :: a -> a -> Maybe a
  unify = (<->)
  (<->) :: a -> a -> Maybe a
  (<->) = unify


type Solve a = ExceptT Error (State Unifier) a

newtype Infer a = Infer
  { runInfer :: ExceptT Error (State Env) a
  }
  deriving (Functor, Applicative, Monad, MonadFix, MonadState Env, MonadError Error)

instance Show (Infer a) where
  show x = "foo"

newtype TypeEnv = TypeEnv (Map.Map Name Scheme)
  deriving (Show, Eq)

type Substitute a = Reader Subst a

instance Substitutable TypeEnv where
  applySubst s (TypeEnv env) = TypeEnv (Map.map (applySubst s) env)
  freeTypeVars (TypeEnv env) = Set.unions $ map freeTypeVars (Map.elems env)

extendEnv :: Name -> Scheme -> TypeEnv -> TypeEnv
extendEnv name ty (TypeEnv env) = TypeEnv (Map.insert name ty env)

lookupScheme :: Name -> TypeEnv -> Maybe Scheme
lookupScheme name (TypeEnv env) = Map.lookup name env

lookupEnv :: Name -> TypeEnv -> Infer Type
lookupEnv name (TypeEnv env) =
  case Map.lookup name env of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ NotInScopeError name

data Env = Env {
  vars :: [TypeEnv]
, tvars :: [Constraint] -- type variable constraints
, count :: Int -- count to generate unique names
} deriving (Show, Eq)

defaultEnv :: Env
defaultEnv = Env { 
  vars = [TypeEnv (Map.fromList builtinFuncs)]
, tvars = []
, count = 0
}

newScope :: (MonadState Env m) => m ()
newScope = modify $ \env -> env {vars = TypeEnv Map.empty : vars env}

dropScope :: (MonadState Env m) => m ()
dropScope = modify $ \env -> env {vars = tail $ vars env}

withScope :: (MonadState Env m) => m a -> m a
withScope action = do
  newScope
  ret <- action
  dropScope
  return ret

pushConstraint :: (MonadState Env m) => Constraint -> m ()
pushConstraint constraint = modify $ \env -> env {tvars = constraint : tvars env}

pushVar :: Name -> Scheme -> Infer ()
pushVar name ty = modify $ \env ->
  case vars env of
    [] -> env {vars = [TypeEnv (Map.singleton name ty)]}
    (x : xs) -> env {vars = extendEnv name ty x : xs}

getVar :: Name -> Infer Type
getVar name = do
  ret <- gets $ \env -> foldl (<|>) Nothing (map (lookupScheme name) (vars env))
  case ret of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ NotInScopeError name

-- | An infinite list of unique names
letters :: [Text]
letters = pack <$> ([1 ..] >>= flip replicateM ['a' .. 'z'])

-- | Gets a fresh unique name (based on the state count)
fresh :: (MonadState Env m) => m Type
fresh = do
  idx <- gets count
  modify $ \env -> env {count = idx + 1}
  let letter = letters !! idx
  return $ TVar $ TV (letter)


getLambdaReturnType :: Type -> Type
getLambdaReturnType (TLambda _ ty) = ty
getLambdaReturnType ty = error $ "Asked for the return type of a non-callable: " <> show ty

inferExpr :: SpannedExpr -> Infer TypedExpr
inferExpr (Ann range expr) = case expr of
  ELet (Variable varName) bodyExpr -> do
    inferredBody@(Ann (_, ty) _) <- inferExpr bodyExpr
    -- Needs to check if already in scope, not for reassignment prevention, but for type signatures which are WIP
    maybeVar <- gets $ \env -> foldl (<|>) Nothing (map (lookupScheme varName) (vars env))
    case maybeVar of
      Nothing            -> return ()
      Just (Scheme _ tv) -> pushConstraint $ Matches tv ty
    pushVar varName (Scheme [] ty)
    return $ Ann (range, TUnit) $ ELet (Variable varName) inferredBody
  ELiteral lit -> do
    inferredLit@(Ann (_, ty) _) <- inferLiteral lit
    return $ Ann (range, ty) $ ELiteral inferredLit
  EVar (Variable varName) -> do
    inferredVar <- getVar varName
    return $ Ann (range, inferredVar) $ EVar (Variable varName)
  EApply fn body -> do
    inferredFn <- inferExpr fn
    inferredBody <- inferExpr body
    let (Ann _ fnName) = inferredFn
    let inferredFnType = snd (annotation inferredFn)
        inferredBodyType = snd (annotation inferredBody)
    retTy <- apply inferredFnType inferredBodyType
    return $ Ann (range, retTy) $ EApply inferredFn inferredBody
  ELambda maybeParam bodyExpr -> withScope $ do
    tv <- fresh
    inferredParam <- traverse inferParam maybeParam
    inferredBody <- inferExpr bodyExpr
    case inferredParam of
      Nothing -> return ()
      Just (Ann (_, argTy) (FPSimple (Variable paramName))) -> do
        pushVar paramName (Scheme [] argTy)
    let inferredParamType = maybe TUnit (snd . annotation) inferredParam
    let returnType = snd (annotation inferredBody)
    pushConstraint $ Matches tv (TLambda inferredParamType returnType)
    return $ Ann (range, tv)
           $ ELambda inferredParam inferredBody
  ETypeSignature (Variable varName) tcConstraints ty -> do
    tv <- fresh
    pushVar varName (Scheme [] tv)
    pushConstraint $ Matches tv ty
    return $ Ann (range, TUnit)
           $ ETypeSignature (Variable varName) tcConstraints ty
  EBlock exprs -> do
    inferredExprs <- mapM inferExpr exprs
    let (Ann (_, ty) _) = last inferredExprs
    return $ Ann (range, ty) $ EBlock inferredExprs
  e -> do
    env <- get
    error $ "Inference not implemented for this expression: " <> show e <> "\nCurrent env: " <> show env

inferLiteral :: SpannedLiteral -> Infer TypedLiteral
inferLiteral (Ann range lit) = case lit of
  LInt val -> do
    tv <- fresh
    pushConstraint $ Implements tv [Trait "Num"]
    return $ Ann (range, TCon (TC "Integer") []) $ LInt val
  LFloat val -> do
    tv <- fresh
    pushConstraint $ Implements tv [Trait "Fractional"]
    return $ Ann (range, TCon (TC "Float") []) $ LFloat val
  LBool val -> do
    return $ Ann (range, TCon (TC "Bool") []) $ LBool val
  LString val -> do
    return $ Ann (range, TCon (TC "String") []) $ LString val
    -- return $ Ann (range, TList (TCon (TC "Int") [])) $ LString val
  -- Empty List
  LList [] -> do
    tv <- fresh
    return $ Ann (range, TList tv) (LList [])
  -- Nonempty list
  LList listEls -> do
    tv <- fresh
    typedEls <- mapM inferExpr listEls
    mapM (\(Ann (_, ty) _) -> pushConstraint $ Matches tv ty) typedEls
    return $ Ann (range, TList tv) $ LList typedEls
  -- Tuple
  (LTuple tupleEls) -> do
    typedEls <- mapM inferExpr tupleEls
    elTypes <- mapM (\(Ann (_, ty) _) -> do
      tv <- fresh
      pushConstraint $ Matches tv ty
      return tv
      ) typedEls
    return $ Ann (range, TTuple elTypes) $ LTuple typedEls

  l -> error $ "Inference not implemented for the literal " <> show l

inferParam :: SpannedParam -> Infer TypedParam
inferParam (Ann range param) = case param of
  FPSimple (Variable name) -> do
    tv <- fresh
    pushVar name (Scheme [] tv)
    return $ Ann (range, tv) $ FPSimple (Variable name)
  FPPattern p -> do
    env <- get
    error "Inference for pattern matching not implemented yet."

implementsTraits :: MonadError Error m => [Trait] -> TCon -> m ()
implementsTraits traits ty = forM_ traits $ \trait ->
  case Map.lookup trait traitsTable of
    Nothing -> throwError $ TraitNotInScopeError trait
    Just (types, _) -> unless (ty `elem` types) $ throwError $ NotImplTraitError (TCon ty []) trait

defaultTraitType :: MonadError Error m => Trait -> m Type
defaultTraitType trait =
    case Map.lookup trait traitsTable of
        Nothing -> throwError $ TraitNotInScopeError trait
        Just (_, def) -> return (TCon def [])

apply :: Type -> Type -> Infer Type
apply (TLambda schemeArgTy bodyTy) providedArgTy = do
  case (schemeArgTy, providedArgTy) of
    (TVar tv, pArgTy)                   -> do
      pushConstraint $ Matches (TVar tv) pArgTy
    (sArgTy, TVar tv)                   -> do
      pushConstraint $ Matches (TVar tv) sArgTy

    (sArgTy@(TList sArgTv@(TVar _)), pArgTy@(TList pArgTv@(TVar _)))       -> do
      pushConstraint $ Matches sArgTv pArgTv
      -- pushConstraint $ Matches sArgTy pArgTy
    (tv@(TList (TVar _)), pArgTy)       -> do
      pushConstraint $ Matches tv pArgTy
    (sArgTy, tv@(TList (TVar _)))       -> do
      pushConstraint $ Matches tv sArgTy

    (sArgTy, pArgTy) | sArgTy == pArgTy -> do
      return ()
    (sArgTy, pArgTy)                    -> do
      throwError $ TypeError sArgTy pArgTy
  return bodyTy

apply (TVar t) providedArgTy = do
    retTv <- fresh
    argTv <- fresh
    pushConstraint $ Matches argTv providedArgTy
    pushConstraint $ Matches (TVar t) (TLambda argTv retTv)
    return retTv

apply t1 t2 = error $ "Non-function call application: " <> show t1 <> " onto " <> show t2

(<<>>) :: TVar -> Type -> Solve Subst
name <<>> (TVar v) | boundToSelf = return mempty
  where boundToSelf = name == v
name <<>> ty | name >|< ty = throwError (CantConstructInfiniteTypeError name ty)
  where n >|< t = Set.member n (freeTypeVars t)
name <<>> ty = return (Subst $ Map.singleton (TVar name) ty)

-- generalize func needed? (not yet, but later)

instantiate :: Scheme -> Infer Type
instantiate (Scheme cs ty) = do
  subst <- substituteAllWithFresh cs
  return (applySubst subst ty)
  where
    substituteAllWithFresh :: [(TVar, [Trait])] -> Infer Subst
    substituteAllWithFresh xs = do
      let action (tvar, traits) = do
            tv <- fresh
            pushConstraint $ Implements tv traits
            return (TVar tvar, tv)
      freshSubstActions <- mapM action xs
      let freshSubsts = Map.fromList freshSubstActions
      return (Subst freshSubsts)

infer :: SpannedProgram -> Either Error TypedProgram
infer exprs = do
  let (maybe_ast, env) = runState (runExceptT (runInfer (mapM inferExpr exprs))) defaultEnv
  ast <- maybe_ast
  solutions <- runSolve (fuseConstraints (reverse (tvars env)))
  return (runReader (mapM substExpr ast) solutions)

-- TODO: add list and tuple handling here bc the current solution of adding a constraint for the inner types in apply is a hack
unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return mempty
unifies (TVar v) t = v <<>> t
unifies t (TVar v) = v <<>> t
unifies lam1@(TLambda _ _) lam2@(TLambda _ _) = unifiesLambda lam1 lam2
unifies t1 t2 = throwError $ TypeError t1 t2

unifiesLambda :: Type -> Type -> Solve Subst
unifiesLambda tl1@(TLambda param1 body1) tl2@(TLambda param2 body2) = do
  su1 <- unifies param1 param2
  su2 <- unifies body1 body2
  return (su2 <> su1)

unifyMany :: [(Type, Type)] -> Solve Subst
unifyMany [] = return mempty
unifyMany ((t1, t2) : ts) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (applySubst su1 ts)
     return (su2 <> su1)

remains :: Type -> [Constraint] -> Bool
remains t1 = \case
    (Matches t2 _:_) | t1 == t2 -> True
    (Matches _ t2:_) | t1 == t2 -> True
    (_:cs) -> remains t1 cs
    [] -> False

fuseConstraints :: [Constraint] -> [Constraint]
fuseConstraints cs =
  let (impls, matches) = partition isImplConstraint cs
      merged = foldl (\acc (Implements var traits) -> Map.insertWith (<>) var traits acc) Map.empty impls
      final_impls = map (uncurry Implements) (Map.toList merged)
   in matches <> final_impls

solveConstraints :: Solve Subst
solveConstraints = do
  (su, constraints) <- get
  case constraints of
    [] -> return su
    (Matches var ty : cs) -> do
      su1 <- unifies var ty
      put (su1 <> su, fuseConstraints (applySubst su1 cs))
      solveConstraints
    (Implements _ [] : cs) -> do
      put (su, cs)
      solveConstraints
    (Implements var traits : cs) -> do
      case var of
        TVar _ -> case getSubst var su of
          Just (TCon tc _) -> do
            implementsTraits traits tc
            put (su, cs)
            solveConstraints
          Just (TVar _) | remains var cs -> do
            put (su, cs <> [Implements var traits])
            solveConstraints
          Just (TVar _) -> do
            let action trait = do
                  def <- defaultTraitType trait
                  let TCon tc [] = def
                  implementsTraits traits tc
                  let su1 = Subst (Map.singleton var def)
                  put (su1 <> su, fuseConstraints $ applySubst su1 cs)
                  solveConstraints
            foldl (<|>) (throwError CantInferTypeError) $ map action traits
          Nothing | remains var cs -> do
            put (su, cs <> [Implements var traits])
            solveConstraints
          Nothing -> do
            let action trait = do
                  def <- defaultTraitType trait
                  let TCon tc _ = def
                  implementsTraits traits tc
                  let su1 = Subst (Map.singleton var def)
                  put (su1 <> su, fuseConstraints $ applySubst su1 cs)
                  solveConstraints
            foldl (<|>) (throwError CantInferTypeError) $ map action traits
        TCon tc _ -> do
          implementsTraits traits tc
          put (su, cs)
          solveConstraints
        TLambda param body -> do
          put (su, cs <> [Matches var (TLambda param body)])
          solveConstraints

        
        e -> error $ "Implements constraint not implemented for " <> show e <> " // " <> show (var, traits, cs)


runSolve :: [Constraint] -> Either Error Subst
runSolve cs = evalState (runExceptT solveConstraints) (mempty, cs)

substExpr :: TypedExpr -> Substitute TypedExpr
substExpr ex@(Ann (range, ty) expr) = case expr of
  ELiteral lit -> do
    s <- ask
    substitutedLiteral <- substLiteral lit
    return $ Ann (range, applySubst s ty) $ ELiteral substitutedLiteral
  ELet (Variable varName) bodyExpr -> do
    s <- ask
    substitutedBody <- substExpr bodyExpr
    return $ Ann (range, applySubst s ty) $ ELet (Variable varName) substitutedBody
  EVar (Variable varName) -> do
    s <- ask
    return $ Ann (range, applySubst s ty) $ EVar (Variable varName)
  EApply fn arg -> do
    s <- ask
    substFn@(Ann (_, fnType) _) <- substExpr fn
    substArg <- substExpr arg
    case fnType of
        (TLambda _ resultType) -> return $ Ann (range, applySubst s resultType) $ EApply substFn substArg
        t@(TVar _) -> error $ "Function type unresolved after substitution: " <> show t
        t -> error $ "Attempted to apply a non-function type " <> show t
  ELambda maybeParam bodyExpr -> do
    s <- ask
    substitutedParam <- traverse substParam maybeParam
    substitutedBody <- substExpr bodyExpr
    return $ Ann (range, applySubst s ty) $ ELambda substitutedParam substitutedBody
  ETypeSignature (Variable varName) tcConstraints tsTy -> do
    s <- ask
    return $ Ann (range, applySubst s ty) $ ETypeSignature (Variable varName) tcConstraints ty
  EBlock exprs -> do
    s <- ask
    substitutedExprs <- mapM substExpr exprs
    return $ Ann (range, applySubst s ty) $ EBlock substitutedExprs
  e -> do
    subst <- ask
    error $ "Substitution not implemented for this expression: " <> show ex <> "\nCurrent subst: " <> show subst
  
substLiteral :: TypedLiteral -> Substitute TypedLiteral
substLiteral tl@(Ann (range, ty) lit) = case lit of
  LList xs -> do
    s <- ask
    substitutedEls <- mapM substExpr xs
    return $ Ann (range, applySubst s ty) (LList substitutedEls)
  LTuple xs -> do
    s <- ask
    substitutedEls <- mapM substExpr xs
    return $ Ann (range, applySubst s ty) (LTuple substitutedEls)
  _ -> substSimpleLiteral tl

substSimpleLiteral :: TypedLiteral -> Substitute TypedLiteral
substSimpleLiteral (Ann (range, ty) lit) = do
  s <- ask
  return $ Ann (range, applySubst s ty) lit

substParam :: TypedParam -> Substitute TypedParam
substParam (Ann (range, ty) param) = case param of
  FPSimple (Variable name) -> do
    s <- ask
    return $ Ann (range, applySubst s ty) $ FPSimple (Variable name)
  FPPattern p -> do
    s <- ask
    error $ "Substitution for pattern matching not implemented yet."
  -- FPPattern p -> do
  --   s <- ask
  --   return $ Ann (range, applySubst s ty) $ FPPattern p
