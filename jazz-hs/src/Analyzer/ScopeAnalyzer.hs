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

module Analyzer.ScopeAnalyzer where

-- import           AST


import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List (partition)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Foldable (traverse_)
import           Data.Text (Text, pack, unpack)
import Debug.Trace

import AST
import Errors
import Types

type VarEnv = [[Name]]

data Env = Env {
  vars :: VarEnv
} deriving (Show, Eq)

newtype Scoped a = Scoped { runScoped :: ExceptT Error (State Env) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadState Env, MonadError Error)

defaultEnv :: Env
defaultEnv = Env {
  vars = [map fst builtinFuncs]
}

newScope :: (MonadState Env m) => m ()
newScope = modify $ \env -> env { vars = [] : vars env }

dropScope :: (MonadState Env m) => m ()
dropScope = modify $ \env -> env {vars = tail $ vars env}

withScope :: (MonadState Env m) => m a -> m a
withScope action = do
  newScope
  ret <- action
  dropScope
  return ret

pushVar :: Name -> Scoped ()
pushVar name = modify $ \env ->
  case vars env of
    [] -> error "No scope to push variable into"
    (v:vs) -> env { vars = (name : v) : vs }

varExists :: Name -> Scoped Bool
varExists name = gets $ \env -> any (elem name) (vars env)

analyzeProgramScope :: TypedProgram -> AnalysisResult
analyzeProgramScope exprs = do
  let analyzedExprs = mapM analyzeExprScope exprs
  let scopedExprs = runScoped analyzedExprs
  let exceptedExprs = runExceptT scopedExprs
  let maybeAst = evalState exceptedExprs defaultEnv
  maybeAst

-- TODO: This doesn't seem to really need to return an actual value because nothing about the
--   Expr is changed, so Scoped () should be fine. 
analyzeExprScope :: TypedExpr -> Scoped TypedExpr
analyzeExprScope annedExpr@(Ann _ rawExpr) = case rawExpr of
  EVar (Variable name) -> do
    exists <- varExists name
    if exists
      then return annedExpr
      else throwError $ NotInScopeError name
  ELiteral lit -> do
    analyzeLiteralScope lit
    return annedExpr
  ELambda maybeParam body -> withScope $ do
    traverse_ analyzeParamScope maybeParam
    analyzeExprScope body
  ELet (Variable name) body -> do
    pushVar name
    analyzeExprScope body
    return annedExpr
  EApply f a -> do
    analyzeExprScope f
    analyzeExprScope a
    return annedExpr
  EData name types cons -> undefined
  EBlock exprs -> withScope $ do
    mapM_ analyzeExprScope exprs
    return annedExpr
  EModule name expr -> withScope $ do
    analyzeExprScope expr
    return annedExpr
  EIf cond then' else' -> do
    withScope $ analyzeExprScope cond
    withScope $ analyzeExprScope then'
    withScope $ analyzeExprScope else'
    return annedExpr
  -- Rest to do in the future
  _ -> return annedExpr

analyzeLiteralScope :: TypedLiteral -> Scoped TypedLiteral
analyzeLiteralScope annedLit@(Ann _ lit) = case lit of
  LList exprs -> do
    mapM_ analyzeExprScope exprs
    return annedLit
  LTuple exprs -> do
    mapM_ analyzeExprScope exprs
    return annedLit
  _ -> return annedLit

analyzeParamScope :: TypedParam -> Scoped ()
analyzeParamScope (Ann _ (FPSimple (Variable name))) = pushVar name
analyzeParamScope (Ann _ (FPPattern _)) = error "Scope analysis not implemented for patterns yet." --analyzePattern