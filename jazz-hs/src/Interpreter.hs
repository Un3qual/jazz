{-# LANGUAGE FlexibleInstances, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Interpreter where

import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import AST  -- Assuming your AST definitions are here
import Control.Monad.State
import System.Process (CreateProcess(env))

-- Environment to hold variable bindings
type ScopeEnv = Map.Map T.Text Value
type Env = [ScopeEnv]

data Value
  = VInt Integer
  | VFloat Double
  | VBool Bool
  | VString T.Text
  | VList [Value]
  | VTuple [Value]
  | VLambda [FunParam] Expr Env  -- Lambda with captured environment for closures
  | VBuiltinFunction (Value -> InterpResult)
  deriving (Show)

instance Show (Value -> InterpResult) where
  show _ = "<function>"

type InterpResult = ExceptT T.Text (StateT Env IO) Value

initialEnv :: Env
initialEnv = [Map.fromList
  [ 
    ("+", curryBinOp (+))
  -- , ("-", curryBinOp (-))
  -- , ("*", curryBinOp (*))
  -- , ("/", curryBinOp div)
  -- , ("head", VBuiltinFunction headFunc)
  -- , ("tail", VBuiltinFunction tailFunc)
  , ("map", VBuiltinFunction mapFunc)
  ]]
  where
    headFunc :: Value -> InterpResult
    headFunc (VList (x:_)) = return x
    headFunc _ = throwError "Invalid argument for 'head'"

    tailFunc :: Value -> InterpResult
    tailFunc (VList (_:x)) = return (VList x)
    tailFunc _ = throwError "Invalid argument for 'head'"

    mapFunc :: Value -> InterpResult
    mapFunc (VLambda [FPSimple (Variable paramName _)] body closureEnv) = return $ VBuiltinFunction $
      \case
        VList xs -> VList <$> mapM (\x -> withTempEnv (envInsert paramName x closureEnv) (interpretExpr body)) xs
        _ -> throwError "Invalid argument for 'map'"

curryBinOp :: (Integer -> Integer -> Integer) -> Value
curryBinOp op = VBuiltinFunction $ \a -> return $ VBuiltinFunction $ \b -> case (a, b) of
  (VInt x, VInt y) -> return $ VInt (op x y)
  _ -> throwError "Invalid arguments for binary operator"

-- logDebug :: String -> InterpResult ()
-- logDebug msg = liftIO $ when debugMode $ putStrLn ("[Debug] " ++ msg)

-- debugMode :: Bool
-- debugMode = True

withTempEnv :: Env -> InterpResult -> InterpResult
withTempEnv newEnv action = do
  originalEnv <- get  -- Save the current environment
  put newEnv          -- Set the new temporary environment
  result <- action    -- Perform the action in the context of the new environment
  put originalEnv     -- Restore the original environment
  return result

envLookup :: T.Text -> Env -> Maybe Value
envLookup _ [] = Nothing  -- If the stack is empty, the variable is not found
envLookup name (envi:rest) = case Map.lookup name envi of
  Just value -> Just value   -- Variable found in the current scope
  Nothing -> envLookup name rest  -- Variable not found, search in the outer scope

envInsert :: T.Text -> Value -> Env -> Env
envInsert name value [] = [Map.singleton name value]  -- If the stack is empty, insert the variable in a new scope
envInsert name value (envi:rest) = Map.insert name value envi : rest  -- Insert the variable in the current scope

envNewScope :: Env -> Env
envNewScope envi = Map.empty : envi  -- Add a new empty scope to the environment stack

envInsertNewScope :: T.Text -> Value -> Env -> Env
envInsertNewScope name value envi = envInsert name value (envNewScope envi)


-- Entry point for the interpreter
interpret :: Program -> IO (Either T.Text Value, Env)
interpret program = runStateT (runExceptT (interpretProgram program)) initialEnv

interpretProgram :: Program -> InterpResult
interpretProgram [] = throwError "Empty program"
interpretProgram [expr] = interpretExpr expr
interpretProgram (expr:rest) = do
  interpretExpr expr
  interpretProgram rest

interpretExpr :: Expr -> InterpResult
interpretExpr (ELiteral lit) = interpretLiteral lit
interpretExpr (EVar (Variable name _)) = do
  envi <- get
  case envLookup name envi of
    Just val -> return val
    Nothing -> throwError $ "Unbound variable: " <> name <> " in " <> (T.pack $ show envi)
interpretExpr (ELet (Variable name _) expr) = do
  val <- interpretExpr expr
  modify $ envInsert name val
  return val
interpretExpr (EApply fun arg) = do
  funVal <- interpretExpr fun
  argVal <- interpretExpr arg
  case funVal of
    VBuiltinFunction f -> f argVal
    VLambda params body closureEnv -> applyFunction params body closureEnv argVal
    -- VLambda params body closureEnv -> applyFunction params body (closureEnv `Map.union` Map.singleton name argVal)
    _ -> throwError "Trying to apply a non-function value"

-- interpretExpr (ELambda params body) = do
--   env <- get
--   return $ VLambda params body env
-- interpretExpr other = throwError $ "Unsupported expression: " <> (T.pack $ show other)
-- interpretExpr (ELet (Variable name _) expr) = do
--   val <- interpretExpr expr
--   modify $ Map.insert name val
--   return val
-- interpretExpr (EApply fun arg) = do
--   funVal <- interpretExpr fun
--   argVal <- interpretExpr arg
--   case funVal of
--     VBuiltinFunction f -> f argVal
--     VLambda params body closureEnv -> applyFunction params body closureEnv argVal
--     _ -> throwError "Trying to apply a non-function value"
-- interpretExpr (ELambda params body) = do
--   env <- ask
--   return $ VLambda params body env

interpretLiteral :: Literal -> InterpResult
interpretLiteral (LInt i) = return $ VInt i
interpretLiteral (LFloat f) = return $ VFloat f
interpretLiteral (LBool b) = return $ VBool b
interpretLiteral (LString s) = return $  VString s
interpretLiteral (LList xs) = VList <$> mapM interpretExpr xs
interpretLiteral _ = undefined
-- interpretLiteral (LList xs) = VList $ map (either (const VInt 0) id . runReader (runExceptT . interpretExpr) Map.empty) xs
-- interpretLiteral (LTuple xs) = VTuple $ map (either (const VInt 0) id . runReader (runExceptT . interpretExpr) Map.empty) xs

applyFunction :: [FunParam] -> Expr -> Env -> Value -> InterpResult
applyFunction [] _ _ _ = throwError "Applying function with no parameters"
applyFunction (param:params) body closureEnv arg = case params of
  [] -> case param of
    FPSimple (Variable paramName _) -> do
      -- For the last parameter, apply the argument and evaluate the body in the extended environment
      let extendedEnv = envInsert paramName arg closureEnv
      withTempEnv extendedEnv (interpretExpr body)
    FPPattern _ -> throwError "Pattern matching in function parameters not implemented"
  _ -> do
    -- For curried functions, return a new lambda with one less parameter, representing the partial application
    return $ VLambda params body (envInsert (getParamName param) arg closureEnv)
  where
    getParamName :: FunParam -> T.Text
    getParamName (FPSimple (Variable name _)) = name
    getParamName _ = error "Unsupported parameter type"
-- applyFunction :: [FunParam] -> Expr -> Env -> Value -> InterpResult
-- -- applyFunction _ _ _ _ = throwError "applyFunction"
-- applyFunction [FPSimple (Variable paramName _)] body closureEnv arg = do
--   let extendedEnv = Map.insert paramName arg closureEnv
--   withStateT (const extendedEnv) (interpretExpr body)
-- applyFunction _ _ _ _ = throwError "Unsupported function application or parameter pattern"
