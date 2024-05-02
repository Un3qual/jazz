{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeOperators #-}
module Analyzer
 where


import AST
import Errors
import Analyzer.TypeInference


analyze :: SpannedProgram -> AnalysisResult
analyze prog = do
  inferredProgram <- infer prog
  return inferredProgram




-- inferTypesExpr :: Expr -> Expr
-- inferTypesExpr = id

-- inferTypes :: Program -> Program
-- -- inferTypes p = map inferTypesExpr p
-- inferTypes = id

-- analysisStages :: [Program -> Program]
-- analysisStages = [inferTypes]

-- analyze :: Program -> Program
-- analyze p = pipe analysisStages p
--   where
--     pipe :: [a -> a] -> a -> a
--     pipe fs a = foldl (flip ($)) a fs