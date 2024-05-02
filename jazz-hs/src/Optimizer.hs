{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TypeOperators #-}
module Optimizer where


import AST
import Optimizer.ConstantFolding

optimize :: TypedProgram -> TypedProgram
optimize = map constantFold
