{-# LANGUAGE OverloadedStrings #-}

module JazzNext.Compiler.PreludeContract
  ( validatePreludeKernelBridges
  ) where

import Data.Text (Text)
import JazzNext.Compiler.AST
  ( Expr (..),
    Statement (..)
  )
import JazzNext.Compiler.BuiltinCatalog
  ( isBuiltinSymbolName,
    kernelBridgeTargetName
  )

-- Validates explicit prelude bridge declarations that map prelude-visible
-- names to kernel-owned builtin symbols.
validatePreludeKernelBridges :: Expr -> [Text]
validatePreludeKernelBridges preludeExpr =
  case preludeExpr of
    EScope statements ->
      concatMap validateStatement statements
    _ -> []
  where
    validateStatement :: Statement -> [Text]
    validateStatement statement =
      case statement of
        SLet bindingName _ bindingExpr ->
          case kernelBridgeTargetName bindingName of
            Nothing -> []
            Just targetName
              | not (isBuiltinSymbolName targetName) ->
                  [ "E0004: prelude kernel bridge '"
                      <> bindingName
                      <> "' references unknown kernel symbol '"
                      <> targetName
                      <> "'"
                  ]
              | otherwise ->
                  case bindingExpr of
                    EVar rhsName
                      | rhsName == targetName -> []
                      | otherwise ->
                          [ "E0005: prelude kernel bridge '"
                              <> bindingName
                              <> "' must reference kernel symbol '"
                              <> targetName
                              <> "', found '"
                              <> rhsName
                              <> "'"
                          ]
                    _ ->
                      [ "E0005: prelude kernel bridge '"
                          <> bindingName
                          <> "' must be a direct symbol reference to '"
                          <> targetName
                          <> "'"
                      ]
        _ -> []
