{-# LANGUAGE QuasiQuotes #-}
module CodeGen.Javascript where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.List (intercalate)
import Data.String.QQ

import AST

stdLib :: String
stdLib = [s|
const add = l => r => l + r
const subtract = l => r => l - r
const multiply = l => r => l * r
const divide = l => r => l / r
const map = f => xs => xs.map(f)
const hd = ([x]) => x
const tl = ([, ...xs]) => xs
const isEq = l => r => l == r
|]

generate :: TypedProgram -> String
generate es = intercalate "\n" $ stdLib : map generateExpr es

generateExpr :: TypedExpr -> String
generateExpr expr@(Ann _ (EApply _ _ )) = generateApply expr
generateExpr (Ann _ expr) = case expr of
  ELiteral l -> do
    let (Ann _ lit) = l
    case lit of
      LInt i -> show i
      LFloat f -> show f
      LBool b -> if b then "true" else "false"
      LString s -> "\"" <> unpack s <> "\""
      LList es -> "[" <> (intercalate ", " $ map generateExpr es) <> "]"
      LTuple _ -> error "JS generation not implemented for tuples."
  ELambda param body -> "(" <> generateParam param <> ") => " <> generateExpr body
  ELet (Variable v) body -> "let " <> unpack v <> " = " <> generateExpr body
  EVar (Variable v) -> unpack v
  EBlock es -> "{" <> (intercalate "; " $ map generateExpr es) <> "}"
  EIf c t e -> "if (" <> generateExpr c <> ") { " <> generateExpr t <> " } else { " <> generateExpr e <> " }"
  e -> error "JS generation not implemented for the expression " <> show e 


generateApply :: TypedExpr -> String
generateApply (Ann _ (EApply (Ann _ (EVar (Variable "+"))) arg)) = "add" <> "(" <> generateExpr arg <> ")"
generateApply (Ann _ (EApply (Ann _ (EVar (Variable "-"))) arg)) = "subtract" <> "(" <> generateExpr arg <> ")"
generateApply (Ann _ (EApply (Ann _ (EVar (Variable "*"))) arg)) = "multiply" <> "(" <> generateExpr arg <> ")"
generateApply (Ann _ (EApply (Ann _ (EVar (Variable "/"))) arg)) = "divide" <> "(" <> generateExpr arg <> ")"
generateApply (Ann _ (EApply (Ann _ (EVar (Variable "=="))) arg)) = "isEq" <> "(" <> generateExpr arg <> ")"
generateApply (Ann _ (EApply (Ann _ (EVar (Variable "print!"))) arg)) = "console.log" <> "(" <> generateExpr arg <> ")"
generateApply (Ann _ (EApply fun arg)) = generateExpr fun <> "(" <> generateExpr arg <> ")"


generateParam :: Maybe TypedParam -> String
generateParam Nothing = ""
generateParam (Just (Ann _ (FPSimple (Variable v)))) = unpack v
generateParam _ = error "Generation for pattern matching not implemented yet."