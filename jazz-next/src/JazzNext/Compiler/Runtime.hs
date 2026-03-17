{-# LANGUAGE OverloadedStrings #-}

-- | Small interpreter/runtime for the currently-supported core language. It is
-- intentionally simple and mirrors the same builtin/operator contracts enforced
-- by analysis and type inference.
module JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    evaluateRuntimeExprWithBuiltins,
    evaluateRuntimeExpr,
    renderRuntimeValue
  ) where

import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( Expr (..),
    Literal (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    mkDiagnostic
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol (..),
    builtinSymbolArity,
    builtinSymbolName,
    lookupBuiltinSymbolInMode
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText
  )

-- | Runtime values produced by the interpreter, including partially applied
-- builtins/operators.
data RuntimeValue
  = VInt Int
  | VBool Bool
  | VList [RuntimeValue]
  | VClosure RuntimeEnv Identifier Expr
  | VBuiltin BuiltinSymbol [RuntimeValue]
  | VOperator Text [RuntimeValue]
  | VSectionLeft Text RuntimeValue
  | VSectionRight Text RuntimeValue
  deriving (Eq, Show)

evaluateRuntimeExpr :: Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExpr = evaluateRuntimeExprWithBuiltins ResolveKernelOnly

-- | Evaluate an expression under the builtin resolution mode chosen by the
-- caller, returning a terminal scope value when one exists.
evaluateRuntimeExprWithBuiltins :: BuiltinResolutionMode -> Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltins builtinMode expr =
  case expr of
    EBlock statements -> evalScope builtinMode Map.empty statements
    _ -> Just <$> evalValue builtinMode Map.empty expr

renderRuntimeValue :: RuntimeValue -> Text
renderRuntimeValue value =
  case value of
    VInt intValue -> Text.pack (show intValue)
    VBool boolValue ->
      if boolValue
        then "True"
        else "False"
    VList elements ->
      "[" <> Text.intercalate ", " (map renderRuntimeValue elements) <> "]"
    VClosure {} -> "<function>"
    VBuiltin _ _ -> "<function>"
    VOperator {} -> "<function>"
    VSectionLeft {} -> "<function>"
    VSectionRight {} -> "<function>"

type RuntimeEnv = Map Text RuntimeValue

-- | Evaluate a block scope in order. Declarations clear `lastExprValue`, so
-- `evalScope` returns `Just` only when the final surviving statement is an
-- `SExpr`; otherwise the block yields `Nothing`.
evalScope :: BuiltinResolutionMode -> RuntimeEnv -> [Statement] -> Either Diagnostic (Maybe RuntimeValue)
evalScope builtinMode initialEnv statements = go initialEnv Nothing indexedStatements
  where
    indexedStatements = zip [0 ..] statements
    recursiveLambdaGroups = inferRecursiveLambdaGroups initialEnv indexedStatements
    recursiveLambdaBindings = collectRecursiveLambdaBindings indexedStatements

    go :: RuntimeEnv -> Maybe RuntimeValue -> [(Int, Statement)] -> Either Diagnostic (Maybe RuntimeValue)
    go env lastExprValue remainingStatements =
      case remainingStatements of
        [] ->
          -- Declaration-only scopes intentionally remain `Nothing` until a terminal `SExpr` sets a value.
          Right lastExprValue
        (statementIndex, statement) : rest ->
          case statement of
            SSignature {} ->
              go env Nothing rest
            SModule {} ->
              go env Nothing rest
            SImport {} ->
              go env Nothing rest
            -- Keep runtime recursion aligned with analyzer visibility by tying
            -- lambda-only SCCs into the environment when the first binding in
            -- the group is encountered.
            SLet _ _ _
              | Just groupMembers <- Map.lookup statementIndex recursiveLambdaGroups ->
                  case groupMembers of
                    leader : _
                      | statementIndex == leader ->
                          go (activateRecursiveLambdaGroup env groupMembers recursiveLambdaBindings) Nothing rest
                      | otherwise ->
                          go env Nothing rest
                    [] ->
                      go env Nothing rest
            SLet name _ (ELambda parameterName bodyExpr) ->
              let bindingNameText = identifierText name
                  envWithSelf = Map.insert bindingNameText value env
                  value = VClosure envWithSelf parameterName bodyExpr
               in go envWithSelf Nothing rest
            SLet name _ valueExpr -> do
              value <- evalValue builtinMode env valueExpr
              go (Map.insert (identifierText name) value env) Nothing rest
            SExpr _ expr -> do
              value <- evalValue builtinMode env expr
              go env (Just value) rest

collectRecursiveLambdaBindings :: [(Int, Statement)] -> Map Int (Text, Identifier, Expr)
collectRecursiveLambdaBindings =
  foldl' collect Map.empty
  where
    collect bindingsByStatement (statementIndex, statement) =
      case statement of
        SLet bindingName _ (ELambda parameterName bodyExpr) ->
          Map.insert
            statementIndex
            (identifierText bindingName, parameterName, bodyExpr)
            bindingsByStatement
        _ -> bindingsByStatement

activateRecursiveLambdaGroup ::
  RuntimeEnv ->
  [Int] ->
  Map Int (Text, Identifier, Expr) ->
  RuntimeEnv
activateRecursiveLambdaGroup env groupMembers lambdaBindingsByStatement =
  groupEnv
  where
    groupBindings =
      foldl'
        insertGroupBinding
        Map.empty
        [ bindingInfo
          | statementIndex <- groupMembers,
            Just bindingInfo <- [Map.lookup statementIndex lambdaBindingsByStatement]
        ]
    groupEnv = groupBindings `Map.union` env

    insertGroupBinding bindings (bindingNameText, parameterName, bodyExpr) =
      Map.insert
        bindingNameText
        (VClosure groupEnv parameterName bodyExpr)
        bindings

-- | Mirror the analyzer's recursive-group resolution so runtime closure capture
-- stays consistent with the binding visibility already accepted at compile time.
inferRecursiveLambdaGroups ::
  RuntimeEnv ->
  [(Int, Statement)] ->
  Map Int [Int]
inferRecursiveLambdaGroups outerEnv indexedStatements =
  Map.fromList
    [ (statementIndex, componentStatements)
      | component <- stronglyConnComp graphNodes,
        let componentStatements = componentStatementIndices component,
        isRecursiveComponent component,
        all (`Map.member` recursiveLambdaBindings) componentStatements,
        statementIndex <- componentStatements
    ]
  where
    declarationInfo =
      [ (statementIndex, identifierText bindingName, valueExpr)
        | (statementIndex, SLet bindingName _ valueExpr) <- indexedStatements
      ]
    declarationStatementsByName =
      foldl'
        collectDeclaration
        Map.empty
        declarationInfo
    outerBindingNames = Map.keysSet outerEnv
    baseDependencies =
      Map.fromList
        [ (statementIndex, Set.empty)
          | (statementIndex, _, _) <- declarationInfo
        ]
    dependenciesByStatement =
      foldl' addBindingDependencies baseDependencies declarationInfo
    graphNodes =
      [ (statementIndex, statementIndex, Set.toList dependencies)
        | (statementIndex, dependencies) <- Map.toList dependenciesByStatement
      ]
    recursiveLambdaBindings = collectRecursiveLambdaBindings indexedStatements

    collectDeclaration declarationsByName (statementIndex, bindingNameText, _) =
      Map.insertWith (\new old -> old ++ new) bindingNameText [statementIndex] declarationsByName

    addBindingDependencies dependencies (statementIndex, bindingNameText, valueExpr) =
      let localDependencyNames =
            Set.filter
              (`Map.member` declarationStatementsByName)
              (freeVarsExprWithBound (Set.singleton bindingNameText) valueExpr)
          resolvedDependencies =
            Set.fromList
              [ dependencyStatementIndex
                | dependencyName <- Set.toList localDependencyNames,
                  Just dependencyStatementIndex <- [resolveDependencyStatement statementIndex dependencyName]
              ]
       in
        Map.insert statementIndex resolvedDependencies dependencies

    resolveDependencyStatement statementIndex dependencyName =
      case Map.lookup dependencyName declarationStatementsByName of
        Nothing -> Nothing
        Just declarationStatements ->
          case closestPriorDeclaration declarationStatements of
            Just prior -> Just prior
            Nothing
              | Set.member dependencyName outerBindingNames -> Nothing
              | otherwise -> closestFutureDeclaration declarationStatements
      where
        closestPriorDeclaration declarations =
          case filter (< statementIndex) declarations of
            [] -> Nothing
            priorDeclarations -> Just (last priorDeclarations)

        closestFutureDeclaration declarations =
          case filter (> statementIndex) declarations of
            [] -> Nothing
            firstFuture : _ -> Just firstFuture

    componentStatementIndices component =
      let componentIndices =
            case component of
              AcyclicSCC statementIndex -> Set.singleton statementIndex
              CyclicSCC statementIndices -> Set.fromList statementIndices
       in
        [ statementIndex
          | (statementIndex, _) <- indexedStatements,
            Set.member statementIndex componentIndices
        ]

    isRecursiveComponent component =
      case component of
        CyclicSCC _ -> True
        AcyclicSCC statementIndex ->
          Set.member
            statementIndex
            (Map.findWithDefault Set.empty statementIndex dependenciesByStatement)

freeVarsExprWithBound :: Set Text -> Expr -> Set Text
freeVarsExprWithBound bound expr =
  case expr of
    ELit _ -> Set.empty
    EVar name
      | Set.member nameText bound -> Set.empty
      | otherwise -> Set.singleton nameText
      where
        nameText = identifierText name
    ELambda parameterName bodyExpr ->
      freeVarsExprWithBound
        (Set.insert (identifierText parameterName) bound)
        bodyExpr
    EOperatorValue _ -> Set.empty
    EList elements ->
      Set.unions (map (freeVarsExprWithBound bound) elements)
    EApply functionExpr argumentExpr ->
      Set.union
        (freeVarsExprWithBound bound functionExpr)
        (freeVarsExprWithBound bound argumentExpr)
    EIf conditionExpr thenExpr elseExpr ->
      freeVarsExprWithBound bound (ECase conditionExpr thenExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr ->
      Set.unions
        [ freeVarsExprWithBound bound conditionExpr,
          freeVarsExprWithBound bound thenExpr,
          freeVarsExprWithBound bound elseExpr
        ]
    EBinary _ leftExpr rightExpr ->
      Set.union
        (freeVarsExprWithBound bound leftExpr)
        (freeVarsExprWithBound bound rightExpr)
    ESectionLeft leftExpr _ ->
      freeVarsExprWithBound bound leftExpr
    ESectionRight _ rightExpr ->
      freeVarsExprWithBound bound rightExpr
    EBlock statements -> freeVarsScopeWithBound bound statements

freeVarsScopeWithBound :: Set Text -> [Statement] -> Set Text
freeVarsScopeWithBound initialBound statements =
  snd (foldl' step (initialBound, Set.empty) statements)
  where
    step (boundNames, freeNames) statement =
      case statement of
        SSignature {} -> (boundNames, freeNames)
        SModule {} -> (boundNames, freeNames)
        SImport {} -> (boundNames, freeNames)
        SExpr _ expr ->
          ( boundNames,
            Set.union freeNames (freeVarsExprWithBound boundNames expr)
          )
        SLet bindingName _ valueExpr ->
          let boundWithSelf = Set.insert (identifierText bindingName) boundNames
           in
            ( boundWithSelf,
              Set.union freeNames (freeVarsExprWithBound boundWithSelf valueExpr)
            )

evalValue builtinMode env expr =
  case expr of
    ELit literal -> Right (literalRuntimeValue literal)
    EVar name ->
      case Map.lookup nameText env of
        Just value -> Right value
        Nothing ->
          case lookupBuiltinSymbolInMode builtinMode nameText of
            Just builtinFunction -> Right (VBuiltin builtinFunction [])
            Nothing ->
              Left
                ( runtimeDiagnostic
                    "E3002"
                    ("runtime unbound variable '" <> nameText <> "'")
                )
      where
        nameText = identifierText name
    ELambda parameterName bodyExpr ->
      Right (VClosure env parameterName bodyExpr)
    EOperatorValue operatorSymbol ->
      Right (VOperator operatorSymbol [])
    EList elements ->
      VList <$> mapM (evalValue builtinMode env) elements
    EApply functionExpr argumentExpr -> do
      functionValue <- evalValue builtinMode env functionExpr
      argumentValue <- evalValue builtinMode env argumentExpr
      applyRuntimeFunction builtinMode functionValue argumentValue
    EIf conditionExpr thenExpr elseExpr ->
      evalValue builtinMode env (ECase conditionExpr thenExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr -> do
      conditionValue <- evalValue builtinMode env conditionExpr
      case conditionValue of
        VBool True -> evalValue builtinMode env thenExpr
        VBool False -> evalValue builtinMode env elseExpr
        other ->
          Left
            ( runtimeDiagnostic
                "E3003"
                ("runtime branch condition must be Bool, found " <> renderRuntimeType other)
            )
    EBinary operatorSymbol leftExpr rightExpr -> do
      leftValue <- evalValue builtinMode env leftExpr
      rightValue <- evalValue builtinMode env rightExpr
      evalBinary builtinMode operatorSymbol leftValue rightValue
    ESectionLeft leftExpr operatorSymbol -> do
      leftValue <- evalValue builtinMode env leftExpr
      Right (VSectionLeft operatorSymbol leftValue)
    ESectionRight operatorSymbol rightExpr -> do
      rightValue <- evalValue builtinMode env rightExpr
      Right (VSectionRight operatorSymbol rightValue)
    EBlock statements ->
      case evalScope builtinMode env statements of
        Left err -> Left err
        Right Nothing ->
          Left
            (runtimeDiagnostic "E3006" "block expression has no terminal expression result at runtime")
        Right (Just value) -> Right value

literalRuntimeValue :: Literal -> RuntimeValue
literalRuntimeValue literal =
  case literal of
    LInt value -> VInt value
    LBool value -> VBool value

-- | Apply any callable runtime value, including sections, builtin primitives,
-- and curried operator values.
applyRuntimeFunction :: BuiltinResolutionMode -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeFunction builtinMode functionValue argumentValue =
  case functionValue of
    VSectionLeft operatorSymbol leftValue ->
      evalBinary builtinMode operatorSymbol leftValue argumentValue
    VSectionRight operatorSymbol rightValue ->
      evalBinary builtinMode operatorSymbol argumentValue rightValue
    VClosure capturedEnv parameterName bodyExpr ->
      evalValue
        builtinMode
        (Map.insert (identifierText parameterName) argumentValue capturedEnv)
        bodyExpr
    VBuiltin builtinFunction capturedArgs ->
      applyBuiltin builtinMode builtinFunction (capturedArgs ++ [argumentValue])
    VOperator operatorSymbol capturedArgs ->
      applyOperator builtinMode operatorSymbol (capturedArgs ++ [argumentValue])
    _ ->
      Left
        ( runtimeDiagnostic
            "E3008"
            ("runtime cannot apply non-function value of type " <> renderRuntimeType functionValue)
        )

applyOperator :: BuiltinResolutionMode -> Text -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyOperator builtinMode operatorSymbol arguments =
  case arguments of
    [leftValue] ->
      Right (VOperator operatorSymbol [leftValue])
    [leftValue, rightValue] ->
      evalBinary builtinMode operatorSymbol leftValue rightValue
    _ ->
      Left
        ( runtimeDiagnostic
            "E3016"
            ("runtime primitive '" <> operatorSymbol <> "' received invalid arguments")
        )

-- | Builtin primitives are curried, so under-applied calls stay as function
-- values and only exact arity triggers evaluation.
applyBuiltin :: BuiltinResolutionMode -> BuiltinSymbol -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyBuiltin builtinMode builtinFunction arguments
  | length arguments < builtinSymbolArity builtinFunction =
      Right (VBuiltin builtinFunction arguments)
  | length arguments == builtinSymbolArity builtinFunction =
      evalBuiltin builtinMode builtinFunction arguments
  | otherwise =
      Left
        ( runtimeDiagnostic
            "E3014"
            ("runtime primitive '" <> builtinSymbolName builtinFunction <> "' received too many arguments")
        )

-- | Evaluate builtin semantics once enough arguments have been collected.
evalBuiltin :: BuiltinResolutionMode -> BuiltinSymbol -> [RuntimeValue] -> Either Diagnostic RuntimeValue
evalBuiltin builtinMode builtinFunction arguments =
  case (builtinFunction, arguments) of
    (BuiltinHd, [VList []]) ->
      Left (runtimeDiagnostic "E3009" "runtime primitive 'hd' failed: empty list")
    (BuiltinHd, [VList (headValue : _)]) ->
      Right headValue
    (BuiltinHd, [other]) ->
      Left
        ( runtimeDiagnostic
            "E3011"
            ("runtime primitive 'hd' expects a list argument, found " <> renderRuntimeType other)
        )
    (BuiltinTl, [VList []]) ->
      Left (runtimeDiagnostic "E3010" "runtime primitive 'tl' failed: empty list")
    (BuiltinTl, [VList (_ : tailValues)]) ->
      Right (VList tailValues)
    (BuiltinTl, [other]) ->
      Left
        ( runtimeDiagnostic
            "E3012"
            ("runtime primitive 'tl' expects a list argument, found " <> renderRuntimeType other)
        )
    (BuiltinMap, [mapper, collection])
      | not (isFunctionValue mapper) ->
          Left
            ( runtimeDiagnostic
                "E3015"
                ("runtime primitive 'map' expects a function as its first argument, found " <> renderRuntimeType mapper)
            )
      | otherwise ->
          case collection of
            VList elements ->
              VList <$> mapM (applyRuntimeFunction builtinMode mapper) elements
            other ->
              Left
                ( runtimeDiagnostic
                    "E3013"
                    ("runtime primitive 'map' expects a list as its second argument, found " <> renderRuntimeType other)
                )
    (BuiltinFilter, [predicate, collection])
      | not (isFunctionValue predicate) ->
          Left
            ( runtimeDiagnostic
                "E3017"
                ("runtime primitive 'filter' expects a function as its first argument, found " <> renderRuntimeType predicate)
            )
      | otherwise ->
          case collection of
            VList elements ->
              VList <$> filterElements builtinMode predicate elements
            other ->
              Left
                ( runtimeDiagnostic
                    "E3018"
                    ("runtime primitive 'filter' expects a list as its second argument, found " <> renderRuntimeType other)
                )
    -- Stub-v1 keeps `print!` side effects out of runtime plumbing; it returns
    -- its evaluated argument so expression pipelines remain deterministic.
    (BuiltinPrint, [value]) ->
      Right value
    _ ->
      Left
        ( runtimeDiagnostic
            "E3016"
            ("runtime primitive '" <> builtinSymbolName builtinFunction <> "' received invalid arguments")
        )

-- | Evaluate filter predicates element-by-element and enforce that each
-- predicate application returns a Bool.
filterElements :: BuiltinResolutionMode -> RuntimeValue -> [RuntimeValue] -> Either Diagnostic [RuntimeValue]
filterElements builtinMode predicate values = do
  results <- mapM applyPredicate values
  pure [value | (value, True) <- results]
  where
    -- Preserve runtime safety for partially-known function values that can slip
    -- past compile-time checks in direct `evaluateRuntimeExpr` tests.
    applyPredicate :: RuntimeValue -> Either Diagnostic (RuntimeValue, Bool)
    applyPredicate value = do
      predicateResult <- applyRuntimeFunction builtinMode predicate value
      case predicateResult of
        VBool shouldKeep -> Right (value, shouldKeep)
        other ->
          Left
            ( runtimeDiagnostic
                "E3019"
                ("runtime primitive 'filter' predicate must return Bool, found " <> renderRuntimeType other)
            )

isFunctionValue :: RuntimeValue -> Bool
isFunctionValue value =
  case value of
    VSectionLeft {} -> True
    VSectionRight {} -> True
    VClosure {} -> True
    VBuiltin {} -> True
    VOperator {} -> True
    _ -> False

-- | Evaluate the builtin operator subset supported by the runtime.
evalBinary :: BuiltinResolutionMode -> Text -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
evalBinary builtinMode operatorSymbol leftValue rightValue =
  case (operatorSymbol, leftValue, rightValue) of
    ("+", VInt leftInt, VInt rightInt) -> Right (VInt (leftInt + rightInt))
    ("-", VInt leftInt, VInt rightInt) -> Right (VInt (leftInt - rightInt))
    ("*", VInt leftInt, VInt rightInt) -> Right (VInt (leftInt * rightInt))
    ("/", VInt _, VInt 0) ->
      Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
    ("/", VInt leftInt, VInt rightInt) ->
      Right (VInt (leftInt `div` rightInt))
    ("<", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt < rightInt))
    ("<=", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt <= rightInt))
    (">", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt > rightInt))
    (">=", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt >= rightInt))
    ("==", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt == rightInt))
    ("==", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool == rightBool))
    ("!=", VInt leftInt, VInt rightInt) -> Right (VBool (leftInt /= rightInt))
    ("!=", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool /= rightBool))
    ("$", functionValue, argumentValue) ->
      applyRuntimeFunction builtinMode functionValue argumentValue
    _ ->
      Left
        ( runtimeDiagnostic
            "E3007"
            ( "runtime primitive '"
                <> operatorSymbol
                <> "' cannot be applied to "
                <> renderRuntimeType leftValue
                <> " and "
                <> renderRuntimeType rightValue
            )
        )

-- | Runtime-specific wrapper for mkDiagnostic.
-- This alias exists solely to improve readability and make it clear that
-- diagnostics are being created in a runtime evaluation context rather than
-- during parsing or type checking.
runtimeDiagnostic :: Text -> Text -> Diagnostic
runtimeDiagnostic = mkDiagnostic

-- | Render coarse runtime type names for diagnostics.
renderRuntimeType :: RuntimeValue -> Text
renderRuntimeType value =
  case value of
    VInt {} -> "Int"
    VBool {} -> "Bool"
    VList {} -> "List"
    VSectionLeft {} -> "Function"
    VSectionRight {} -> "Function"
    VClosure {} -> "Function"
    VBuiltin {} -> "Function"
    VOperator {} -> "Function"
