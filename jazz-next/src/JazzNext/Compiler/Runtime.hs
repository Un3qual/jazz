{-# LANGUAGE OverloadedStrings #-}

-- | Small interpreter/runtime for the currently-supported core language. It is
-- intentionally simple and mirrors the same builtin/operator contracts enforced
-- by analysis and type inference.
module JazzNext.Compiler.Runtime
  ( RuntimeValue (..),
    evaluateRuntimeExprWithBuiltinsAndBindingHints,
    evaluateRuntimeExprWithBuiltins,
    evaluateRuntimeExpr,
    runtimeValueExactlyMatchesConstraint,
    renderRuntimeValue
  ) where

import Control.Monad (foldM, zipWithM)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import JazzNext.Compiler.AST
  ( CaseArm (..),
    ClassMethodSignature (..),
    ConstraintSignatureType (..),
    DataConstructorArgument (..),
    DataConstructor (..),
    Expr (..),
    ImplMethod (..),
    Literal (..),
    NumericType (..),
    Pattern (..),
    SignaturePayload (..),
    SignatureType (..),
    Statement (..)
  )
import JazzNext.Compiler.Diagnostics
  ( Diagnostic,
    mkDiagnostic
  )
import JazzNext.Compiler.FractionalLiteral
  ( FractionalLiteralSource,
    fractionalLiteralExceedsMagnitude,
    fractionalLiteralIntegralValue
  )
import JazzNext.Compiler.BuiltinCatalog
  ( BuiltinResolutionMode (..),
    BuiltinSymbol (..),
    builtinNamesInMode,
    builtinSymbolArity,
    builtinSymbolName,
    builtinSymbolNumericConversionTarget,
    lookupBuiltinSymbolInMode,
    numericTypeFloatMax,
    numericTypeIntegerBounds
  )
import JazzNext.Compiler.CapabilityFacts
  ( concreteConstraintArgument,
    constraintFunctionArgumentTypes,
    qualifiedMethodKey,
    signaturePayloadConstraintType,
    substituteClassMethodSignature
  )
import JazzNext.Compiler.Identifier
  ( Identifier,
    identifierText,
    mkIdentifier,
    operatorBindingIdentifierText
  )
import JazzNext.Compiler.Parser.Operator
  ( isBuiltinOperatorSymbol
  )
import JazzNext.Compiler.RecursiveBindings
  ( collectBindingNames,
    freeVarsExprWithBound,
    freeVarsScopeWithBound,
    inferRecursiveGroupsOrdered,
    inferSelfRecursiveBindings
  )
import JazzNext.Compiler.RuntimeHints
  ( BindingRuntimeHintKey,
    bindingRuntimeHintKeyInModule
  )

-- | Runtime values produced by the interpreter, including partially applied
-- builtins/operators.
data RuntimeFloatMetadata = RuntimeFloatMetadata
  { runtimeFloatLiteralSource :: Maybe FractionalLiteralSource,
    runtimeFloatTargetType :: Maybe NumericType
  }
  deriving (Eq, Show)

data RuntimeIntMetadata = RuntimeIntMetadata
  { runtimeIntTargetType :: Maybe NumericType
  }
  deriving (Eq, Show)

data RuntimeMethodCandidate = RuntimeMethodCandidate ConstraintSignatureType (Either Diagnostic RuntimeValue)

data RuntimeValue
  = VInt Integer RuntimeIntMetadata
  | VFloat Double RuntimeFloatMetadata
  | VBool Bool
  | VList [RuntimeValue] (Maybe ConstraintSignatureType)
  | VTuple [RuntimeValue]
  | VClosure RuntimeEnv Identifier Expr (Maybe ConstraintSignatureType) (Maybe [Text])
  | VBuiltin BuiltinSymbol [RuntimeValue]
  | VOperator Text [RuntimeValue]
  | VSectionLeft Text RuntimeValue
  | VSectionRight Text RuntimeValue
  | VConstructor Identifier [Identifier] Identifier [DataConstructorArgument] [RuntimeValue]
  | VQualifiedMethod Text Text SignaturePayload [RuntimeMethodCandidate] [RuntimeValue]
  | VTyped ConstraintSignatureType RuntimeValue

instance Eq RuntimeValue where
  leftValue == rightValue =
    case (leftValue, rightValue) of
      (VTyped _ leftInner, rightInner) -> leftInner == rightInner
      (leftInner, VTyped _ rightInner) -> leftInner == rightInner
      (VInt leftInt _, VInt rightInt _) -> leftInt == rightInt
      (VFloat leftFloat _, VFloat rightFloat _) -> leftFloat == rightFloat
      (VBool leftBool, VBool rightBool) -> leftBool == rightBool
      (VList leftElements _, VList rightElements _) -> leftElements == rightElements
      (VTuple leftElements, VTuple rightElements) -> leftElements == rightElements
      ( VConstructor leftTypeName leftTypeParameters leftName leftConstructorArguments leftArgs,
        VConstructor rightTypeName rightTypeParameters rightName rightConstructorArguments rightArgs
        )
          | constructorIsSaturated leftConstructorArguments leftArgs,
            constructorIsSaturated rightConstructorArguments rightArgs ->
          leftTypeName == rightTypeName
            && leftTypeParameters == rightTypeParameters
            && leftName == rightName
            && leftConstructorArguments == rightConstructorArguments
            && leftArgs == rightArgs
      _ -> False

instance Eq RuntimeMethodCandidate where
  RuntimeMethodCandidate leftTarget leftCell == RuntimeMethodCandidate rightTarget rightCell =
    leftTarget == rightTarget && leftCell == rightCell

instance Show RuntimeValue where
  show value =
    case value of
      VInt intValue _ -> "VInt " <> show intValue
      VFloat floatValue _ -> "VFloat " <> show floatValue
      VBool boolValue -> "VBool " <> show boolValue
      VList elements maybeTypeHint -> "VList " <> show elements <> " " <> show maybeTypeHint
      VTuple elements -> "VTuple " <> show elements
      VClosure _ parameterName bodyExpr maybeTypeHint modulePath ->
        "VClosure <env> " <> show parameterName <> " " <> show bodyExpr <> " " <> show maybeTypeHint <> " " <> show modulePath
      VBuiltin builtinSymbol capturedArgs ->
        "VBuiltin " <> show builtinSymbol <> " " <> show capturedArgs
      VOperator operatorSymbol capturedArgs ->
        "VOperator " <> show operatorSymbol <> " " <> show capturedArgs
      VSectionLeft operatorSymbol operand ->
        "VSectionLeft " <> show operatorSymbol <> " " <> show operand
      VSectionRight operatorSymbol operand ->
        "VSectionRight " <> show operatorSymbol <> " " <> show operand
      VConstructor typeName _ constructorName constructorArguments capturedArgs ->
        "VConstructor " <> show typeName <> " " <> show constructorName <> " " <> show constructorArguments <> " " <> show capturedArgs
      VQualifiedMethod methodKey _ _ candidates capturedArgs ->
        "VQualifiedMethod " <> show methodKey <> " " <> show candidates <> " " <> show capturedArgs
      VTyped typeHint innerValue ->
        "VTyped " <> show typeHint <> " " <> show innerValue

instance Show RuntimeMethodCandidate where
  show (RuntimeMethodCandidate implTarget _) =
    "RuntimeMethodCandidate " <> show implTarget

evaluateRuntimeExpr :: Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExpr = evaluateRuntimeExprWithBuiltins ResolveKernelOnly

-- | Evaluate an expression under the builtin resolution mode chosen by the
-- caller, returning a terminal scope value when one exists.
evaluateRuntimeExprWithBuiltins :: BuiltinResolutionMode -> Expr -> Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltins builtinMode expr =
  evaluateRuntimeExprWithBuiltinsAndBindingHints builtinMode Map.empty expr

evaluateRuntimeExprWithBuiltinsAndBindingHints ::
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey ConstraintSignatureType ->
  Expr ->
  Either Diagnostic (Maybe RuntimeValue)
evaluateRuntimeExprWithBuiltinsAndBindingHints builtinMode bindingTypeHints expr =
  case expr of
    EBlock statements -> evalScope builtinMode bindingTypeHints Map.empty statements
    _ -> Just <$> evalValue builtinMode bindingTypeHints Map.empty expr

renderRuntimeValue :: RuntimeValue -> Text
renderRuntimeValue value =
  case value of
    VInt intValue _ -> Text.pack (show intValue)
    VFloat floatValue _ -> Text.pack (show floatValue)
    VBool boolValue ->
      if boolValue
        then "True"
        else "False"
    VList elements _ ->
      "[" <> Text.intercalate ", " (map renderRuntimeValue elements) <> "]"
    VTuple elements ->
      "(" <> Text.intercalate ", " (map renderRuntimeValue elements) <> ")"
    VClosure {} -> "<function>"
    VBuiltin _ _ -> "<function>"
    VOperator {} -> "<function>"
    VSectionLeft {} -> "<function>"
    VSectionRight {} -> "<function>"
    VConstructor _ _ constructorName constructorArguments capturedArgs
      | constructorIsSaturated constructorArguments capturedArgs ->
          renderConstructorValue constructorName capturedArgs
      | otherwise ->
          "<function>"
    VQualifiedMethod {} -> "<function>"
    VTyped _ innerValue -> renderRuntimeValue innerValue

renderConstructorValue :: Identifier -> [RuntimeValue] -> Text
renderConstructorValue constructorName arguments =
  case arguments of
    [] -> renderConstructorName constructorName
    _ ->
      renderConstructorName constructorName
        <> "("
        <> Text.intercalate ", " (map renderRuntimeValue arguments)
        <> ")"

renderConstructorName :: Identifier -> Text
renderConstructorName constructorName =
  let nameText = identifierText constructorName
      segments = Text.splitOn "::" nameText
   in case segments of
        "__module" : _ -> last segments
        _ -> nameText

-- | Runtime cells can hold either a value or the deterministic failure for a
-- recursive binding that cannot be forced safely.
type RuntimeCell = Either Diagnostic RuntimeValue

type RuntimeEnv = Map Text RuntimeCell

-- | Evaluate a block scope in order. Declarations clear `lastExprValue`, so
-- `evalScope` returns `Just` only when the final surviving statement is an
-- `SExpr`; otherwise the block yields `Nothing`.
evalScope :: BuiltinResolutionMode -> Map BindingRuntimeHintKey ConstraintSignatureType -> RuntimeEnv -> [Statement] -> Either Diagnostic (Maybe RuntimeValue)
evalScope =
  evalScopeWithModulePath Nothing

evalScopeWithModulePath :: Maybe [Text] -> BuiltinResolutionMode -> Map BindingRuntimeHintKey ConstraintSignatureType -> RuntimeEnv -> [Statement] -> Either Diagnostic (Maybe RuntimeValue)
evalScopeWithModulePath currentModulePath builtinMode bindingTypeHints initialEnv statements = go initialEnv Nothing indexedStatements
  where
    indexedStatements = zip [0 ..] statements
    statementsByIndex = Map.fromList indexedStatements
    modulePathsByStatement = collectModulePathsByStatement currentModulePath indexedStatements
    recursiveGroups =
      inferRecursiveGroupsOrdered
        (Set.union (Map.keysSet initialEnv) (builtinNamesInMode builtinMode))
        indexedStatements
    selfRecursiveFunctionStatements =
      inferSelfRecursiveBindings exprContainsFunctionBranch indexedStatements
    bindingNamesByStatement = collectBindingNames indexedStatements
    bindingCells = map (uncurry cellForStatement) indexedStatements

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
            SClass _ capabilityName parameters methods ->
              go (insertClassMethods capabilityName parameters methods env) Nothing rest
            SImpl _ capabilityName arguments methods ->
              go (insertImplMethods (modulePathForStatement statementIndex) capabilityName arguments methods env) Nothing rest
            SData _ typeName typeParameters constructors ->
              go (insertDataConstructors typeName typeParameters constructors env) Nothing rest
            SLet name _ _ -> do
              value <- bindingCellAt statementIndex
              go (Map.insert (identifierText name) (Right value) env) Nothing rest
            SExpr _ expr -> do
              value <- evalValueAt statementIndex env expr
              go env (Just value) rest

    modulePathForStatement :: Int -> Maybe [Text]
    modulePathForStatement statementIndex =
      Map.findWithDefault currentModulePath statementIndex modulePathsByStatement

    evalValueAt :: Int -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
    evalValueAt statementIndex =
      evalValueWithModulePath (modulePathForStatement statementIndex) builtinMode bindingTypeHints

    bindingCellAt :: Int -> RuntimeCell
    bindingCellAt statementIndex =
      case drop statementIndex bindingCells of
        cell : _ -> cell
        [] ->
          Left
            (runtimeDiagnostic "E3020" "internal runtime error: missing binding cell for statement")
    
    cellForStatement :: Int -> Statement -> RuntimeCell
    cellForStatement statementIndex statement =
      case statement of
        SLet bindingName _ valueExpr ->
          bindingCell statementIndex bindingName valueExpr
        _ ->
          Left
            (runtimeDiagnostic "E3020" "internal runtime error: expected binding statement")

    bindingCell :: Int -> Identifier -> Expr -> RuntimeCell
    bindingCell statementIndex bindingName valueExpr =
      case selectedRecursiveAliasTarget statementIndex visibleEnv valueExpr of
        Left diagnostic ->
          Left diagnostic
        Right (Just targetIndex) ->
          case resolveRecursiveAliasTarget (Set.singleton statementIndex) targetIndex of
            Left diagnostic -> Left diagnostic
            Right resolvedTargetIndex -> bindingCellAt resolvedTargetIndex
        Right Nothing
          | Map.member statementIndex recursiveGroups,
            exprDefinitelyNotFunctionValue valueExpr ->
              Left (runtimeDiagnostic "E3021" "runtime recursive binding has no concrete value")
          | otherwise ->
              do
                evaluatedValue <- evalBindingValue statementIndex bindingName visibleEnv valueExpr
                Right (attachSelfRecursiveBinding statementIndex bindingName evaluatedValue)
      where
        visibleEnv = bindingEnv statementIndex bindingName

    evalBindingValue :: Int -> Identifier -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
    evalBindingValue statementIndex bindingName env valueExpr =
      case previousSignatureNumericTarget statementIndex bindingName of
        Just targetType -> do
          runtimeValue <- evalNumericSignatureBinding statementIndex targetType env valueExpr
          attachRuntimeTypeHint (previousSignatureRuntimeTypeHint statementIndex bindingName) runtimeValue
            >>= attachDefaultBindingIntegerTarget
        Nothing -> do
          runtimeValue <- evalValueAt statementIndex env valueExpr
          attachRuntimeTypeHint (bindingRuntimeTypeHint statementIndex bindingName) runtimeValue
            >>= attachDefaultBindingIntegerTarget

    evalNumericSignatureBinding :: Int -> NumericType -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
    evalNumericSignatureBinding statementIndex targetType env valueExpr =
      case valueExpr of
        ELit (LInt literalValue) ->
          convertIntegerToNumericTarget conversionBuiltin targetType literalValue
        ELit (LFloat literalValue literalSource _) ->
          convertFloatToNumericTarget conversionBuiltin targetType literalValue (Just literalSource)
        _ -> do
          runtimeValue <- evalValueAt statementIndex env valueExpr
          evalNumericConversion conversionBuiltin targetType runtimeValue
      where
        conversionBuiltin = numericConversionBuiltinForTarget targetType

    previousSignatureNumericTarget :: Int -> Identifier -> Maybe NumericType
    previousSignatureNumericTarget statementIndex bindingName =
      case Map.lookup (statementIndex - 1) statementsByIndex of
        Just (SSignature signatureName _ signaturePayload)
          | identifierText signatureName == identifierText bindingName ->
              signatureNumericTarget signaturePayload
        _ -> Nothing

    previousSignatureRuntimeTypeHint :: Int -> Identifier -> Maybe ConstraintSignatureType
    previousSignatureRuntimeTypeHint statementIndex bindingName =
      case Map.lookup (statementIndex - 1) statementsByIndex of
        Just (SSignature signatureName _ signaturePayload)
          | identifierText signatureName == identifierText bindingName ->
              signaturePayloadConstraintType signaturePayload
        _ -> Nothing

    bindingRuntimeTypeHint :: Int -> Identifier -> Maybe ConstraintSignatureType
    bindingRuntimeTypeHint statementIndex bindingName =
      case previousSignatureRuntimeTypeHint statementIndex bindingName of
        Just signatureHint -> Just signatureHint
        Nothing ->
          case Map.lookup statementIndex statementsByIndex of
            Just (SLet _ bindingSpan _) ->
              Map.lookup
                (bindingRuntimeHintKeyInModule (modulePathForStatement statementIndex) bindingName bindingSpan)
                bindingTypeHints
            _ -> Nothing

    collectModulePathsByStatement :: Maybe [Text] -> [(Int, Statement)] -> Map Int (Maybe [Text])
    collectModulePathsByStatement initialModulePath =
      snd . foldl' collectModulePath (initialModulePath, Map.empty)
      where
        collectModulePath (currentModulePath, pathsByStatement) (statementIndex, statement) =
          let nextModulePath =
                case statement of
                  SModule _ modulePath -> Just modulePath
                  _ -> currentModulePath
           in ( nextModulePath,
                Map.insert statementIndex nextModulePath pathsByStatement
              )

    signatureNumericTarget :: SignaturePayload -> Maybe NumericType
    signatureNumericTarget signaturePayload =
      case signaturePayload of
        SignatureType TypeInt -> Just NumericInt64
        SignatureType TypeFloat -> Just NumericFloat64
        SignatureType (TypeNumeric targetType) -> Just targetType
        ConstrainedSignature _ signatureType ->
          constraintSignatureNumericTarget signatureType
        _ -> Nothing

    constraintSignatureNumericTarget :: ConstraintSignatureType -> Maybe NumericType
    constraintSignatureNumericTarget signatureType =
      case signatureType of
        ConstraintTypeName typeName ->
          case identifierText typeName of
            "Int" -> Just NumericInt64
            "Int8" -> Just NumericInt8
            "Int16" -> Just NumericInt16
            "Int32" -> Just NumericInt32
            "Int64" -> Just NumericInt64
            "UInt8" -> Just NumericUInt8
            "UInt16" -> Just NumericUInt16
            "UInt32" -> Just NumericUInt32
            "UInt64" -> Just NumericUInt64
            "Float" -> Just NumericFloat64
            "Float16" -> Just NumericFloat16
            "Float32" -> Just NumericFloat32
            "Float64" -> Just NumericFloat64
            _ -> Nothing
        _ -> Nothing

    -- Alias bridges can legitimately point across a recursive SCC, but pure
    -- alias loops need a deterministic diagnostic instead of infinite forcing.
    resolveRecursiveAliasTarget :: Set Int -> Int -> Either Diagnostic Int
    resolveRecursiveAliasTarget visited statementIndex
      | Set.member statementIndex visited =
          Left (runtimeDiagnostic "E3021" "runtime recursive alias cycle has no concrete value")
      | otherwise =
          case Map.lookup statementIndex statementsByIndex of
            Just (SLet bindingName _ aliasExpr) ->
              case selectedRecursiveAliasTarget statementIndex (bindingEnv statementIndex bindingName) aliasExpr of
                Left diagnostic ->
                  Left diagnostic
                Right (Just nextTargetIndex) ->
                  resolveRecursiveAliasTarget (Set.insert statementIndex visited) nextTargetIndex
                Right Nothing ->
                  Right statementIndex
            Just _ ->
              Left
                (runtimeDiagnostic "E3020" "internal runtime error: expected binding statement while resolving alias")
            Nothing ->
              Left
                (runtimeDiagnostic "E3020" "internal runtime error: missing binding statement while resolving alias")

    bindingEnv :: Int -> Identifier -> RuntimeEnv
    bindingEnv statementIndex bindingName =
      case functionSelfReferenceCell statementIndex bindingNameText of
        Just selfCell ->
          Map.insert
            bindingNameText
            selfCell
            peerVisibleEnv
        Nothing
          | recursiveBindingNeedsSelf statementIndex ->
              Map.insert
                bindingNameText
                (bindingCellAt statementIndex)
                peerVisibleEnv
          | otherwise -> peerVisibleEnv
      where
        bindingNameText = identifierText bindingName
        peerVisibleEnv = recursivePeerEnv statementIndex (envBefore statementIndex)

    functionSelfReferenceCell :: Int -> Text -> Maybe RuntimeCell
    functionSelfReferenceCell statementIndex bindingNameText
      | recursiveFunctionNeedsSelf statementIndex bindingNameText =
          Just (Left (runtimeDiagnostic "E3021" "runtime recursive binding has no concrete value"))
      | otherwise =
          Nothing

    recursiveFunctionNeedsSelf :: Int -> Text -> Bool
    recursiveFunctionNeedsSelf statementIndex bindingNameText =
      Set.member statementIndex selfRecursiveFunctionStatements
        && Map.notMember bindingNameText (envBefore statementIndex)

    recursiveBindingNeedsSelf :: Int -> Bool
    recursiveBindingNeedsSelf statementIndex =
      -- Function-valued self recursion gets stitched onto the resulting
      -- closure after wrapper evaluation. Pre-seeding `self` here is only
      -- needed for non-function recursive bindings; doing it eagerly for block
      -- alias wrappers can blackhole before the closure is returned.
      Map.member statementIndex recursiveGroups
        && Set.notMember statementIndex selfRecursiveFunctionStatements

    -- Wrapper expressions like `if` and `{ g = \(x) -> f x. g. }` should
    -- evaluate to their closure first, then get their own binding stitched
    -- into the captured env without forcing the whole wrapper through a
    -- self-referential scope during evaluation.
    attachSelfRecursiveBinding :: Int -> Identifier -> RuntimeValue -> RuntimeValue
    attachSelfRecursiveBinding statementIndex bindingName runtimeValue
      | recursiveFunctionNeedsSelf statementIndex (identifierText bindingName) =
          case runtimeValue of
            VClosure capturedEnv parameterName bodyExpr maybeTypeHint closureModulePath ->
              VClosure
                (Map.insert (identifierText bindingName) (bindingCellAt statementIndex) capturedEnv)
                parameterName
                bodyExpr
                maybeTypeHint
                closureModulePath
            _ -> runtimeValue
      | otherwise =
          runtimeValue

    recursiveAliasTarget :: Set Text -> Int -> Expr -> Maybe Int
    recursiveAliasTarget locallyBoundNames statementIndex valueExpr =
      case peelSingleExprBlock valueExpr of
        EVar targetName ->
          if Set.member (identifierText targetName) locallyBoundNames
            then Nothing
            else
              case Map.lookup statementIndex recursiveGroups of
                Just groupMembers ->
                  lookupRecursivePeer targetName groupMembers
                Nothing -> Nothing
        EOperatorValue operatorSymbol
          | not (isBuiltinOperatorSymbol operatorSymbol) ->
              let targetName = mkIdentifier (operatorBindingIdentifierText operatorSymbol)
               in
                if Set.member (identifierText targetName) locallyBoundNames
                  then Nothing
                  else
                    case Map.lookup statementIndex recursiveGroups of
                      Just groupMembers ->
                        lookupRecursivePeer targetName groupMembers
                      Nothing -> Nothing
        _ -> Nothing

    -- Preserve wrapper runtime semantics by evaluating the branch condition
    -- first, then following alias resolution only through the selected branch.
    selectedRecursiveAliasTarget :: Int -> RuntimeEnv -> Expr -> Either Diagnostic (Maybe Int)
    selectedRecursiveAliasTarget =
      selectedRecursiveAliasTargetWithBound Set.empty

    selectedRecursiveAliasTargetWithBound ::
      Set Text ->
      Int ->
      RuntimeEnv ->
      Expr ->
      Either Diagnostic (Maybe Int)
    selectedRecursiveAliasTargetWithBound locallyBoundNames statementIndex env expr =
      case peelSingleExprBlock expr of
        EIf conditionExpr thenExpr elseExpr ->
          selectRecursiveAliasTarget locallyBoundNames statementIndex env conditionExpr thenExpr elseExpr
        ECase conditionExpr thenExpr elseExpr ->
          selectRecursiveAliasTarget locallyBoundNames statementIndex env conditionExpr thenExpr elseExpr
        EPatternCase scrutineeExpr caseArms -> do
          scrutineeValue <- evalValueAt statementIndex env scrutineeExpr
          selectedArm <-
            selectMatchingCaseArmForAlias
              (evalValueAt statementIndex)
              env
              scrutineeValue
              caseArms
          case selectedArm of
            Just (newLocallyBoundNames, armEnv, bodyExpr) ->
              selectedRecursiveAliasTargetWithBound
                (Set.union locallyBoundNames newLocallyBoundNames)
                statementIndex
                armEnv
                bodyExpr
            Nothing ->
              Right Nothing
        peeledExpr ->
          Right (recursiveAliasTarget locallyBoundNames statementIndex peeledExpr)

    selectRecursiveAliasTarget :: Set Text -> Int -> RuntimeEnv -> Expr -> Expr -> Expr -> Either Diagnostic (Maybe Int)
    selectRecursiveAliasTarget locallyBoundNames statementIndex env conditionExpr thenExpr elseExpr = do
      conditionValue <- evalValueAt statementIndex env conditionExpr
      case conditionValue of
        VBool True ->
          selectedRecursiveAliasTargetWithBound locallyBoundNames statementIndex env thenExpr
        VBool False ->
          selectedRecursiveAliasTargetWithBound locallyBoundNames statementIndex env elseExpr
        other ->
          Left
            ( runtimeDiagnostic
                "E3003"
                ("runtime branch condition must be Bool, found " <> renderRuntimeType other)
            )

    selectMatchingCaseArmForAlias ::
      (RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue) ->
      RuntimeEnv ->
      RuntimeValue ->
      [CaseArm] ->
      Either Diagnostic (Maybe (Set Text, RuntimeEnv, Expr))
    selectMatchingCaseArmForAlias evalGuard env scrutineeValue =
      go
      where
        go remainingArms =
          case remainingArms of
            [] -> Right Nothing
            caseArm : rest ->
              chooseArm caseArm rest

        chooseArm caseArm rest =
          case matchCaseArm env scrutineeValue caseArm of
            Just (armEnv, guardExpr, bodyExpr) ->
              case guardExpr of
                Nothing ->
                  Right
                    ( Just
                        ( caseArmBoundNames caseArm,
                          armEnv,
                          bodyExpr
                        )
                    )
                Just conditionExpr -> do
                  guardValue <- evalGuard armEnv conditionExpr
                  case guardValue of
                    VBool True ->
                      Right
                        ( Just
                            ( caseArmBoundNames caseArm,
                              armEnv,
                              bodyExpr
                            )
                        )
                    VBool False ->
                      go rest
                    other ->
                      Left
                        ( runtimeDiagnostic
                            "E3003"
                            ("runtime case guard must be Bool, found " <> renderRuntimeType other)
                        )
            Nothing ->
              go rest

    caseArmBoundNames :: CaseArm -> Set Text
    caseArmBoundNames (CaseArm pattern _ _) =
      patternBoundNames pattern

    -- Single-expression blocks are semantically transparent here, so peel
    -- them before following recursive alias edges and cycle detection.
    peelSingleExprBlock :: Expr -> Expr
    peelSingleExprBlock expr =
      case expr of
        EBlock [SExpr _ innerExpr] -> peelSingleExprBlock innerExpr
        _ -> expr

    terminalBlockLocalAliasExpr :: [Statement] -> Maybe ([Statement], Expr)
    terminalBlockLocalAliasExpr statements =
      case reverse statements of
        SExpr _ (EVar aliasName) : precedingStatements ->
          let prefixStatements = reverse precedingStatements
           in fmap
                (\aliasExpr -> (prefixStatements, aliasExpr))
                (followLocalAlias Set.empty aliasName (localAliasBindings prefixStatements))
        _ -> Nothing

    localAliasBindings :: [Statement] -> Map Text Expr
    localAliasBindings =
      foldl' collectBinding Map.empty
      where
        collectBinding bindings statement =
          case statement of
            SLet bindingName _ bindingExpr ->
              Map.insert (identifierText bindingName) bindingExpr bindings
            _ -> bindings

    followLocalAlias :: Set Text -> Identifier -> Map Text Expr -> Maybe Expr
    followLocalAlias visitedNames aliasName localBindings =
      let aliasNameText = identifierText aliasName
       in if Set.member aliasNameText visitedNames
            then Nothing
            else
              case Map.lookup aliasNameText localBindings of
                Just aliasExpr ->
                  case peelSingleExprBlock aliasExpr of
                    EVar nextAliasName
                      | Map.member (identifierText nextAliasName) localBindings ->
                          followLocalAlias (Set.insert aliasNameText visitedNames) nextAliasName localBindings
                    _ -> Just aliasExpr
                Nothing ->
                  Nothing

    blockLocalAliasEnv :: Maybe [Text] -> RuntimeEnv -> [Statement] -> RuntimeEnv
    blockLocalAliasEnv blockModulePath blockInitialEnv blockStatements =
      case blockStatements of
        [] -> blockInitialEnv
        _ -> blockEnvAfter (length blockStatements - 1)
      where
        indexedBlockStatements = zip [0 ..] blockStatements
        blockStatementsByIndex = Map.fromList indexedBlockStatements
        blockBindingCells = map (uncurry blockCellForStatement) indexedBlockStatements

        blockEnvBefore statementIndex
          | statementIndex <= 0 = blockInitialEnv
          | otherwise = blockEnvAfter (statementIndex - 1)

        blockEnvAfter statementIndex =
          case Map.lookup statementIndex blockStatementsByIndex of
            Just (SLet bindingName _ _) ->
              Map.insert
                (identifierText bindingName)
                (blockBindingCellAt statementIndex)
                (blockEnvBefore statementIndex)
            Just (SData _ typeName typeParameters constructors) ->
              insertDataConstructors typeName typeParameters constructors (blockEnvBefore statementIndex)
            Just (SClass _ capabilityName parameters methods) ->
              insertClassMethods capabilityName parameters methods (blockEnvBefore statementIndex)
            Just (SImpl _ capabilityName arguments methods) ->
              insertImplMethods blockModulePath capabilityName arguments methods (blockEnvBefore statementIndex)
            Just _ ->
              blockEnvBefore statementIndex
            Nothing ->
              blockEnvBefore statementIndex

        blockBindingCellAt statementIndex =
          case drop statementIndex blockBindingCells of
            cell : _ -> cell
            [] ->
              Left
                (runtimeDiagnostic "E3020" "internal runtime error: missing block binding cell for alias selection")

        blockCellForStatement statementIndex statement =
          case statement of
            SLet _ _ valueExpr ->
              evalValueWithModulePath blockModulePath builtinMode bindingTypeHints (blockEnvBefore statementIndex) valueExpr
                >>= attachDefaultBindingIntegerTarget
            _ ->
              Left
                (runtimeDiagnostic "E3020" "internal runtime error: expected block binding statement for alias selection")

    lookupRecursivePeer :: Identifier -> [Int] -> Maybe Int
    lookupRecursivePeer targetName =
      foldl' chooseTarget Nothing
      where
        targetNameText = identifierText targetName

        chooseTarget currentChoice peerIndex =
          case Map.lookup peerIndex bindingNamesByStatement of
            Just peerName
              | peerName == targetNameText ->
                  Just peerIndex
            _ -> currentChoice

    envBefore :: Int -> RuntimeEnv
    envBefore statementIndex
      | statementIndex <= 0 = initialEnv
      | otherwise = envAfter (statementIndex - 1)

    envAfter :: Int -> RuntimeEnv
    envAfter statementIndex =
      case Map.lookup statementIndex statementsByIndex of
        Just (SLet bindingName _ _) ->
          Map.insert
            (identifierText bindingName)
            (bindingCellAt statementIndex)
            (envBefore statementIndex)
        Just (SData _ typeName typeParameters constructors) ->
          insertDataConstructors typeName typeParameters constructors (envBefore statementIndex)
        Just (SClass _ capabilityName parameters methods) ->
          insertClassMethods capabilityName parameters methods (envBefore statementIndex)
        Just (SImpl _ capabilityName arguments methods) ->
          insertImplMethods (modulePathForStatement statementIndex) capabilityName arguments methods (envBefore statementIndex)
        Just _ ->
          envBefore statementIndex
        Nothing ->
          envBefore statementIndex

    recursivePeerEnv :: Int -> RuntimeEnv -> RuntimeEnv
    recursivePeerEnv statementIndex envBeforeValue =
      case Map.lookup statementIndex recursiveGroups of
        Nothing -> envBeforeValue
        Just groupMembers ->
          foldl' insertPeer envBeforeValue groupMembers
      where
        insertPeer envAcc peerIndex
          | peerIndex == statementIndex = envAcc
          | otherwise =
              case
                  Map.lookup peerIndex bindingNamesByStatement of
                Just peerName
                  | Map.notMember peerName envBeforeValue ->
                      Map.insert peerName (bindingCellAt peerIndex) envAcc
                _ ->
                  envAcc

    insertDataConstructors :: Identifier -> [Identifier] -> [DataConstructor] -> RuntimeEnv -> RuntimeEnv
    insertDataConstructors typeName typeParameters constructors env =
      foldl' insertConstructor env constructors
      where
        insertConstructor envAcc (DataConstructor constructorName constructorArguments) =
          Map.insert
            (identifierText constructorName)
            (Right (VConstructor typeName typeParameters constructorName constructorArguments []))
            envAcc

    insertClassMethods :: Identifier -> [Identifier] -> [ClassMethodSignature] -> RuntimeEnv -> RuntimeEnv
    insertClassMethods capabilityName parameters methods env =
      case parameters of
        [classParameter] ->
          foldl' (insertMethod (identifierText classParameter)) env methods
        _ -> env
      where
        insertMethod classParameter envAcc (ClassMethodSignature methodName _ methodSignature) =
          let methodKey = qualifiedMethodKey capabilityName methodName
           in if Map.member methodKey envAcc
                then envAcc
                else Map.insert methodKey (Right (VQualifiedMethod methodKey classParameter methodSignature [] [])) envAcc

    insertImplMethods :: Maybe [Text] -> Identifier -> [ConstraintSignatureType] -> [ImplMethod] -> RuntimeEnv -> RuntimeEnv
    insertImplMethods methodModulePath capabilityName arguments methods env =
      case arguments of
        [implTarget]
          | concreteConstraintArgument implTarget ->
              methodEnv
          where
            methodEnv = foldl' insertCandidate env methodCandidates
            methodExprsByKey =
              Map.fromList
                [ (qualifiedMethodKey capabilityName methodName, methodExpr)
                  | ImplMethod methodName _ methodExpr <- methods
                ]
            methodCandidates =
              map
                ( \(ImplMethod methodName _ methodExpr) ->
                    let methodKey = qualifiedMethodKey capabilityName methodName
                     in ( methodKey,
                          RuntimeMethodCandidate implTarget (methodCandidateCell implTarget methodKey methodExpr)
                        )
                )
                methods
            methodCandidateCell implTarget methodKey methodExpr =
              case selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey Set.empty methodEnv methodKey methodExpr of
                Left diagnostic ->
                  Left diagnostic
                Right True ->
                  Left
                    ( runtimeDiagnostic
                        "E3021"
                        ("runtime recursive qualified method alias cycle '" <> methodKey <> "' has no concrete value")
                    )
                Right False ->
                  evalValueWithModulePath methodModulePath builtinMode bindingTypeHints methodEnv methodExpr
                    >>= attachRuntimeMethodSignature methodEnv implTarget methodKey
            insertCandidate envAcc (methodKey, methodCandidate) =
              Map.adjust (addMethodCandidate methodCandidate) methodKey envAcc
        _ -> env
      where
        addMethodCandidate methodCandidate methodCell =
          case methodCell of
            Right (VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs) ->
              Right (VQualifiedMethod methodKey classParameter methodSignature (candidates ++ [methodCandidate]) capturedArgs)
            _ -> methodCell

    attachRuntimeMethodSignature ::
      RuntimeEnv ->
      ConstraintSignatureType ->
      Text ->
      RuntimeValue ->
      Either Diagnostic RuntimeValue
    attachRuntimeMethodSignature env implTarget methodKey methodValue =
      case Map.lookup methodKey env of
        Just (Right (VQualifiedMethod _ classParameter methodSignature _ _)) ->
          attachRuntimeTypeHint
            (substituteClassMethodSignature classParameter implTarget methodSignature)
            methodValue
        _ ->
          Right methodValue

    selectedQualifiedMethodAliasTarget :: Maybe [Text] -> Map Text Expr -> Set Text -> RuntimeEnv -> Text -> Expr -> Either Diagnostic Bool
    selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey expr
      | Set.member methodKey visitedMethodKeys =
          Right True
      | otherwise =
          case peelSingleExprBlock expr of
            EIf conditionExpr thenExpr elseExpr ->
              selectQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey conditionExpr thenExpr elseExpr
            ECase conditionExpr thenExpr elseExpr ->
              selectQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey conditionExpr thenExpr elseExpr
            EPatternCase scrutineeExpr caseArms -> do
              scrutineeValue <- evalValueWithModulePath methodModulePath builtinMode bindingTypeHints env scrutineeExpr
              selectedArm <-
                selectMatchingCaseArmForAlias
                  (evalValueWithModulePath methodModulePath builtinMode bindingTypeHints)
                  env
                  scrutineeValue
                  caseArms
              case selectedArm of
                Just (_, armEnv, bodyExpr) ->
                  selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys armEnv methodKey bodyExpr
                Nothing ->
                  Right False
            EBlock statements ->
              case terminalBlockLocalAliasExpr statements of
                Just (prefixStatements, aliasExpr) ->
                  selectedQualifiedMethodAliasTarget
                    methodModulePath
                    methodExprsByKey
                    visitedMethodKeys
                    (blockLocalAliasEnv methodModulePath env prefixStatements)
                    methodKey
                    aliasExpr
                Nothing ->
                  Right False
            EVar aliasName ->
              let aliasNameText = identifierText aliasName
               in case Map.lookup aliasNameText methodExprsByKey of
                    Just aliasExpr ->
                      selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey nextVisitedMethodKeys env aliasNameText aliasExpr
                    Nothing ->
                      Right (aliasNameText == methodKey)
            _ ->
              Right False
      where
        nextVisitedMethodKeys = Set.insert methodKey visitedMethodKeys

    selectQualifiedMethodAliasTarget :: Maybe [Text] -> Map Text Expr -> Set Text -> RuntimeEnv -> Text -> Expr -> Expr -> Expr -> Either Diagnostic Bool
    selectQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey conditionExpr thenExpr elseExpr = do
      conditionValue <- evalValueWithModulePath methodModulePath builtinMode bindingTypeHints env conditionExpr
      case conditionValue of
        VBool True ->
          selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey thenExpr
        VBool False ->
          selectedQualifiedMethodAliasTarget methodModulePath methodExprsByKey visitedMethodKeys env methodKey elseExpr
        other ->
          Left
            ( runtimeDiagnostic
                "E3003"
                ("runtime branch condition must be Bool, found " <> renderRuntimeType other)
            )

-- Match the type checker: self-seed recursion when any branch exposes a
-- lambda, so wrapped self-recursive closures capture their own binding before
-- runtime branch selection happens.
exprContainsFunctionBranch :: Expr -> Bool
exprContainsFunctionBranch expr =
  case expr of
    ELambda {} -> True
    EIf _ thenExpr elseExpr ->
      exprContainsFunctionBranch thenExpr
        || exprContainsFunctionBranch elseExpr
    ECase _ thenExpr elseExpr ->
      exprContainsFunctionBranch thenExpr
        || exprContainsFunctionBranch elseExpr
    EPatternCase _ caseArms ->
      any
        (\(CaseArm _ _ bodyExpr) -> exprContainsFunctionBranch bodyExpr)
        caseArms
    EBlock statements ->
      scopeContainsFunctionBranch statements
    _ -> False

scopeContainsFunctionBranch :: [Statement] -> Bool
scopeContainsFunctionBranch statements =
  case reverse statements of
    SExpr _ expr : _ ->
      exprContainsFunctionBranchViaScopeBindings
        (collectScopeBindingExprs statements)
        Set.empty
        expr
    _ -> False
  where
    -- Block expressions can return a locally-bound alias like `g`, so resolve
    -- same-block alias chains before deciding whether the terminal value is a
    -- lambda-shaped recursive binding.
    exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings scopeExpr =
      case scopeExpr of
        EVar bindingName ->
          case Map.lookup (identifierText bindingName) scopeBindings of
            Just bindingExpr
              | Set.notMember (identifierText bindingName) visitedBindings ->
                  exprContainsFunctionBranchViaScopeBindings
                    scopeBindings
                    (Set.insert (identifierText bindingName) visitedBindings)
                    bindingExpr
            _ -> False
        ELambda {} -> True
        EIf _ thenExpr elseExpr ->
          exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings thenExpr
            || exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings elseExpr
        ECase _ thenExpr elseExpr ->
          exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings thenExpr
            || exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings elseExpr
        EPatternCase _ caseArms ->
          any
            ( \(CaseArm _ _ bodyExpr) ->
                exprContainsFunctionBranchViaScopeBindings scopeBindings visitedBindings bodyExpr
            )
            caseArms
        EBlock nestedStatements ->
          scopeContainsFunctionBranch nestedStatements
        _ -> False

    collectScopeBindingExprs =
      foldl' collect Map.empty
      where
        collect scopeBindings statement =
          case statement of
            SLet bindingName _ valueExpr ->
              Map.insert (identifierText bindingName) valueExpr scopeBindings
            _ -> scopeBindings

-- Fail fast only when a recursive SCC member is obviously non-function-valued;
-- anything more ambiguous should keep the previous runtime path.
exprDefinitelyNotFunctionValue :: Expr -> Bool
exprDefinitelyNotFunctionValue expr =
  case expr of
    ELit {} -> True
    EList {} -> True
    ETuple {} -> True
    EBinary {} -> True
    EIf _ thenExpr elseExpr ->
      exprDefinitelyNotFunctionValue thenExpr
        && exprDefinitelyNotFunctionValue elseExpr
    ECase _ thenExpr elseExpr ->
      exprDefinitelyNotFunctionValue thenExpr
        && exprDefinitelyNotFunctionValue elseExpr
    EPatternCase {} ->
      False
    EBlock statements ->
      scopeDefinitelyNotFunctionValue statements
    _ -> False

scopeDefinitelyNotFunctionValue :: [Statement] -> Bool
scopeDefinitelyNotFunctionValue statements =
  case reverse statements of
    SExpr _ expr : _ -> exprDefinitelyNotFunctionValue expr
    _ -> False

evalValue :: BuiltinResolutionMode -> Map BindingRuntimeHintKey ConstraintSignatureType -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
evalValue =
  evalValueWithModulePath Nothing

evalValueWithModulePath :: Maybe [Text] -> BuiltinResolutionMode -> Map BindingRuntimeHintKey ConstraintSignatureType -> RuntimeEnv -> Expr -> Either Diagnostic RuntimeValue
evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env expr =
  case expr of
    ELit literal -> Right (literalRuntimeValue literal)
    EVar name ->
      case Map.lookup nameText env of
        Just value -> value >>= forceQualifiedMethodValue builtinMode bindingTypeHints
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
      Right (VClosure env parameterName bodyExpr Nothing currentModulePath)
    EOperatorValue operatorSymbol
      | isBuiltinOperatorSymbol operatorSymbol ->
          Right (VOperator operatorSymbol [])
      | otherwise ->
          lookupOperatorBindingRuntimeValue builtinMode bindingTypeHints operatorSymbol env
    EList elements ->
      (`VList` Nothing) <$> mapM (evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env) elements
    ETuple elements ->
      VTuple <$> mapM (evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env) elements
    EApply functionExpr argumentExpr -> do
      functionValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env functionExpr
      argumentValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env argumentExpr
      applyRuntimeFunction builtinMode bindingTypeHints functionValue argumentValue
    EIf conditionExpr thenExpr elseExpr ->
      evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env (ECase conditionExpr thenExpr elseExpr)
    ECase conditionExpr thenExpr elseExpr -> do
      conditionValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env conditionExpr
      case conditionValue of
        VBool True -> evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env thenExpr
        VBool False -> evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env elseExpr
        other ->
          Left
            ( runtimeDiagnostic
                "E3003"
                ("runtime branch condition must be Bool, found " <> renderRuntimeType other)
            )
    EPatternCase scrutineeExpr caseArms -> do
      scrutineeValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env scrutineeExpr
      evalPatternCase currentModulePath builtinMode bindingTypeHints env scrutineeValue caseArms
    EBinary operatorSymbol leftExpr rightExpr
      | isBuiltinOperatorSymbol operatorSymbol -> do
          leftValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env leftExpr
          rightValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env rightExpr
          evalBinary builtinMode bindingTypeHints operatorSymbol leftValue rightValue
      | otherwise -> do
          operatorValue <- lookupOperatorBindingRuntimeValue builtinMode bindingTypeHints operatorSymbol env
          leftValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env leftExpr
          partialValue <- applyRuntimeFunction builtinMode bindingTypeHints operatorValue leftValue
          rightValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env rightExpr
          applyRuntimeFunction builtinMode bindingTypeHints partialValue rightValue
    ESectionLeft leftExpr operatorSymbol -> do
      leftValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env leftExpr
      if isBuiltinOperatorSymbol operatorSymbol
        then Right (VSectionLeft operatorSymbol leftValue)
        else do
          operatorValue <- lookupOperatorBindingRuntimeValue builtinMode bindingTypeHints operatorSymbol env
          applyRuntimeFunction builtinMode bindingTypeHints operatorValue leftValue
    ESectionRight operatorSymbol rightExpr -> do
      rightValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints env rightExpr
      if isBuiltinOperatorSymbol operatorSymbol
        then Right (VSectionRight operatorSymbol rightValue)
        else do
          operatorValue <- lookupOperatorBindingRuntimeValue builtinMode bindingTypeHints operatorSymbol env
          Right (declaredOperatorRightSectionClosure currentModulePath operatorValue rightValue env)
    EBlock statements ->
      case evalScopeWithModulePath currentModulePath builtinMode bindingTypeHints env statements of
        Left err -> Left err
        Right Nothing ->
          Left
            (runtimeDiagnostic "E3006" "block expression has no terminal expression result at runtime")
        Right (Just value) -> Right value

forceQualifiedMethodValue :: BuiltinResolutionMode -> Map BindingRuntimeHintKey ConstraintSignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
forceQualifiedMethodValue builtinMode bindingTypeHints runtimeValue =
  case runtimeValue of
    VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs ->
      applyQualifiedMethod
        builtinMode
        bindingTypeHints
        methodKey
        classParameter
        methodSignature
        candidates
        capturedArgs
    _ ->
      Right runtimeValue

lookupOperatorBindingRuntimeValue ::
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey ConstraintSignatureType ->
  Text ->
  RuntimeEnv ->
  Either Diagnostic RuntimeValue
lookupOperatorBindingRuntimeValue builtinMode bindingTypeHints operatorSymbol env =
  case Map.lookup (operatorBindingIdentifierText operatorSymbol) env of
    Just value ->
      value >>= forceQualifiedMethodValue builtinMode bindingTypeHints
    Nothing ->
      Left
        ( runtimeDiagnostic
            "E3027"
            ("operator '" <> operatorSymbol <> "' has no executable binding")
        )

declaredOperatorRightSectionClosure :: Maybe [Text] -> RuntimeValue -> RuntimeValue -> RuntimeEnv -> RuntimeValue
declaredOperatorRightSectionClosure currentModulePath operatorValue rightValue env =
  VClosure
    capturedEnv
    leftParameter
    (EApply (EApply (EVar functionName) (EVar leftParameter)) (EVar rightParameter))
    Nothing
    currentModulePath
  where
    functionName = mkIdentifier "$operator_section_function"
    leftParameter = mkIdentifier "$operator_section_left"
    rightParameter = mkIdentifier "$operator_section_right"
    capturedEnv =
      Map.insert (identifierText functionName) (Right operatorValue) $
        Map.insert (identifierText rightParameter) (Right rightValue) env

literalRuntimeValue :: Literal -> RuntimeValue
literalRuntimeValue literal =
  case literal of
    LInt value -> VInt value untypedIntMetadata
    LFloat value literalSource maybeTargetType ->
      case maybeTargetType of
        Just targetType ->
          VFloat
            (roundFloatTarget targetType value)
            (targetedFloatMetadataWithSource targetType (Just literalSource))
        Nothing ->
          VFloat value (untypedFloatMetadata (Just literalSource))
    LBool value -> VBool value

attachRuntimeTypeHint :: Maybe ConstraintSignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
attachRuntimeTypeHint maybeTypeHint runtimeValue =
  case maybeTypeHint of
    Just typeHint ->
      applyRuntimeTypeHint typeHint runtimeValue
    Nothing ->
      Right runtimeValue

applyRuntimeTypeHint :: ConstraintSignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeTypeHint typeHint runtimeValue =
  case runtimeValue of
    VTyped _ innerValue ->
      applyRuntimeTypeHint typeHint innerValue
    _ ->
      case (typeHint, runtimeValue) of
        (ConstraintTypeName typeName, _)
          | Just targetType <- constraintTypeNameNumericTarget typeName -> do
              convertedValue <- evalNumericConversion (numericConversionBuiltinForTarget targetType) targetType runtimeValue
              if identifierText typeName == "Int" || identifierText typeName == "Float"
                then Right (VTyped typeHint convertedValue)
                else Right convertedValue
        (ConstraintTypeName hintedTypeName, VConstructor typeName typeParameters constructorName constructorArguments capturedArgs)
          | identifierText hintedTypeName == identifierText typeName,
            constructorIsSaturated constructorArguments capturedArgs -> do
              hintedCapturedArgs <-
                zipWithM
                  (applyConstructorArgumentRuntimeHint Map.empty)
                  constructorArguments
                  capturedArgs
              Right (VConstructor typeName typeParameters constructorName constructorArguments hintedCapturedArgs)
        (ConstraintTypeList elementType, VList elements _) -> do
          hintedElements <- mapM (applyRuntimeTypeHint elementType) elements
          Right (VList hintedElements (Just typeHint))
        (ConstraintTypeTuple elementTypes, VTuple elements)
          | length elementTypes == length elements ->
              VTuple <$> zipWithM applyRuntimeTypeHint elementTypes elements
        (ConstraintTypeFunction {}, VClosure capturedEnv parameterName bodyExpr _ closureModulePath) ->
          Right (VClosure capturedEnv parameterName bodyExpr (Just typeHint) closureModulePath)
        (ConstraintTypeFunction {}, _)
          | isFunctionValue runtimeValue ->
              Right (VTyped typeHint runtimeValue)
        (ConstraintTypeApplication hintedTypeName hintedArguments, VConstructor typeName typeParameters constructorName constructorArguments capturedArgs)
          | identifierText hintedTypeName == identifierText typeName,
            length hintedArguments == length typeParameters -> do
              let typeParameterHints =
                    Map.fromList (zip (map identifierText typeParameters) hintedArguments)
              hintedCapturedArgs <-
                zipWithM
                  (applyConstructorArgumentRuntimeHint typeParameterHints)
                  constructorArguments
                  capturedArgs
              Right (VTyped typeHint (VConstructor typeName typeParameters constructorName constructorArguments hintedCapturedArgs))
        _ ->
          Right runtimeValue

applyConstructorArgumentRuntimeHint ::
  Map Text ConstraintSignatureType ->
  DataConstructorArgument ->
  RuntimeValue ->
  Either Diagnostic RuntimeValue
applyConstructorArgumentRuntimeHint typeParameterHints constructorArgument runtimeValue =
  case constructorArgument of
    DataConstructorArgumentName argumentName ->
      attachRuntimeTypeHint (constructorArgumentRuntimeHint typeParameterHints argumentName) runtimeValue
    DataConstructorArgumentOpaque ->
      Right runtimeValue

constructorArgumentRuntimeHint :: Map Text ConstraintSignatureType -> Identifier -> Maybe ConstraintSignatureType
constructorArgumentRuntimeHint typeParameterHints argumentName =
  case Map.lookup (identifierText argumentName) typeParameterHints of
    Just hintedType -> Just hintedType
    Nothing -> concreteConstructorPayloadRuntimeHint argumentName

concreteConstructorPayloadRuntimeHint :: Identifier -> Maybe ConstraintSignatureType
concreteConstructorPayloadRuntimeHint argumentName
  | Just _ <- constraintTypeNameNumericTarget argumentName =
      Just (ConstraintTypeName argumentName)
  | identifierText argumentName == "Bool" =
      Just (ConstraintTypeName argumentName)
  | otherwise =
      Nothing

constraintTypeNameNumericTarget :: Identifier -> Maybe NumericType
constraintTypeNameNumericTarget typeName =
  case identifierText typeName of
    "Int" -> Just NumericInt64
    "Int8" -> Just NumericInt8
    "Int16" -> Just NumericInt16
    "Int32" -> Just NumericInt32
    "Int64" -> Just NumericInt64
    "UInt8" -> Just NumericUInt8
    "UInt16" -> Just NumericUInt16
    "UInt32" -> Just NumericUInt32
    "UInt64" -> Just NumericUInt64
    "Float" -> Just NumericFloat64
    "Float16" -> Just NumericFloat16
    "Float32" -> Just NumericFloat32
    "Float64" -> Just NumericFloat64
    _ -> Nothing

untypedIntMetadata :: RuntimeIntMetadata
untypedIntMetadata =
  RuntimeIntMetadata {runtimeIntTargetType = Nothing}

targetedIntMetadata :: NumericType -> RuntimeIntMetadata
targetedIntMetadata targetType =
  RuntimeIntMetadata {runtimeIntTargetType = Just targetType}

untypedFloatMetadata :: Maybe FractionalLiteralSource -> RuntimeFloatMetadata
untypedFloatMetadata literalSource =
  RuntimeFloatMetadata
    { runtimeFloatLiteralSource = literalSource,
      runtimeFloatTargetType = Nothing
    }

targetedFloatMetadata :: NumericType -> RuntimeFloatMetadata
targetedFloatMetadata targetType =
  targetedFloatMetadataWithSource targetType Nothing

targetedFloatMetadataWithSource :: NumericType -> Maybe FractionalLiteralSource -> RuntimeFloatMetadata
targetedFloatMetadataWithSource targetType literalSource =
  RuntimeFloatMetadata
    { runtimeFloatLiteralSource =
        case targetType of
          NumericFloat64 -> literalSource
          _ -> Nothing,
      runtimeFloatTargetType = Just targetType
    }

evalPatternCase ::
  Maybe [Text] ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey ConstraintSignatureType ->
  RuntimeEnv ->
  RuntimeValue ->
  [CaseArm] ->
  Either Diagnostic RuntimeValue
evalPatternCase currentModulePath builtinMode bindingTypeHints env scrutineeValue caseArms = do
  selectedArm <- selectMatchingCaseArm currentModulePath builtinMode bindingTypeHints env scrutineeValue caseArms
  case selectedArm of
    Just (armEnv, bodyExpr) ->
      evalValueWithModulePath currentModulePath builtinMode bindingTypeHints armEnv bodyExpr
    Nothing ->
      Left
        ( runtimeDiagnostic
            "E3022"
            "pattern case matched no arms"
        )

selectMatchingCaseArm ::
  Maybe [Text] ->
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey ConstraintSignatureType ->
  RuntimeEnv ->
  RuntimeValue ->
  [CaseArm] ->
  Either Diagnostic (Maybe (RuntimeEnv, Expr))
selectMatchingCaseArm currentModulePath builtinMode bindingTypeHints env scrutineeValue =
  go
  where
    go remainingArms =
      case remainingArms of
        [] -> Right Nothing
        caseArm : rest ->
          chooseArm caseArm rest

    chooseArm caseArm rest =
      case matchCaseArm env scrutineeValue caseArm of
        Just (armEnv, guardExpr, bodyExpr) ->
          case guardExpr of
            Nothing ->
              Right (Just (armEnv, bodyExpr))
            Just conditionExpr -> do
              guardValue <- evalValueWithModulePath currentModulePath builtinMode bindingTypeHints armEnv conditionExpr
              case guardValue of
                VBool True ->
                  Right (Just (armEnv, bodyExpr))
                VBool False ->
                  go rest
                other ->
                  Left
                    ( runtimeDiagnostic
                        "E3003"
                        ("runtime case guard must be Bool, found " <> renderRuntimeType other)
                    )
        Nothing ->
          go rest

-- | Pattern bindings are prepended to the arm environment so they shadow outer
-- runtime bindings only while evaluating the selected arm body.
matchCaseArm ::
  RuntimeEnv ->
  RuntimeValue ->
  CaseArm ->
  Maybe (RuntimeEnv, Maybe Expr, Expr)
matchCaseArm env scrutineeValue (CaseArm pattern guardExpr bodyExpr) =
  case matchPattern scrutineeValue pattern of
    Just patternBindings ->
      Just (Map.union patternBindings env, guardExpr, bodyExpr)
    Nothing -> Nothing

matchPattern :: RuntimeValue -> Pattern -> Maybe RuntimeEnv
matchPattern scrutineeValue pattern =
  case pattern of
    PWildcard -> Just Map.empty
    PVariable name ->
      Just
        (Map.singleton (identifierText name) (Right scrutineeValue))
    PLiteral literal
      | scrutineeValue == literalRuntimeValue literal ->
          Just Map.empty
      | otherwise ->
          Nothing
    PConstructor constructorName patterns ->
      case constructorPatternScrutinee scrutineeValue of
        VConstructor _ _ valueConstructorName constructorArguments capturedArgs
          | valueConstructorName == constructorName,
            constructorIsSaturated constructorArguments capturedArgs,
            length capturedArgs == length patterns ->
              matchPatternList capturedArgs patterns
        _ -> Nothing
    PList patterns ->
      case scrutineeValue of
        VList elements _
          | length elements == length patterns ->
              matchPatternList elements patterns
        _ -> Nothing
    PConsList headPattern tailPattern ->
      case scrutineeValue of
        VList (headValue : tailValues) maybeTypeHint -> do
          headBindings <- matchPattern headValue headPattern
          tailBindings <- matchPattern (VList tailValues maybeTypeHint) tailPattern
          Just (tailBindings `Map.union` headBindings)
        _ -> Nothing
    PTuple patterns ->
      case scrutineeValue of
        VTuple elements
          | length elements == length patterns ->
              matchPatternList elements patterns
        _ -> Nothing
    PAs name pattern -> do
      patternBindings <- matchPattern scrutineeValue pattern
      Just (Map.insert (identifierText name) (Right scrutineeValue) patternBindings)
    POr alternatives ->
      matchFirstAlternative scrutineeValue alternatives

matchFirstAlternative :: RuntimeValue -> [Pattern] -> Maybe RuntimeEnv
matchFirstAlternative scrutineeValue alternatives =
  case alternatives of
    [] -> Nothing
    alternative : rest ->
      case matchPattern scrutineeValue alternative of
        Just patternBindings -> Just patternBindings
        Nothing -> matchFirstAlternative scrutineeValue rest

matchPatternList :: [RuntimeValue] -> [Pattern] -> Maybe RuntimeEnv
matchPatternList values patterns =
  foldM step Map.empty (zip values patterns)
  where
    step bindings (value, pattern) =
      case matchPattern value pattern of
        Just patternBindings -> Just (patternBindings `Map.union` bindings)
        Nothing -> Nothing

constructorPatternScrutinee :: RuntimeValue -> RuntimeValue
constructorPatternScrutinee runtimeValue =
  case runtimeValue of
    VTyped _ innerValue -> constructorPatternScrutinee innerValue
    _ -> runtimeValue

-- | Apply any callable runtime value, including sections, builtin primitives,
-- and curried operator values.
applyRuntimeFunction ::
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey ConstraintSignatureType ->
  RuntimeValue ->
  RuntimeValue ->
  Either Diagnostic RuntimeValue
applyRuntimeFunction builtinMode bindingTypeHints functionValue argumentValue =
  case functionValue of
    VTyped typeHint innerFunctionValue -> do
      hintedArgumentValue <- applyRuntimeFunctionArgumentHint typeHint argumentValue
      resultValue <- applyRuntimeFunction builtinMode bindingTypeHints innerFunctionValue hintedArgumentValue
      applyRuntimeFunctionResultHint typeHint resultValue
    VSectionLeft operatorSymbol leftValue ->
      evalBinary builtinMode bindingTypeHints operatorSymbol leftValue argumentValue
    VSectionRight operatorSymbol rightValue ->
      evalBinary builtinMode bindingTypeHints operatorSymbol argumentValue rightValue
    VClosure capturedEnv parameterName bodyExpr maybeTypeHint closureModulePath -> do
      hintedArgumentValue <-
        case maybeTypeHint of
          Just typeHint -> applyRuntimeFunctionArgumentHint typeHint argumentValue
          Nothing -> Right argumentValue
      resultValue <-
        evalValueWithModulePath
          closureModulePath
          builtinMode
          bindingTypeHints
          (Map.insert (identifierText parameterName) (Right hintedArgumentValue) capturedEnv)
          bodyExpr
      case maybeTypeHint of
        Just typeHint -> applyRuntimeFunctionResultHint typeHint resultValue
        Nothing -> attachDefaultBindingIntegerTarget resultValue
    VBuiltin builtinFunction capturedArgs ->
      applyBuiltin builtinMode bindingTypeHints builtinFunction (capturedArgs ++ [argumentValue])
    VOperator operatorSymbol capturedArgs ->
      applyOperator builtinMode bindingTypeHints operatorSymbol (capturedArgs ++ [argumentValue])
    VConstructor typeName typeParameters constructorName constructorArguments capturedArgs ->
      applyConstructor typeName typeParameters constructorName constructorArguments (capturedArgs ++ [argumentValue])
    VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs ->
      applyQualifiedMethod
        builtinMode
        bindingTypeHints
        methodKey
        classParameter
        methodSignature
        candidates
        (capturedArgs ++ [argumentValue])
    _ ->
      Left
        ( runtimeDiagnostic
            "E3008"
            ("runtime cannot apply non-function value of type " <> renderRuntimeType functionValue)
        )

applyRuntimeFunctionResultHint :: ConstraintSignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeFunctionResultHint typeHint runtimeValue =
  case typeHint of
    ConstraintTypeFunction _ resultType ->
      applyRuntimeTypeHint resultType runtimeValue
    _ ->
      Right runtimeValue

applyRuntimeFunctionArgumentHint :: ConstraintSignatureType -> RuntimeValue -> Either Diagnostic RuntimeValue
applyRuntimeFunctionArgumentHint typeHint runtimeValue =
  case typeHint of
    ConstraintTypeFunction argumentType _ ->
      applyRuntimeTypeHint argumentType runtimeValue
    _ ->
      Right runtimeValue

applyQualifiedMethod ::
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey ConstraintSignatureType ->
  Text ->
  Text ->
  SignaturePayload ->
  [RuntimeMethodCandidate] ->
  [RuntimeValue] ->
  Either Diagnostic RuntimeValue
applyQualifiedMethod builtinMode bindingTypeHints methodKey classParameter methodSignature candidates arguments =
  case preferredCandidates of
    [] ->
      Left (runtimeDiagnostic "E3026" ("no matching qualified method body '" <> methodKey <> "'"))
    [RuntimeMethodCandidate _ methodCell] ->
      applyRuntimeMethodCandidate builtinMode bindingTypeHints methodCell arguments
    _
      | runtimeQualifiedMethodIsFullyApplied classParameter methodSignature arguments preferredCandidates ->
          Left (runtimeDiagnostic "E3026" ("ambiguous qualified method body '" <> methodKey <> "'"))
      | otherwise ->
          Right (VQualifiedMethod methodKey classParameter methodSignature preferredCandidates arguments)
  where
    preferredCandidates =
      case exactMatchingCandidates of
        [] -> matchingCandidates
        exactMatches -> exactMatches

    exactMatchingCandidates =
      filter
        (runtimeMethodCandidateExactlyMatches classParameter methodSignature arguments)
        matchingCandidates

    matchingCandidates =
      filter
        (runtimeMethodCandidateMatches classParameter methodSignature arguments)
        candidates

runtimeQualifiedMethodIsFullyApplied ::
  Text ->
  SignaturePayload ->
  [RuntimeValue] ->
  [RuntimeMethodCandidate] ->
  Bool
runtimeQualifiedMethodIsFullyApplied classParameter methodSignature arguments candidates =
  any candidateIsFullyApplied candidates
  where
    candidateIsFullyApplied (RuntimeMethodCandidate implTarget _) =
      case substituteClassMethodSignature classParameter implTarget methodSignature of
        Just substitutedSignature ->
          let (argumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
           in length arguments >= length argumentTypes
        Nothing ->
          False

applyRuntimeMethodCandidate ::
  BuiltinResolutionMode ->
  Map BindingRuntimeHintKey ConstraintSignatureType ->
  Either Diagnostic RuntimeValue ->
  [RuntimeValue] ->
  Either Diagnostic RuntimeValue
applyRuntimeMethodCandidate builtinMode bindingTypeHints methodCell arguments = do
  methodValue <- methodCell
  foldM (applyRuntimeFunction builtinMode bindingTypeHints) methodValue arguments

runtimeMethodCandidateExactlyMatches :: Text -> SignaturePayload -> [RuntimeValue] -> RuntimeMethodCandidate -> Bool
runtimeMethodCandidateExactlyMatches classParameter methodSignature arguments (RuntimeMethodCandidate implTarget _) =
  case (signaturePayloadConstraintType methodSignature, substituteClassMethodSignature classParameter implTarget methodSignature) of
    (Just genericSignature, Just substitutedSignature) ->
      let (genericArgumentTypes, _) = constraintFunctionArgumentTypes genericSignature
          (argumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
          suppliedArgumentCount = length arguments
          suppliedGenericArgumentTypes = take suppliedArgumentCount genericArgumentTypes
          suppliedArgumentTypes = take suppliedArgumentCount argumentTypes
          targetArgumentPositions =
            map (runtimeConstraintSignatureTypeContainsClassParameter classParameter) suppliedGenericArgumentTypes
       in suppliedArgumentCount <= length genericArgumentTypes
            && suppliedArgumentCount <= length argumentTypes
            && or targetArgumentPositions
            && and
              ( zipWith3
                  runtimeExactCandidateArgumentMatches
                  targetArgumentPositions
                  suppliedArgumentTypes
                  arguments
              )
    _ ->
      False

runtimeExactCandidateArgumentMatches :: Bool -> ConstraintSignatureType -> RuntimeValue -> Bool
runtimeExactCandidateArgumentMatches targetArgumentPosition signatureType runtimeValue =
  not targetArgumentPosition || runtimeValueExactlyMatchesConstraint signatureType runtimeValue

runtimeConstraintSignatureTypeContainsClassParameter :: Text -> ConstraintSignatureType -> Bool
runtimeConstraintSignatureTypeContainsClassParameter classParameter signatureType =
  case signatureType of
      ConstraintTypeApplication _ arguments ->
        any (runtimeConstraintSignatureTypeContainsClassParameter classParameter) arguments
      ConstraintTypeList innerType ->
        runtimeConstraintSignatureTypeContainsClassParameter classParameter innerType
      ConstraintTypeTuple elementTypes ->
        any (runtimeConstraintSignatureTypeContainsClassParameter classParameter) elementTypes
      ConstraintTypeFunction argumentType resultType ->
        runtimeConstraintSignatureTypeContainsClassParameter classParameter argumentType
          || runtimeConstraintSignatureTypeContainsClassParameter classParameter resultType
      ConstraintTypeName typeName ->
        identifierText typeName == classParameter

runtimeValueExactlyMatchesConstraint :: ConstraintSignatureType -> RuntimeValue -> Bool
runtimeValueExactlyMatchesConstraint signatureType runtimeValue =
  case runtimeValue of
    VTyped typeHint _ ->
      typeHint == signatureType
    VClosure _ _ _ (Just typeHint) _ ->
      typeHint == signatureType
    VInt _ metadata ->
      case signatureType of
        ConstraintTypeName typeName ->
          runtimeIntExactlyMatchesTypeName (identifierText typeName) metadata
        _ -> False
    VFloat _ metadata ->
      case signatureType of
        ConstraintTypeName typeName ->
          runtimeFloatExactlyMatchesTypeName (identifierText typeName) metadata
        _ -> False
    VList _ (Just typeHint) ->
      typeHint == signatureType
    VList elements Nothing ->
      case signatureType of
        ConstraintTypeList elementType ->
          not (null elements)
            && all (runtimeValueExactlyMatchesConstraint elementType) elements
        _ -> False
    VTuple elements ->
      case signatureType of
        ConstraintTypeTuple elementTypes
          | length elementTypes == length elements ->
              and (zipWith runtimeValueExactlyMatchesConstraint elementTypes elements)
        _ -> False
    VConstructor {} ->
      case signatureType of
        ConstraintTypeName typeName ->
          runtimeValueExactlyMatchesDataTypeName typeName runtimeValue
        ConstraintTypeApplication typeName typeArguments ->
          runtimeValueExactlyMatchesDataTypeApplication typeName typeArguments runtimeValue
        _ -> False
    _ -> False

runtimeIntExactlyMatchesTypeName :: Text -> RuntimeIntMetadata -> Bool
runtimeIntExactlyMatchesTypeName typeName metadata =
  case (typeName, runtimeIntTargetType metadata) of
    ("Int", Nothing) -> True
    ("Int8", Just NumericInt8) -> True
    ("Int16", Just NumericInt16) -> True
    ("Int32", Just NumericInt32) -> True
    ("Int64", Just NumericInt64) -> True
    ("UInt8", Just NumericUInt8) -> True
    ("UInt16", Just NumericUInt16) -> True
    ("UInt32", Just NumericUInt32) -> True
    ("UInt64", Just NumericUInt64) -> True
    _ -> False

runtimeFloatExactlyMatchesTypeName :: Text -> RuntimeFloatMetadata -> Bool
runtimeFloatExactlyMatchesTypeName typeName metadata =
  case (typeName, runtimeFloatTargetType metadata) of
    ("Float", Nothing) -> True
    ("Float16", Just NumericFloat16) -> True
    ("Float32", Just NumericFloat32) -> True
    ("Float64", Just NumericFloat64) -> True
    _ -> False

runtimeMethodCandidateMatches :: Text -> SignaturePayload -> [RuntimeValue] -> RuntimeMethodCandidate -> Bool
runtimeMethodCandidateMatches classParameter methodSignature arguments (RuntimeMethodCandidate implTarget _) =
  case substituteClassMethodSignature classParameter implTarget methodSignature of
    Just substitutedSignature ->
      let (argumentTypes, _) = constraintFunctionArgumentTypes substitutedSignature
       in length arguments <= length argumentTypes
            && and (zipWith runtimeValueMatchesConstraint argumentTypes arguments)
    Nothing ->
      False

runtimeValueMatchesConstraint :: ConstraintSignatureType -> RuntimeValue -> Bool
runtimeValueMatchesConstraint signatureType runtimeValue =
  case runtimeValue of
    VTyped typeHint _ ->
      runtimeConstraintTypesCompatible typeHint signatureType
    _ ->
      case signatureType of
        ConstraintTypeName typeName ->
          runtimeValueMatchesTypeName (identifierText typeName) runtimeValue
        ConstraintTypeApplication typeName typeArguments ->
          runtimeValueMatchesDataTypeApplication typeName typeArguments runtimeValue
        ConstraintTypeList elementType ->
          case runtimeValue of
            VList elements maybeTypeHint ->
              case maybeTypeHint of
                Just typeHint -> runtimeConstraintTypesCompatible typeHint signatureType
                Nothing -> all (runtimeValueMatchesConstraint elementType) elements
            _ -> False
        ConstraintTypeTuple elementTypes ->
          case runtimeValue of
            VTuple elements
              | length elementTypes == length elements ->
                  and (zipWith runtimeValueMatchesConstraint elementTypes elements)
            _ -> False
        ConstraintTypeFunction {} ->
          case runtimeValue of
            VClosure _ _ _ (Just typeHint) _ -> runtimeConstraintTypesCompatible typeHint signatureType
            _ -> isFunctionValue runtimeValue

runtimeConstraintTypesCompatible :: ConstraintSignatureType -> ConstraintSignatureType -> Bool
runtimeConstraintTypesCompatible leftType rightType =
  case (leftType, rightType) of
    (ConstraintTypeName leftName, ConstraintTypeName rightName) ->
      runtimeConstraintNamesCompatible leftName rightName
    (ConstraintTypeApplication leftName leftArguments, ConstraintTypeApplication rightName rightArguments)
      | runtimeConstraintNamesCompatible leftName rightName,
        length leftArguments == length rightArguments ->
          and (zipWith runtimeConstraintTypesCompatible leftArguments rightArguments)
    (ConstraintTypeList leftElementType, ConstraintTypeList rightElementType) ->
      runtimeConstraintTypesCompatible leftElementType rightElementType
    (ConstraintTypeTuple leftElementTypes, ConstraintTypeTuple rightElementTypes)
      | length leftElementTypes == length rightElementTypes ->
          and (zipWith runtimeConstraintTypesCompatible leftElementTypes rightElementTypes)
    (ConstraintTypeFunction leftArgumentType leftResultType, ConstraintTypeFunction rightArgumentType rightResultType) ->
      runtimeConstraintTypesCompatible leftArgumentType rightArgumentType
        && runtimeConstraintTypesCompatible leftResultType rightResultType
    _ -> False

runtimeConstraintNamesCompatible :: Identifier -> Identifier -> Bool
runtimeConstraintNamesCompatible leftName rightName =
  normalizeRuntimeConstraintName (identifierText leftName)
    == normalizeRuntimeConstraintName (identifierText rightName)

normalizeRuntimeConstraintName :: Text -> Text
normalizeRuntimeConstraintName typeName =
  case typeName of
    "Int" -> "Int64"
    "Float" -> "Float64"
    _ -> typeName

runtimeValueMatchesTypeName :: Text -> RuntimeValue -> Bool
runtimeValueMatchesTypeName typeName runtimeValue =
  case typeName of
    "Int" -> runtimeIntMatchesIntAlias runtimeValue
    "Int8" -> runtimeIntMatchesTarget NumericInt8 runtimeValue
    "Int16" -> runtimeIntMatchesTarget NumericInt16 runtimeValue
    "Int32" -> runtimeIntMatchesTarget NumericInt32 runtimeValue
    "Int64" -> runtimeIntMatchesTarget NumericInt64 runtimeValue
    "UInt8" -> runtimeIntMatchesTarget NumericUInt8 runtimeValue
    "UInt16" -> runtimeIntMatchesTarget NumericUInt16 runtimeValue
    "UInt32" -> runtimeIntMatchesTarget NumericUInt32 runtimeValue
    "UInt64" -> runtimeIntMatchesTarget NumericUInt64 runtimeValue
    "Float" -> runtimeFloatMatchesFloatAlias runtimeValue
    "Float16" -> runtimeFloatHasTarget NumericFloat16 runtimeValue
    "Float32" -> runtimeFloatHasTarget NumericFloat32 runtimeValue
    "Float64" -> runtimeFloatHasTarget NumericFloat64 runtimeValue
    "Bool" -> isRuntimeBool runtimeValue
    _ -> runtimeValueMatchesDataTypeName typeName runtimeValue

runtimeValueMatchesDataTypeName :: Text -> RuntimeValue -> Bool
runtimeValueMatchesDataTypeName typeName runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName _ _ constructorArguments capturedArgs ->
      identifierText valueTypeName == typeName
        && constructorIsSaturated constructorArguments capturedArgs
    _ -> False

runtimeValueMatchesDataTypeApplication :: Identifier -> [ConstraintSignatureType] -> RuntimeValue -> Bool
runtimeValueMatchesDataTypeApplication typeName typeArguments runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName typeParameters _ constructorArguments capturedArgs
      | valueTypeName == typeName,
        length typeParameters == length typeArguments,
        constructorIsSaturated constructorArguments capturedArgs ->
          let typeParameterBindings = Map.fromList (zip (map identifierText typeParameters) typeArguments)
           in and
                ( zipWith
                    (runtimeValueMatchesConstructorArgument typeParameterBindings)
                    constructorArguments
                    capturedArgs
                )
    _ -> False

runtimeValueExactlyMatchesDataTypeName :: Identifier -> RuntimeValue -> Bool
runtimeValueExactlyMatchesDataTypeName typeName runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName _ _ constructorArguments capturedArgs ->
      valueTypeName == typeName
        && constructorIsSaturated constructorArguments capturedArgs
    _ -> False

runtimeValueExactlyMatchesDataTypeApplication :: Identifier -> [ConstraintSignatureType] -> RuntimeValue -> Bool
runtimeValueExactlyMatchesDataTypeApplication typeName typeArguments runtimeValue =
  case runtimeValue of
    VConstructor valueTypeName typeParameters _ constructorArguments capturedArgs
      | valueTypeName == typeName,
        length typeParameters == length typeArguments,
        constructorIsSaturated constructorArguments capturedArgs ->
          let typeParameterBindings = Map.fromList (zip (map identifierText typeParameters) typeArguments)
           in and
                ( zipWith
                    (runtimeValueExactlyMatchesConstructorArgument typeParameterBindings)
                    constructorArguments
                    capturedArgs
                )
    _ -> False

runtimeValueMatchesConstructorArgument :: Map Text ConstraintSignatureType -> DataConstructorArgument -> RuntimeValue -> Bool
runtimeValueMatchesConstructorArgument typeParameterBindings constructorArgument runtimeValue =
  case constructorArgument of
    DataConstructorArgumentName argumentName ->
      case constructorArgumentRuntimeHint typeParameterBindings argumentName of
        Just concreteArgumentType ->
          runtimeValueMatchesConstraint concreteArgumentType runtimeValue
        Nothing ->
          True
    DataConstructorArgumentOpaque ->
      True

runtimeValueExactlyMatchesConstructorArgument :: Map Text ConstraintSignatureType -> DataConstructorArgument -> RuntimeValue -> Bool
runtimeValueExactlyMatchesConstructorArgument typeParameterBindings constructorArgument runtimeValue =
  case constructorArgument of
    DataConstructorArgumentName argumentName ->
      case constructorArgumentRuntimeHint typeParameterBindings argumentName of
        Just concreteArgumentType ->
          runtimeValueExactlyMatchesConstraint concreteArgumentType runtimeValue
        Nothing ->
          True
    DataConstructorArgumentOpaque ->
      True

runtimeIntMatchesIntAlias :: RuntimeValue -> Bool
runtimeIntMatchesIntAlias runtimeValue =
  case runtimeValue of
    VInt _ metadata ->
      case runtimeIntTargetType metadata of
        Just NumericInt64 -> True
        Just _ -> False
        Nothing -> True
    _ -> False

runtimeIntMatchesTarget :: NumericType -> RuntimeValue -> Bool
runtimeIntMatchesTarget targetType runtimeValue =
  case runtimeValue of
    VInt integerValue metadata ->
      case runtimeIntTargetType metadata of
        Just runtimeTarget -> runtimeTarget == targetType
        Nothing -> integerValueMatchesTarget targetType integerValue
    _ -> False

integerValueMatchesTarget :: NumericType -> Integer -> Bool
integerValueMatchesTarget targetType integerValue =
  case numericTypeIntegerBounds targetType of
    Just bounds -> integerValueWithinBounds integerValue bounds
    Nothing -> False

runtimeFloatMatchesFloatAlias :: RuntimeValue -> Bool
runtimeFloatMatchesFloatAlias runtimeValue =
  case runtimeValue of
    VFloat _ metadata ->
      case runtimeFloatTargetType metadata of
        Just NumericFloat64 -> True
        Just _ -> False
        Nothing -> True
    _ -> False

runtimeFloatHasTarget :: NumericType -> RuntimeValue -> Bool
runtimeFloatHasTarget targetType runtimeValue =
  case runtimeValue of
    VFloat _ metadata ->
      case runtimeFloatTargetType metadata of
        Just runtimeTarget -> runtimeTarget == targetType
        Nothing -> targetType == NumericFloat64
    _ -> False

isRuntimeBool :: RuntimeValue -> Bool
isRuntimeBool runtimeValue =
  case runtimeValue of
    VBool {} -> True
    _ -> False

applyOperator :: BuiltinResolutionMode -> Map BindingRuntimeHintKey ConstraintSignatureType -> Text -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyOperator builtinMode bindingTypeHints operatorSymbol arguments =
  case arguments of
    [leftValue] ->
      Right (VOperator operatorSymbol [leftValue])
    [leftValue, rightValue] ->
      evalBinary builtinMode bindingTypeHints operatorSymbol leftValue rightValue
    _ ->
      Left
        ( runtimeDiagnostic
            "E3016"
            ("runtime primitive '" <> operatorSymbol <> "' received invalid arguments")
        )

-- | Constructor values are curried like builtins until their declared arity is
-- saturated; extra applications are runtime errors.
applyConstructor :: Identifier -> [Identifier] -> Identifier -> [DataConstructorArgument] -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyConstructor typeName typeParameters constructorName constructorArguments arguments
  | length arguments <= constructorArity =
      Right (VConstructor typeName typeParameters constructorName constructorArguments arguments)
  | otherwise =
      Left
        ( runtimeDiagnostic
            "E3023"
            ( "runtime constructor '"
                <> identifierText constructorName
                <> "' expected "
                <> renderArityCount constructorArity
                <> " but received "
                <> renderArityCount (length arguments)
            )
        )
  where
    constructorArity = length constructorArguments

renderArityCount :: Int -> Text
renderArityCount count =
  Text.pack (show count) <> " " <> argumentWord
  where
    argumentWord =
      if count == 1
        then "argument"
        else "arguments"

-- | Builtin primitives are curried, so under-applied calls stay as function
-- values and only exact arity triggers evaluation.
applyBuiltin :: BuiltinResolutionMode -> Map BindingRuntimeHintKey ConstraintSignatureType -> BuiltinSymbol -> [RuntimeValue] -> Either Diagnostic RuntimeValue
applyBuiltin builtinMode bindingTypeHints builtinFunction arguments
  | length arguments < builtinSymbolArity builtinFunction =
      Right (VBuiltin builtinFunction arguments)
  | length arguments == builtinSymbolArity builtinFunction =
      evalBuiltin builtinMode bindingTypeHints builtinFunction arguments
  | otherwise =
      Left
        ( runtimeDiagnostic
            "E3014"
            ("runtime primitive '" <> builtinSymbolName builtinFunction <> "' received too many arguments")
        )

-- | Evaluate builtin semantics once enough arguments have been collected.
evalBuiltin :: BuiltinResolutionMode -> Map BindingRuntimeHintKey ConstraintSignatureType -> BuiltinSymbol -> [RuntimeValue] -> Either Diagnostic RuntimeValue
evalBuiltin builtinMode bindingTypeHints builtinFunction arguments =
  case (builtinFunction, arguments) of
    (_, [value])
      | Just targetType <- builtinSymbolNumericConversionTarget builtinFunction ->
          evalNumericConversion builtinFunction targetType value
    (BuiltinHd, [VList [] _]) ->
      Left (runtimeDiagnostic "E3009" "runtime primitive 'hd' failed: empty list")
    (BuiltinHd, [VList (headValue : _) maybeTypeHint]) ->
      case maybeTypeHint of
        Just (ConstraintTypeList elementType) ->
          applyRuntimeTypeHint elementType headValue
        _ ->
          Right headValue
    (BuiltinHd, [other]) ->
      Left
        ( runtimeDiagnostic
            "E3011"
            ("runtime primitive 'hd' expects a list argument, found " <> renderRuntimeType other)
        )
    (BuiltinTl, [VList [] _]) ->
      Left (runtimeDiagnostic "E3010" "runtime primitive 'tl' failed: empty list")
    (BuiltinTl, [VList (_ : tailValues) maybeTypeHint]) ->
      Right (VList tailValues maybeTypeHint)
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
            VList elements maybeCollectionTypeHint -> do
              mappedElements <- mapM (applyRuntimeFunction builtinMode bindingTypeHints mapper) elements
              let maybeMappedTypeHint = ConstraintTypeList <$> runtimeMapResultElementType mapper maybeCollectionTypeHint
              Right (VList mappedElements maybeMappedTypeHint)
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
            VList elements maybeTypeHint ->
              (`VList` maybeTypeHint) <$> filterElements builtinMode bindingTypeHints predicate elements
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

evalNumericConversion :: BuiltinSymbol -> NumericType -> RuntimeValue -> Either Diagnostic RuntimeValue
evalNumericConversion builtinFunction targetType value =
  case value of
    VTyped _ innerValue ->
      evalNumericConversion builtinFunction targetType innerValue
    VInt integerValue _ ->
      convertIntegerToNumericTarget builtinFunction targetType integerValue
    VFloat floatValue floatMetadata ->
      convertFloatToNumericTarget builtinFunction targetType floatValue (runtimeFloatLiteralSource floatMetadata)
    other ->
      Left
        ( runtimeDiagnostic
            "E3024"
            ( "runtime numeric conversion '"
                <> builtinSymbolName builtinFunction
                <> "' expects a numeric value, found "
                <> renderRuntimeType other
            )
        )

numericConversionBuiltinForTarget :: NumericType -> BuiltinSymbol
numericConversionBuiltinForTarget targetType =
  case targetType of
    NumericInt8 -> BuiltinToInt8
    NumericInt16 -> BuiltinToInt16
    NumericInt32 -> BuiltinToInt32
    NumericInt64 -> BuiltinToInt64
    NumericUInt8 -> BuiltinToUInt8
    NumericUInt16 -> BuiltinToUInt16
    NumericUInt32 -> BuiltinToUInt32
    NumericUInt64 -> BuiltinToUInt64
    NumericFloat16 -> BuiltinToFloat16
    NumericFloat32 -> BuiltinToFloat32
    NumericFloat64 -> BuiltinToFloat64

convertIntegerToNumericTarget :: BuiltinSymbol -> NumericType -> Integer -> Either Diagnostic RuntimeValue
convertIntegerToNumericTarget builtinFunction targetType integerValue =
  case numericTypeIntegerBounds targetType of
    Just bounds ->
      if integerValueWithinBounds integerValue bounds
        then Right (VInt integerValue (targetedIntMetadata targetType))
        else Left (numericConversionRangeDiagnostic builtinFunction targetType integerValue bounds)
    Nothing ->
      convertIntegerToFloatTarget builtinFunction targetType integerValue

convertFloatToNumericTarget :: BuiltinSymbol -> NumericType -> Double -> Maybe FractionalLiteralSource -> Either Diagnostic RuntimeValue
convertFloatToNumericTarget builtinFunction targetType floatValue literalSource
  | isNaN floatValue || isInfinite floatValue =
      Left
        ( runtimeDiagnostic
            "E3024"
            ( "runtime numeric conversion '"
                <> builtinSymbolName builtinFunction
                <> "' cannot convert non-finite Float value"
            )
        )
  | otherwise =
      case numericTypeIntegerBounds targetType of
        Just bounds ->
          convertFloatToIntegerTarget builtinFunction targetType floatValue literalSource bounds
        Nothing ->
          convertFiniteFloatToFloatTarget builtinFunction targetType floatValue literalSource

convertFloatToIntegerTarget ::
  BuiltinSymbol ->
  NumericType ->
  Double ->
  Maybe FractionalLiteralSource ->
  (Integer, Integer) ->
  Either Diagnostic RuntimeValue
convertFloatToIntegerTarget builtinFunction targetType floatValue literalSource bounds =
  case literalSource of
    Just source ->
      case fractionalLiteralIntegralValue source of
        Just integralValue
          | integerValueWithinBounds integralValue bounds ->
              Right (VInt integralValue (targetedIntMetadata targetType))
        _ ->
          Left (numericConversionFloatToIntegralDiagnostic builtinFunction targetType floatValue bounds)
    Nothing ->
      -- `round` is half-to-even, but the equality check below rejects every
      -- non-integral value instead of observing a rounding mode.
      let roundedInteger = round floatValue :: Integer
       in
        if fromInteger roundedInteger == floatValue && integerValueWithinBounds roundedInteger bounds
          then Right (VInt roundedInteger (targetedIntMetadata targetType))
          else Left (numericConversionFloatToIntegralDiagnostic builtinFunction targetType floatValue bounds)

convertIntegerToFloatTarget :: BuiltinSymbol -> NumericType -> Integer -> Either Diagnostic RuntimeValue
convertIntegerToFloatTarget builtinFunction targetType integerValue =
  if integerExceedsFloatTarget targetType integerValue
    then Left (numericConversionFloatOverflowDiagnostic builtinFunction targetType)
    else
      let floatValue = fromInteger integerValue :: Double
       in
        if isInfinite floatValue || exceedsFloatTarget targetType floatValue
          then Left (numericConversionFloatOverflowDiagnostic builtinFunction targetType)
          else Right (VFloat (roundFloatTarget targetType floatValue) (targetedFloatMetadata targetType))

integerExceedsFloatTarget :: NumericType -> Integer -> Bool
integerExceedsFloatTarget targetType integerValue =
  case numericTypeFloatMax targetType of
    Just maxMagnitude ->
      abs integerValue > (floor maxMagnitude :: Integer)
    Nothing -> False

convertFiniteFloatToFloatTarget :: BuiltinSymbol -> NumericType -> Double -> Maybe FractionalLiteralSource -> Either Diagnostic RuntimeValue
convertFiniteFloatToFloatTarget builtinFunction targetType floatValue literalSource =
  if exceedsFloatTarget targetType floatValue || sourceExceedsFloatTarget targetType literalSource
    then Left (numericConversionFloatOverflowDiagnostic builtinFunction targetType)
    else Right (VFloat (roundFloatTarget targetType floatValue) (targetedFloatMetadataWithSource targetType literalSource))

roundFloatTarget :: NumericType -> Double -> Double
roundFloatTarget targetType value =
  case targetType of
    NumericFloat16 -> roundFloat16 value
    NumericFloat32 -> realToFrac (realToFrac value :: Float)
    _ -> value

roundFloat16 :: Double -> Double
roundFloat16 value
  | value == 0 = 0
  | magnitude < (halfMinSubnormal / 2.0) = 0
  | magnitude < halfMinNormal =
      withSign (fromInteger (round (magnitude / halfMinSubnormal) :: Integer) * halfMinSubnormal)
  | otherwise =
      let exponentValue = floor (logBase 2 magnitude) :: Int
          unit = 2.0 ** fromIntegral (exponentValue - 10)
          roundedMagnitude = fromInteger (round (magnitude / unit) :: Integer) * unit
       in withSign (min float16MaxFinite roundedMagnitude)
  where
    magnitude = abs value
    float16MaxFinite = 65504.0 :: Double
    halfMinNormal = 2.0 ** (-14.0 :: Double)
    halfMinSubnormal = 2.0 ** (-24.0 :: Double)
    withSign roundedMagnitude =
      if value < 0
        then negate roundedMagnitude
        else roundedMagnitude

exceedsFloatTarget :: NumericType -> Double -> Bool
exceedsFloatTarget targetType value =
  case numericTypeFloatMax targetType of
    Just maxMagnitude -> abs value > maxMagnitude
    Nothing -> False

sourceExceedsFloatTarget :: NumericType -> Maybe FractionalLiteralSource -> Bool
sourceExceedsFloatTarget targetType literalSource =
  case (numericTypeFloatMax targetType, literalSource) of
    (Just maxMagnitude, Just source) ->
      fractionalLiteralExceedsMagnitude source maxMagnitude
    _ -> False

integerValueWithinBounds :: Integer -> (Integer, Integer) -> Bool
integerValueWithinBounds value (lowerBound, upperBound) =
  value >= lowerBound && value <= upperBound

numericConversionRangeDiagnostic :: BuiltinSymbol -> NumericType -> Integer -> (Integer, Integer) -> Diagnostic
numericConversionRangeDiagnostic builtinFunction targetType value (lowerBound, upperBound) =
  runtimeDiagnostic
    "E3024"
    ( "runtime numeric conversion '"
        <> builtinSymbolName builtinFunction
        <> "' failed: integer value "
        <> Text.pack (show value)
        <> " outside "
        <> renderNumericTypeName targetType
        <> " range "
        <> Text.pack (show lowerBound)
        <> ".."
        <> Text.pack (show upperBound)
    )

numericConversionFloatToIntegralDiagnostic :: BuiltinSymbol -> NumericType -> Double -> (Integer, Integer) -> Diagnostic
numericConversionFloatToIntegralDiagnostic builtinFunction targetType value (lowerBound, upperBound) =
  runtimeDiagnostic
    "E3024"
    ( "runtime numeric conversion '"
        <> builtinSymbolName builtinFunction
        <> "' failed: Float value "
        <> Text.pack (show value)
        <> " must be integral and inside "
        <> renderNumericTypeName targetType
        <> " range "
        <> Text.pack (show lowerBound)
        <> ".."
        <> Text.pack (show upperBound)
    )

numericConversionFloatOverflowDiagnostic :: BuiltinSymbol -> NumericType -> Diagnostic
numericConversionFloatOverflowDiagnostic builtinFunction targetType =
  runtimeDiagnostic
    "E3024"
    ( "runtime numeric conversion '"
        <> builtinSymbolName builtinFunction
        <> "' failed: value cannot be represented as finite "
        <> renderNumericTypeName targetType
    )

renderNumericTypeName :: NumericType -> Text
renderNumericTypeName numericType =
  case numericType of
    NumericInt8 -> "Int8"
    NumericInt16 -> "Int16"
    NumericInt32 -> "Int32"
    NumericInt64 -> "Int64"
    NumericUInt8 -> "UInt8"
    NumericUInt16 -> "UInt16"
    NumericUInt32 -> "UInt32"
    NumericUInt64 -> "UInt64"
    NumericFloat16 -> "Float16"
    NumericFloat32 -> "Float32"
    NumericFloat64 -> "Float64"

-- | Evaluate filter predicates element-by-element and enforce that each
-- predicate application returns a Bool.
filterElements :: BuiltinResolutionMode -> Map BindingRuntimeHintKey ConstraintSignatureType -> RuntimeValue -> [RuntimeValue] -> Either Diagnostic [RuntimeValue]
filterElements builtinMode bindingTypeHints predicate values = do
  results <- mapM applyPredicate values
  pure [value | (value, True) <- results]
  where
    -- Preserve runtime safety for partially-known function values that can slip
    -- past compile-time checks in direct `evaluateRuntimeExpr` tests.
    applyPredicate :: RuntimeValue -> Either Diagnostic (RuntimeValue, Bool)
    applyPredicate value = do
      predicateResult <- applyRuntimeFunction builtinMode bindingTypeHints predicate value
      case predicateResult of
        VBool shouldKeep -> Right (value, shouldKeep)
        other ->
          Left
            ( runtimeDiagnostic
                "E3019"
                ("runtime primitive 'filter' predicate must return Bool, found " <> renderRuntimeType other)
            )

runtimeFunctionResultType :: RuntimeValue -> Maybe ConstraintSignatureType
runtimeFunctionResultType runtimeValue =
  case runtimeValue of
    VTyped (ConstraintTypeFunction _ resultType) _ ->
      Just resultType
    VClosure _ _ _ (Just (ConstraintTypeFunction _ resultType)) _ ->
      Just resultType
    _ ->
      Nothing

runtimeMapResultElementType :: RuntimeValue -> Maybe ConstraintSignatureType -> Maybe ConstraintSignatureType
runtimeMapResultElementType mapper maybeCollectionTypeHint =
  case runtimeFunctionResultType mapper of
    Just resultType ->
      Just resultType
    Nothing ->
      runtimeBuiltinMapResultElementType mapper maybeCollectionTypeHint

runtimeBuiltinMapResultElementType :: RuntimeValue -> Maybe ConstraintSignatureType -> Maybe ConstraintSignatureType
runtimeBuiltinMapResultElementType mapper maybeCollectionTypeHint =
  case (mapper, maybeCollectionTypeHint) of
    (VBuiltin BuiltinHd [], Just (ConstraintTypeList (ConstraintTypeList elementType))) ->
      Just elementType
    (VClosure _ parameterName (EVar resultName) Nothing _, Just (ConstraintTypeList elementType))
      | resultName == parameterName ->
          Just elementType
    _ ->
      Nothing

attachDefaultBindingIntegerTarget :: RuntimeValue -> Either Diagnostic RuntimeValue
attachDefaultBindingIntegerTarget runtimeValue =
  case runtimeValue of
    VInt integerValue metadata
      | runtimeIntTargetType metadata == Nothing,
        integerValueMatchesTarget NumericInt64 integerValue ->
          Right (VInt integerValue (targetedIntMetadata NumericInt64))
    VList elements maybeTypeHint ->
      (`VList` maybeTypeHint) <$> traverse attachDefaultBindingIntegerTarget elements
    VTuple elements ->
      VTuple <$> traverse attachDefaultBindingIntegerTarget elements
    VBuiltin builtinSymbol capturedArgs ->
      VBuiltin builtinSymbol <$> traverse attachDefaultBindingIntegerTarget capturedArgs
    VOperator operatorSymbol capturedArgs ->
      VOperator operatorSymbol <$> traverse attachDefaultBindingIntegerTarget capturedArgs
    VSectionLeft operatorSymbol operand ->
      VSectionLeft operatorSymbol <$> attachDefaultBindingIntegerTarget operand
    VSectionRight operatorSymbol operand ->
      VSectionRight operatorSymbol <$> attachDefaultBindingIntegerTarget operand
    VConstructor typeName typeParameters constructorName constructorArguments capturedArgs ->
      VConstructor typeName typeParameters constructorName constructorArguments
        <$> traverse attachDefaultBindingIntegerTarget capturedArgs
    VQualifiedMethod methodKey classParameter methodSignature candidates capturedArgs ->
      VQualifiedMethod methodKey classParameter methodSignature candidates
        <$> traverse attachDefaultBindingIntegerTarget capturedArgs
    VTyped typeHint innerValue
      | ConstraintTypeFunction {} <- typeHint ->
          Right (VTyped typeHint innerValue)
      | otherwise ->
          VTyped typeHint <$> attachDefaultBindingIntegerTarget innerValue
    _ ->
      Right runtimeValue

isFunctionValue :: RuntimeValue -> Bool
isFunctionValue value =
  case value of
    VTyped _ innerValue -> isFunctionValue innerValue
    VSectionLeft {} -> True
    VSectionRight {} -> True
    VClosure {} -> True
    VBuiltin {} -> True
    VOperator {} -> True
    VConstructor _ _ _ constructorArguments capturedArgs ->
      not (constructorIsSaturated constructorArguments capturedArgs)
    VQualifiedMethod {} -> True
    _ -> False

-- | Evaluate the builtin operator subset supported by the runtime.
evalBinary :: BuiltinResolutionMode -> Map BindingRuntimeHintKey ConstraintSignatureType -> Text -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
evalBinary builtinMode bindingTypeHints operatorSymbol leftValue rightValue
  | isStrictEqualityOperator operatorSymbol,
    isFunctionValue leftValue || isFunctionValue rightValue =
      Left (runtimeCallableEqualityDiagnostic operatorSymbol leftValue rightValue)
  | otherwise =
  case (operatorSymbol, leftValue, rightValue) of
    ("$", functionValue, argumentValue) ->
      applyRuntimeFunction builtinMode bindingTypeHints functionValue argumentValue
    (_, VTyped leftTypeHint leftInnerValue, _)
      | isStrictEqualityOperator operatorSymbol,
        runtimeTypeHintRequiresStructuralEquality leftTypeHint ->
          evalStructuralEquality operatorSymbol leftValue rightValue
      | otherwise ->
          evalBinary builtinMode bindingTypeHints operatorSymbol leftInnerValue rightValue
    (_, _, VTyped rightTypeHint rightInnerValue)
      | isStrictEqualityOperator operatorSymbol,
        runtimeTypeHintRequiresStructuralEquality rightTypeHint ->
          evalStructuralEquality operatorSymbol leftValue rightValue
      | otherwise ->
          evalBinary builtinMode bindingTypeHints operatorSymbol leftValue rightInnerValue
    ("+", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerArithmetic "+" leftMetadata rightMetadata (leftInt + rightInt)
    ("-", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerArithmetic "-" leftMetadata rightMetadata (leftInt - rightInt)
    ("*", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerArithmetic "*" leftMetadata rightMetadata (leftInt * rightInt)
    ("/", VInt _ _, VInt 0 _) ->
      Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
    ("/", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerArithmetic "/" leftMetadata rightMetadata (leftInt `div` rightInt)
    ("+", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatArithmetic "+" leftMetadata rightMetadata (leftFloat + rightFloat)
    ("-", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatArithmetic "-" leftMetadata rightMetadata (leftFloat - rightFloat)
    ("*", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatArithmetic "*" leftMetadata rightMetadata (leftFloat * rightFloat)
    ("/", VFloat _ _, VFloat rightFloat _)
      | floatIsZero rightFloat ->
          Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
    ("/", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatArithmetic "/" leftMetadata rightMetadata (leftFloat / rightFloat)
    ("+", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Arithmetic "+" rightMetadata leftInt rightFloat (+)
    ("+", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerArithmetic "+" leftMetadata leftFloat rightInt (+)
    ("-", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Arithmetic "-" rightMetadata leftInt rightFloat (-)
    ("-", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerArithmetic "-" leftMetadata leftFloat rightInt (-)
    ("*", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Arithmetic "*" rightMetadata leftInt rightFloat (*)
    ("*", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerArithmetic "*" leftMetadata leftFloat rightInt (*)
    ("/", VInt _ leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata,
        floatIsZero rightFloat ->
          Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
    ("/", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64PromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Arithmetic "/" rightMetadata leftInt rightFloat (/)
    ("/", VFloat _ leftMetadata, VInt 0 rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          Left (runtimeDiagnostic "E3001" "runtime primitive '/' failed: division by zero")
    ("/", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64PromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerArithmetic "/" leftMetadata leftFloat rightInt (/)
    ("<", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerPredicate "<" leftInt leftMetadata rightInt rightMetadata (leftInt < rightInt)
    ("<=", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerPredicate "<=" leftInt leftMetadata rightInt rightMetadata (leftInt <= rightInt)
    (">", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerPredicate ">" leftInt leftMetadata rightInt rightMetadata (leftInt > rightInt)
    (">=", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerPredicate ">=" leftInt leftMetadata rightInt rightMetadata (leftInt >= rightInt)
    ("<", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate "<" leftMetadata rightMetadata (leftFloat < rightFloat)
    ("<=", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate "<=" leftMetadata rightMetadata (leftFloat <= rightFloat)
    (">", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate ">" leftMetadata rightMetadata (leftFloat > rightFloat)
    (">=", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate ">=" leftMetadata rightMetadata (leftFloat >= rightFloat)
    ("<", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Predicate "<" leftInt rightFloat (<)
    ("<", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerPredicate "<" leftFloat rightInt (<)
    ("<=", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Predicate "<=" leftInt rightFloat (<=)
    ("<=", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerPredicate "<=" leftFloat rightInt (<=)
    (">", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Predicate ">" leftInt rightFloat (>)
    (">", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerPredicate ">" leftFloat rightInt (>)
    (">=", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Predicate ">=" leftInt rightFloat (>=)
    (">=", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerPredicate ">=" leftFloat rightInt (>=)
    ("==", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerEquality "==" leftInt leftMetadata rightInt rightMetadata
    ("==", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate "==" leftMetadata rightMetadata (leftFloat == rightFloat)
    ("==", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Equality "==" leftInt rightFloat
    ("==", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerEquality "==" leftFloat rightInt
    ("==", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool == rightBool))
    ("==", VList {}, VList {}) -> evalStructuralEquality "==" leftValue rightValue
    ("==", VTuple {}, VTuple {}) -> evalStructuralEquality "==" leftValue rightValue
    ("==", VConstructor {}, VConstructor {}) -> evalStructuralEquality "==" leftValue rightValue
    ("!=", VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      evalIntegerEquality "!=" leftInt leftMetadata rightInt rightMetadata
    ("!=", VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      evalFloatPredicate "!=" leftMetadata rightMetadata (leftFloat /= rightFloat)
    ("!=", VInt leftInt leftMetadata, VFloat rightFloat rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted leftMetadata rightMetadata ->
          evalIntegerFloat64Equality "!=" leftInt rightFloat
    ("!=", VFloat leftFloat leftMetadata, VInt rightInt rightMetadata)
      | runtimeIntFloat64ComparisonPromotionAccepted rightMetadata leftMetadata ->
          evalFloat64IntegerEquality "!=" leftFloat rightInt
    ("!=", VBool leftBool, VBool rightBool) -> Right (VBool (leftBool /= rightBool))
    ("!=", VList {}, VList {}) -> evalStructuralEquality "!=" leftValue rightValue
    ("!=", VTuple {}, VTuple {}) -> evalStructuralEquality "!=" leftValue rightValue
    ("!=", VConstructor {}, VConstructor {}) -> evalStructuralEquality "!=" leftValue rightValue
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

isStrictEqualityOperator :: Text -> Bool
isStrictEqualityOperator operatorSymbol =
  operatorSymbol == "==" || operatorSymbol == "!="

runtimeTypeHintRequiresStructuralEquality :: ConstraintSignatureType -> Bool
runtimeTypeHintRequiresStructuralEquality signatureType =
  case signatureType of
    ConstraintTypeApplication {} -> True
    ConstraintTypeList {} -> True
    ConstraintTypeTuple {} -> True
    _ -> False

runtimeCallableEqualityDiagnostic :: Text -> RuntimeValue -> RuntimeValue -> Diagnostic
runtimeCallableEqualityDiagnostic operatorSymbol leftValue rightValue =
  runtimeDiagnostic
    "E3007"
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' cannot compare callable values; callable values are not equality-supported, found "
        <> renderRuntimeType leftValue
        <> " and "
        <> renderRuntimeType rightValue
    )

evalIntegerArithmetic ::
  Text ->
  RuntimeIntMetadata ->
  RuntimeIntMetadata ->
  Integer ->
  Either Diagnostic RuntimeValue
evalIntegerArithmetic operatorSymbol leftMetadata rightMetadata result = do
  targetType <- selectIntegerBinaryTarget operatorSymbol leftMetadata rightMetadata
  evalIntegerBinary operatorSymbol targetType result

selectIntegerBinaryTarget :: Text -> RuntimeIntMetadata -> RuntimeIntMetadata -> Either Diagnostic (Maybe NumericType)
selectIntegerBinaryTarget operatorSymbol leftMetadata rightMetadata =
  case (runtimeIntTargetType leftMetadata, runtimeIntTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget)
      | leftTarget == rightTarget -> Right (Just leftTarget)
      | otherwise -> Left (mixedIntegerArithmeticDiagnostic operatorSymbol (Just leftTarget) (Just rightTarget))
    (Just leftTarget, Nothing) -> Right (Just leftTarget)
    (Nothing, Just rightTarget) -> Right (Just rightTarget)
    _ -> Right Nothing

evalIntegerBinary :: Text -> Maybe NumericType -> Integer -> Either Diagnostic RuntimeValue
evalIntegerBinary operatorSymbol maybeTarget result =
  case maybeTarget of
    Just targetType ->
      case numericTypeIntegerBounds targetType of
        Just bounds
          | integerValueWithinBounds result bounds ->
              Right (VInt result (targetedIntMetadata targetType))
          | otherwise ->
              Left (runtimeIntegerArithmeticOverflowDiagnostic operatorSymbol targetType result bounds)
        Nothing ->
          Right (VInt result (targetedIntMetadata targetType))
    Nothing ->
      Right (VInt result untypedIntMetadata)

mixedIntegerArithmeticDiagnostic :: Text -> Maybe NumericType -> Maybe NumericType -> Diagnostic
mixedIntegerArithmeticDiagnostic operatorSymbol leftTarget rightTarget =
  runtimeDiagnostic
    "E3007"
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' cannot mix "
        <> renderIntegerOperandTarget leftTarget
        <> " and "
        <> renderIntegerOperandTarget rightTarget
    )

renderIntegerOperandTarget :: Maybe NumericType -> Text
renderIntegerOperandTarget maybeTarget =
  case maybeTarget of
    Just targetType -> renderNumericTypeName targetType
    Nothing -> "Int"

runtimeIntegerArithmeticOverflowDiagnostic :: Text -> NumericType -> Integer -> (Integer, Integer) -> Diagnostic
runtimeIntegerArithmeticOverflowDiagnostic operatorSymbol targetType result (lowerBound, upperBound) =
  runtimeDiagnostic
    "E3025"
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' failed: integer value "
        <> Text.pack (show result)
        <> " outside "
        <> renderNumericTypeName targetType
        <> " range "
        <> Text.pack (show lowerBound)
        <> ".."
        <> Text.pack (show upperBound)
    )

floatIsZero :: Double -> Bool
floatIsZero value =
  -- Jazz's finite runtime primitive subset treats both signed zeroes as
  -- division by zero rather than producing infinities.
  value == 0

evalFloatArithmetic ::
  Text ->
  RuntimeFloatMetadata ->
  RuntimeFloatMetadata ->
  Double ->
  Either Diagnostic RuntimeValue
evalFloatArithmetic operatorSymbol leftMetadata rightMetadata result = do
  targetType <- selectFloatBinaryTarget operatorSymbol leftMetadata rightMetadata
  evalFloatBinary operatorSymbol targetType result

selectFloatBinaryTarget :: Text -> RuntimeFloatMetadata -> RuntimeFloatMetadata -> Either Diagnostic (Maybe NumericType)
selectFloatBinaryTarget operatorSymbol leftMetadata rightMetadata =
  case (runtimeFloatTargetType leftMetadata, runtimeFloatTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget)
      | leftTarget == rightTarget -> Right (Just leftTarget)
      | otherwise -> Left (mixedFloatArithmeticDiagnostic operatorSymbol (Just leftTarget) (Just rightTarget))
    (Just NumericFloat64, Nothing) -> Right (Just NumericFloat64)
    (Nothing, Just NumericFloat64) -> Right (Just NumericFloat64)
    (Just targetType, Nothing) -> Left (mixedFloatArithmeticDiagnostic operatorSymbol (Just targetType) Nothing)
    (Nothing, Just targetType) -> Left (mixedFloatArithmeticDiagnostic operatorSymbol Nothing (Just targetType))
    (Nothing, Nothing) -> Right Nothing

evalFloatBinary :: Text -> Maybe NumericType -> Double -> Either Diagnostic RuntimeValue
evalFloatBinary operatorSymbol targetType result
  | isNaN result || isInfinite result =
      Left
        ( runtimeDiagnostic
            "E3025"
            ("runtime primitive '" <> operatorSymbol <> "' failed: non-finite Float result")
        )
  | Just floatTarget <- targetType,
    exceedsFloatTarget floatTarget result =
      Left (runtimeFloatArithmeticOverflowDiagnostic operatorSymbol floatTarget)
  | Just floatTarget <- targetType =
      Right (VFloat (roundFloatTarget floatTarget result) (targetedFloatMetadata floatTarget))
  | otherwise = Right (VFloat result (untypedFloatMetadata Nothing))

runtimeIntFloat64PromotionAccepted :: RuntimeIntMetadata -> RuntimeFloatMetadata -> Bool
runtimeIntFloat64PromotionAccepted intMetadata floatMetadata =
  runtimeIntMetadataIsIntegral intMetadata
    && runtimeFloatMetadataIsFloat64Domain floatMetadata

runtimeIntFloat64ComparisonPromotionAccepted :: RuntimeIntMetadata -> RuntimeFloatMetadata -> Bool
runtimeIntFloat64ComparisonPromotionAccepted intMetadata floatMetadata =
  runtimeIntMetadataIsIntegral intMetadata
    && runtimeFloatMetadataIsFloat64Domain floatMetadata

runtimeIntMetadataIsIntegral :: RuntimeIntMetadata -> Bool
runtimeIntMetadataIsIntegral intMetadata =
  case runtimeIntTargetType intMetadata of
    Just numericType -> numericTypeIsIntegral numericType
    Nothing -> True

numericTypeIsIntegral :: NumericType -> Bool
numericTypeIsIntegral numericType =
  case numericType of
    NumericInt8 -> True
    NumericInt16 -> True
    NumericInt32 -> True
    NumericInt64 -> True
    NumericUInt8 -> True
    NumericUInt16 -> True
    NumericUInt32 -> True
    NumericUInt64 -> True
    NumericFloat16 -> False
    NumericFloat32 -> False
    NumericFloat64 -> False

runtimeFloatMetadataIsFloat64Domain :: RuntimeFloatMetadata -> Bool
runtimeFloatMetadataIsFloat64Domain floatMetadata =
  case runtimeFloatTargetType floatMetadata of
    Just NumericFloat64 -> True
    Nothing -> True
    Just _ -> False

evalIntegerFloat64Arithmetic :: Text -> RuntimeFloatMetadata -> Integer -> Double -> (Double -> Double -> Double) -> Either Diagnostic RuntimeValue
evalIntegerFloat64Arithmetic operatorSymbol floatMetadata integerValue floatValue combine = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  evalFloatBinary operatorSymbol (runtimeFloatTargetType floatMetadata) (combine integerFloat floatValue)

evalFloat64IntegerArithmetic :: Text -> RuntimeFloatMetadata -> Double -> Integer -> (Double -> Double -> Double) -> Either Diagnostic RuntimeValue
evalFloat64IntegerArithmetic operatorSymbol floatMetadata floatValue integerValue combine = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  evalFloatBinary operatorSymbol (runtimeFloatTargetType floatMetadata) (combine floatValue integerFloat)

evalIntegerFloat64Predicate :: Text -> Integer -> Double -> (Double -> Double -> Bool) -> Either Diagnostic RuntimeValue
evalIntegerFloat64Predicate _ integerValue floatValue predicate = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  pure (VBool (predicate integerFloat floatValue))

evalFloat64IntegerPredicate :: Text -> Double -> Integer -> (Double -> Double -> Bool) -> Either Diagnostic RuntimeValue
evalFloat64IntegerPredicate _ floatValue integerValue predicate = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  pure (VBool (predicate floatValue integerFloat))

evalIntegerFloat64Equality :: Text -> Integer -> Double -> Either Diagnostic RuntimeValue
evalIntegerFloat64Equality operatorSymbol integerValue floatValue = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  pure (VBool (float64MixedEqualityResult operatorSymbol integerFloat floatValue))

evalFloat64IntegerEquality :: Text -> Double -> Integer -> Either Diagnostic RuntimeValue
evalFloat64IntegerEquality operatorSymbol floatValue integerValue = do
  integerFloat <- promotedIntegerFloat64Operand integerValue
  pure (VBool (float64MixedEqualityResult operatorSymbol floatValue integerFloat))

float64MixedEqualityResult :: Text -> Double -> Double -> Bool
float64MixedEqualityResult operatorSymbol leftValue rightValue =
  if operatorSymbol == "!="
    then leftValue /= rightValue
    else leftValue == rightValue

promotedIntegerFloat64Operand :: Integer -> Either Diagnostic Double
promotedIntegerFloat64Operand integerValue =
  case convertIntegerToFloatTarget BuiltinToFloat64 NumericFloat64 integerValue of
    Right (VFloat floatValue _) -> Right floatValue
    Right _ -> Left (numericConversionFloatOverflowDiagnostic BuiltinToFloat64 NumericFloat64)
    Left diagnostic -> Left diagnostic

mixedFloatArithmeticDiagnostic :: Text -> Maybe NumericType -> Maybe NumericType -> Diagnostic
mixedFloatArithmeticDiagnostic operatorSymbol leftTarget rightTarget =
  runtimeDiagnostic
    "E3007"
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' cannot mix "
        <> renderFloatOperandTarget leftTarget
        <> " and "
        <> renderFloatOperandTarget rightTarget
    )

renderFloatOperandTarget :: Maybe NumericType -> Text
renderFloatOperandTarget maybeTarget =
  case maybeTarget of
    Just targetType -> renderNumericTypeName targetType
    Nothing -> "Float"

runtimeFloatArithmeticOverflowDiagnostic :: Text -> NumericType -> Diagnostic
runtimeFloatArithmeticOverflowDiagnostic operatorSymbol targetType =
  runtimeDiagnostic
    "E3025"
    ( "runtime primitive '"
        <> operatorSymbol
        <> "' failed: value cannot be represented as finite "
        <> renderNumericTypeName targetType
    )

evalStructuralEquality :: Text -> RuntimeValue -> RuntimeValue -> Either Diagnostic RuntimeValue
evalStructuralEquality operatorSymbol leftValue rightValue =
  if runtimeValueContainsFunction leftValue || runtimeValueContainsFunction rightValue
    then Left (runtimeCallableEqualityDiagnostic operatorSymbol leftValue rightValue)
    else
      case runtimeStructuralEquality leftValue rightValue of
        Just equalityResult ->
          Right
            ( VBool
                ( if operatorSymbol == "!="
                    then not equalityResult
                    else equalityResult
                )
            )
        Nothing ->
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

runtimeValueContainsFunction :: RuntimeValue -> Bool
runtimeValueContainsFunction value =
  isFunctionValue value
    || runtimeContainerContainsFunction value
  where
    runtimeContainerContainsFunction runtimeValue =
      case runtimeValue of
        VList elements _ ->
          any runtimeValueContainsFunction elements
        VTuple elements ->
          any runtimeValueContainsFunction elements
        VConstructor _ _ _ _ capturedArgs ->
          any runtimeValueContainsFunction capturedArgs
        VTyped _ innerValue ->
          runtimeValueContainsFunction innerValue
        _ ->
          False

runtimeStructuralEquality :: RuntimeValue -> RuntimeValue -> Maybe Bool
runtimeStructuralEquality leftValue rightValue =
  case (leftValue, rightValue) of
    (VTyped leftTypeHint leftInnerValue, VTyped rightTypeHint rightInnerValue)
      | runtimeConstraintTypesCompatible leftTypeHint rightTypeHint ->
          runtimeStructuralEquality leftInnerValue rightInnerValue
      | otherwise ->
          Just False
    (VTyped _ leftInnerValue, _) ->
      runtimeStructuralEquality leftInnerValue rightValue
    (_, VTyped _ rightInnerValue) ->
      runtimeStructuralEquality leftValue rightInnerValue
    (VInt leftInt leftMetadata, VInt rightInt rightMetadata) ->
      runtimeIntegerStructuralEquality leftInt leftMetadata rightInt rightMetadata
    (VFloat leftFloat leftMetadata, VFloat rightFloat rightMetadata) ->
      runtimeFloatStructuralEquality leftFloat leftMetadata rightFloat rightMetadata
    (VBool leftBool, VBool rightBool) -> Just (leftBool == rightBool)
    (VList leftElements _, VList rightElements _) ->
      structuralElementEquality leftElements rightElements
    (VTuple leftElements, VTuple rightElements) ->
      structuralElementEquality leftElements rightElements
    ( VConstructor leftTypeName _ leftName leftConstructorArguments leftArgs,
      VConstructor rightTypeName _ rightName rightConstructorArguments rightArgs
      )
      | constructorIsSaturated leftConstructorArguments leftArgs,
        constructorIsSaturated rightConstructorArguments rightArgs,
        leftTypeName == rightTypeName,
        leftName == rightName,
        leftConstructorArguments == rightConstructorArguments ->
          structuralElementEquality leftArgs rightArgs
      | constructorIsSaturated leftConstructorArguments leftArgs,
        constructorIsSaturated rightConstructorArguments rightArgs ->
          Just False
    _ -> Nothing

structuralElementEquality :: [RuntimeValue] -> [RuntimeValue] -> Maybe Bool
structuralElementEquality leftElements rightElements
  | length leftElements /= length rightElements =
      Just False
  | otherwise =
      fmap and
        (traverse (uncurry runtimeStructuralEquality) (zip leftElements rightElements))

evalIntegerPredicate :: Text -> Integer -> RuntimeIntMetadata -> Integer -> RuntimeIntMetadata -> Bool -> Either Diagnostic RuntimeValue
evalIntegerPredicate operatorSymbol leftInt leftMetadata rightInt rightMetadata predicateResult =
  case runtimeIntegerMetadataCompatible leftInt leftMetadata rightInt rightMetadata of
    True ->
      Right (VBool predicateResult)
    False ->
      Left
        ( runtimeDiagnostic
            "E3007"
            ( "runtime primitive '"
                <> operatorSymbol
                <> "' cannot compare "
                <> renderIntegerOperandTarget (runtimeIntTargetType leftMetadata)
                <> " and "
                <> renderIntegerOperandTarget (runtimeIntTargetType rightMetadata)
            )
        )

evalIntegerEquality :: Text -> Integer -> RuntimeIntMetadata -> Integer -> RuntimeIntMetadata -> Either Diagnostic RuntimeValue
evalIntegerEquality operatorSymbol leftInt leftMetadata rightInt rightMetadata =
  case runtimeIntegerMetadataCompatible leftInt leftMetadata rightInt rightMetadata of
    True ->
      Right
        ( VBool
            ( if operatorSymbol == "!="
                then leftInt /= rightInt
                else leftInt == rightInt
            )
        )
    False ->
      Left
        ( runtimeDiagnostic
            "E3007"
            ( "runtime primitive '"
                <> operatorSymbol
                <> "' cannot compare "
                <> renderIntegerOperandTarget (runtimeIntTargetType leftMetadata)
                <> " and "
                <> renderIntegerOperandTarget (runtimeIntTargetType rightMetadata)
            )
        )

evalFloatPredicate :: Text -> RuntimeFloatMetadata -> RuntimeFloatMetadata -> Bool -> Either Diagnostic RuntimeValue
evalFloatPredicate operatorSymbol leftMetadata rightMetadata predicateResult =
  if runtimeFloatMetadataCompatible leftMetadata rightMetadata
    then Right (VBool predicateResult)
    else
      Left
        ( runtimeDiagnostic
            "E3007"
            ( "runtime primitive '"
                <> operatorSymbol
                <> "' cannot compare "
                <> renderFloatOperandTarget (runtimeFloatTargetType leftMetadata)
                <> " and "
                <> renderFloatOperandTarget (runtimeFloatTargetType rightMetadata)
            )
        )

runtimeIntegerStructuralEquality :: Integer -> RuntimeIntMetadata -> Integer -> RuntimeIntMetadata -> Maybe Bool
runtimeIntegerStructuralEquality leftInt leftMetadata rightInt rightMetadata =
  if runtimeIntegerMetadataCompatible leftInt leftMetadata rightInt rightMetadata
    then Just (leftInt == rightInt)
    else Nothing

runtimeFloatStructuralEquality :: Double -> RuntimeFloatMetadata -> Double -> RuntimeFloatMetadata -> Maybe Bool
runtimeFloatStructuralEquality leftFloat leftMetadata rightFloat rightMetadata =
  if runtimeFloatMetadataCompatible leftMetadata rightMetadata
    then Just (leftFloat == rightFloat)
    else Nothing

runtimeIntegerMetadataCompatible :: Integer -> RuntimeIntMetadata -> Integer -> RuntimeIntMetadata -> Bool
runtimeIntegerMetadataCompatible leftInt leftMetadata rightInt rightMetadata =
  case (runtimeIntTargetType leftMetadata, runtimeIntTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget) ->
      leftTarget == rightTarget
    (Just leftTarget, Nothing) ->
      integerValueMatchesTarget leftTarget rightInt
    (Nothing, Just rightTarget) ->
      integerValueMatchesTarget rightTarget leftInt
    (Nothing, Nothing) ->
      True

runtimeFloatMetadataCompatible :: RuntimeFloatMetadata -> RuntimeFloatMetadata -> Bool
runtimeFloatMetadataCompatible leftMetadata rightMetadata =
  case (runtimeFloatTargetType leftMetadata, runtimeFloatTargetType rightMetadata) of
    (Just leftTarget, Just rightTarget) ->
      leftTarget == rightTarget
    (Just NumericFloat64, Nothing) ->
      True
    (Nothing, Just NumericFloat64) ->
      True
    (Nothing, Nothing) ->
      True
    _ ->
      False

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
    VInt _ metadata ->
      case runtimeIntTargetType metadata of
        Just targetType -> renderNumericTypeName targetType
        Nothing -> "Int"
    VFloat {} -> "Float"
    VBool {} -> "Bool"
    VList {} -> "List"
    VTuple {} -> "Tuple"
    VSectionLeft {} -> "Function"
    VSectionRight {} -> "Function"
    VClosure {} -> "Function"
    VBuiltin {} -> "Function"
    VOperator {} -> "Function"
    VConstructor _ _ _ constructorArguments capturedArgs
      | constructorIsSaturated constructorArguments capturedArgs -> "Data"
      | otherwise -> "Function"
    VQualifiedMethod {} -> "Function"
    VTyped _ innerValue -> renderRuntimeType innerValue

constructorIsSaturated :: [DataConstructorArgument] -> [RuntimeValue] -> Bool
constructorIsSaturated constructorArguments capturedArgs =
  length capturedArgs >= length constructorArguments

extendBoundWithPattern :: Pattern -> Set Text -> Set Text
extendBoundWithPattern pattern bound =
  Set.union bound (patternBoundNames pattern)

patternBoundNames :: Pattern -> Set Text
patternBoundNames pattern =
  case pattern of
    PVariable name -> Set.singleton (identifierText name)
    PWildcard -> Set.empty
    PLiteral {} -> Set.empty
    PConstructor _ patterns ->
      Set.unions (map patternBoundNames patterns)
    PList patterns ->
      Set.unions (map patternBoundNames patterns)
    PConsList headPattern tailPattern ->
      Set.union (patternBoundNames headPattern) (patternBoundNames tailPattern)
    PTuple patterns ->
      Set.unions (map patternBoundNames patterns)
    PAs name pattern ->
      Set.insert (identifierText name) (patternBoundNames pattern)
    POr alternatives ->
      commonPatternBoundNames alternatives

commonPatternBoundNames :: [Pattern] -> Set Text
commonPatternBoundNames alternatives =
  case alternatives of
    [] -> Set.empty
    firstAlternative : rest ->
      foldl'
        Set.intersection
        (patternBoundNames firstAlternative)
        (map patternBoundNames rest)
