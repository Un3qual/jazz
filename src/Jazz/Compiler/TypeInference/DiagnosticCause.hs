{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Inference error causes independent of solver state and source-name ownership.
-- Type names are projected to their diagnostic spelling at the reporting boundary;
-- variable identities survive until all types in one cause are rendered together.
module Jazz.Compiler.TypeInference.DiagnosticCause
  ( DiagnosticType,
    TypeErrorCause (..),
    renderTypeErrorCause,
    renderDiagnosticType,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Jazz.Compiler.BuiltinCatalog (renderNumericTypeName)
import Jazz.Compiler.TypeRepresentation (InferenceVariable, SemanticType (..))

type DiagnosticType = SemanticType Text InferenceVariable

data TypeErrorCause typeValue
  = SignatureTypeMismatch Text typeValue typeValue
  | RecursiveBindingTypeMismatch Text typeValue typeValue
  | ImplMethodTypeMismatch Text typeValue typeValue
  | ApplicationTypeMismatch typeValue typeValue
  | ListElementTypeMismatch typeValue typeValue
  | IfBranchTypeMismatch typeValue typeValue
  | UnsatisfiedNumericConstraint typeValue
  | UnsatisfiedStrictEqualityConstraint typeValue
  | NoMatchingMethodArguments Text [typeValue]
  | UndeclaredSignatureConstraint Text Bool Text typeValue
  | AmbiguousDeferredConstraint Text typeValue
  | PatternTypeMismatch typeValue typeValue
  | ListPatternTypeMismatch typeValue
  | TuplePatternTypeMismatch typeValue
  | PatternBranchTypeMismatch typeValue typeValue
  | IfConditionTypeMismatch typeValue
  | CaseGuardTypeMismatch typeValue
  | OrPatternBinderTypeMismatch Text typeValue typeValue
  deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData)

renderTypeErrorCause :: TypeErrorCause DiagnosticType -> Text
renderTypeErrorCause cause = renderCause (evalState (traverse (traverse rename) cause) Map.empty)
  where
    rename :: InferenceVariable -> State (Map.Map InferenceVariable InferenceVariable) InferenceVariable
    rename variable = do
      names <- get
      case Map.lookup variable names of
        Just name -> pure name
        Nothing -> do
          let name = fromIntegral (Map.size names)
          put (Map.insert variable name names)
          pure name

    renderCause normalized = case normalized of
      SignatureTypeMismatch binding expected actual ->
        "binding '" <> binding <> "' declared as " <> render expected <> " but inferred as " <> render actual
      RecursiveBindingTypeMismatch binding expected actual ->
        "binding '" <> binding <> "' is used recursively as type " <> render expected <> " but its definition inferred " <> render actual
      ImplMethodTypeMismatch method expected actual ->
        "impl method '" <> method <> "' declared as " <> render expected <> " but inferred as " <> render actual
      ApplicationTypeMismatch function argument ->
        "cannot apply function of type " <> render function <> " to argument of type " <> render argument
      ListElementTypeMismatch expected actual ->
        "list literal elements must have matching types, found " <> render expected <> " and " <> render actual
      IfBranchTypeMismatch expected actual ->
        "if branches must have matching types, found " <> render expected <> " and " <> render actual
      UnsatisfiedNumericConstraint found ->
        "primitive numeric constraint cannot be satisfied by " <> render found
      UnsatisfiedStrictEqualityConstraint found ->
        "primitive strict equality constraint cannot be satisfied by " <> render found
      NoMatchingMethodArguments key arguments ->
        "no matching qualified method body '" <> key <> "' for argument types " <> renderTypes arguments
      UndeclaredSignatureConstraint binding primitive name argument ->
        "signature for '"
          <> binding
          <> "' does not declare required "
          <> (if primitive then "primitive " else "")
          <> "constraint '"
          <> name
          <> "("
          <> render argument
          <> ")'"
      AmbiguousDeferredConstraint name argument ->
        let constraint = name <> "(" <> render argument <> ")"
         in "ambiguous/defaulting explicit constraint '" <> constraint <> "': explicit constrained signatures do not default unresolved type variables"
      PatternTypeMismatch patternType scrutinee ->
        "case pattern of type " <> render patternType <> " does not match scrutinee type " <> render scrutinee
      ListPatternTypeMismatch scrutinee ->
        "case pattern of list type does not match scrutinee type " <> render scrutinee
      TuplePatternTypeMismatch scrutinee ->
        "tuple case pattern does not match scrutinee type " <> render scrutinee
      PatternBranchTypeMismatch left right ->
        "case arms must have matching types, found " <> render left <> " and " <> render right
      IfConditionTypeMismatch found ->
        "if condition must have type Bool, found " <> render found
      CaseGuardTypeMismatch found ->
        "case guard must have type Bool, found " <> render found
      OrPatternBinderTypeMismatch name left right ->
        "or-pattern binder '" <> name <> "' has incompatible types " <> render left <> " and " <> render right

    renderTypes = Text.intercalate ", " . map render

    render = renderDiagnosticType

-- | Render type syntax; callers choose whether variable identities were renamed.
renderDiagnosticType :: DiagnosticType -> Text
renderDiagnosticType = render
  where
    render typeValue = case typeValue of
      SemanticInt -> "Int"
      SemanticFloat -> "Float"
      SemanticNumeric numeric -> renderNumericTypeName numeric
      SemanticBool -> "Bool"
      SemanticChar -> "Char"
      SemanticText -> "Text"
      SemanticList element -> "[" <> render element <> "]"
      SemanticTuple elements -> "(" <> renderMany elements <> ")"
      SemanticData name [] -> name
      SemanticData name arguments -> name <> "<" <> renderMany arguments <> ">"
      SemanticFunction argument result -> renderAtom argument <> " -> " <> render result
      SemanticVariable variable -> "t" <> Text.pack (show variable)
      SemanticListConstructor -> "List"
      SemanticNamedConstructor name -> name
      SemanticApplication constructor argument -> render constructor <> "(" <> render argument <> ")"
    renderMany = Text.intercalate ", " . map render
    renderAtom typeValue = case typeValue of
      SemanticFunction {} -> "(" <> render typeValue <> ")"
      _ -> render typeValue
