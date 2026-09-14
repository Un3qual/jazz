{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Declaration-local kind inference. Solved metadata never contains variables.
module Jazz.Compiler.KindInference (inferDataKinds, inferSignatureKinds) where

import Control.Monad (foldM, replicateM, unless, zipWithM_, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify')
import Data.Bifoldable (bifoldMap)
import Data.Bifunctor (first, second)
import Data.Foldable (toList)
import Data.Graph (flattenSCC, stronglyConnComp)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void, absurd)
import Jazz.Compiler.Name (ResolvedName, identifierText)
import Jazz.Compiler.TypeRepresentation (Kind (..), SemanticType (..))

newtype KindVariableId = KindVariableId Int deriving (Eq, Ord)

type KindCheck = StateT (Int, Map KindVariableId (Kind KindVariableId)) (Either Text)

freshKind :: KindCheck (Kind KindVariableId)
freshKind = do
  (next, _) <- get
  modify' (\(_, substitution) -> (next + 1, substitution))
  pure (KindVariable (KindVariableId next))

resolveKind :: Kind KindVariableId -> KindCheck (Kind KindVariableId)
resolveKind kind = case kind of
  TypeKind -> pure TypeKind
  FunctionKind argument result -> FunctionKind <$> resolveKind argument <*> resolveKind result
  KindVariable variable -> do
    (_, substitution) <- get
    maybe (pure kind) resolveKind (Map.lookup variable substitution)

unifyKinds :: Kind KindVariableId -> Kind KindVariableId -> KindCheck ()
unifyKinds left right = do
  resolvedLeft <- resolveKind left
  resolvedRight <- resolveKind right
  unless (resolvedLeft == resolvedRight) $ case (resolvedLeft, resolvedRight) of
    (KindVariable variable, replacement) -> bind variable replacement
    (replacement, KindVariable variable) -> bind variable replacement
    (FunctionKind leftArgument leftResult, FunctionKind rightArgument rightResult) ->
      unifyKinds leftArgument rightArgument >> unifyKinds leftResult rightResult
    _ -> lift (Left "a complete type and a type constructor are not interchangeable")
  where
    bind variable replacement
      | variable `elem` replacement = lift (Left "infinite constructor kind")
      | otherwise = modify' (second (Map.insert variable replacement))

fixedKind :: Kind KindVariableId -> KindCheck (Kind Void)
fixedKind kind = defaultKind <$> resolveKind kind
  where
    defaultKind TypeKind = TypeKind
    defaultKind (FunctionKind argument result) = FunctionKind (defaultKind argument) (defaultKind result)
    defaultKind KindVariable {} = TypeKind

typeKind :: (Ord variable) => Map ResolvedName (Kind KindVariableId) -> Map variable (Kind KindVariableId) -> SemanticType ResolvedName variable -> KindCheck (Kind KindVariableId)
typeKind constructors variables = infer
  where
    infer expression = case expression of
      SemanticVariable variable -> maybe (lift (Left "unbound kind variable")) pure (Map.lookup variable variables)
      SemanticNamedConstructor name -> maybe (lift (Left ("unknown type constructor '" <> identifierText name <> "'"))) pure (Map.lookup name constructors)
      SemanticListConstructor -> pure (FunctionKind TypeKind TypeKind)
      SemanticApplication constructor argument -> do
        constructorKind <- infer constructor
        argumentKind <- infer argument
        resultKind <- freshKind
        unifyKinds constructorKind (FunctionKind argumentKind resultKind)
        pure resultKind
      SemanticFunction argument result -> complete argument >> complete result >> pure TypeKind
      SemanticTuple elements -> mapM_ complete elements >> pure TypeKind
      _ -> pure TypeKind
    complete expression = infer expression >>= unifyKinds TypeKind

inferSignatureKinds :: (Ord variable) => Map ResolvedName (Kind Void) -> Map variable (Kind Void) -> [SemanticType ResolvedName variable] -> Either Text (Map variable (Kind Void))
inferSignatureKinds constructors known expressions = flip evalStateT (0, Map.empty) $ do
  variables <- traverse (const freshKind) (Map.fromSet (const ()) (Set.fromList (concatMap toList expressions) <> Map.keysSet known))
  mapM_ (\(variable, kind) -> maybe (pure ()) (unifyKinds (fmap absurd kind)) (Map.lookup variable variables)) (Map.toList known)
  mapM_ (typeKind (fmap (fmap absurd) constructors) variables >=> unifyKinds TypeKind) expressions
  traverse fixedKind variables

inferDataKinds :: Map ResolvedName (Kind Void) -> [(ResolvedName, [Text], [SemanticType ResolvedName Text])] -> Either (ResolvedName, Text) (Map ResolvedName [Kind Void])
inferDataKinds existing declarations = foldM inferGroup Map.empty groups
  where
    groups = map flattenSCC (stronglyConnComp [(declaration, name, Set.toList (foldMap (bifoldMap Set.singleton (const Set.empty)) fields)) | declaration@(name, _, fields) <- declarations])
    inferGroup solved [] = Right solved
    inferGroup solved group@((name, _, _) : _) = do
      kinds <- first (name,) (inferDataGroup (fmap (foldr FunctionKind TypeKind) solved <> existing) group)
      pure (kinds <> solved)

inferDataGroup :: Map ResolvedName (Kind Void) -> [(ResolvedName, [Text], [SemanticType ResolvedName Text])] -> Either Text (Map ResolvedName [Kind Void])
inferDataGroup existing declarations = flip evalStateT (0, Map.empty) $ do
  skeletons <- traverse (\(name, parameters, _) -> (,) name <$> replicateM (length parameters) freshKind) declarations
  let parametersByName = Map.fromList skeletons
      constructors = fmap (foldr FunctionKind TypeKind) parametersByName <> fmap (fmap absurd) existing
  mapM_ (checkDeclaration constructors parametersByName) declarations
  traverse (traverse fixedKind) parametersByName
  where
    checkDeclaration constructors parametersByName (name, parameters, fields) = do
      let kinds = Map.findWithDefault [] name parametersByName
          variables = Map.fromList (zip parameters kinds)
      fieldKinds <- traverse (typeKind constructors variables) fields
      zipWithM_ unifyKinds fieldKinds (repeat TypeKind)
