{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Shared binder semantics for canonical core patterns.
module Jazz.Compiler.Pattern
  ( commonPatternBinderNames,
    extendBoundWithPattern,
    patternBinderNames,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Jazz.Compiler.AST (CoreNameAt, Pattern (..))

extendBoundWithPattern :: (Ord (CoreNameAt phase)) => Pattern phase -> Set (CoreNameAt phase) -> Set (CoreNameAt phase)
extendBoundWithPattern patternValue bound =
  Set.union bound (patternBinderNames patternValue)

commonPatternBinderNames :: (Ord (CoreNameAt phase)) => [Pattern phase] -> Set (CoreNameAt phase)
commonPatternBinderNames alternatives =
  case alternatives of
    [] -> Set.empty
    firstAlternative : rest ->
      foldl'
        Set.intersection
        (patternBinderNames firstAlternative)
        (map patternBinderNames rest)

patternBinderNames :: (Ord (CoreNameAt phase)) => Pattern phase -> Set (CoreNameAt phase)
patternBinderNames patternValue =
  case patternValue of
    PVariable _ name -> Set.singleton name
    PWildcard _ -> Set.empty
    PLiteral {} -> Set.empty
    PConstructor _ _ patterns ->
      Set.unions (map patternBinderNames patterns)
    PList _ patterns ->
      Set.unions (map patternBinderNames patterns)
    PConsList _ headPattern tailPattern ->
      Set.union (patternBinderNames headPattern) (patternBinderNames tailPattern)
    PTuple _ patterns ->
      Set.unions (map patternBinderNames patterns)
    PAs _ name nestedPattern ->
      Set.insert name (patternBinderNames nestedPattern)
    POr _ alternatives ->
      commonPatternBinderNames alternatives
