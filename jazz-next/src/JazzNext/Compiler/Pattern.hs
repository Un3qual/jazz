-- | Shared binder semantics for lowered core patterns.
module JazzNext.Compiler.Pattern
  ( commonPatternBinderNames,
    extendBoundWithPattern,
    patternBinderNames
  ) where

import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import JazzNext.Compiler.AST (Pattern (..))
import JazzNext.Compiler.Name (Name)

extendBoundWithPattern :: Pattern -> Set Name -> Set Name
extendBoundWithPattern patternValue bound =
  Set.union bound (patternBinderNames patternValue)

commonPatternBinderNames :: [Pattern] -> Set Name
commonPatternBinderNames alternatives =
  case alternatives of
    [] -> Set.empty
    firstAlternative : rest ->
      foldl'
        Set.intersection
        (patternBinderNames firstAlternative)
        (map patternBinderNames rest)

patternBinderNames :: Pattern -> Set Name
patternBinderNames patternValue =
  case patternValue of
    PVariable name -> Set.singleton name
    PWildcard -> Set.empty
    PLiteral {} -> Set.empty
    PConstructor _ patterns ->
      Set.unions (map patternBinderNames patterns)
    PList patterns ->
      Set.unions (map patternBinderNames patterns)
    PConsList headPattern tailPattern ->
      Set.union (patternBinderNames headPattern) (patternBinderNames tailPattern)
    PTuple patterns ->
      Set.unions (map patternBinderNames patterns)
    PAs name nestedPattern ->
      Set.insert name (patternBinderNames nestedPattern)
    POr alternatives ->
      commonPatternBinderNames alternatives
